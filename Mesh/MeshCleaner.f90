!
!////////////////////////////////////////////////////////////////////////
!
!      MeshCleaner.f90
!      Created: 2011-06-06 14:19:58 -0400 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module MeshCleaner
      USE Geometry
      USE SMMeshClass
      USE MeshQualityAnalysisClass
      USE MeshBoundaryMethodsModule
      USE ConectionsModule
      USE ElementOperations
      USE InterfaceElementMethods
      IMPLICIT NONE 
!
!--------------------------------------------------------------------
!> Apply various mesh improvement techniques to eliminate bad elements
!! from a mesh.
!--------------------------------------------------------------------
!
!@mark -
      CONTAINS
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PerformTopologyCleanup( mesh )
         USE SMMeshClass
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMMesh) :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER  :: numberOfValenceChanges, numberOfDiamondsRemoved
         LOGICAL  :: valenceHasChanged, diamondsHaveBeenRemoved
         
         valenceHasChanged       = .false.
         diamondsHaveBeenRemoved = .false.
!
!        ------------------------------------
!        Reduce valence at high valence nodes
!        ------------------------------------
!
         CALL ReduceNodeValences( mesh, numberOfValenceChanges )
          
         IF ( numberOfValenceChanges > 0 )     THEN
            valenceHasChanged = .true.
            IF ( printMessage )     THEN
               PRINT *, "      Valences have been modified ", numberOfValenceChanges, " time(s)"
            END IF 
         END IF
!
!        ---------------
!        Remove diamonds
!        ---------------
!
         CALL RemoveDiamondElements( mesh, numberOfDiamondsRemoved )
         
         IF ( numberOfDiamondsRemoved > 0 )     THEN
            diamondsHaveBeenRemoved = .true.
            IF ( PrintMessage )     THEN
               PRINT *, "      Number of diamond elements removed = ", numberOfDiamondsRemoved
            END IF 
         END IF
!
!        -------------------------------------------------
!        If the topology has changed, we must generate new
!        edges
!        -------------------------------------------------
!
         IF ( valenceHasChanged .OR. diamondsHaveBeenRemoved )     THEN
            CALL renumberObjects(mesh,NODES)
            CALL mesh % syncEdges()
            CALL UnmarkNodesNearBoundaries( mesh % nodesIterator )
         END IF

      END SUBROUTINE PerformTopologyCleanup
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReduceNodeValences( mesh, numberOfValenceChanges )
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) :: mesh
         INTEGER       :: numberOfValenceChanges
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                                 :: nodeID
         INTEGER     , DIMENSION(:), ALLOCATABLE :: localNumElementsForNode

         numberOfValenceChanges = 0

!
!        -------------------------------------------------------------
!        Make current connections and keep a copy of the
!        valences since the array will be created and destroyed by the
!        cleanup routines.
!        -------------------------------------------------------------
!                  
         CALL MakeNodeToElementConnections( mesh )
         
         ALLOCATE(localNumElementsForNode(SIZE(numElementsForNode)))
         localNumElementsForNode = numElementsForNode
!
!        --------------------------------
!        Check and clean up node valences
!        --------------------------------
!        
         DO nodeID = 1, SIZE(localNumElementsForNode)
            SELECT CASE ( localNumElementsForNode(nodeID) )
               CASE( 7 )
                  CALL CleanUp7ValenceNode_InMesh(nodeID, mesh)
                  numberOfValenceChanges = numberOfValenceChanges + 1
               CASE( 8 )
                  CALL CleanUp8ValenceNode_InMesh(nodeID, mesh)
               CASE DEFAULT
                  !Do nothing
            END SELECT
         END DO
         
         DEALLOCATE(localNumElementsForNode)

      END SUBROUTINE ReduceNodeValences
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RemoveDiamondElements( mesh, numberOfDiamondsRemoved ) 
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) :: mesh
         INTEGER                :: numberOfDiamondsRemoved
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMElement)           , POINTER :: currentElement  => NULL()
         CLASS(FTLinkedListIterator), POINTER :: elementIterator => NULL()
         CLASS(FTObject)            , POINTER :: obj             => NULL()
!
         numberOfDiamondsRemoved = 0
         CALL MakeNodeToElementConnections( mesh )
!
!        -------------------------------------------
!        Go through each element and remove diamonds
!        -------------------------------------------
!
         elementIterator => mesh % elementsIterator
         CALL elementIterator % setToStart()
         
         DO WHILE ( .NOT.elementIterator % isAtEnd() )
            obj => elementIterator % object()
            CALL cast(obj,currentElement)
            
            CALL DeleteElementIfDiamond( currentElement, mesh )
            
            IF ( currentElement % remove )     THEN
               numberOfDiamondsRemoved = numberOfDiamondsRemoved + 1
            END IF
                        
            CALL elementIterator % moveToNext()
         END DO
!
!        ------------------------------------------
!        Clean up & remove deleted elements & nodes
!        ------------------------------------------
!
         CALL deallocateNodeToElementConnections()

         IF ( numberOfDiamondsRemoved > 0 )     THEN
            CALL DoLazyDelete( mesh )
         END IF
         
         
      END SUBROUTINE RemoveDiamondElements 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CleanUp7ValenceNode_InMesh(id,mesh) 
      USE ErrorTypesModule
!
!     ----------------------------------------------
!     A valence 7 node can be reduced to a valence
!     5 node by inserting a new node into one of the
!     elements and inserting a diamond element.
!     ----------------------------------------------
!
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                :: id
         TYPE (SMMesh) :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                     :: k, localID, localIDForTarget
         INTEGER                     :: j, newID
         INTEGER                     :: cnt
         REAL(KIND=RP)               :: theta, thetaMax
         INTEGER, DIMENSION(4)       :: diagonalMap = [3,4,1,2]
         REAL(KIND=RP)               :: x1(3), x2(3), x(3)
         
         CLASS(SMNode)     , POINTER :: node => NULL(), nodeForThisID => NULL()
         CLASS(SMElement)  , POINTER :: e => NULL(), eTarget => NULL()
         CLASS(SMEdge)     , POINTER :: edge => NULL()
         CLASS(FTObject)   , POINTER :: obj => NULL()
         
         TYPE(SMNodePtr), DIMENSION(4) :: elementNodes
         TYPE(SMElementPtr)            :: eNeighbors(2)
         TYPE(SMEdgePtr)               :: sharedEdgePointers(2), newEdgePointers(2)
         
         CHARACTER(LEN=ERROR_EXCEPTION_MSG_LENGTH) :: msg
!
!        ------------------------
!        Make current connections
!        ------------------------
!                  
         CALL MakeNodeToElementConnections( mesh )
         CALL MakeNodeToEdgeConnections   ( mesh )
!
!        ---------------------------------------
!        Find the element with the largest angle
!        ---------------------------------------
!
         thetaMax = 0.0_RP
         
         DO k = 1, numElementsForNode(id)
            e       => elementsForNodes(k,id) % element
            localID = ElementLocalNodeIDForNodeID( id, e )
            theta   = AngleAtLocalNode_ForElement(localID,e)
            
            IF(theta > thetaMax)     THEN
               thetaMax         = theta
               eTarget          => e
               localIDForTarget = localID
            END IF
         END DO
!
!        -----------------------------------------
!        Find the elements that flank this element
!        -----------------------------------------
!
         cnt = 0
         DO k = 1, numEdgesForNodes(id)
            edge => edgesForNodes(k,id) % edge
!
!           --------------------------------------------
!           See if this edge is along the target element
!           --------------------------------------------
!
            IF ( edge % elements(1) % element % id == eTarget % id )          THEN
               IF ( ASSOCIATED(edge % elements(2) % element) )              THEN
                  cnt                       = cnt + 1
                  eNeighbors(cnt) % element => edge % elements(2) % element
                  sharedEdgePointers(cnt) % edge => edge
               END IF
            ELSE IF ( edge % elements(2) % element % id == eTarget % id )   THEN
               IF ( ASSOCIATED(edge % elements(1) % element) )                  THEN
                  cnt                     = cnt + 1
                  eNeighbors(cnt) % element => edge % elements(1) % element
                  sharedEdgePointers(cnt) % edge => edge
              END IF
            END IF
            IF(cnt == 2) EXIT
         END DO
!
!        --------------------------------------------------------
!        If there aren't two neighbors, then something is wrong. 
!        Bail when this happens.
!        --------------------------------------------------------
!
         IF( cnt /= 2 )     THEN
            obj => eTarget % nodes % objectAtIndex(localIDForTarget)
            CALL cast(obj,node)
               WRITE(msg,*) "Bad element topology for node at ", node % x, ". Aborting cleanup."
               CALL ThrowErrorExceptionOfType("CleanUp7ValenceNode_InMesh",msg,FT_ERROR_WARNING)
            CALL deallocateNodeToEdgeConnections()
            CALL deallocateNodeToElementConnections()
            RETURN
         END IF
!
!        -------------------------------------------------------------
!        Find the edges that are in the two neighbor elements, but are
!        not the shared edges.
!        -------------------------------------------------------------
!
         cnt = 0
         DO k = 1, numEdgesForNodes(id)
            edge => edgesForNodes(k,id) % edge
            IF ( ASSOCIATED(edge,sharedEdgePointers(1) % edge) .OR. &
                 ASSOCIATED(edge,sharedEdgePointers(2) % edge))              CYCLE
!
!           ------------------------------------------------------
!           See if this edge is along one of the neighbor elements
!           ------------------------------------------------------
!
            DO j = 1, 2
               IF ( edge % elements(1) % element % id == eNeighbors(j) % element % id .OR. &
                  ( edge % elements(2) % element % id == eNeighbors(j) % element % id )) THEN
                  cnt                       = cnt + 1
                  newEdgePointers(cnt) % edge => edge
               END IF
            END DO
            IF(cnt == 2) EXIT
         END DO
         IF( cnt /= 2 )     THEN
            obj => eTarget % nodes % objectAtIndex(localIDForTarget)
            CALL cast(obj,node)
            PRINT *, "Bad element topology in CleanUp7ValenceNode_InMesh for node at ", node % x
            CALL deallocateNodeToEdgeConnections()
            CALL deallocateNodeToElementConnections()
            RETURN
         END IF
!
!        -------------------------------------------
!        Create a new node inside the target element      
!        -------------------------------------------
!
         obj => eTarget % nodes % objectAtIndex(localIDForTarget)
         CALL cast(obj,node)
         
         nodeForThisID => node ! save pointer before overwriting
         
         x1 = node % x
         
         obj => eTarget % nodes % objectAtIndex(diagonalMap(localIDForTarget))
         CALL cast(obj,node)
         x2 = node % x
         x  = 0.5_RP*(x1 + x2)
         
         ALLOCATE(node)
         CALL node % initWithLocationAndID(x,mesh % newNodeID())
         obj               => node
         CALL mesh % nodes % add(obj)
         CALL node % release()
!
!        --------------------------------------------
!        Change the corners to point to this new node
!        --------------------------------------------
!
         CALL eTarget % nodes % replaceObjectAtIndexWithObject(localIDForTarget,obj)
         
         localID = ElementLocalNodeIDForNodeID( id, eNeighbors(1) % element )
         CALL eNeighbors(1) % element % nodes % replaceObjectAtIndexWithObject(localID,obj)

         localID = ElementLocalNodeIDForNodeID( id, eNeighbors(2) % element )
         CALL eNeighbors(2) % element % nodes % replaceObjectAtIndexWithObject(localID,obj)
!
!        --------------------
!        Create a new element
!        --------------------
!
         elementNodes(1) % node => nodeForThisID
         
         IF ( newEdgePointers(1) % edge % nodes(1) % node % id == id )     THEN
            elementNodes(2) % node => newEdgePointers(1) % edge % nodes(2) % node
         ELSE
            elementNodes(2) % node => newEdgePointers(1) % edge % nodes(1) % node
         END IF
         
         elementNodes(3) % node => node

         IF ( newEdgePointers(2) % edge % nodes(1) % node % id == id )     THEN
            elementNodes(4) % node => newEdgePointers(2) % edge % nodes(2) % node
         ELSE
            elementNodes(4) % node => newEdgePointers(2) % edge % nodes(1) % node
         END IF
         
         newID = mesh % newElementID()
         ALLOCATE(e)
         CALL e % initWithNodesIDAndType( elementNodes, newID, QUAD )
         obj => e
         CALL mesh % elements % add(obj)
         CALL e % release()
!
         CALL deallocateNodeToEdgeConnections()
         CALL deallocateNodeToElementConnections()
         
      END SUBROUTINE CleanUp7ValenceNode_InMesh 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CleanUp8ValenceNode_InMesh(id,mesh) 
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                :: id
         TYPE (SMMesh) :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
!         INTEGER                  :: k
!         CLASS(SMElement), POINTER :: e
!         REAL(KIND=RP)            :: theta
!
!        ----------------------------------------
!        Find the element with the smallest angle
!        ----------------------------------------
!
         
      END SUBROUTINE CleanUp8ValenceNode_InMesh 
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PerformFinalMeshCleanup( mesh , model ) 
         USE SMModelClass
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMMesh) , POINTER :: mesh
         CLASS(SMModel), POINTER :: model
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTMutableObjectArray) , POINTER     :: badElements => NULL()
         CLASS(SMElement)            , POINTER     :: e => NULL()
         CLASS(FTObject)             , POINTER     :: obj => NULL()
         REAL(KIND=RP)               , ALLOCATABLE :: shapeMeasures(:,:)
         LOGICAL                     , ALLOCATABLE :: badElementMeasure(:,:)
         INTEGER                                   :: numberOfBadElements
         INTEGER                                   :: k
         INTEGER                                   :: numberOfChevrons
!
!        -----------------------
!        Gather the bad elements
!        -----------------------
!
         badElements => BadElementsInMesh( mesh )
!
!        -----------------------------------------------
!        If there are no bad elements, clean up around
!        the boundaryies anyway. Otherwise go around and
!        clean up the bad interior elements.
!        -----------------------------------------------
!
         IF( .NOT.ASSOCIATED(badElements) )     THEN
!
!           --------------------------
!           Clean up boundary elements
!           --------------------------
!
            CALL CleanUpBoundaryElements( mesh, model )
         ELSE
!
!           -----------------------
!           Save the shape measures
!           -----------------------
!
            numberOfBadElements = badElements % COUNT()
            ALLOCATE( shapeMeasures    ( NUMBER_OF_SHAPE_MEASURES, numberOfBadElements ) )
            ALLOCATE( badElementMeasure( NUMBER_OF_SHAPE_MEASURES, numberOfBadElements ) )
            
            DO k = 1, numberOfBadElements
               obj => badElements % objectAtIndex(k)
               CALL cast(obj,e)
               CALL ComputeElementShapeMeasures( e, shapeMeasures(:,k) )
               CALL ExtractBadElementInfo( shapeMeasures(:,k), badElementMeasure(:,k) )
            END DO
!
!           -----------
!           Fix lefties
!           -----------
!
            DO k = 1, numberOfBadElements
               obj => badElements % objectAtIndex(k)
               CALL cast(obj,e)
               IF( shapeMeasures(AREA_SIGN,k ) < 0.0_RP ) CALL MakeElement_RightHanded( e )
            END DO
!
!           -------------------------------
!           Fix triangular/chevron elements
!           -------------------------------
!
            CALL MakeNodeToElementConnections( mesh )
            CALL CleanUpChevronElements( badElements, shapeMeasures, numberOfChevrons )
            CALL deallocateNodeToElementConnections
!
!           ---------------
!           Do lazy deletes
!           ---------------
!
            CALL badElements % release()
            DEALLOCATE( badElements )
            DEALLOCATE( shapeMeasures, badElementMeasure )
            
            IF ( numberOfChevrons > 0 )     THEN
               CALL DoLazyDelete( mesh )
               CALL mesh % syncEdges()
            END IF 
!
!           -------------------------------------------
!           Bail if the mesh is so bad that it can't be
!           fixed
!           -------------------------------------------
!
!            badElements => BadElementsInMesh( mesh )
!            IF(ASSOCIATED(badElements))     THEN 
!               DO k = 1, numberOfBadElements
!                  obj => badElements % objectAtIndex(k)
!                  CALL cast(obj,e)
!                  IF( shapeMeasures(ASPECT_RATIO_INDEX,k ) < 0.0_RP .OR. &
!                      shapeMeasures(JACOBIAN_INDEX,k) <= 0.0_RP     .OR. &
!                      shapeMeasures(CONDITION_INDEX,k) > 1000.0_RP) THEN
!                      PRINT *,shapeMeasures(ASPECT_RATIO_INDEX,k ),&
!                      shapeMeasures(JACOBIAN_INDEX,k),&
!                      shapeMeasures(CONDITION_INDEX,k)
!                      PRINT *
!                      PRINT *, "     **************************************************************"
!                      PRINT *, "     The mesh generated appears to be so bad that I cannot fix it."
!                      PRINT *, "     Try choosing a smaller mesh size and try again"
!                      PRINT *, "     **************************************************************"
!                      PRINT *
!                      CALL badElements % release()
!                      DEALLOCATE(badElements)
!                      DEALLOCATE(shapeMeasures, badElementMeasure)
!                      RETURN 
!                  END IF 
!               END DO
!            END IF 
!
!           --------------------------
!           Clean up boundary elements
!           --------------------------
!
            CALL UnmarkNodesNearBoundaries( mesh % nodesIterator )
            CALL CleanUpBoundaryElements( mesh, model )
         END IF
         
         
      END SUBROUTINE PerformFinalMeshCleanup
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MakeElement_RightHanded( e ) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMElement) :: e
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject), POINTER :: obj2 => NULL(), obj4 => NULL()      
         
         obj2 => e % nodes % objectAtIndex(2)
         obj4 => e % nodes % objectAtIndex(4)
         CALL obj2 % retain()
         CALL obj4 % retain()
         
         CALL e % nodes % replaceObjectAtIndexWithObject(2,obj4)
         CALL e % nodes % replaceObjectAtIndexWithObject(4,obj2)
         
         CALL obj2 % release()
         CALL obj4 % release()
         
      END SUBROUTINE MakeElement_RightHanded
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CleanUpBoundaryElements( mesh, model ) 
         USE SMModelClass
         USE ConectionsModule
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMMesh) , POINTER :: mesh
         CLASS(SMModel), POINTER :: model
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                              :: numBoundaries
         INTEGER                              :: j, k, nodeCount
         CLASS(FTLinkedList)        , POINTER :: currentEdgeList => NULL()
         CLASS(FTLinkedList)        , POINTER :: interfaceElements => NULL()
         CLASS(SMEDGE)              , POINTER :: currentEdge => NULL()
         CLASS(SMElement)           , POINTER :: e => NULL()
         CLASS(SMNode)              , POINTER :: elementNode => NULL(), meshNode => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()
         TYPE (FTLinkedListIterator)          :: edgeListIterator
         CLASS(FTLinkedListIterator), POINTER :: nodesIterator => NULL()
         
         numBoundaries = model % numberOfInnerCurves &
                       + model % numberOfOuterCurves &
                       + model % numberOfInterfaceCurves
         IF (numBoundaries == 0)     RETURN
!
!        ------------------------------------------------
!        Gather boundary Edges, etc in the following call
!        ------------------------------------------------
!
         CALL SetNodeActiveStatus(mesh,model)
!
!        -----------------------------------------------
!        Run through physical boundary elements and make
!        the boundary angles 90 degrees.
!        -----------------------------------------------
!
         CALL makeNodeToElementConnections(mesh)
         DO j = 1, numBoundaries
            IF( boundaryEdgesType(j) == INTERFACE_EDGES ) CYCLE
            
            obj  => boundaryEdgesArray % objectAtIndex(j)
            CALL cast(obj,currentEdgeList)
            
            CALL edgeListIterator % initWithFTLinkedList(currentEdgeList)
            CALL edgeListIterator % setToStart()
            
            DO WHILE ( .NOT.EdgeListIterator % isAtEnd() )
               obj => EdgeListIterator % object()
               CALL cast(obj,currentEdge)
               IF ( currentEdge % edgeType == ON_BOUNDARY )     THEN
                  e   => currentEdge % elements(1) % element
!               BUG: puts boundary points in the wrong place.
                  CALL CleanUpBoundaryElement( e, model )
               END IF 

               CALL EdgeListIterator % moveToNext()
            END DO
            
            CALL edgeListIterator % release()
         END DO
         CALL deallocateNodeToEdgeConnections
!
!        -------------------------------------------
!        Run through the interface elements
!        and split them as necessary
!        -------------------------------------------
!
         IF ( model % numberOfInterfaceCurves > 0 )     THEN
         
           ALLOCATE(interfaceElements)
           CALL interfaceElements % init()
!
!          ----------------------------------------
!          Gather all elements along the interfaces
!          into a list of elements.
!          ----------------------------------------
!
           DO j = 1, numBoundaries
               IF( boundaryEdgesType(j) == BOUNDARY_EDGES ) CYCLE
               
               obj  => boundaryEdgesArray % objectAtIndex(j)
               CALL cast(obj,currentEdgeList)
               
               CALL edgeListIterator % initWithFTLinkedList(currentEdgeList)
               CALL edgeListIterator % setToStart()
               
               DO WHILE ( .NOT.EdgeListIterator % isAtEnd() )
                  obj => EdgeListIterator % object()
                  CALL cast(obj,currentEdge)
                  
                  IF ( currentEdge % edgeType == ON_INTERFACE )     THEN
                  
                     e   => currentEdge % elements(1) % element
                     obj => e
                     CALL interfaceElements % add(obj)
                     
                     e   => currentEdge % elements(2) % element
                     obj => e
                     CALL interfaceElements % add(obj)
                     
                  END IF
                  
                  CALL EdgeListIterator % moveToNext()
               END DO
               
               CALL edgeListIterator % release()
           END DO
!
!          ----------------------------------------------------
!          It is also possible that a single node of an element
!          falls on the interface boundary.
!          ----------------------------------------------------
!
            CALL makeNodeToElementConnections(mesh)
            
            nodesIterator => mesh % nodesIterator
            CALL nodesIterator % setToStart()
            DO WHILE ( .NOT.nodesIterator % isAtEnd() )
               obj => nodesIterator % object()
               CALL cast(obj,meshNode)
!
!              ------------------------------
!              See it this is a boundary node
!              ------------------------------
!
               IF ( meshNode % bCurveID > UNDEFINED .AND. meshNode % distToBoundary == 0.0_RP )     THEN
!
!                 --------------------------------------------
!                 See if it is a boundary node on an interface
!                 --------------------------------------------
!
                  IF ( boundaryEdgesType(meshNode % bCurveID) == INTERFACE_EDGES )     THEN
!
!                    ------------------------------------------------
!                    Find the elements associated with this node that
!                    ave only one node on the boundary
!                    ------------------------------------------------
!
                     DO j = 1, numElementsForNode( meshNode % id )
                        e         => elementsForNodes(j,meshNode % id) % element
                        nodecount = 0
                        DO k = 1, 4
                           obj => e % nodes % objectAtIndex(k)
                           CALL cast(obj,elementNode)
                           IF ( elementNode % distToBoundary == 0.0_RP )     THEN
                              nodeCount = nodeCount + 1 
                           END IF
                        END DO
                        
                        IF ( nodeCount == 1 )     THEN
                           obj => e
                           CALL interfaceElements % add(obj)
                        END IF 
                        
                     END DO   
                  END IF 
               END IF 
               
               CALL nodesIterator % moveToNext()
            END DO
            CALL deallocateNodeToElementConnections()
!
!          -------------------------------
!          Split the elements as necessary
!          -------------------------------
!
           CALL splitInterfaceElements( mesh, interfaceElements )
!
           CALL interfaceElements % release()
           IF(interfaceElements % isUnreferenced()) DEALLOCATE(interfaceElements)
         END IF 
!
!        --------
!        Clean up
!        --------
!
         CALL mesh % destroyEdgeArrays()
         CALL deallocateNodeToElementConnections

      END SUBROUTINE CleanUpBoundaryElements
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CleanUpChevronElements( badElements, shapeMeasures, numberOfChevrons )
       
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTMutableObjectArray) :: badElements
         REAL(KIND=RP)               :: shapeMeasures(:,:)
         INTEGER                     :: numberOfChevrons
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMElement), POINTER :: e => NULL(), eNbr => NULL()
         CLASS(FTObject) , POINTER :: obj => NULL()
         CLASS(SMNode)   , POINTER :: node => NULL()
         REAL(KIND=RP)             :: angles(4)
         INTEGER                   :: numberOfBadElements
         INTEGER                   :: k, j, badNodeID, lastSharedNodeID
         INTEGER                   :: nbrNodeLocalID , badNodeLocalID, lastSharedNodeLocalID
         
         INTEGER, EXTERNAL :: Loop
         
         numberOfBadElements = badElements % COUNT()
         numberOfChevrons    = 0

         DO k = 1, numberOfBadElements
            obj => badElements % objectAtIndex(k)
            CALL cast(obj,e)
            IF( e % remove )     CYCLE
!
!           -------------------------------------
!           For now, look for new 180 degree node
!           -------------------------------------
!
            IF( shapeMeasures(MAX_ANGLE_INDEX,k) > 175.0_RP ) THEN !TODO make 175 a variable
               CALL ElementAngles( e, angles, .true. ) !Lefties have been reversed
               DO j = 1, 4 
                  IF( angles(j) > 175.0_RP )     THEN
                     badNodeLocalID = j
                     EXIT
                  END IF
               END DO
               obj => e % nodes % objectAtIndex(badNodeLocalID)
               CALL cast(obj,node)
               badNodeID = node % id
!
!              -------------------------------------------
!              Check the topology - remove triangles only.
!              -------------------------------------------
!
               IF ( numElementsForNode(badNodeID) == 2 )     THEN
                  IF( ASSOCIATED(e,elementsForNodes(1,badNodeID) % element) )     THEN
                     eNbr => elementsForNodes(2,badNodeID) % element
                  ELSE IF ( ASSOCIATED(e,elementsForNodes(2,badNodeID) % element) )   THEN 
                     eNbr => elementsForNodes(1,badNodeID) % element
                  ELSE
                     PRINT *, "Unassociated pointer for node ", badNodeID, " in element ", e % id
                     CYCLE 
                  END IF
!
!                 -------------------------------------------------------
!                 Replace the bad node in this element by the unique node
!                 in the neigboring element.
!                 -------------------------------------------------------
!
                  lastSharedNodeLocalID = Loop(badNodeLocalID+3,4)
                  obj => e % nodes % objectAtIndex(lastSharedNodeLocalID)
                  CALL cast(obj,node)
                  lastSharedNodeID = node % id
!
!                 ----------------------------------------
!                 Find where in the neighbor this node is.
!                 ----------------------------------------
!
                  j = -1
                  DO j = 1,4
                     obj => e % nodes % objectAtIndex(j)
                     CALL cast(obj,node)
                     IF( node % id == lastSharedNodeID )     THEN
                        nbrNodeLocalID = j
                        EXIT
                     END IF
                  END DO
                  
                  IF ( j < 0 )     THEN
                     PRINT *, "Bad shared element connection, ignoring elements ", e % id, eNbr % id
                     CYCLE
                  END IF

                  nbrNodeLocalID = Loop(nbrNodeLocalID+1,4)
!
!                 -------------
!                 Make the swap
!                 -------------
!
                  obj => e % nodes % objectAtIndex(badNodeLocalID)
                  CALL obj % release()
                  IF ( obj % isUnreferenced() )     THEN
                     CALL cast(obj,node)
                     DEALLOCATE(node)
                  END IF 
                  obj => eNbr % nodes % objectAtIndex(nbrNodeLocalID)
                  CALL e % nodes % replaceObjectAtIndexWithObject(badNodeLocalID, obj)
                  eNbr % remove = .true.
                  numberOfChevrons = numberOfChevrons + 1
               ELSE !TODO Do something here at some point.
!
!                 ------------------------------------
!                 This usually means a concave element
!                 ------------------------------------
!
!                  PRINT *, "Alternate Topology on bad angle", numElementsForNode(badNodeID)
!                  DO j = 1,4
!                     PRINT *, e % nodes(j) % node % x 
!                  END DO
!                  PRINT *, "-----------"
               END IF
            END IF
            
         END DO
         IF(numberOfChevrons > 0) PRINT *, numberOfChevrons, " chevron elements removed from mesh." 
         
      END SUBROUTINE CleanUpChevronElements 
!
!///////////////////////////////////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CleanUpBoundaryElement( e, model )
         USE SMModelClass
         USE ErrorTypesModule
         USE fMinModule
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMElement)          :: e
         CLASS(SMModel)  , POINTER :: model
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, PARAMETER               :: ON_BOUNDARY = 1, OFF_BOUNDARY = 0
         CLASS(SMNode)  , POINTER         :: sourceNode => NULL(), node => NULL()
         CLASS(FTObject), POINTER         :: obj => NULL()
         TYPE(SMNodePtr)                  :: elementNodes(4)  !Temp container for this procedure
         INTEGER                          :: boundaryNodeCount
         INTEGER                          :: k, m, j
         INTEGER                          :: mark(4)
         
         REAL(KIND=RP)  , DIMENSION(3)    :: p
         REAL(KIND=RP)                    :: t, tSav
         
         CLASS(SMCurve)       , POINTER   :: c => NULL()
         CLASS(SMChainedCurve), POINTER   :: chain  => NULL()      ! Returned also so that it, too can be used.
         TYPE(SMNode)                     :: nodeCopy(4) ! Two extra but not a big deal.
         CLASS(SMElement)     , POINTER   :: eTest => NULL()
         LOGICAL                          :: badArrayOriginal(6)      ! Node valence <= 6
         INTEGER                          :: nodeID
         
         CHARACTER(LEN=ERROR_EXCEPTION_MSG_LENGTH) :: msg
!
!        -------------------------------------------------------
!        A boundary element has at least two nodes on a boundary
!        -------------------------------------------------------
!
         boundaryNodeCount = 0
         mark              = OFF_BOUNDARY
         DO k = 1, 4
            obj => e % nodes % objectAtIndex(k)
            CALL cast(obj, node)
            elementNodes(k) % node => node
            IF ( node % bCurveID > UNDEFINED .AND. node % distToBoundary == 0.0_RP )     THEN
               boundaryNodeCount = boundaryNodeCount + 1
               mark(k) = ON_BOUNDARY 
            END IF
         END DO
!
!        ---------------------------------------------------
!        We check to make sure that this was called properly
!        ---------------------------------------------------
!
         IF( boundaryNodeCount < 2 )   RETURN
!
!        --------------------------------
!        See what we are starting with
!        i.e., see if any of the elements
!        are bad.
!        --------------------------------
!
         badArrayOriginal = .false.
         DO k = 1,4
            IF( mark(k) == ON_BOUNDARY )     THEN
!
!              -------------------------------------------
!              Find the quality of the neigboring elements
!              -------------------------------------------
!
               nodeID = elementNodes(k) % node % id

               DO j = 1, numElementsForNode(nodeID)
                  eTest => elementsForNodes(j,nodeID) % element
                  IF(.NOT.ASSOCIATED(etest))     THEN
                     WRITE(msg,*) "Neighbors to node ", node % x, "Not associated. Aborting cleanup."
                     CALL ThrowErrorExceptionOfType("CleanUpBoundaryElement",msg,FT_ERROR_WARNING)
                     RETURN
                  END IF 
                  badArrayOriginal(j) = elementIsBad(eTest)
               END DO
            END IF
         END DO
!
!        ---------------------
!        Adjust boundary nodes
!        ---------------------
!
         DO k = 1, 4
            IF( mark(k) == OFF_BOUNDARY            )              CYCLE
            IF(elementNodes(k) % node % activeStatus == INACTIVE) CYCLE
            IF(elementNodes(k) % node % nodeType /= ROW_SIDE)     CYCLE
!
!           -----------------------------------
!           Find which node is off the boundary
!           -----------------------------------
!
            IF ( mark(sourceNodeLocalID(1,k)) == OFF_BOUNDARY )          THEN
               m = sourceNodeLocalID(1,k)
            ELSE IF ( mark(sourceNodeLocalID(2,k)) == OFF_BOUNDARY )     THEN
               m = sourceNodeLocalID(2,k)
            ELSE
               node => elementNodes(k) % node
               WRITE(msg,*) "Something wrong with node at ", node % x, ". Aborting cleanup."
               CALL ThrowErrorExceptionOfType("CleanUpBoundaryElement",msg,FT_ERROR_WARNING)
               RETURN
            END IF
            
            sourceNode  => elementNodes(m) % node
            node        => elementNodes(k) % node
            nodeCopy(k) =  node % typeCopy()
            
            p      = sourceNode % x
            t      = node % whereOnBoundary
            tSav   = t
!
            c => model % curveWithID(node % bCurveID, chain)
            t = ParametrizationAtPointNear(c,p,t)
!            tStart = t - 0.49_RP
!            tEnd   = t + 0.49_RP
!            tStart = MAX(tStart, 0.0_RP)
!            tEnd   = MIN(tEnd, 1.0_RP)
!!
!            t = fmin(tStart, tEnd, c, p, (/0.0_RP, 0.0_RP, 0.0_RP/), minimizationTolerance )
            
            node % whereOnBoundary  = t
            node % distToBoundary   = 0.0_RP !This is a boundary node
            node % gWhereOnBoundary = chain % ChainTForCurveTInCurve(t,c)

            node % x                = c % positionAt(t)
         END DO
!
!        ----------------------------------------------------------------
!        This element has been adjusted. See if the new element and the
!        neigbors of the nodes have been messed up. If so, go back to the
!        original.
!        ----------------------------------------------------------------
!
!         badArrayNew = .false.
!         DO k = 1, 4
!            IF( mark(k) == OFF_BOUNDARY )     CYCLE
!            IF(elementNodes(k) % node % activeStatus == INACTIVE) CYCLE
!            IF(elementNodes(k) % node % nodeType /= ROW_SIDE)     CYCLE
!!
!!           -----------------------------------
!!           Check the elements around this node
!!           -----------------------------------
!!
!            nodeID   = elementNodes(k) % node % id
!            failed = .false.
!            DO j = 1, numElementsForNode(nodeID)
!               eTest          => elementsForNodes(j,nodeID) % element
!               badArrayNew(j) = elementIsBad(eTest)
!               IF(badArrayNew(j) .AND. (.NOT.badArrayOriginal(j))) failed = .true.
!            END DO
!            IF(failed) THEN
!               nodeCopy(k) % activeStatus = INACTIVE
!               obj => e % nodes % objectAtIndex(k)
!               CALL cast(obj,node)
!               CALL copyOfNodeType(nodeCopy(k),node)
!            END IF 
!         END DO
      END SUBROUTINE CleanUpBoundaryElement
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DeleteElementIfDiamond(e, mesh) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMElement), POINTER :: e
         TYPE(SMMesh)              :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                   :: valences(4)
         INTEGER                   :: k, j, id, idNbr, localIds(2)
         LOGICAL                   :: hasA3ValenceNode, isDiamond
         REAL(KIND=RP)             :: x(3), corners(3,4)
         CLASS(SMElement), POINTER :: eNbr => NULL()
         CLASS(FTObject) , POINTER :: obj => NULL()
         CLASS(SMNode)   , POINTER :: node => NULL()
         TYPE(SMNodePtr)           :: elementNodes(4)  !Temp container for this procedure
!
!        --------------------------------------------
!        Gather the valences of the four corners of e
!        --------------------------------------------
!
         hasA3ValenceNode = .false.
         DO k = 1,4
            obj => e % nodes % objectAtIndex(k)
            CALL cast(obj, node)
            elementNodes(k) % node => node
            id          = node  % id
            valences(k) = numElementsForNode(id)
            IF(valences(k) == 3) hasA3ValenceNode = .true.
         END DO
         IF(.NOT.hasA3ValenceNode) RETURN 
!
!        -------------------------------------------------------
!        A diamond has two non-adjacent 3-valence nodes, n1 & n2
!        -------------------------------------------------------
!
         isDiamond = .false.
         DO k = 1,2
            IF ( valences(k) == 3 )          THEN
               IF ( valences(k+2) == 3 )     THEN
                  isDiamond = .true.
                  localIds(1)  = k
                  localIds(2)  = k + 2
                  EXIT 
               END IF
            END IF
         END DO
         
         IF(.NOT.isDiamond) RETURN
!
!        --------------------------------------
!        Do not remove a diamond if one node is
!        attached to a boundary
!        --------------------------------------
!
         IF ( elementNodes(localIDs(1)) % node % bCurveID /= UNDEFINED .OR. &
              elementNodes(localIDs(2)) % node % bCurveID /= UNDEFINED )     THEN
            RETURN
         END IF

         e % remove = .true.
!
!        ---------------------------------
!        Create a node inside this element
!        ---------------------------------
!
         DO k = 1, 4
            corners(:,k) = elementNodes(k) % node % x
         END DO
         CALL ComputeCentroid(corners,x)
         ALLOCATE(node)
         CALL node % initWithLocationAndID(x,mesh % newNodeID())
         obj => node
         CALL mesh % nodes % add(obj)
         CALL node % release()
!
!        ----------------------------------------------
!        Find the elements that share the two nodes,
!        Find the local ids of them within the elements
!        and point those to the new node
!        ----------------------------------------------
!
         DO j = 1, 2
            id = elementNodes(localIDs(j)) % node  % id
            DO k = 1, numelementsForNode(id)
               eNbr => elementsForNodes(k,id) % element
               IF(ASSOCIATED(eNbr,e)) CYCLE
!
!              ----
!              Find
!              ----
!
               idNbr = ElementLocalNodeIDForNodeID( id, eNbr )
!
!              -------------------
!              Release and repoint
!              -------------------
!
               CALL eNbr % nodes % replaceObjectAtIndexWithObject(idNbr,obj)
            END DO
         END DO
!
!        -------------------------------------------------------------------
!        New nodes were added, so we must re-compute the element connections
!        -------------------------------------------------------------------
!
         CALL makeNodeToElementConnections(mesh)
               
      END SUBROUTINE DeleteElementIfDiamond 

      END Module MeshCleaner

! MIT License
!
! Copyright (c) 2010-present David A. Kopriva and other contributors: AUTHORS.md
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
! --- End License
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
      USE SMMeshObjectsModule
      USE SMMeshClass
      USE MeshQualityAnalysisClass
      USE MeshBoundaryMethodsModule
      USE ConnectionsModule
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
      SUBROUTINE PerformTopologyCleanup( mesh, errorCode )
         USE SMMeshClass
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) :: mesh
         INTEGER       :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER  :: numberOfValenceChanges, numberOfDiamondsRemoved
         LOGICAL  :: valenceHasChanged, diamondsHaveBeenRemoved

         valenceHasChanged       = .false.
         diamondsHaveBeenRemoved = .false.
         numberOfValenceChanges  = 0
         numberOfDiamondsRemoved = 0
!
!        ------------------------------------
!        Reduce valence at high valence nodes
!        ------------------------------------
!
         CALL ReduceNodeValences( mesh, numberOfValenceChanges, errorCode )
         IF(errorCode > A_OK_ERROR_CODE)     RETURN

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
         CALL RemoveDiamondElements( mesh, numberOfDiamondsRemoved, errorCode )
         IF(errorCode > A_OK_ERROR_CODE)     RETURN

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
            CALL mesh % renumberObjects(whichIterator = NODES)
            CALL mesh % syncEdges()
            CALL UnmarkNodesNearBoundaries( mesh % nodesIterator )
         END IF

      END SUBROUTINE PerformTopologyCleanup
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReduceNodeValences( mesh, numberOfValenceChanges, errorCode )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) :: mesh
         INTEGER       :: numberOfValenceChanges
         INTEGER       :: errorCode
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
         CALL MakeNodeToElementConnections( mesh, errorCode )
         IF(errorCode > A_OK_ERROR_CODE)     RETURN

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
                  CALL CleanUp7ValenceNode_InMesh(nodeID, mesh, errorCode)
                  numberOfValenceChanges = numberOfValenceChanges + 1
               CASE( 8 )
!                  CALL CleanUp8ValenceNode_InMesh(nodeID, mesh) TODO:
               CASE DEFAULT
                  !Do nothing
            END SELECT
         END DO

         DEALLOCATE(localNumElementsForNode)

      END SUBROUTINE ReduceNodeValences
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RemoveDiamondElements( mesh, numberOfDiamondsRemoved, errorCode )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) :: mesh
         INTEGER       :: numberOfDiamondsRemoved
         INTEGER       :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (SMElement)           , POINTER :: currentElement  => NULL()
         CLASS(FTLinkedListIterator), POINTER :: elementIterator => NULL()
         CLASS(FTObject)            , POINTER :: obj             => NULL()
!
         numberOfDiamondsRemoved = 0
         CALL MakeNodeToElementConnections( mesh, errorCode )
!
!        -------------------------------------------
!        Go through each element and remove diamonds
!        -------------------------------------------
!
         elementIterator => mesh % elementsIterator
         CALL elementIterator % setToStart()

         DO WHILE ( .NOT.elementIterator % isAtEnd() )
            obj => elementIterator % object()
            CALL castToSMElement(obj,currentElement)

            CALL DeleteElementIfDiamond( currentElement, mesh, errorCode )

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
      SUBROUTINE CleanUp7ValenceNode_InMesh(id, mesh, errorCode)
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
         INTEGER       :: id
         TYPE (SMMesh) :: mesh
         INTEGER       :: errorCode
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

         TYPE (SMNode)     , POINTER :: node => NULL(), nodeForThisID => NULL(), newNode => NULL()
         TYPE (SMElement)  , POINTER :: e => NULL(), eTarget => NULL()
         TYPE (SMEdge)     , POINTER :: edge => NULL()
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
         CALL MakeNodeToElementConnections( mesh, errorCode )
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
            WRITE(msg,*) "Bad element topology for node at ", &
                           eTarget % nodes(localIDForTarget) % node % x, ". Aborting cleanup."
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
            PRINT *, "Bad element topology in CleanUp7ValenceNode_InMesh for node at ", &
                     eTarget % nodes(localIDForTarget) % node % x
            CALL deallocateNodeToEdgeConnections()
            CALL deallocateNodeToElementConnections()
            RETURN
         END IF
!
!        -------------------------------------------
!        Create a new node inside the target element
!        -------------------------------------------
!
         node => eTarget % nodes(localIDForTarget) % node

         nodeForThisID => node ! save pointer before overwriting

         x1 = node % x
         x2 = eTarget % nodes(diagonalMap(localIDForTarget)) % node % x
         x  = 0.5_RP*(x1 + x2)

         ALLOCATE(newNode)
         CALL newNode % initWithLocationAndID(x,mesh % newNodeID())
         obj               => newNode
         CALL mesh % nodes % add(obj)
         CALL releaseSMNode(newNode)
!
!        --------------------------------------------
!        Change the corners to point to this new node
!        --------------------------------------------
!
         CALL releaseSMNode(eTarget % nodes(localIDForTarget) % node)
         eTarget % nodes(localIDForTarget) % node => newNode
         CALL newNode % retain()

         localID = ElementLocalNodeIDForNodeID( id, eNeighbors(1) % element )
         CALL releaseSMNode(eNeighbors(1) % element % nodes(localID) % node)
         eNeighbors(1) % element % nodes(localID) % node => newNode
         CALL newNode % retain()

         localID = ElementLocalNodeIDForNodeID( id, eNeighbors(2) % element )
         CALL releaseSMNode(eNeighbors(2) % element % nodes(localID) % node)
         eNeighbors(2) % element % nodes(localID) % node => newNode
         CALL newNode % retain()
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

         elementNodes(3) % node => newNode

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
         CALL releaseSMElement(e)
!
         CALL deallocateNodeToEdgeConnections()
         CALL deallocateNodeToElementConnections()

      END SUBROUTINE CleanUp7ValenceNode_InMesh
!
!////////////////////////////////////////////////////////////////////////
!
!      SUBROUTINE CleanUp8ValenceNode_InMesh(id,mesh)
!         IMPLICIT NONE
!!
!!        ---------
!!        Arguments
!!        ---------
!!
!         INTEGER       :: id
!         TYPE (SMMesh) :: mesh
!!
!!        ---------------
!!        Local variables
!!        ---------------
!!
!!         INTEGER                  :: k
!!         TYPE(SMElement), POINTER :: e
!!         REAL(KIND=RP)            :: theta
!!
!!        ----------------------------------------
!!        Find the element with the smallest angle
!!        ----------------------------------------
!!
!
!      END SUBROUTINE CleanUp8ValenceNode_InMesh
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PerformFinalMeshCleanup( mesh , model, errorCode )
         USE SMModelClass
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) , POINTER :: mesh
         TYPE (SMModel), POINTER :: model
         INTEGER                 :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (FTMutableObjectArray) , POINTER     :: badElements => NULL()
         TYPE (SMElement)            , POINTER     :: e => NULL()
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
            CALL CleanUpBoundaryElements( mesh, model, errorCode )
         ELSE
!
!           -----------------------
!           Save the shape measures
!           -----------------------
!
            numberOfBadElements = badElements % COUNT()
            ALLOCATE( shapeMeasures    ( NUMBER_OF_2D_SHAPE_MEASURES, numberOfBadElements ) )
            ALLOCATE( badElementMeasure( NUMBER_OF_2D_SHAPE_MEASURES, numberOfBadElements ) )

            DO k = 1, numberOfBadElements
               obj => badElements % objectAtIndex(k)
               CALL castToSMElement(obj,e)
               CALL ComputeElementShapeMeasures2D( e, shapeMeasures(:,k) )
               CALL ExtractBadElementInfo( shapeMeasures(:,k), badElementMeasure(:,k) )
            END DO
!
!           -----------
!           Fix lefties
!           -----------
!
            DO k = 1, numberOfBadElements
               obj => badElements % objectAtIndex(k)
               CALL castToSMElement(obj,e)
               IF( shapeMeasures(AREA_SIGN,k ) < 0.0_RP ) CALL MakeElement_RightHanded( e )
            END DO
!
!           -------------------------------
!           Fix triangular/chevron elements
!           -------------------------------
!
            CALL MakeNodeToElementConnections( mesh, errorCode )
            CALL CleanUpChevronElements( badElements, shapeMeasures, numberOfChevrons )
            CALL deallocateNodeToElementConnections
!
!           ---------------
!           Do lazy deletes
!           ---------------
!
            CALL releaseFTMutableObjectArray(badElements)
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
            CALL CleanUpBoundaryElements( mesh, model, errorCode )
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
         TYPE (SMElement) :: e
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE(SMNode), POINTER :: nptr2 => NULL(), nptr4 => NULL()
!
!        ------------------------------------------------------------
!        To make a left handed element right handed, swap nodes 2 & 4
!        ------------------------------------------------------------
!
         nptr2 => e % nodes(2) % node
         nptr4 => e % nodes(4) % node

         e % nodes(2) % node => nptr4
         e % nodes(4) % node => nptr2

      END SUBROUTINE MakeElement_RightHanded
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CleanUpBoundaryElements( mesh, model, errorCode )
         USE SMModelClass
         USE ConnectionsModule
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) , POINTER :: mesh
         TYPE (SMModel), POINTER :: model
         INTEGER                 :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                              :: numBoundaries
         INTEGER                              :: j, k, nodeCount
         CLASS(FTLinkedList)        , POINTER :: currentEdgeList => NULL()
         CLASS(FTLinkedList)        , POINTER :: interfaceElements => NULL()
         TYPE (SMEdge)              , POINTER :: currentEdge => NULL()
         CLASS(SMElement)           , POINTER :: e => NULL()
         TYPE (SMNode)              , POINTER :: elementNode => NULL(), meshNode => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()
         TYPE (FTLinkedListIterator), POINTER :: edgeListIterator
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
         CALL SetNodeActiveStatus(mesh,model, errorCode)
!
!        -----------------------------------------------
!        Run through physical boundary elements and make
!        the boundary angles 90 degrees.
!        -----------------------------------------------
!
         IF( errorCode == A_OK_ERROR_CODE)     THEN
            CALL makeNodeToElementConnections(mesh, errorCode)
            DO j = 1, numBoundaries
               ALLOCATE(edgeListIterator)
               IF( boundaryEdgesType(j) == INTERFACE_EDGES ) CYCLE

               obj  => boundaryEdgesArray % objectAtIndex(j)
               CALL cast(obj,currentEdgeList)

               CALL edgeListIterator % initWithFTLinkedList(currentEdgeList)
               CALL edgeListIterator % setToStart()

               DO WHILE ( .NOT.EdgeListIterator % isAtEnd() )
                  obj => EdgeListIterator % object()
                  CALL castToSMEdge(obj,currentEdge)
                  IF ( currentEdge % edgeType == ON_BOUNDARY )     THEN
                     e   => currentEdge % elements(1) % element
!               BUG: puts boundary points in the wrong place.
                     CALL CleanUpBoundaryElement( e, model )
                  END IF

                  CALL EdgeListIterator % moveToNext()
               END DO

               CALL releaseFTLinkedListIterator(edgeListIterator)
            END DO
            CALL deallocateNodeToEdgeConnections
         ELSE
            CALL mesh % destroyEdgeArrays()
            CALL deallocateNodeToElementConnections
            RETURN
         END IF
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

               ALLOCATE(edgeListIterator)
               CALL edgeListIterator % initWithFTLinkedList(currentEdgeList)
               CALL edgeListIterator % setToStart()

               DO WHILE ( .NOT.EdgeListIterator % isAtEnd() )
                  obj => EdgeListIterator % object()
                  CALL castToSMEdge(obj,currentEdge)

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

               CALL releaseFTLinkedListIterator(edgeListIterator)
           END DO
!
!          ----------------------------------------------------
!          It is also possible that a single node of an element
!          falls on the interface boundary.
!          ----------------------------------------------------
!
            CALL makeNodeToElementConnections(mesh, errorCode)

            IF( errorCode == A_OK_ERROR_CODE)     THEN
               nodesIterator => mesh % nodesIterator
               CALL nodesIterator % setToStart()
               DO WHILE ( .NOT.nodesIterator % isAtEnd() )
                  obj => nodesIterator % object()
                  CALL castToSMNode(obj,meshNode)
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
!                    have only one node on the boundary
!                    ------------------------------------------------
!
                        DO j = 1, numElementsForNode( meshNode % id )
                           e         => elementsForNodes(j,meshNode % id) % element
                           nodecount = 0
                           DO k = 1, 4
                              elementNode => e % nodes(k) % node

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
            ELSE
               CALL mesh % destroyEdgeArrays()
               CALL deallocateNodeToElementConnections
               RETURN
            END IF

!
!          -------------------------------
!          Split the elements as necessary
!          -------------------------------
!
           CALL splitInterfaceElements( mesh, interfaceElements )
!
           CALL releaseFTLinkedList(interfaceElements)
         END IF
!
!        --------
!        Clean up
!        --------
!
         CALL DoLazyDelete( mesh )

         CALL mesh % renumberAllLists()
         CALL mesh % syncEdges()
         CALL mesh % destroyEdgeArrays()
         CALL deallocateNodeToElementConnections()

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
         TYPE (SMElement), POINTER :: e => NULL(), eNbr => NULL()
         CLASS(FTObject) , POINTER :: obj => NULL()
         TYPE (SMNode)   , POINTER :: node => NULL()
         REAL(KIND=RP)             :: angles(4)
         INTEGER                   :: numberOfBadElements
         INTEGER                   :: k, j, badNodeID
         INTEGER                   :: nbrNodeLocalID , badNodeLocalID

         INTEGER, EXTERNAL :: Loop

         numberOfBadElements = badElements % COUNT()
         numberOfChevrons    = 0

         DO k = 1, numberOfBadElements
            obj => badElements % objectAtIndex(k)
            CALL castToSMElement(obj,e)
            IF( e % remove )     CYCLE
!
!           -------------------------------------
!           For now, look for new 180 degree node
!           -------------------------------------
!
            IF( shapeMeasures(MAX_ANGLE_INDEX,k) > 175.0_RP ) THEN !TODO make 175 a variable
               CALL ElementAngles( e, angles, .true. ) !Lefties have been reversed
               badNodeLocalID = -1
               DO j = 1, 4
                  IF( angles(j) > 175.0_RP )     THEN
                     badNodeLocalID = j
                     EXIT
                  END IF
               END DO
               IF(badNodeLocalID < 0) CYCLE !Bail on this one

               node => e % nodes(badNodeLocalID) % node
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
!                 ---------------------------------------------
!                 Replace the bad node in this element by
!                 the opposite node in the neighboring element.
!                 Find where in the neighbor this node is.
!                 ---------------------------------------------
!
                  j = -1
                  DO j = 1,4
!                     obj => eNbr % nodes % objectAtIndex(j)
!                     CALL cast(obj,node)
                     node => eNbr % nodes(j) % node
                     IF( node % id == badNodeID )     THEN
                        nbrNodeLocalID = j
                        EXIT
                     END IF
                  END DO

                  IF ( j < 0 )     THEN
                     PRINT *, "Bad shared element connection, ignoring elements ", e % id, eNbr % id
                     CYCLE
                  END IF
!
!                 ----------------------------------------------------
!                 Get the opposite facing node in the neighbor element
!                 ----------------------------------------------------
!

                  nbrNodeLocalID = Loop(nbrNodeLocalID+2,4)
!
!                 -------------
!                 Make the swap
!                 -------------
!
!                  obj => eNbr % nodes % objectAtIndex(nbrNodeLocalID)
!                  CALL e % nodes % replaceObjectAtIndexWithObject(badNodeLocalID, obj)
                  CALL releaseSMNode(e % nodes(badNodeLocalID) % node)
                  e % nodes(badNodeLocalID) % node => eNbr % nodes(nbrNodeLocalID) % node
                  CALL e % nodes(badNodeLocalID) % node % retain()
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
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS (SMElement), POINTER :: e
         TYPE (SMModel)  , POINTER :: model
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, PARAMETER               :: CBE_ON_BOUNDARY = 1, CBE_OFF_BOUNDARY = 0
         CLASS (SMNode)  , POINTER         :: sourceNode => NULL(), node => NULL()
         TYPE(SMNodePtr)                  :: elementNodes(4)  !Temp container for this procedure
         INTEGER                          :: boundaryNodeCount
         INTEGER                          :: k, m, j
         INTEGER                          :: mark(4)

         REAL(KIND=RP)  , DIMENSION(3)    :: p
         REAL(KIND=RP)                    :: t, tSave

         CLASS(SMCurve)       , POINTER   :: c => NULL()
         CLASS(SMChainedCurve), POINTER   :: chain  => NULL()      ! Returned also so that it, too can be used.
         TYPE(SMNode)                     :: nodeCopy(4) ! Two extra but not a big deal.
         TYPE (SMElement)     , POINTER   :: eTest => NULL()
         LOGICAL                          :: badArrayOriginal(6)      ! Node valence <= 6
         INTEGER                          :: nodeID

         CHARACTER(LEN=ERROR_EXCEPTION_MSG_LENGTH) :: msg
!
!        -------------------------------------------------------
!        A boundary element has at least two nodes on a boundary
!        -------------------------------------------------------
!
         boundaryNodeCount = 0
         mark              = CBE_OFF_BOUNDARY
         DO k = 1, 4
            elementNodes(k) % node => e % nodes(k) % node
            IF ( e % nodes(k) % node % bCurveID > UNDEFINED .AND. &
                 e % nodes(k) % node % distToBoundary == 0.0_RP )     THEN
               boundaryNodeCount = boundaryNodeCount + 1
               mark(k) = CBE_ON_BOUNDARY
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
            IF( mark(k) == CBE_ON_BOUNDARY )     THEN
!
!              --------------------------------------------
!              Find the quality of the neighboring elements
!              --------------------------------------------
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
            IF( mark(k) == CBE_OFF_BOUNDARY            )              CYCLE
            IF(elementNodes(k) % node % activeStatus == INACTIVE) CYCLE
            IF(elementNodes(k) % node % nodeType /= ROW_SIDE)     CYCLE
!
!           -----------------------------------
!           Find which node is off the boundary
!           -----------------------------------
!
            IF ( mark(sourceNodeLocalID(1,k)) == CBE_OFF_BOUNDARY )          THEN
               m = sourceNodeLocalID(1,k)
            ELSE IF ( mark(sourceNodeLocalID(2,k)) == CBE_OFF_BOUNDARY )     THEN
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
            tSave  = t
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
!        --------------------------------------------------------------
!        This element has been adjusted. See if the new element and the
!        neighbors of the nodes have been messed up. If so, go back to
!        the original.
!        --------------------------------------------------------------
!
!         badArrayNew = .false.
!         DO k = 1, 4
!            IF( mark(k) == CBE_OFF_BOUNDARY )     CYCLE
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
!               obj => e % nodes % objectAtIndex(k) !Supersceeded with new data structure
!               CALL cast(obj,node)
!               CALL copyOfNodeType(nodeCopy(k),node)
!            END IF
!         END DO
      END SUBROUTINE CleanUpBoundaryElement
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DeleteElementIfDiamond(e, mesh, errorCode)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMElement), POINTER :: e
         TYPE(SMMesh)              :: mesh
         INTEGER                   :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                   :: valences(4)
         INTEGER                   :: k, j, id, idNbr, localIds(2)
         LOGICAL                   :: hasA3ValenceNode, isDiamond
         REAL(KIND=RP)             :: x(3), corners(3,4)
         TYPE (SMElement), POINTER :: eNbr    => NULL()
         CLASS(FTObject) , POINTER :: obj     => NULL()
         TYPE (SMNode)   , POINTER :: newNode => NULL()
         TYPE(SMNodePtr)           :: elementNodes(4)  !Temp container for this procedure
!
!        --------------------------------------------
!        Gather the valences of the four corners of e
!        --------------------------------------------
!
         hasA3ValenceNode = .false.
         DO k = 1,4
            elementNodes(k) % node => e % nodes(k) % node
            id                      = e % nodes(k) % node  % id
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
         ALLOCATE(newNode)
         CALL newNode % initWithLocationAndID(x,mesh % newNodeID())
         obj => newNode
         CALL mesh % nodes % add(obj)
         CALL releaseSMNode(newNode)
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
               CALL releaseSMNode( eNbr % nodes(idNbr) % node)
               eNbr % nodes(idNbr) % node => newNode
               CALL newNode % retain()
            END DO
         END DO
!
!        -------------------------------------------------------------------
!        New nodes were added, so we must re-compute the element connections
!        -------------------------------------------------------------------
!
         CALL makeNodeToElementConnections(mesh, errorCode)

      END SUBROUTINE DeleteElementIfDiamond

      END Module MeshCleaner

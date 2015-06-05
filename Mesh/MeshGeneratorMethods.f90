!
!////////////////////////////////////////////////////////////////////////
!
!      MeshGeneratorMethods.f90
!      Created: August 21, 2013 1:01 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   Module MeshGenerationMethods
      USE QuadTreeGridGeneratorModule
      USE MeshProjectClass
      USE MeshOperationsModule
      USE ProgramGlobals
      USE MeshBoundaryMethodsModule
      USE ErrorTypesModule
      IMPLICIT NONE
   
!
!     ========
      CONTAINS 
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE GenerateQuadMeshForProject( project )
      USE BoundaryEdgeCleaningModule
!
!     ------------------------------------------------------------
!     This is the main procedure that actually generates the mesh.
!     ------------------------------------------------------------
!
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      CLASS( MeshProject ), POINTER :: project
!
!     ---------------
!     Local variables
!     ---------------
!
      CLASS(QuadTreeGrid), POINTER :: grid      => NULL()
      CLASS(SMMesh)      , POINTER :: mesh      => NULL()
      CLASS(SMModel)     , POINTER :: model     => NULL()
      CLASS(MeshSizer)   , POINTER :: sizer     => NULL()
      CLASS(FTObject)    , POINTER :: obj       => NULL()
      CLASS(FTLinkedList), POINTER :: list      => NULL()
      CLASS(FTException) , POINTER :: exception => NULL()
      
      INTEGER                      :: numberOfBoundaries,numBoundaryEdgeLists
      INTEGER                      :: j
      INTEGER                      :: idOfOuterBoundary
!
!     ---------------------
!     Generate the quadtree
!     ---------------------
!
      IF(PrintMessage) PRINT *, "   Generate quadtree..."
         CALL GenerateGridWithSizerAndType( project % grid, project % sizer, &
                                            project % meshParams % meshType)
      IF(PrintMessage) PRINT *, "   Quadtree grid generated"
!
!     ---------------
!     Create the mesh
!     ---------------
!
      ALLOCATE( project % mesh )
      CALL project % mesh % init()
      
      mesh  => project % mesh
      model => project % model
      grid  => project % grid
      sizer => project % sizer
!
!     -------------------------------------
!     Create the nodes, elements, and edges
!     from the nested grid
!     -------------------------------------
!
      IF(PrintMessage) PRINT *, "   Generate nodes and elements..."
         CALL GenerateNodesAndElements( mesh, grid )
      IF(PrintMessage) PRINT *, "   Nodes and elements were generated..."
!
!     ---------------------------------------------
!     Free up memory since grid is no longer needed
!     ---------------------------------------------
!
      CALL grid % release()
      IF ( grid % isUnreferenced() )     THEN
         DEALLOCATE(grid)
         project % grid => NULL()
      END IF 
!
!     --------------------------------------------------------------
!     Now make the mesh conform to exterior or interface boundaries. 
!
!     Determining exterior quads requires a curve defined in terms
!     of an array. Convert and temporarily save the boundary curves.
!     ConvertBoundaryCurvesToArrays allocates and fills temporaries
!     in the baseMethods module.
!     --------------------------------------------------------------
!
     CALL generateTemporaryBoundaryArrays( sizer )
!
!     --------------------------------------------
!     Mark the elements outside or near boundaries
!     then remove.
!     --------------------------------------------
!
      numberOfBoundaries = model % numberOfInnerCurves + model % numberOfOuterCurves &
                         + model % numberOfInterfaceCurves
      IF( numberOfBoundaries > 0 )     THEN
         ALLOCATE( aPointInsideTheCurve(3,numberOfBoundaries) )
         ALLOCATE( curveTypeForID(numberOfBoundaries) )
         CALL flagBoundaryTypes
      END IF

      CALL MarkExteriorElements ( mesh, project % backgroundParams )
      CALL MarkInterfaceElements( mesh )
!
!     ----------------------------------------
!     Actually do the deletion. At this point,
!     any temoraries are out of sync.
!     ----------------------------------------
!
      CALL DoLazyDelete( mesh )
!
!     -------------------------------------
!     We no longer need the boundary arrays
!     -------------------------------------
!
      CALL destroyTemporaryBoundaryArrays
!
!     -------------------------------------------
!     Prepare for generating boundary elements by
!     building, collecting and ordering boundary
!     edges
!     -------------------------------------------
!
      CALL mesh % buildEdgeList()

      IF( model % curveCount == 0 )     RETURN
  
      numBoundaryEdgeLists = model%numberOfInnerCurves + &
                             model%numberOfOuterCurves + &
                             model%numberOfInterfaceCurves
                             
      CALL AllocateBoundaryEdgesArray(numBoundaryEdgeLists)
      CALL CollectBoundaryEdges( mesh )
      CALL OrderBoundaryEdges( mesh )
      
      IF ( catch(WARNING_ERROR_EXCEPTION) )     THEN  ! Pass the error up the chain
         exception => errorObject()
         CALL throw(exception)
         RETURN
      END IF 
!
!     -------------------------------------------------------
!     Smooth the edges along exterior and interior boundaries
!     before generating the boundary elements.
!     -------------------------------------------------------
!
      CALL smoothBoundaryEdges
!
!     -------------------------------------------------------------
!     Associate the boundary edges with elements and see if any
!     elements must be eliminated for having nodes too close to
!     a boundary, etc.
!     -------------------------------------------------------------
!
      idOfOuterBoundary = UNDEFINED
      IF( ASSOCIATED( sizer % outerBoundary ) ) idOfOuterBoundary = sizer % outerBoundary % id
      
      CALL LocateEdgeImagesOnBoundaries( mesh, model, idOfOuterBoundary, skipInterfaces = .FALSE. )

      IF ( catch(FATAL_ERROR_EXCEPTION) )     THEN  ! Pass the error up the chain
         CALL destroyTemporaryBoundaryArrays
         exception => errorObject()
         CALL throw(exception)
         RETURN
      END IF 

      CALL CleanUpBoundaryCurves( mesh, model )
!
!     ------------------------------------------
!     The edge lists are also no longer in sync.
!     They may have had edges added or deleted.
!     Re-configure and smooth the boundary edges
!     ------------------------------------------
!
      CALL OrderBoundaryEdges( mesh )
      CALL smoothBoundaryEdges
!
!     ----------------------------------------------------------------
!     Next, find where the edges should project onto the 
!     boundary. Try to keep them uniformly distributed if possible
!     by moving the proposed node locations along the boundary, except
!     for the end nodes, which will be attached to corners.
!     ----------------------------------------------------------------
!
      CALL LocateEdgeImagesOnBoundaries( mesh, model, idOfOuterBoundary, skipInterfaces = .true. )
      IF ( catch(FATAL_ERROR_EXCEPTION) )     THEN  ! Pass the error up the chain
         CALL destroyTemporaryBoundaryArrays
         exception => errorObject()
         CALL throw(exception)
         RETURN
      END IF
      
      DO j = 1, boundaryEdgesArray % COUNT() 
         obj => boundaryEdgesArray % objectAtIndex(j)
         CALL cast(obj,list)
         CALL FlagEndNodes( list, model )
         CALL SmoothBoundaryLocations( list, model )
      END DO
!
!     ------------------------------------------
!     Create new elements by projecting onto the
!     boundary curves.
!     ------------------------------------------
!
      DO j = 1, boundaryEdgesArray % COUNT()
         IF( boundaryEdgesType(j) == INTERFACE_EDGES ) CYCLE
         
         obj => boundaryEdgesArray % objectAtIndex(j)
         CALL cast(obj,list)
         CALL GenerateBoundaryElements( mesh, model, list ) 
      END DO
!
!     -------------------------------
!     The edges are no longer in sync
!     -------------------------------
!
      CALL mesh % destroyEdgeArrays()
      CALL DoLazyDelete( mesh )
      CALL unmarkNodesNearBoundaries( mesh % nodesIterator )
      CALL mesh % syncEdges()
      CALL mesh % renumberAllLists()
      
      IF(PrintMessage) PRINT *, "   Nodes and elements generated"

      END SUBROUTINE GenerateQuadMeshForProject
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE GenerateNodesAndElements( mesh, grid )
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(QuadTreeGrid), POINTER :: grid
      CLASS(SMMesh)      , POINTER :: mesh
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER                        :: i, j, k, N, M
      INTEGER                        :: elementID
      CLASS(SMElement), POINTER      :: e   => NULL()
      CLASS(FTObject) , POINTER      :: obj => NULL()
      TYPE( SMNodePtr), DIMENSION(4) :: eNodes
!
!     -------------------------------------------------------
!     Top level operations: Remove flags and number the nodes
!     -------------------------------------------------------
!
      IF( grid % level == 0 ) THEN
         CALL AssignNodeIdsForGrid_( grid )
         CALL FlagNodeIds( grid, .false. )
      END IF
!
!     ---------------------------------------------
!     Generate the nodes: For now this makes copies
!     ---------------------------------------------
!
      N = grid % N(1)
      M = grid % N(2)
!
!     ----------------------------------------------------------------------------
!     For grid: Write out nodes in the order in which they were created <=> nodeID
!     Flag with a sign change to ignore aliases as having already been written out.
!     ----------------------------------------------------------------------------
!
      DO j = 0, M 
         DO i = 0, N
            IF(.NOT.ASSOCIATED(grid % nodes(i,j) % node) )            CYCLE
            IF( grid % nodes(i,j) % node % refCount() == 1 )          CYCLE  ! => Only the main grid, not quads references this
            IF( grid % nodes(i,j) % node % activeStatus == REMOVE )   CYCLE  ! Marked as outside
            IF( grid % nodes(i,j) % node % id > 0 )     THEN
            
               obj => grid % nodes(i,j) % node
               CALL mesh % nodes % add(obj)
               grid % nodes(i,j) % node % id = -ABS(grid % nodes(i,j) % node % id)
            END IF
            
         END DO
      END DO
!
!     -------------------------------------
!     Now generate the element connectivity
!     -------------------------------------
!
      DO j = 1, M 
         DO i = 1, N 
            IF ( ASSOCIATED(grid % quads(i,j) % quad) )     THEN
               IF ( ASSOCIATED(grid % children(i,j) % grid) )  CYCLE
               elementID = mesh % newElementID()
               DO k = 1, 4 
                  eNodes(k) % node => grid % quads(i,j) % quad % nodes(k) % node
               END DO
               ALLOCATE(e)
               CALL e % initWithNodesIDAndType(eNodes, elementID, QUAD )
               obj => e
               CALL mesh % elements % add(obj)
               CALL e % release()
            END IF
         END DO
      END DO
!
!     ------------
!     For children
!     ------------
!
      IF( ASSOCIATED(grid % children) )     THEN 
         DO j = 1, M
            DO i = 1, N 
               IF( ASSOCIATED( grid % children(i,j) % grid ) ) &
                   CALL GenerateNodesAndElements( mesh, grid % children(i,j) % grid )
            END DO
         END DO
      END IF
!
!     --------------------
!     Remove node flagging
!     --------------------
!
      IF( grid % level == 0 ) CALL FlagNodeIds( grid, .false. )
     
      END SUBROUTINE GenerateNodesAndElements
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GenerateBoundaryElements( mesh, model, list ) 
         USE fMinModule
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMMesh)      , POINTER :: mesh
         CLASS(SMModel)     , POINTER :: model
         CLASS(FTLinkedList), POINTER :: list
!
!        ---------------
!        Local Variables
!        ---------------
!
         TYPE(SMNodePtr), DIMENSION(:), POINTER :: nodeArray => NULL()
         
         INTEGER, EXTERNAL :: Loop

         
         CLASS(SMNode)  , POINTER       :: node => NULL(), prevNode => NULL(), jointNode => NULL(), nextNode => NULL()
         CLASS(SMNode)  , POINTER       :: startNode => NULL()
         TYPE(SMNodePtr), DIMENSION(2)  :: newNodes
         TYPE(SMNodePtr), DIMENSION(2)  :: boundaryEdgeNodes
         TYPE(SMNodePtr), DIMENSION(4)  :: elementNodes

         CLASS(SMCurve)       , POINTER :: cStart => NULL(), cEnd => NULL()
         CLASS(SMChainedCurve), POINTER :: chain => NULL()
         CLASS(SMElement)     , POINTER :: e => NULL()
         CLASS(FTObject)      , POINTER :: obj => NULL()
         
         INTEGER                        :: k, nodeArraySize
         INTEGER                        :: jointType, bCurveSide
         
         REAL(KIND=RP)                  :: t, p(3)
         REAL(KIND=RP)                  :: tStart, tEnd
!
!        ---------
!        Externals
!        ---------
!
!
!        ---------------------------------------
!        Step through each edge on this boundary
!        and add a new node along the boundary
!        itself to make new elements.
!        ---------------------------------------
!
         nodeArray     => GatheredNodes( list )
         nodeArraySize = SIZE(nodeArray)
!
!        --------------------------
!        Get the first node started
!        --------------------------
!
         node       => nodeArray(1) % node
         bCurveSide =  node % bCurveSide
         t          =  node % whereOnBoundary
         cStart     => model % curveWithID(node % bCurveID, chain)
         
         ALLOCATE(node)
         CALL initBoundaryNode( cStart, chain, t, bCurveSide, mesh % newNodeID(), node )
         obj => node
         CALL mesh % nodes % add(obj)
         CALL node % release()! We won't be keeping it beyond this scope
         prevNode  => node
         startNode => node
!
!        ------------------------
!        Now work around the loop
!        ------------------------
!
         k = 1
         DO WHILE ( k <= nodeArraySize )
         
            IF ( k < nodeArraySize )     THEN
               jointType = nodeArray(k+1) % node % nodeType
            ELSE
               jointType = nodeArray(1) % node % nodeType
            END IF
            
            SELECT CASE ( jointType )
            
               CASE( NONE, ROW_SIDE )
!
!                 ---------------------------------------------------------------
!                 Create one element by projecting the edge to the boundary curve
!                 ---------------------------------------------------------------
!
                  newNodes(1) % node          => prevNode
                  boundaryEdgeNodes(1) % node => nodeArray(k) % node
!
!                 -----------------------------------------------------------------
!                 Before the end of the loop, create a new node along the boundary,
!                 otherwise, wrap around to the beginning.
!                 -----------------------------------------------------------------
!
                  IF( k < nodeArraySize )     THEN
                     node                        => nodeArray(k+1) % node
                     boundaryEdgeNodes(2) % node => nodeArray(k+1) % node
                     t                           =  node % whereOnBoundary
                     cEnd                        => model % curveWithID(node % bCurveID, chain)
                     
                     ALLOCATE(node)
                     CALL initBoundaryNode( cEnd, chain, t, bCurveSide, mesh % newNodeID(), node )

                     newNodes(2) % node => node
                     obj => node
                     CALL mesh % nodes % add(obj)
                     CALL node % release()
                  ELSE
                     boundaryEdgeNodes(2) % node => nodeArray(1) % node
                     newNodes(2) % node          => startNode
                  END IF
!
!                 ----------------------------------------------------
!                 Create element and add element and nodes to the mesh
!                 ----------------------------------------------------
!
                  e   => boundaryElementForNodes( mesh % newElementID(), boundaryEdgeNodes, newNodes )
                  obj => e
                  CALL mesh % elements % add(obj)
                  CALL e % release()
                  IF( k == nodeArraySize )   CALL newNodes(2) % node % release()
                  prevNode => newNodes(2) % node
                  
               CASE( ROW_END )
!
!                 ---------------------------------------------------------------
!                 A row-end sees the insertion of at least three elements. One is
!                 a wedge element whose corner is at the joint between the two
!                 curves. The (small) angle is not split this way.
!
!                 First Projection Element - like a row side
!                 ---------------------------------------------------------------
!
                  newNodes(1) % node          => prevNode ! first corner node
                  boundaryEdgeNodes(1) % node => nodeArray(k) % node
                  IF( k < nodeArraySize )     THEN
                     jointNode => nodeArray(k+1) % node
                  ELSE ! Wrap around
                     jointNode => nodeArray(1) % node
                  END IF
                  boundaryEdgeNodes(2) % node => jointNode
                  p = jointNode % x

                  tStart = nodeArray(k) % node % whereOnBoundary
                  tEnd   = 1.0_RP
                  cStart => model % curveWithID(nodeArray(k) % node % bCurveID, chain)

                  t      = fMin( tStart, tEnd, cStart, p, (/0.0_RP, 0.0_RP, 0.0_RP/), minimizationTolerance )
                  
                  ALLOCATE(node)
                  CALL initBoundaryNode( cStart, chain, t, bCurveSide, mesh % newNodeID(), node )
                  
                  newNodes(2) % node => node
                  obj => node
                  CALL mesh % nodes % add(obj)
                  CALL node % release()
!
                  e => boundaryElementForNodes( mesh % newElementID(), boundaryEdgeNodes, newNodes )
                  obj => e
                  CALL mesh % elements % add(obj)
                  CALL e % release()
                  prevNode => newNodes(2) % node
                  CALL newNodes(2) % node % release()
!
!                 -------------
!                 Wedge Element
!                 -------------
!
                  elementNodes(1) % node => jointNode
                  elementNodes(2) % node => prevNode
!
!                 ------------
!                 Joint corner
!                 ------------
!
                  t    =  1.0_RP
                  
                  ALLOCATE(node)
                  CALL initBoundaryNode( cStart, chain, t, bCurveSide, mesh % newNodeID(), node )
                  elementNodes(3) % node => node
                  obj => node
                  CALL mesh % nodes % add(obj)
                  CALL node % release()
                  elementNodes(3) % node % nodeType = ROW_END
!
!                 ----------------------------------------------
!                 Closest point on the *next* curve in the chain
!                 ----------------------------------------------
!
                  IF ( k+2 <= nodeArraySize )     THEN
                     nextNode => nodeArray(k+2) % node
                  ELSE
                     nextNode => nodeArray(1) % node !Wrap around
                  END IF
                  
                  cEnd   => model % curveWithID(nextNode % bCurveID, chain)
                  tStart = 0.0_RP
                  tEnd   = nextNode % whereOnBoundary ! No further than the next node along the chain
                  t      = fMin( tStart, tEnd, cEnd, p, (/0.0_RP, 0.0_RP, 0.0_RP/), minimizationTolerance )
                  
                  ALLOCATE(node)
                  CALL initBoundaryNode( cEnd, chain, t, bCurveSide, mesh % newNodeID(), node )
                  
                  elementNodes(4) % node => node
                  obj => node
                  CALL mesh % nodes % add(obj)
                  CALL node % release()
!
!                 ----------------------------------------
!                 Create wedge element and add to the mesh
!                 ----------------------------------------
!
                  e   => boundaryElementFor4Nodes( mesh % newElementID(), elementNodes )
                  obj => e
                  CALL mesh % elements % add(obj)
                  CALL e % release()
!
!                 ----------------------------------------------------------------
!                 Prepare for the next element, which should be a row-side element
!                 ----------------------------------------------------------------
!
                  prevNode => elementNodes(4) % node
!
                  
               CASE( ROW_CORNER )
!
!                 -----------------------------------------------------
!                 A row corner node needs a single wedge element
!                 to avoid creating triangles. The wedge element
!                 uses the previous and next nodes along the boundary
!                 edge plus the reversal point along the boundary. This
!                 requires the prev % node % x to be moved to that point.
!                 -----------------------------------------------------
!
                  elementNodes(1) % node => nodeArray(k) % node
                  IF ( k < nodeArraySize )     THEN
                     elementNodes(4) % node => nodeArray(k+1) % node
                  ELSE
                     elementNodes(4) % node => nodeArray(1) % node
                  END IF
                  IF ( k < nodeArraySize-1 )     THEN
                     elementNodes(3) % node => nodeArray(k+2) % node
                  ELSE IF ( k == nodeArraySize-1 )     THEN
                     elementNodes(3) % node => nodeArray(1) % node
                  ELSE
                     elementNodes(3) % node => nodeArray(2) % node
                  END IF
!
!                 -------------------------------------------------
!                 Find the location of the joint along the boundary
!                 and move the prevNode position to that point. Use
!                 that as a corner in the wedge element.
!                 -------------------------------------------------
!
                  cStart => model % curveWithID(nodeArray(k) % node % bCurveID, chain)
                  p                           = cStart % positionAt(1.0_RP)
                  prevNode % x                = p
                  prevNode % whereOnBoundary  = 1.0_RP
                  prevNode % gWhereOnBoundary = chain % ChainTForCurveTInCurve( 1.0_RP, cStart )
                  prevNode % nodeType         = ROW_CORNER
                  
                  elementNodes(2) % node => prevNode
                  
                  e => boundaryElementFor4Nodes( mesh % newElementID(), elementNodes )
                  obj => e
                  CALL mesh % elements % add(obj)
                  CALL e % release()
!
!                 ----------------------------------------
!                 Get ready to move on - skip the k+1 node
!                 ----------------------------------------
!
                  prevNode => elementNodes(2) % node
                  k = k + 1
                  
               CASE( ROW_REVERSAL )
!
!                 --------------------------------------------------------------
!                 A Row reversal node needs TWO elements to be created from four
!                 consecutive edges.
!                 --------------------------------------------------------------
!
                  elementNodes(1) % node => nodeArray(k) % node
                  elementNodes(2) % node => nodeArray(Loop(k+1,nodeArraySize)) % node
                  elementNodes(3) % node => nodeArray(Loop(k+2,nodeArraySize)) % node
!
!                 -------------------------------------------------
!                 Find the location of the joint along the boundary
!                 and move the prevNode position to that point. Use
!                 that as a corner in the wedge element.
!                 -------------------------------------------------
!
                  cStart => model % curveWithID(nodeArray(k) % node % bCurveID, chain)
                  p                           = cStart % positionAt(1.0_RP)
                  prevNode % x                = p
                  prevNode % whereOnBoundary  = 1.0_RP
                  prevNode % gWhereOnBoundary = chain % ChainTForCurveTInCurve( 1.0_RP, cStart )
                  prevNode % nodeType         = ROW_REVERSAL
                  elementNodes(4) % node => prevNode
                  e => boundaryElementFor4Nodes( mesh % newElementID(), elementNodes )
                  obj => e
                  CALL mesh % elements % add(obj)
                  CALL e % release()
!
!                 ----------------------------------------------
!                 Get ready to move on - skip the next two nodes
!                 ----------------------------------------------
!
                  prevNode => elementNodes(4) % node
                  k = k + 2
!
!                 --------------------
!                 Second Wedge Element
!                 --------------------
!
                  elementNodes(1) % node => prevNode
                  elementNodes(2) % node => nodeArray(k) % node
                  elementNodes(3) % node => nodeArray(Loop(k+1,nodeArraySize)) % node
                  elementNodes(4) % node => nodeArray(Loop(k+2,nodeArraySize)) % node
                  
                  e => boundaryElementFor4Nodes( mesh % newElementID(), elementNodes )
                  obj => e
                  CALL mesh % elements % add(obj)
                  CALL e % release()
                  
                  prevNode => elementNodes(1) % node
                  k = k + 1
                  
               CASE DEFAULT
                  PRINT *, "BAD row type" !DEBUG
                  STOP
            END SELECT
            k = k + 1
         END DO
         
         DEALLOCATE( nodeArray )         
         
      END SUBROUTINE GenerateBoundaryElements
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE flagBoundaryTypes
!
!     ------------------------------------------------------------------------
!     Assign a boundary type (EXTERIOR, INTERIOR, INTERFACE) according to the 
!     curve's id
!     ------------------------------------------------------------------------
!
         IMPLICIT NONE
         INTEGER :: k
         
         IF(ASSOCIATED( outerBoundaryCurve ))     THEN
            curveTypeForID(outerBoundaryCurve % id) = OUTER
         END IF
         
          IF(ASSOCIATED( interiorCurves ))     THEN
            DO k = 1, SIZE(interiorCurves)
               curveTypeForID(interiorCurves(k) % curveArray % id) = INNER
            END DO
         END IF
        
         IF( ASSOCIATED( interfaceCurves ) )     THEN
             DO k = 1, SIZE(interfaceCurves)
               curveTypeForID(interfaceCurves(k) % curveArray % id) = INTERIOR_INTERFACE
            END DO
         END IF
         
      END SUBROUTINE flagBoundaryTypes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MarkExteriorElements( mesh, backgroundParams )
      USE Geometry
      IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMMesh)                 , POINTER :: mesh
         TYPE(BackgroundGridParameters)          :: backgroundParams
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedList)        , POINTER :: elements => NULL()
         CLASS(SMElement)           , POINTER :: e => NULL()
         CLASS(SegmentedCurveArray) , POINTER :: curveArray => NULL()
         CLASS(SMNode)              , POINTER :: node1 => NULL(), node2 => NULL(), node => NULL()
         CLASS(FTLinkedListIterator), POINTER :: elementIterator => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()
         
         INTEGER                             :: k, l, j, m, mSteps
         INTEGER                             :: w
         REAL(KIND=RP)                       :: nodes(3,4)
         REAL(KIND=RP)                       :: x1(3), x2(3), xRight, yTop, xLeft, yBottom
         REAL(KIND=RP)                       :: h, hMin, dz, z(3), segmentLength
         LOGICAL                             :: removedHere
         
         elements        => mesh % elements
         elementIterator => mesh % elementsIterator
!
!        ------------------------------------------------
!        Mark elements outside or near the outer boundary
!        ------------------------------------------------
!
         IF( ASSOCIATED( outerBoundaryCurve ) )    THEN
            
            CALL elementIterator % setToStart()
            DO WHILE(.NOT.elementIterator % isAtEnd())
               obj => elementIterator % object()
               CALL cast(obj,e)
!
!              -----------------------------------------------
!              Remove element if it has a node that is outside
!              the outer boundary curve (winding number =0)
!              -----------------------------------------------
!
               removedHere = .false.
               
               DO k = 1, 4
                  obj => e % nodes % objectAtIndex(k)
                  CALL cast(obj,node)
                  
                  w = ACWindingFunction( node % x, outerBoundaryCurve % x, &
                                         outerBoundaryCurve % nSegments )
                  IF ( abs(w) == 0 ) THEN
                     e % remove    = .true.
                     removedHere   = .true.
                     EXIT
                  END IF
                  
               END DO
!
!              --------------------------------------------
!              Let the nodes know which curve they are near
!              --------------------------------------------
!
               IF( e % remove .AND. removedHere )     THEN
                  DO k = 1, 4
                     obj => e % nodes % objectAtIndex(k)
                     CALL cast(obj,node)
                     node % bCurveChainID = outerBoundaryCurve % id
                     node % bCurveSide    = INSIDE
                  END DO
               ELSE
!
!                 -----------------------------------------------
!                 Any node in this element is inside, so mark one
!                 -----------------------------------------------
!
                  obj => e % nodes % objectAtIndex(1)
                  CALL cast(obj,node)
                  aPointInsidetheCurve(:,outerBoundaryCurve % id) = node % x
               END IF
               
               CALL elementIterator % moveToNext() 
            END DO  
            
         ELSE ! we just have the outer box
        
            xRight  = backgroundParams % x0(1) + backgroundParams % dx(1)*backgroundParams % N(1)
            yTop    = backgroundParams % x0(2) + backgroundParams % dx(2)*backgroundParams % N(2)
            xLeft   = backgroundParams % x0(1)
            yBottom = backgroundParams % x0(2)
            
            CALL elementIterator % setToStart()
            DO WHILE ( .NOT.elementIterator % isAtEnd() )
               obj => elementIterator % object()
               CALL cast(obj,e)
               removedHere = .false.
               
               DO k = 1, 4
                  obj => e % nodes % objectAtIndex(edgeMap(1,k))
                  CALL cast(obj,node1)
                  obj => e % nodes % objectAtIndex(edgeMap(2,k))
                  CALL cast(obj,node2)
                  
                  x1    = node1 % x
                  x2    = node2 % x
                  IF( AlmostEqual(x1(2),yBottom) .AND. AlmostEqual(x2(2),yBottom) )      THEN
                      node1 % bCurveID      = BOTTOM
                      node1 % bCurveChainID = OUTER_BOX
                      node1 % bCurveSide    = INSIDE
                      node2 % bCurveID      = BOTTOM
                      node2 % bCurveChainID = OUTER_BOX
                      node2 % bCurveSide    = INSIDE
                      node1 % distToBoundary = 0.0_RP
                      node2 % distToBoundary = 0.0_RP
                      IF( AlmostEqual(x1(1),xLeft) .OR. AlmostEqual(x1(1), xRight) )     THEN
                          node1 % nodeType = CORNER_NODE
                      END IF
                      IF( AlmostEqual(x2(1), xLeft) .OR. AlmostEqual(x2(1), xRight) )    THEN
                          node2 % nodeType = CORNER_NODE
                      END IF
                  ELSE IF ( AlmostEqual(x1(2),yTop) .AND. AlmostEqual(x2(2), yTop) )     THEN
                      node1 % bCurveID      = TOP
                      node1 % bCurveChainID = OUTER_BOX
                      node1 % bCurveSide    = INSIDE
                      node2 % bCurveID      = TOP
                      node2 % bCurveChainID = OUTER_BOX
                      node2 % bCurveSide    = INSIDE
                      node1 % distToBoundary = 0.0_RP
                      node2 % distToBoundary = 0.0_RP
                      IF( AlmostEqual(x1(1),xLeft) .OR. AlmostEqual(x1(1),xRight))       THEN
                          node1 % nodeType = CORNER_NODE
                      END IF
                      IF( AlmostEqual(x2(1),xLeft) .OR. AlmostEqual(x2(1),xRight))       THEN
                          node2 % nodeType = CORNER_NODE
                      END IF
                  ELSE IF ( AlmostEqual(x1(1),xLeft) .AND. AlmostEqual(x2(1),xLeft))     THEN
                      node1 % bCurveID      = LEFT
                      node1 % bCurveChainID = OUTER_BOX
                      node1 % bCurveSide    = INSIDE
                      node2 % bCurveID      = LEFT
                      node2 % bCurveChainID = OUTER_BOX
                      node2 % bCurveSide    = INSIDE
                      node1 % distToBoundary = 0.0_RP
                      node2 % distToBoundary = 0.0_RP
                      IF( AlmostEqual(x1(2),yTop) .OR. AlmostEqual(x1(2),yBottom) )      THEN
                          node1 % nodeType = CORNER_NODE
                      END IF
                      IF( AlmostEqual(x2(2), yTop) .OR. AlmostEqual(x2(2), yBottom) )    THEN
                          node2 % nodeType = CORNER_NODE
                      END IF
                  ELSE IF ( AlmostEqual(x1(1),xRight) .AND. AlmostEqual(x2(1),xRight) )  THEN
                      node1 % bCurveID      = RIGHT
                      node1 % bCurveChainID = OUTER_BOX
                      node1 % bCurveSide    = INSIDE
                      node2 % bCurveID      = RIGHT
                      node2 % bCurveChainID = OUTER_BOX
                      node2 % bCurveSide    = INSIDE
                      node1 % distToBoundary = 0.0_RP
                      node2 % distToBoundary = 0.0_RP
                      IF( AlmostEqual(x1(2),yTop) .OR. &
                          AlmostEqual(x1(2),yBottom) )     THEN
                          node1 % nodeType = CORNER_NODE
                      END IF
                      IF( AlmostEqual(x2(2),yTop) .OR. AlmostEqual(x2(2),yBottom) )     THEN
                          node2 % nodeType = CORNER_NODE
                      END IF
                  END IF
                 
               END DO
               
               CALL elementIterator % moveToNext()
            END DO
         END IF
!
!        --------------------------------
!        Do the same for inner boundaries
!        --------------------------------
!
         IF( ASSOCIATED( interiorCurves ) )    THEN         
            DO l = 1, SIZE(interiorCurves)
               curveArray => interiorCurves(l) % curveArray
               
               CALL elementIterator % setToStart()
               
               DO WHILE ( .NOT.elementIterator % isAtEnd() )
                  obj => elementIterator % object()
                  CALL cast(obj,e)
                  removedHere = .FALSE.
                  
                  IF( .NOT.e % remove )     THEN
                     
                        DO k = 1, 4
                           
                          obj => e % nodes % objectAtIndex(k)
                          CALL cast(obj,node)
                          w = ACWindingFunction( node % x, curveArray % x, curveArray % nSegments )
                          IF ( ABS(w) >= 1 ) THEN
                             aPointInsidetheCurve(:,curveArray % id) = node % x
                             e % remove                              = .true.
                             removedHere                             = .true.
                             EXIT
                          END IF
                        END DO
!
!                       ----------------------------------------------------------------
!                       For thin, sharp interior objects like an airfoil, it is possible
!                       for all points in an element to be outside, yet boundary points
!                       fall within an element. Check for this, too.
!                       TODO: This is just a linear search through all elements. Should
!                       use a better data structure like a spatial tree.
!                       ----------------------------------------------------------------
!
                        IF( .NOT.e % remove )     THEN
                        
                           DO k = 1, 4 
                              obj => e % nodes % objectAtIndex(k)
                              CALL cast(obj,node)
                              nodes(:,k) = node % x
                           END DO
!
!                          --------------------------
!                          See how big the element is
!                          --------------------------
!
                           hMin = SQRT( (nodes(1,1) - nodes(1,4))**2 + (nodes(2,1) - nodes(2,4))**2)
                           DO k = 1, 3 
                              h = SQRT( (nodes(1,k+1) - nodes(1,k))**2 + (nodes(2,k+1) - nodes(2,k))**2)
                              hMin = MIN( h, hMin )
                           END DO
!
!                          --------------------------------------------------------------------
!                          See if any of the nodes are inside the element. If the elements 
!                          are small compared to the spacing on the curve, linearly interpolate
!                          the curve segment and see if those fall inside the element. HACK:
!                          the decision on how big is too big is arbitrary at the moment.
!                          --------------------------------------------------------------------
!

                           DO j = 0, curveArray % nSegments
!
!                             ---------------------------------------------------
!                             See if one of the boundary points is in the element
!                             ---------------------------------------------------
!
                              IF ( PointInQuad( nodes, curveArray % x(:,j) ) )     THEN
                                 e % remove  = .true.
                                 removedHere = .true.
                                 EXIT
                              END IF
!!
!                             ------------------------------------------------------------------
!                             If not, interpolate (currently see how much smaller the element is
!                             and divide by 5 )
!                             ------------------------------------------------------------------
!
                              x1 = curveArray % x(:,j)
                              IF ( j == curveArray % nSegments  )     THEN
                                 x2 = curveArray % x(:,0)
                              ELSE
                                 x2 = curveArray % x(:,j+1)
                              END IF 
                              segmentLength = SQRT( (x2(1) - x1(1))**2 + (x2(2) - x1(2))**2)
                              IF( segmentLength > 0.25_RP*hMin )     THEN
                                 mSteps = NINT(segmentLength/hMin)*5
                                 dz = 1.0_RP/DBLE(mSteps)
                                 DO m = 1, mSteps-1
                                    z = x1 + m*dz*(x2 - x1)
                                    IF ( PointInQuad( nodes, z ) )     THEN
                                       e % remove    = .true.
                                       removedHere   = .true.
                                       EXIT
                                    END IF
                                 END DO
                                 IF( e % remove )     EXIT
                              END IF
                           END DO
                        END IF
!
!                       --------------------------------------------
!                       Let the nodes know which curve they are near
!                       --------------------------------------------
!
                        IF ( e % remove .AND. removedHere )     THEN
                          DO k = 1, 4 
                              obj => e % nodes % objectAtIndex(k)
                              CALL cast(obj,node)
                              node % bCurveChainID = curveArray % id
                              node % bCurveSide    = OUTSIDE
                          END DO
                        END IF
                     
                  END IF
                  
                  CALL elementIterator % moveToNext()
               END DO
               
            END DO
!
!           ----------------------------------------------
!           A point guaranteed to be inside an outer curve
!           is inside an inner curve
!           ----------------------------------------------
!
            IF( ASSOCIATED( outerBoundaryCurve ) ) THEN 
               aPointInsidetheCurve(:,1) = aPointInsidetheCurve(:,2)
            END IF
         END IF
        
      END SUBROUTINE MarkExteriorElements
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MarkInterfaceElements( mesh )
      USE Geometry
      IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMMesh)                 , POINTER :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedList)        , POINTER :: elements   => NULL()
         CLASS(SMElement)           , POINTER :: e          => NULL()
         CLASS(SegmentedCurveArray) , POINTER :: curveArray => NULL()
         CLASS(SMNode)              , POINTER :: node       => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator   => NULL()
         CLASS(FTObject)            , POINTER :: obj        => NULL()
         
         INTEGER                              :: k, l, j, m, mSteps
         REAL(KIND=RP)                        :: w
         REAL(KIND=RP)                        :: nodes(3,4)
         REAL(KIND=RP)                        :: x1(3), x2(3) !, xRight, yTop, xLeft, yBottom
         REAL(KIND=RP)                        :: h, hMin, dz, z(3), segmentLength
         LOGICAL                              :: isInterfaceElement
         INTEGER                              :: numInside, numOutside, side(4)
         
         IF( .NOT.ASSOCIATED( interfaceCurves ) )    RETURN
!
         elements => mesh % elements
         iterator => mesh % elementsIterator
!
!        --------------------------------------
!        Mark elements and nodes for each curve
!        --------------------------------------
!
         DO l = 1, SIZE(interfaceCurves)
            curveArray => interfaceCurves(l)%curveArray
            
            CALL iterator % setToStart()
!
!           -------------------------
!           Loop through each element
!           -------------------------
!
            DO WHILE ( .NOT.iterator % isAtEnd() )
               obj => iterator % object()
               CALL cast(obj,e)
               isInterfaceElement = .false.
               
               IF( .NOT.e % remove )     THEN
!
!                 -------------------------------------------------------
!                 Remove elements that have both inside and outside nodes
!                 -------------------------------------------------------
!
                  numInside  = 0
                  numOutside = 0
                  DO k = 1, 4
                     obj => e % nodes % objectAtIndex(k)
                     CALL cast(obj,node)
                     
                     w = ACWindingFunction( node % x, curveArray % x, curveArray % nSegments-1 )
                     IF ( ABS(w) > 0.6_RP ) THEN
                        side(k)                       = INSIDE
                        numInside                     = numInside + 1
                        aPointInsidetheCurve(:,curveArray % id) = node % x
                      ELSE IF ( abs(w) <= EPSILON(w) ) THEN
                        side(k)                       = OUTSIDE
                        numOutside                    = numOutside + 1
                      END IF
                  END DO
!
!                 ----------------------------------------------------
!                 Those that do straddle the curve and must be removed
!                 Otherwise, check if the element is too small and do
!                 an interpolation to see if a point is inside.
!                 ----------------------------------------------------
!
                  IF ( numInside > 0 .AND. numOutside > 0 )     THEN
                     isInterfaceElement = .TRUE.
                     DO k = 1,4
                        obj => e % nodes % objectAtIndex(k)
                        CALL cast(obj,node)
                        node % bCurveSide = side(k)
                     END DO
                  ELSE
!
!                    ------------------------------------
!                    Grab the four corners of the element
!                    ------------------------------------
!
                     DO k = 1, 4 
                        obj => e % nodes % objectAtIndex(k)
                        CALL cast(obj,node)
                        nodes(:,k) = node % x
                     END DO
!
!                    --------------------------
!                    See how big the element is
!                    --------------------------
!
                     hMin = SQRT( (nodes(1,1) - nodes(1,4))**2 + (nodes(2,1) - nodes(2,4))**2)
                     DO k = 1, 3 
                        h = SQRT( (nodes(1,k+1) - nodes(1,k))**2 + (nodes(2,k+1) - nodes(2,k))**2)
                        hMin = MIN( h, hMin )
                     END DO
!
!                    ----------------------------------------------------------------------
!                    See if any of the curve points are inside the element. If the elements 
!                    are small compared to the spacing on the curve, linearly interpolate
!                    the curve segment and see if those fall inside the element. HACK:
!                    the decision on how big is too big is arbitrary at the moment.
!                    -----------------------------------------------------------------------
!
                     DO j = 1, curveArray % nSegments-1
!
!                       ---------------------------------------------------
!                       See if one of the boundary points is in the element
!                       ---------------------------------------------------
!
                        IF ( PointInQuad( nodes, curveArray % x(:,j) ) )     THEN
                           isInterfaceElement = .true.
                           EXIT
                        END IF
!
!                       ------------------------------------------------------------------
!                       If not, interpolate (currently see how much smaller the element is
!                       and divide by 5 )
!                       ------------------------------------------------------------------
!
                        x1 = curveArray % x(:,j)
                        x2 = curveArray % x(:,j+1)
                        segmentLength = SQRT( (x2(1) - x1(1))**2 + (x2(2) - x1(2))**2)
                        
                        IF( segmentLength > 0.25*hMin )     THEN
                           mSteps = NINT(segmentLength/hMin)*5
                           dz = 1.0_RP/DBLE(mSteps)
                           DO m = 1, mSteps-1
                              z = x1 + m*dz*(x2 - x1)
                              IF ( PointInQuad( nodes, z ) )     THEN
                                 isInterfaceElement = .true.
                                EXIT
                              END IF
                           END DO
                           IF( isInterfaceElement )     EXIT
                        END IF
                        
                     END DO !Loop over segments
                  END IF
                  
               END IF !(.NOT.e%remove)
!
!              --------------------------------------------
!              Let the nodes know which curve they are near
!              --------------------------------------------
!
               IF ( isInterfaceElement )     THEN                        
                  DO k = 1, 4 
                     obj => e % nodes % objectAtIndex(k)
                     CALL cast(obj,node)
                     node % bCurveChainID = curveArray % id
!
!                    ------------------------------------------------------------------
!                    To order boundary edges, we need to know a point inside this curve
!                    ------------------------------------------------------------------
!
                     IF(node % bCurveSide == UNDEFINED )     THEN 
                        w = ACWindingFunction( node % x, curveArray % x, curveArray % nSegments-1 )

                        IF ( ABS(w) > 0.6_RP ) THEN
                           aPointInsidetheCurve(:,curveArray % id) = node % x
                           node % bCurveSide    = INSIDE
                        ELSE IF ( abs(w) <= EPSILON(w) ) THEN
                           node % bCurveSide    = OUTSIDE
                        END IF
                     END IF
                  END DO
               END IF
               
               CALL iterator % moveToNext()
            END DO !WHILE ( .NOT.iterator % isAtEnd )
            
         END DO !l = 1, SIZE(interfaceCurves)
         
      END SUBROUTINE MarkInterfaceElements
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initBoundaryNode( c, chain, t, bCurveSide, id, node )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP)                  :: t
         CLASS(SMCurve)       , POINTER :: c
         CLASS(SMChainedCurve), POINTER :: chain
         CLASS(SMNode)        , POINTER :: node
         INTEGER                        :: bCurveSide, id
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP) :: p(3)
         
         p = c % positionAt(t)
         CALL node % initWithLocationAndID(p,id)
         
         node % whereOnBoundary  = t
         node % gWhereOnBoundary = chain % ChainTForCurveTInCurve( t, c )
         node % distToBoundary   = 0.0_RP
         node % bCurveID         = c % id()
         node % bCurveChainID    = chain % id()
         node % bCurveSide       = bCurveSide
         node % nodeType         = ROW_SIDE !Most will be, change later if necessary
         
      END SUBROUTINE initBoundaryNode
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION boundaryElementForNodes( elementID, oldNodes, newNodes ) RESULT(e)
         USE Geometry
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                       :: elementID
         TYPE(SMNodePtr), DIMENSION(2) :: oldNodes, newNodes
         TYPE(SMElement), POINTER      :: e
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP)  , DIMENSION(3,2) :: oldX, newX
         REAL(KIND=RP)  , DIMENSION(3)   :: u, v
         TYPE(SMNodePtr), DIMENSION(4)   :: eNodes
         
         
         oldX(:,1) = oldNodes(1) % node % x
         oldX(:,2) = oldNodes(2) % node % x
         newX(:,1) = newNodes(1) % node % x
         newX(:,2) = newNodes(2) % node % x
         
         u = oldX(:,2) - oldX(:,1)
         v = newX(:,1) - oldX(:,1)
         IF( CrossProductDirection(u,v) == UP )     THEN
            eNodes(1) % node => oldNodes(1) % node
            eNodes(2) % node => oldNodes(2) % node
            eNodes(3) % node => newNodes(2) % node
            eNodes(4) % node => newNodes(1) % node
         ELSE
            eNodes(1) % node => oldNodes(1) % node
            eNodes(2) % node => newNodes(1) % node
            eNodes(3) % node => newNodes(2) % node
            eNodes(4) % node => oldNodes(2) % node
         END IF
         
         ALLOCATE(e)
         CALL e % initWithNodesIDAndType(eNodes, elementID, QUAD )
         
      END FUNCTION boundaryElementForNodes
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION boundaryElementFor4Nodes( elementID, newNodes ) RESULT(e)
         USE Geometry
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(SMNodePtr) , DIMENSION(4)   :: newNodes
         CLASS(SMElement), POINTER        :: e
         INTEGER                          :: elementID
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP)  , DIMENSION(3)   :: x1, x2, x4
         REAL(KIND=RP)  , DIMENSION(3)   :: u, v
         TYPE(SMNodePtr), DIMENSION(4)   :: eNodes
         
         ALLOCATE(e)
         
         x1 = newNodes(1) % node % x
         x2 = newNodes(2) % node % x
         x4 = newNodes(4) % node % x
         
         u = x2 - x1
         v = x4 - x1
         IF( CrossProductDirection(u,v) == UP )     THEN
            CALL e % initWithNodesIDAndType(newNodes, elementID, QUAD )
         ELSE
            eNodes(1) % node => newNodes(1) % node
            eNodes(2) % node => newNodes(4) % node
            eNodes(3) % node => newNodes(3) % node
            eNodes(4) % node => newNodes(2) % node

            CALL e % initWithNodesIDAndType(eNodes, elementID, QUAD )
         END IF
         
      END FUNCTION boundaryElementFor4Nodes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FlagEndNodes( list, model )
      IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList), POINTER :: list
         CLASS(SMModel)     , POINTER :: model
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMChainedCurve)         , POINTER :: boundaryCurve => NULL()
         CLASS(SMCurve)                , POINTER :: currentCurve  => NULL()
         CLASS(SMNodePtr), DIMENSION(:), POINTER :: nodeArray     => NULL()
         CLASS(SMNode)                 , POINTER :: node          => NULL()
         INTEGER                                 :: nodeArraySize
         INTEGER                                 :: j, k, jNode, id, jNode0, jNode1
         REAL(KIND=RP)                           :: t, dt, t0, t1, x, dMin, d
         
         nodeArray     => GatheredNodes( list )
         nodeArraySize = SIZE(nodeArray)
!
!        -------------------------
!        Set the default node type
!        -------------------------
!
         DO j = 1, nodeArraySize 
            nodeArray(j) % node % nodeType = ROW_SIDE
         END DO
!
!        ------------------------
!        Get the associated chain
!        ------------------------
!
         id            = nodeArray(1) % node % bCurveChainID
         boundaryCurve => model % chainWithID(id)
!
!        -----------------------------------------------------------
!        For each joint in the boundary curve, find the closest
!        node in terms of "t" to flag as an end node. This will also
!        set the location on the boundary to that joint location
!
!        TODO: If this gets's expensive, can use a quadtree to
!        bin the nodes so that obviously far nodes aren't checked.
!        -----------------------------------------------------------
!
         dt = 1.0_RP/boundaryCurve % numberOfCurvesInChain
!
!        -------------------
!        Start/End point...
!        TODO: Should bisect
!        -------------------
!
         t0 = HUGE(t0)
         t1 = -10.0_RP !Doesn't really matter...
         
         DO j = 1, nodeArraySize 
            t = nodeArray(j) % node % gWhereOnBoundary            
            IF( t < t0 )     THEN
               t0     = t
               jNode0 = j
            END IF
            IF( t > t1 )     THEN
               t1     = t
               jNode1 = j
            END IF
         END DO
         IF( t0 < 1.0_RP - t1 )     THEN
           nodeArray(jNode0) % node % whereOnBoundary = 0.0_RP
           nodeArray(jNode0) % node % nodeType        = boundaryCurve % jointClassification(0)
           nodeArray(jNode0) % node % bCurveID        = boundaryCurve % myCurveIDs(1)
         ELSE
           nodeArray(jNode1) % node % whereOnBoundary = 1.0_RP
           nodeArray(jNode1) % node % nodeType        = boundaryCurve % jointClassification(0)
         END IF
!
!       --------------------
!       All the other points
!       --------------------
!
         DO k = 1, boundaryCurve % numberOfCurvesInChain-1
            t = k*dt
!
!           ----------------------
!           Find the closest point: 
!           TODO should bisect
!           ----------------------
!
            dMin = HUGE(dMin)
            DO j = 1, SIZE(nodeArray) 
               x = nodeArray(j) % node % gWhereOnBoundary
               d = ABS(t - x)
               IF( d < dMin )     THEN
                  dMin = d
                  jNode = j
               END IF
            END DO
!
!           ------------------------
!           Set the node information
!           ------------------------
!
            nodeArray(jNode) % node % bCurveID        = boundaryCurve % myCurveIDs(k+1)
            nodeArray(jNode) % node % whereOnBoundary = 0.0_RP
            nodeArray(jNode) % node % nodeType        = boundaryCurve % jointClassification(k)
         END DO
!
!        ---------------------------------------------------------------------------------
!        At Row-Reversal points along a curve, it is possible that the row-reversal point
!        is the closest point along the curve to more than one node. To account for this, 
!        mark all points within the tolerance used to find the closest point by the joint
!        classification.
!        ---------------------------------------------------------------------------------
!
         DO k = 0, boundaryCurve % COUNT()-1
            
            IF( boundaryCurve % jointClassification(k) == ROW_REVERSAL )     THEN
               currentCurve => boundaryCurve % curveAtIndex(k+1)
               t            = boundaryCurve % ChainTForCurveTInCurve(0.0_RP,currentCurve)
               t1           = boundaryCurve % ChainTForCurveTInCurve(1.0_RP,currentCurve)
               
               DO j = 1, SIZE(nodeArray)
                  node => nodeArray(j) % node
                  
                  IF ( ABS(node % gWhereOnBoundary - t) <= minimizationTolerance )     THEN
                     node % whereOnBoundary  = 0.0_RP
                     node % gWhereOnBoundary = t
                     node % nodeType         = ROW_REVERSAL
                  ELSE IF ( ABS(t1 - node % whereOnBoundary) < minimizationTolerance )     THEN
                     node % whereOnBoundary  = 1.0_RP
                     node % gWhereOnBoundary = t1
                     node % nodeType         = ROW_REVERSAL
                  END IF
               END DO
               
            END IF
            
         END DO
!
!        --------
!        Clean up
!        --------
!
         DEALLOCATE( nodeArray )
         
      END SUBROUTINE FlagEndNodes

   END MODULE MeshGenerationMethods
!

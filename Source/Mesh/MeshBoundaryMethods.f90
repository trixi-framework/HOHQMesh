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
!      MeshBoundaryMethods.f90
!      Created: August 26, 2013 6:32 PM
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
   Module MeshBoundaryMethodsModule
   USE MeshSizerClass
   USE SMMeshClass
   USE ConnectionsModule
   IMPLICIT NONE
!
!-------------------------------------------------------------------
! Contains the operations on preparing the boundary edges for
! projection onto the model boundaries
!-------------------------------------------------------------------
!
      INTEGER, PARAMETER, PRIVATE :: TEST_NESTING = 1, TEST_INTERSECTION = 2
!
!     ========
      CONTAINS
!     ========
!
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE generateTemporaryBoundaryArrays( sizer )
!
!     --------------------------------------------------------------------
!     The winding number calculation requires an array of points
!     consecutively ordered. So convert the sizer's discrete curves, which
!     have duplicates at the ends of the component curves to
!     arrays with no duplicates
!     --------------------------------------------------------------------
!
         USE CurveConversionsModule
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(MeshSizer) :: sizer
!
!        ---------------
!        Local Variables
!        ---------------
!
         CLASS(FTObject) , POINTER             :: obj => NULL()
         INTEGER                               :: N, k
         TYPE (FTLinkedListIterator) , POINTER :: iterator => NULL()
         CLASS(ChainedSegmentedCurve), POINTER :: chain => NULL()
!
!        --------------
!        Outer boundary
!        --------------
!
         IF( ASSOCIATED( sizer % outerBoundary ) )     THEN
            outerBoundaryCurve => allocateAndInitSegmentedCurveFromSegmentedChain(sizer % outerBoundary)
         END IF
!
!        ----------------------------
!        Inner boundaries conversions
!        ----------------------------
!
         IF( ASSOCIATED(sizer % innerBoundariesList) )     THEN
            N = sizer % noOfInnerBoundaries
            ALLOCATE( interiorCurves(N) )

            ALLOCATE(iterator)
            CALL iterator % initWithFTLinkedList( sizer % innerBoundariesList)
            CALL iterator % setToStart()
            k = 0
            DO WHILE( .NOT.iterator % isAtEnd() )
               k = k+1
               obj => iterator % object()
               CALL castToChainedSegmentedCurve(obj,chain)
               interiorCurves(k) % curveArray => allocateAndInitSegmentedCurveFromSegmentedChain(chain)
               CALL iterator % moveToNext()
            END DO
            CALL releaseFTLinkedListIterator(iterator)
         END IF
!
!        --------------------------------
!        Interface boundaries conversions
!        --------------------------------
!
         IF( ASSOCIATED(sizer % interfaceBoundariesList) )     THEN
            N = sizer % noOfInterfaceBoundaries
            ALLOCATE( interfaceCurves(N) )

            ALLOCATE(iterator)
            CALL iterator % initWithFTLinkedList( sizer % interfaceBoundariesList)
            CALL iterator % setToStart()
            k = 0
            DO WHILE( .NOT.iterator % isAtEnd() )
               k = k+1
               obj => iterator % object()
               CALL castToChainedSegmentedCurve(obj,chain)
               interfaceCurves(k) % curveArray => allocateAndInitSegmentedCurveFromSegmentedChain(chain)
               CALL iterator % moveToNext()
            END DO
            CALL releaseFTLinkedListIterator(iterator)
         END IF

      END SUBROUTINE generateTemporaryBoundaryArrays
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE destroyTemporaryBoundaryArrays
         IMPLICIT NONE
         INTEGER                  :: k
         CLASS(FTObject), POINTER :: obj

         IF( ASSOCIATED( outerBoundaryCurve ) ) THEN
            obj => outerBoundaryCurve
            CALL release(obj)
         END IF

         IF( ASSOCIATED(interiorCurves) )     THEN
            DO k = 1, SIZE(interiorCurves)
               obj => interiorCurves(k) % curveArray
               CALL release(obj)
            END DO
         END IF

         IF( ASSOCIATED(interfaceCurves) )     THEN
            DO k = 1, SIZE(interfaceCurves)
               obj => interfaceCurves(k) % curveArray
               CALL release(obj)
            END DO
         END IF

         NULLIFY( outerBoundaryCurve )
         NULLIFY( interiorCurves )
         NULLIFY( interfaceCurves )
     END SUBROUTINE destroyTemporaryBoundaryArrays
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE AllocateBoundaryEdgesArray(numBoundaries)
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER :: numBoundaries
!
!     ---------------
!     Local variables
!     ---------------
!
      CLASS(FTLinkedList), POINTER :: list => NULL()
      CLASS(FTobject)    , POINTER :: obj  => NULL()
      INTEGER                      :: k
!
!     -------------------------------
!     Clear out old values if present
!     -------------------------------
!
      IF ( ASSOCIATED(boundaryEdgesArray) )     THEN
         CALL releaseFTMutableObjectArray(self = boundaryEdgesArray)
      END IF
      IF ( ALLOCATED(boundaryEdgesType) )     THEN
            DEALLOCATE( boundaryEdgesType )
      END IF
!
!     -------------------
!     Allocate new memory
!     -------------------
!
      ALLOCATE( boundaryEdgesArray )
      CALL boundaryEdgesArray % initwithSize(numBoundaries)
      ALLOCATE(boundaryEdgesType(numBoundaries))
!
!     -------------------------
!     Initialize the edge lists
!     -------------------------
!
      DO k = 1, numBoundaries
         ALLOCATE(list)
         CALL list % init()
         obj => list
         CALL boundaryEdgesArray % addObject(obj)
         CALL releaseFTLinkedList(self = list)
      END DO

      END SUBROUTINE AllocateBoundaryEdgesArray
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CollectBoundaryEdges( mesh, errorCode )
         USE MeshOutputMethods
         USE, INTRINSIC :: iso_fortran_env, only : stderr => ERROR_UNIT
!
!     ---------------------------------------------------
!     Boundary edges are those who have at least one node
!     on a boundary.
!     ---------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE( SMMesh ) :: mesh
         INTEGER        :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (SMEdge)              , POINTER :: edge     => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         CLASS(FTObject)            , POINTER :: obj      => NULL()
         CLASS(FTLinkedList)        , POINTER :: edgeList => NULL()
         INTEGER                              :: curveID, curveSide

         errorCode = A_OK_ERROR_CODE
!
!        --------------------------------
!        Gather "exterior" boundary edges
!        --------------------------------
!
         iterator => mesh % edgesIterator
         CALL iterator % setToStart()
         DO WHILE( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMEdge(obj,edge)

            edge % edgeType = INSIDE
            curveID         = edge % nodes(1) % node % bCurveChainID
            curveSide       = edge % nodes(1) % node % bCurveSide

            IF( .NOT.ASSOCIATED(edge % elements(2) % element ) )    THEN ! "Exterior" boundary
               edge % edgeType = ON_BOUNDARY

               IF( curveID == 0 .OR. curveSide == 0 )     THEN
                  IF( printMessage )     THEN
                     WRITE(stderr,*)  " "
                     WRITE(stderr,*)  "**************************************************************************"
                     WRITE(stderr,*)  "Curve not found for boundary point"
                     WRITE(stderr,*)   edge % nodes(1) % node % x, edge % nodes(2) % node % x
                     WRITE(stderr,*)  "Plot the file 'DebugPlot.tec' to check on the mesh topology"
                     WRITE(stderr,*)  "**************************************************************************"
                     WRITE(stderr,*)  " "
                     CALL WriteSkeletonToTecplot(mesh = mesh,fName = "DebugPlot.tec")
                  END IF
                  errorCode = CURVE_NOT_FOUND_ERROR_CODE
                  RETURN
               END IF

               IF( IsOnBoundaryCurve(edge % nodes(1) % node) .OR. &
                   IsOnBoundaryCurve(edge % nodes(2) % node) )    THEN

                  obj      => boundaryEdgesArray % objectAtIndex(curveID)
                  edgeList => linkedListFromObject(obj)
                  IF ( .NOT.ASSOCIATED(edgeList) )     THEN
                     IF( printMessage )     THEN
                        WRITE(stderr,*)  " "
                        WRITE(stderr,*)  "**************************************************************************"
                        WRITE(stderr,*)  "edge list not associated"
                        WRITE(stderr,*)  ASSOCIATED(obj), ASSOCIATED(boundaryEdgesArray), "CurveID = ", curveID
                        WRITE(stderr,*)  "Plot the file 'DebugPlot.tec' to check on the mesh topology"
                        WRITE(stderr,*)  "**************************************************************************"
                        WRITE(stderr,*)  " "
                        CALL WriteSkeletonToTecplot(mesh = mesh,fName = "DebugPlot.tec")
                     END IF
                     errorCode = UNASSOCIATED_POINTER_ERROR_CODE
                     RETURN
                  END IF
                  obj => edge
                  CALL edgeList % add(obj)
                  boundaryEdgesType(curveID) = BOUNDARY_EDGES
               END IF
            END IF

            CALL iterator % moveToNext()
         END DO
!
!        -------------------------------
!        Gather interior interface edges
!        -------------------------------
!
         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMEdge(obj,edge)

            curveID   = edge % nodes(1) % node % bCurveChainID
            curveSide = edge % nodes(1) % node % bCurveSide

            IF(edge % edgeType == ON_BOUNDARY .OR. curveID <= NONE)     THEN
               CALL iterator % moveToNext()
               CYCLE
            END IF

           IF( mesh % curveTypeForID(curveID) == INTERIOR_INTERFACE )       THEN
!
!              --------------------------------------------------
!              However, the nodes of the edge must both be inside
!              for us to be interested in it
!              --------------------------------------------------
!
               IF ( edge % nodes(1) % node % bCurveSide == INSIDE .AND. &
                    edge % nodes(2) % node % bCurveSide == INSIDE )    THEN

                  edge % edgeType = ON_INTERFACE
                  obj => boundaryEdgesArray % objectAtIndex(curveID)
                  edgeList => linkedListFromObject(obj)
                  obj => edge
                  CALL edgeList % add(obj)
                  boundaryEdgesType(curveID) = INTERFACE_EDGES
               END IF
           END IF
!
            CALL iterator % movetoNext()
         END DO

      END SUBROUTINE CollectBoundaryEdges
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE OrderBoundaryEdges( mesh )
         USE ErrorTypesModule
         USE MeshOutputMethods
         USE Geometry
         USE, INTRINSIC :: iso_fortran_env, only : stderr => ERROR_UNIT
!
!     -----------------------------------------------------------------
!     For each boundary in the boundary edge arrays, re-order the edges
!     in counter-clockwise order by marching through the nodes that
!     are shared by neighboring edges. The result is a circular list
!     with the head pointing to the end of the list
!     -----------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE( SMMesh ) :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (SMEdge)              , POINTER     :: edge => NULL(), currentEdge => NULL(), &
                                                     edge1 => NULL(), edge2 => NULL(), loopStartEdge => NULL()
         CLASS(FTLinkedList)        , POINTER     :: sortedEdges => NULL(), edgeList => NULL()
         TYPE (SMNodePtr)           , POINTER     :: sortedNodes(:) => NULL()
         CLASS(FTobject)            , POINTER     :: obj            => NULL()
         TYPE (FTLinkedListIterator), POINTER     :: iterator       => NULL()
         TYPE (SMEdgePtr)           , ALLOCATABLE :: edgeArray(:)
         INTEGER                    , ALLOCATABLE :: nodeArray(:,:)
         REAL(KIND=RP)              , ALLOCATABLE :: sortedNodeLocations(:,:)

         INTEGER                          :: numBoundaries, numEdges, numNodes
         INTEGER                          :: k, j, m
         INTEGER                          :: locRelativeToBoundary, curveDirection
         INTEGER                          :: id1, id2, sharedNodeID, eId1, eId2, eId, idMin
         REAL(KIND=RP)                    :: x0(3), x1(3), x2(3), c
!
!        -----------------------------------------------
!        Nothing to order if there are no boundary edges
!        -----------------------------------------------
!
         numBoundaries = boundaryEdgesArray % count()
         IF ( numBoundaries == 0 )     RETURN
!
         numNodes = mesh % nodes % COUNT()
         ALLOCATE(nodeArray(2,numNodes))
!
!        ------------------------------
!        Order each boundary edge chain
!        ------------------------------
!
         DO k = 1, numBoundaries
            obj => boundaryEdgesArray % objectAtIndex(k)
            IF( .NOT.ASSOCIATED( obj ) )          CYCLE
            nodeArray(1:2,1:numNodes) = 0
!
!           ----------------------------------------
!           Put edges into an array for fast access.
!           Temporary container with no ownership.
!           ----------------------------------------
!
            CALL cast(obj,edgeList)
            numEdges = edgeList % COUNT()
            ALLOCATE(edgeArray(numEdges))

            ALLOCATE(iterator)
            CALL iterator % initWithFTLinkedList(edgeList)
            CALL iterator % setToStart()

            j     = 1
            idMin = HUGE(idMin)

            DO WHILE ( .NOT.iterator % isAtEnd() )
               obj => iterator % object()
               CALL castToSMEdge(obj,currentEdge)
               edgeArray(j) % edge => currentEdge

               id1   = currentEdge % nodes(1) % node % id
               id2   = currentEdge % nodes(2) % node % id
               idMin = MIN(idMin, id1, id2 )

               IF( nodeArray(1,id1) == 0 )     THEN
                  nodeArray(1,id1) = j
               ELSE
                  nodeArray(2,id1) = j
               END IF

               IF( nodeArray(1,id2) == 0 )     THEN
                  nodeArray(1,id2) = j
               ELSE
                  nodeArray(2,id2) = j
               END IF

               j = j + 1
               locRelativeToBoundary = currentEdge % nodes(2) % node % bCurveSide
               CALL iterator % moveToNext()
            END DO
            CALL releaseFTLinkedListIterator(self = iterator)
            sharedNodeID = idMin
!
!           -------------------------
!           Generate sorted edge list
!           -------------------------
!
            ALLOCATE(sortedEdges)
            CALL sortedEdges % init()
!
!           ---------------------------------------------------------
!           Find the edges associated with the first node in the list
!           ---------------------------------------------------------
!
            eId1  = nodeArray(2,idMin)
            edge1 => edgeArray(eId1) % edge
            eId2  = nodeArray(1,idMin)
            edge2 => edgeArray(eId2) % edge
!
!           ------------------------------------------------
!           Order the edges in counter-clockwise orientation
!           by checking the cross-product of the two vectors
!           relative to the shared node.
!           ------------------------------------------------
!
            id1 = edge1 % nodes(1) % node % id
            id2 = edge1 % nodes(2) % node % id
            IF ( id1 == sharedNodeID )     THEN
               x0 = edge1 % nodes(1) % node % x
               x1 = edge1 % nodes(2) % node % x
            ELSE
               x0 = edge1 % nodes(2) % node % x
               x1 = edge1 % nodes(1) % node % x
            END IF
            id1 = edge2 % nodes(1) % node % id
            id2 = edge2 % nodes(2) % node % id
            IF ( id1 == sharedNodeID )     THEN
               x2 = edge2 % nodes(2) % node % x
            ELSE
               x2 = edge2 % nodes(1) % node % x
            END IF
!
!           ---------------------------------------------------------------------------------
!           Check 1st two segments for possibly giving the proper (counterclockwise) ordering
!           will go back and make sure later.
!           ---------------------------------------------------------------------------------
!
            x2 = x2 - x0
            x1 = x1 - x0
            c  = x1(2)*x2(1) - x1(1)*x2(2)
            IF ( c > 0.0_RP )     THEN
               obj => edge1
               CALL sortedEdges % add(obj)
               loopStartEdge => edge1
               obj => edge2
               CALL sortedEdges % add(obj)
               edge => edge2
               eId = eId2
            ELSE
               obj => edge2
               CALL sortedEdges % add(obj)
               loopStartEdge => edge2
               obj => edge1
               CALL sortedEdges % add(obj)
               edge => edge1
               eId = eId1
            END IF
!
!           --------------------------------------------
!           Follow the track to add the rest to the list
!           --------------------------------------------
!
            numEdges = SIZE(edgeArray)
            DO m = 1, numEdges-2
               id1 = edge % nodes(1) % node % id
               id2 = edge % nodes(2) % node % id

               IF( id1 == sharedNodeID )     THEN
                  sharedNodeID = id2
               ELSE
                  sharedNodeID = id1
               END IF
               eId1   = nodeArray(1,sharedNodeID)
               eId2   = nodeArray(2,sharedNodeID)
               IF ( eId1 == eId )     THEN
                  eId = eId2
               ELSE
                  eId = eId1
               END IF
               IF(eID == 0)     THEN
                  WRITE(stderr,*)  " "
                  WRITE(stderr,*)  "**************************************************************************"
                  WRITE(stderr,*)  "It appears that an edge is not connected"
                  WRITE(stderr,*)  "Plot the file 'DebugPlot.tec' to see where additional resolution is needed"
                  WRITE(stderr,*)  "**************************************************************************"
                  WRITE(stderr,*)  " "
                  CALL WriteSkeletonToTecplot( mesh, "DebugPlot.tec" )
                  ERROR STOP "Meshing Terminated. See stderr"
               END IF
               edge => edgeArray(eId) % edge

               IF ( .NOT.ASSOCIATED(edge) )     THEN
                  CALL ThrowErrorExceptionOfType("OrderBoundaryEdges",&
                                                  "Unable to form boundary edge list", &
                                                  FT_ERROR_WARNING)
                  DEALLOCATE(edgeArray)
                  RETURN
               END IF

               IF(ASSOCIATED(POINTER = edge, TARGET = loopStartEdge) ) EXIT  ! Completed the loop

               obj  => edge
               CALL sortedEdges % add(obj)
            END DO
            DEALLOCATE(edgeArray)
!
!           ---------------------------
!           Make this edgeList circular
!           in the backwards direction.
!           ---------------------------
!
            CALL sortedEdges % makeCircular(.TRUE.)
!
!           ----------------------------------------------
!           Now actually find the orientation of this edge
!           ----------------------------------------------
!
            sortedNodes => GatheredNodes( sortedEdges )
            ALLOCATE( sortedNodeLocations(3,SIZE(sortedNodes)) )
            DO m = 1, SIZE(sortedNodes)
               sortedNodeLocations(:,m) = sortedNodes(m) % node % x
            END DO
            curveDirection = Circulation(sortedNodeLocations)
            DEALLOCATE( sortedNodes )
            DEALLOCATE( sortedNodeLocations )
!
!           ---------------------------------------------------------------
!           If the orientation is clockwise (w < 0 ) then reverse the order
!           Hopefully this won't happen often...
!           ---------------------------------------------------------------
!
            IF ( curveDirection == CLOCKWISE )     THEN
               CALL sortedEdges % reverse()
            END IF
!
!           ------------------------------
!           Swap sorted array for unsorted
!           ------------------------------
!
            obj => sortedEdges
            CALL boundaryEdgesArray % replaceObjectAtIndexWithObject(k,obj)
            CALL releaseFTLinkedList(self = sortedEdges)
         END DO
         DEALLOCATE(nodeArray)

      END SUBROUTINE OrderBoundaryEdges
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION GatheredNodes( list )
!
!     -----------------------------------------------------------------------------
!     Returns an ordered temporary array containing pointers to the nodes in a
!     boundary edgelist. The reference counts of the nodes are not incremented so
!     that the array can be simply deallocated when done.
!     -----------------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList)          , POINTER :: list
         TYPE(SMNodePtr), DIMENSION(:), POINTER :: GatheredNodes
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (FTLinkedListIterator)  , POINTER :: iterator => NULL()
         CLASS(FTLinkedListRecord)    , POINTER :: cr => NULL()
         TYPE (SMEdge)                , POINTER :: currentEdge => NULL(), prevEdge => NULL()
         INTEGER                                :: nNodes, id, idP1, idP2, j
         CLASS(FTObject)              , POINTER :: obj => NULL()

         NULLIFY(GatheredNodes)
         IF ( .NOT.ASSOCIATED(list) )     RETURN

         nNodes = list % COUNT()
         ALLOCATE( GatheredNodes(nNodes) )
         ALLOCATE(iterator)
         CALL iterator % initWithFTLinkedList(list)
!
!        -------------------------------------------------
!        Gather the nodes and positions along the boundary
!        The edge list is  circular.
!        -------------------------------------------------
!
         j = 1
         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMEdge(obj,currentEdge)
            cr  => iterator % currentRecord()
            obj => cr % previous % recordObject
            CALL castToSMEdge(obj,prevEdge)

            id   = currentEdge % nodes(1) % node % id
            idP1 = prevEdge % nodes(1) % node % id
            idP2 = prevEdge % nodes(2) % node % id
            IF ( id == idP1 .OR. id == idP2 )     THEN
               GatheredNodes(j) % node => currentEdge % nodes(1) % node
            ELSE
               GatheredNodes(j) % node => currentEdge % nodes(2) % node
            END IF
            CALL iterator % moveToNext()
            j = j + 1
         END DO

         CALL releaseFTLinkedListIterator(self = iterator)

      END FUNCTION GatheredNodes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SmoothEdgeListNodes( list, nPasses )
!
!     --------------------------------------------------
!     Average the locations of nodes used by an edgeList
!     --------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList) , POINTER :: list
         INTEGER                       :: nPasses
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE(SMNodePtr), POINTER     :: nodeArray(:) => NULL()
         REAL(KIND=RP)  , ALLOCATABLE :: x(:,:)
         INTEGER                      :: nNodes, j, k


         nodeArray => GatheredNodes( list )
         nNodes    = SIZE(nodeArray)
         ALLOCATE( x(3,nNodes) )
!
!        -----------------------------
!        Average the nearest neighbors
!        -----------------------------
!

         DO k = 1, nPasses
            DO j = 2, nNodes-1
               x(:,j) = (nodeArray(j-1)%node%x + 6*nodeArray(j)%node%x + nodeArray(j+1)%node%x)/8.0_RP
            END DO
            x(:,1)      = (nodeArray(nNodes)%node%x   + 6*nodeArray(1)%node%x      + nodeArray(2)%node%x)/8.0_RP
            x(:,nNodes) = (nodeArray(nNodes-1)%node%x + 6*nodeArray(nNodes)%node%x + nodeArray(1)%node%x)/8.0_RP
            DO j = 1, nNodes
               nodeArray(j)%node%x = x(:,j)
            END DO
         END DO

         DEALLOCATE( nodeArray )
         DEALLOCATE( x )

      END SUBROUTINE SmoothEdgeListNodes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE LocateEdgeImagesOnBoundaries( mesh, model, idOfOuterBoundary, skipInterfaces )
         USE SMModelClass
         USE ErrorTypesModule
!
!        ------------------------------------------------------
!        Find location of image of the edges onto the boundary.
!
!        Throws FATAL_ERROR_EXCEPTION if boundaries and edges
!        cannot be matched.
!        ------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) , POINTER :: mesh
         TYPE (SMModel), POINTER :: model
         INTEGER                 :: idOfOuterBoundary
         LOGICAL                 :: skipInterfaces
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMChainedCurve)      , POINTER     :: chain            => NULL()
         TYPE (SMEdge)              , POINTER     :: edge             => NULL()
         CLASS(FTObject)            , POINTER     :: obj              => NULL()
         CLASS(FTLinkedList)        , POINTER     :: edgeList         => NULL()
         TYPE (FTLinkedListIterator), POINTER     :: edgeListIterator => NULL()

         LOGICAL                                  :: isInnerBoundaryCurve
         INTEGER                                  :: j
         INTEGER                                  :: numBoundaries, chainID
         CHARACTER(LEN=ERROR_EXCEPTION_MSG_LENGTH):: msg

         numBoundaries = boundaryEdgesArray % COUNT()

         DO j = 1, numBoundaries

            obj => boundaryEdgesArray % objectAtIndex(j)
            IF( .NOT.ASSOCIATED(obj) )                          CYCLE
            IF ( boundaryEdgesType(j) == INTERFACE_EDGES .AND. &
                 skipInterfaces )                               CYCLE

            CALL cast(obj,edgeList)
            ALLOCATE(edgeListIterator)
            CALL edgeListIterator % initWithFTLinkedList(edgeList)

            CALL edgeListIterator % setToStart()

            obj     => edgeListIterator % object()
            CALL castToSMEdge(obj,edge)
            chainID = edge % nodes(1) % node % bCurveChainID
!
!           ---------------------------------------------------
!           Bail if the edge has no associated curve ID defined
!           ---------------------------------------------------
!
            IF (chainID == UNDEFINED )     THEN
               WRITE(msg,*) "Boundary curve chain ",chainID," not assigned for a boundary edge ", edge % id
               CALL ThrowErrorExceptionOfType("LocateEdgeImagesOnBoundaries",msg,FT_ERROR_FATAL)
               CALL releaseFTLinkedListIterator(self = edgeListIterator)
               RETURN
            END IF

            chain => model % chainWithID(chainID)
!
!           --------------------------------------------------------
!           Bail of the chain for this edge curve ID cannot be found
!           --------------------------------------------------------
!
            IF ( .NOT.ASSOCIATED(chain) )     THEN
               WRITE(msg,*) "Chain with id ", chainID, " Not found in model"
               CALL ThrowErrorExceptionOfType("LocateEdgeImagesOnBoundaries",msg,FT_ERROR_FATAL)
               CALL releaseFTLinkedListIterator(self = edgeListIterator)
               RETURN
            END IF
!
            IF( chainID == idOfOuterBoundary )     THEN
               isInnerBoundaryCurve = .FALSE.
            ELSE IF( edge % nodes(1) % node % bCurveSide == INSIDE )     THEN
               isInnerBoundaryCurve = .FALSE.
            ELSE
               isInnerBoundaryCurve = .TRUE.
            END IF
!
            CALL AssociateBoundaryEdgesToCurve( edgeList, chain, isInnerBoundaryCurve )
            CALL releaseFTLinkedListIterator(self = edgeListIterator)

         END DO

      END SUBROUTINE LocateEdgeImagesOnBoundaries
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE AssociateBoundaryEdgesToCurve( list, chain, isInnerBoundaryCurve )
      USE SMChainedCurveClass
!
!     ---------------------------------------------------------------------
!     Find the nearest location on the curve to each point in the edge list
!     in the direction of the normal to the curve.
!     ---------------------------------------------------------------------
!
         USE ProgramGlobals, ONLY :edgeLengthFactor
         USE Geometry
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList)  , POINTER :: list
         CLASS(SMChainedCurve), POINTER :: chain
         LOGICAL                        :: isInnerBoundaryCurve
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMCurve)                 , POINTER :: c => NULL(), c2 => NULL()
         TYPE(SMNodePtr), DIMENSION(:)  , POINTER :: nodePtrs => NULL()
         CLASS(SMNode)                  , POINTER :: node => NULL()
         REAL(KIND=RP)  , DIMENSION(:,:), POINTER :: nHat => NULL()
         REAL(KIND=RP)  , DIMENSION(:,:), POINTER :: xCurve => NULL()
         REAL(KIND=RP)  , DIMENSION(3)            :: z, p
         REAL(KIND=RP)                            :: tStart, tEnd, t
         REAL(KIND=RP)                            :: d, dMin, dt
         INTEGER                                  :: j, k, M
         INTEGER                                  :: totalCurvePoints

         nodePtrs => GatheredNodes( list )
!
!        -----------------------------------------------------------------
!        Approximate normal to each node as the normal to the line between
!        the two neighboring nodes. [Another possibility is to average the
!        normals to the edges on either side.]
!        -----------------------------------------------------------------
!
         ALLOCATE( nHat(3,SIZE(nodePtrs)) )
         M = SIZE(nHat,2)
         DO j = 2, M-1
            z         = nodePtrs(j+1) % node % x - nodePtrs(j-1) % node % x
            z         = z/SQRT(z(1)**2 + z(2)**2 )
            nHat(1,j) = z(2); nHat(2,j) = -z(1); nHat(3,j) = 0.0_RP
         END DO

         z         = nodePtrs(2)%node%x - nodePtrs(M)%node%x
         z         = z/SQRT(z(1)**2 + z(2)**2 )
         nHat(1,1) = z(2); nHat(2,1) = -z(1); nHat(3,1) = 0.0_RP

         z         = nodePtrs(1) % node % x - nodePtrs(M-1) % node % x
         z         = z/SQRT(z(1)**2 + z(2)**2 )
         nHat(1,M) = z(2); nHat(2,M) = -z(1); nHat(3,M) = 0.0_RP

         IF( isInnerBoundaryCurve ) nHat = -nHat
!
!        ----------------------------------
!        Now find the point along the curve
!        ----------------------------------
!
         totalCurvePoints = 2*numCurvePoints*chain % numberOfCurvesInChain !TODO compute this quantity
         tStart  = 0.0
         tEnd    = 1.0_RP
         dt      = (tEnd - tStart)/totalCurvePoints
!
!        -----------------------------------------------------------------
!        First, estimate the location with a linear search along
!        the boundary. Must do this because there can be local
!        minima that fMin cannot deal with. Compute and store points along
!        the curve to compare against.
!       ------------------------------------------------------------------
!
         ALLOCATE( xCurve(3,0:totalCurvePoints) )
         DO j = 0, totalCurvePoints
            t = j*dt
            xCurve(:,j) = chain % positionAt(t)
         END DO
!
!        -------------------------------------------------------------
!        For each node along the edge boundary, find the closest point
!        -------------------------------------------------------------
!
         DO j = 1, SIZE(nodePtrs)
            node => nodePtrs(j)%node
            p    = node % x
            dMin = HUGE(dMin)
            DO k = 0, totalCurvePoints - 1
               d = SQRT(distanceSquaredBetweenPoints(xCurve(:,k), p, [0.0_RP,0.0_RP,0.0_RP]))
               IF( d < dMin )     THEN
                  t                     = k*dt
                  node%gWhereOnBoundary = t
                  dMin                  = d
               END IF
            END DO
         END DO
!
!        -------------------------------
!        Now zoom precisely to the curve
!        -------------------------------
!
         DO j = 1, SIZE(nodePtrs)
            node => nodePtrs(j)%node
            p      = node % x
            t      = node % gWhereOnBoundary
            c      => chain % curveWithLocation(t)

            tStart = t - dt
            tEnd   = t + dt
            IF(tStart < 0.0_RP) tStart = 0.0_RP
            IF(tEnd> 1.0_RP) tEnd = 1.0_RP
!
!           --------------------------------------------
!           Make sure that the locations do not straddle
!           two curves
!           --------------------------------------------
!
            c  => chain % curveWithLocation(tStart)
            c2 => chain % curveWithLocation(tend)
            IF ( c % id() == c2 % id() )     THEN
               tStart = chain % curveTForChainT(tStart)
               tEnd   = chain % curveTForChainT(tEnd )
               tStart = MAX(tStart, 0.0_RP)
               tEnd   = MIN(tEnd, 1.0_RP)
               t      = fmin(c, tStart, tEnd, minimizationTolerance, p, [0.0_RP,0.0_RP,0.0_RP])
            ELSE
               c => chain % curveWithLocation(t)
               t =  chain % curveTForChainT(t)
            END IF

            d = SQRT(distanceSquared(t, c, p, [0.0_RP,0.0_RP,0.0_RP]))

            node % bCurveID         = c % id()
            node % whereOnBoundary  = t
            node % distToBoundary   = d
            node % gWhereOnBoundary = chain % ChainTForCurveTInCurve(t,c)
         END DO
!
!        ----
!        Done
!        ----
!
         DEALLOCATE(nodePtrs)
         DEALLOCATE(nHat)
         NULLIFY(nodePtrs)
         DEALLOCATE( xCurve )

      END SUBROUTINE AssociateBoundaryEdgesToCurve
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE  SmoothBoundaryLocations( list, model )
         USE SMModelClass
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList), POINTER :: list
         TYPE (SMModel)     , POINTER :: model
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMChainedCurve)        , POINTER :: chain => NULL()
         TYPE(SMNodePtr), DIMENSION(:), POINTER :: nodeArray => NULL()
         CLASS(SMNode)                , POINTER :: node => NULL()
         INTEGER                                :: nodeArraySize, j
         REAL(KIND=RP)                          :: t0, tm, tp, tmpTol = 1.0d-2

         INTEGER, EXTERNAL :: Loop
!
!        ---------------------------------------------------------------
!        If two target points are too close, move one of them to halfway
!        to the next one. This will make the distribution along the
!        boundary more uniform. Pick the left handed corner to move.
!        ---------------------------------------------------------------
!
         nodeArray     => GatheredNodes( list )
         nodeArraySize =  SIZE(nodeArray)
         chain         => model % chainWithID(nodeArray(1) % node % bCurveChainID)

         DO j = 1, nodeArraySize
            node => nodeArray(j) % node
!
!           -------------------------------
!           Move only ROW_SIDE node targets
!           -------------------------------
!
            IF( node % nodetype /= ROW_SIDE )     CYCLE

            t0 = nodeArray(j) % node % gWhereOnBoundary
            tm = nodeArray(Loop(j-1,nodeArraySize)) % node % gWhereOnBoundary
            tp = nodeArray(Loop(j+1,nodeArraySize)) % node % gWhereOnBoundary
!
!           --------------------
!           Massage if necessary
!TODO This section still needs work!!!
!           --------------------
!
            IF( tm > tp )     CYCLE
            IF ( abs(t0 - tm) <= tmpTol*max(t0,tm) )     THEN !Too CLOSE
!               x0 = nodePtrs(j) % node % x
!               xp = nodePtrs(Loop(j+1,nodeArraySize)) % node % x
!               xm = nodePtrs(Loop(j-1,nodeArraySize)) % node % x
!               x0 = x0 - xm
!               xp = xp - x0
!               IF ( CrossProductDirection(x0,xp) == DOWN )     THEN
!                  IF( tm > tp )                  tm = tm - 1.0_RP ! Wrap around
                  t0                                 = 0.5_RP*(t0 + tp)
                  nodeArray(j) % node % gWhereOnBoundary = t0
!                  c                                  => chain % curveWithLocation(t0)
                  nodeArray(j) % node % whereOnBoundary  = chain % curveTForChainT(t0)!               END IF
!            ELSE IF ( tm > t0 )     THEN !Swap the points
!               nodeArray(j) % node % gWhereOnBoundary = tm
!               c                                  => CurveInChain_WithLocation_( chain, tm )
!               nodeArray(j) % node % whereOnBoundary  = CurrentCurveTForChainT( chain, tm )
!
!               nodeArray(Loop(j-1,nodeArraySize)) % node % gWhereOnBoundary = t0
!               c                                  => CurveInChain_WithLocation_( chain, t0 )
!               nodeArray(Loop(j-1,nodeArraySize)) % node % whereOnBoundary  = CurrentCurveTForChainT( chain, t0 )
            END IF

         END DO
!
!        --------
!        Clean up
!        --------
!
         DEALLOCATE( nodeArray )

      END SUBROUTINE SmoothBoundaryLocations
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE unmarkNodesNearBoundaries( nodeIterator )
!
!     --------------------------------------------------------------------
!     When elements that intersect boundary curves are removed, nodes that
!     were left behind were marked as boundary nodes. Once new elements
!     are generated to project onto the true boundaries, remove the
!     designation that what are now interior nodes are boundary nodes
!     --------------------------------------------------------------------
!
      IMPLICIT NONE
      TYPE (FTLinkedListIterator), POINTER :: nodeIterator
      CLASS(FTObject)            , POINTER :: obj  => NULL()
      TYPE (SMNode)              , POINTER :: node => NULL()
      CALL nodeIterator % setToStart()
      DO WHILE( .NOT.nodeIterator % isAtEnd() )
         obj => nodeIterator % object()
         CALL castToSMNode(obj,node)

         IF( node % distToBoundary > 0.0_RP )     THEN
             node % bCurveID        = UNDEFINED
             node % bCurveChainID   = UNDEFINED
             node % bCurveSide      = UNDEFINED
             node % whereOnBoundary = -1.0_RP
         END IF

         CALL nodeIterator % moveToNext()
      END DO

      END SUBROUTINE unmarkNodesNearBoundaries
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetNodeActiveStatus(mesh, model, errorCode)
!
!     ------------------------------------------------------------------
!     Note: In this routine it should not be necessary to use the edges.
!     Instead, the nodes have enough information to determine
!     status, so we should be able to use the node list. Check on
!     what happens at interface nodes, however, before changing.
!     ------------------------------------------------------------------
!
        USE SMModelClass
        USE, INTRINSIC :: iso_fortran_env, only : stderr => ERROR_UNIT
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
         INTEGER                      :: numBoundaries
         INTEGER                      :: j
         CLASS(FTLinkedList), POINTER :: currentEdgeList => NULL()
         TYPE (SMEdge)      , POINTER :: currentEdge => NULL()
         CLASS(FTObject)    , POINTER :: obj => NULL()

         TYPE (FTLinkedListIterator), POINTER :: iterator => NULL()
!
!        ---------------------
!        Gather boundary Edges
!        ---------------------
!
         numBoundaries = model % numberOfInnerCurves + model % numberOfOuterCurves &
                       + model % numberOfInterfaceCurves

         IF ( numBoundaries == 0 )     RETURN

         CALL AllocateBoundaryEdgesArray(numBoundaries)

         ALLOCATE(iterator)
         CALL iterator % init()

         CALL CollectBoundaryEdges( mesh, errorCode )
         IF ( errorCode > A_OK_ERROR_CODE )     THEN
            RETURN
         END IF

         CALL MakeNodeToElementConnections( mesh, errorCode )

         IF( errorCode == A_OK_ERROR_CODE)     THEN
            CALL MakeElementToEdgeConnections( mesh )

            IF ( model % numberOfInnerCurves + model % numberOfOuterCurves > 0 )     THEN

               DO j = 1, numBoundaries
                  IF( boundaryEdgesType(j) == INTERFACE_EDGES ) CYCLE

                  obj => boundaryEdgesArray % objectAtIndex(j)
                  CALL cast(obj,currentEdgeList)
                  IF(.NOT. ASSOCIATED(currentEdgeList)) THEN
                     WRITE(stderr,*)  "Unnassociated edgelist in SetNodeActiveStatus number",j
                     CYCLE
                  END IF

                  CALL iterator % setLinkedList(currentEdgeList)
                  CALL iterator % setToStart()
                  DO WHILE ( .NOT.iterator % isAtEnd() )
                     obj => iterator % object()
                     CALL castToSMEdge(obj,currentEdge)

                     IF ( currentEdge % edgeType == ON_BOUNDARY )     THEN

                        IF ( currentEdge % nodes(1) % node % nodeType == ROW_SIDE .AND. boundarySlipping )     THEN
                           currentEdge % nodes(1) % node % activeStatus = ACTIVE
                        ELSE
                           currentEdge % nodes(1) % node % activeStatus = INACTIVE
                        END IF

                        IF ( currentEdge%nodes(2) % node % nodeType == ROW_SIDE .AND. boundarySlipping )     THEN
                           currentEdge % nodes(2) % node % activeStatus = ACTIVE
                        ELSE
                           currentEdge % nodes(2) % node % activeStatus = INACTIVE
                        END IF

                     END IF
!
!                 ----------------------------------------------------
!                 Nevertheless, deactivate when at the end of a curve.
!                 ----------------------------------------------------
!
                      IF ( currentEdge % nodes(1) % node % whereOnBoundary == 0.0_RP .OR. &
                           currentEdge % nodes(1) % node % whereOnBoundary == 1.0_RP)     THEN
                           currentEdge % nodes(1) % node % activeStatus    = INACTIVE
                      END IF
                      IF ( currentEdge % nodes(2) % node % whereOnBoundary == 0.0_RP .OR. &
                           currentEdge % nodes(2) % node % whereOnBoundary == 1.0_RP)     THEN
                           currentEdge % nodes(2) % node % activeStatus    = INACTIVE
                      END IF

                     CALL iterator % moveToNext()
                  END DO
               END DO

            END IF
            CALL deallocateElementToEdgeConnections()
         END IF
         CALL releaseFTLinkedListIterator(self = iterator)
         CALL deallocateNodeToElementConnections()

      END SUBROUTINE SetNodeActiveStatus
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FindCurveLocationsforNodes( boundaryNodesList, model )
         USE SMModelClass
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList), POINTER :: boundaryNodesList
         TYPE (SMModel)     , POINTER :: model
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (SMNode)              , POINTER :: currentNode => NULL()
         CLASS(SMCurve)             , POINTER :: c => NULL()
         TYPE (FTLinkedListIterator), POINTER :: iterator => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()
         REAL(KIND=RP)                        :: tOld,tNew
         REAL(KIND=RP)                        :: pOld(3),pNew(3)
         CLASS(SMChainedCurve)      , POINTER :: chain => NULL()
         INTEGER                              :: curveID
!
!        --------------------------------------------------------------------
!        Loop through the nodes and find the parameter value %whereOnBoundary
!        for all active nodes
!        --------------------------------------------------------------------
!
         ALLOCATE(iterator)
         CALL iterator % initWithFTLinkedList(boundaryNodesList)
         CALL iterator % setToStart()

         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMNode(obj,currentNode)

            IF ( currentNode % activeStatus == INACTIVE )     THEN
               CALL iterator % moveToNext()
               CYCLE
            END IF

            curveID = currentNode % bCurveID
            c       => model % curveWithID(curveID, chain)

            pOld =  currentNode % x
            tOld =  currentNode % whereOnBoundary

            tNew = ParametrizationAtPointNear( c, pOld, tOld )
            pNew = c % positionAt(tNew)

            currentNode % x                = pNew
            currentNode % whereOnBoundary  = tNew
            currentNode % distToBoundary   = 0.0_RP
            currentNode % gWhereOnBoundary = chain % ChainTForCurveTInCurve(tNew,c)

            CALL iterator % moveToNext()
         END DO

         CALL releaseFTLinkedListIterator(self = iterator)

      END SUBROUTINE FindCurveLocationsforNodes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckForBoundaryIntersections(sizer)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(MeshSizer), POINTER  :: sizer
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER                               :: k, j
         CLASS(SegmentedCurveArray) , POINTER  :: curveArrayA, curveArrayB
         CHARACTER(LEN=STRING_CONSTANT_LENGTH) :: msg

         CALL generateTemporaryBoundaryArrays( sizer )
!
!        -------------------------------------------------
!        See if exterior and any interior curves intersect
!        -------------------------------------------------
!
         IF ( ASSOCIATED( outerBoundaryCurve ) )     THEN
            IF( ASSOCIATED( interiorCurves ) )    THEN

               DO k = 1, SIZE(interiorCurves)

                  IF (TwoCurvesIntersect(curveArrayA = outerBoundaryCurve,                    &
                                         curveArrayB = interiorCurves(k) % curveArray ,       &
                                         testType    = TEST_NESTING     ))                THEN

                     WRITE(msg,*) "Interior curve ", k," overlaps with exterior curve"
                     CALL ThrowErrorExceptionOfType(poster = "CheckForBoundaryIntersections", &
                                                    msg    = msg,                             &
                                                    typ    = FT_ERROR_FATAL)
                     CALL destroyTemporaryBoundaryArrays
                     RETURN

                  END IF
!
               END DO

            END IF
         END IF
!
!        --------------------------------
!        See if interior curves intersect
!        --------------------------------
!
         IF( ASSOCIATED( interiorCurves ) )    THEN
            DO k = 1, SIZE(interiorCurves)
               curveArrayA => interiorCurves(k) % curveArray

               DO j = k+1, SIZE(interiorCurves)

                  curveArrayB => interiorCurves(j) % curveArray

                  IF (TwoCurvesIntersect(curveArrayA = interiorCurves(k) % curveArray,        &
                                         curveArrayB = interiorCurves(j) % curveArray,        &
                                         testType    = TEST_INTERSECTION     ))        THEN

                     WRITE(msg,*) "Interior curves ", k," and ", j, "overlap"
                     CALL ThrowErrorExceptionOfType(poster = "CheckForBoundaryIntersections", &
                                                    msg    = msg,                       &
                                                    typ    = FT_ERROR_FATAL)
                     CALL destroyTemporaryBoundaryArrays
                     RETURN

                  END IF

               END DO
            END DO
         END IF

         CALL destroyTemporaryBoundaryArrays

      END SUBROUTINE CheckForBoundaryIntersections
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION TwoCurvesIntersect(curveArrayA, curveArrayB, testType)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SegmentedCurveArray) , POINTER  :: curveArrayA, curveArrayB
         INTEGER                               :: testType
!
!        ---------------
!        Local Variables
!        ---------------
!
         LOGICAL                      :: intersectionFound
         REAL(KIND=RP)                :: intersection(6)
         REAL(KIND=RP), ALLOCATABLE   :: crv1(:,:), crv2(:,:)

         TwoCurvesIntersect = .FALSE.

         IF ( testType == TEST_NESTING )     THEN ! Easy Full curve BBox check
            intersectionFound = .NOT.BBoxIsNested(BoxA = curveArrayA % boundingBox, &
                                                  BoxB = curveArrayB % boundingBox)
         ELSE
            intersectionFound = BBoxIntersects(BoxA = curveArrayA % boundingBox, &
                                               BoxB = curveArrayB % boundingBox)
         END IF

         IF ( intersectionFound )     THEN ! Check further

            ALLOCATE(crv1(3,0:curveArrayA % nSegments))
            crv1 = curveArrayA % x
            ALLOCATE(crv2(3,0:curveArrayB % nSegments))
            crv2 = curveArrayB % x

            intersection = IntersectionOfBBoxes(boxA = curveArrayA % boundingBox, &
                                                boxB = curveArrayB % boundingBox)

            TwoCurvesIntersect = TwoCurvesAsPointsIntersect(testCurve1   = crv1,                      &
                                                            nPts1        = curveArrayA % nSegments,   &
                                                            testCurve2   = crv2,                      &
                                                            nPts2        = curveArrayB % nSegments,   &
                                                            intersection = intersection)

            IF(TwoCurvesIntersect)     RETURN

         END IF

      END FUNCTION TwoCurvesIntersect
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION TwoCurvesAsPointsIntersect(testCurve1, nPts1, &
                                                    testCurve2, nPts2, &
                                                    intersection) RESULT(theyCross)
!
!     --------------------------------------------------------
!     Call to see if two curves stored as point arrays overlap
!     --------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER        :: nPts1, nPts2
         REAL(KIND=RP)  :: testCurve1(3,0:nPts1), testCurve2(3,0:nPts2)
         LOGICAL        :: theyCross
!
!        ---------------
!        Local Variables
!        ---------------
!
         REAL(KIND=RP)              :: intersection(6)
         REAL(KIND=RP), ALLOCATABLE :: crv1inBox(:,:), crv2inBox(:,:)
         INTEGER                    :: nptsC1, nptsC2
         REAL(KIND=RP)              :: subBox1(6), subBox2(6)

         theyCross = .TRUE.

         ALLOCATE(crv1inBox(3,0:nPts1))

         CALL CollectArrayPointsInBox(array        = testCurve1,    &
                                      bBox         = intersection,  &
                                      collected    = crv1inBox,     &
                                      nCollected   = nPtsC1,        &
                                      collectedBox = subBox1)

         IF ( nPtsC1 == 0)     THEN ! No actual points in curve1 in the intersection box
            theyCross = .FALSE.
            RETURN
         END IF

         ALLOCATE(crv2inBox(3,0:nPts2))
         CALL CollectArrayPointsInBox(array        = testCurve2,    &
                                      bBox         = intersection,  &
                                      collected    = crv2inBox,     &
                                      nCollected   = nPtsC2,         &
                                      collectedBox = subBox2)

         IF ( nPtsC2 == 0 )     THEN ! No actual points in curve1 in the intersection box
            theyCross = .FALSE.
            RETURN
         END IF
!
!        ---------------------------------------------------------------------------
!        One last quick check: The boxes outlining the sets of points must intersect
!        for there to be a crossing
!        ---------------------------------------------------------------------------
!
         IF(.NOT.BBoxIntersects(BoxA = subBox1, BoxB = subBox2) )     THEN
            theyCross = .FALSE.
            RETURN
         END IF
!
!        -------------------------------------------------
!        Now we have to work... Go through the points and
!        see if the curves actually cross
!        -------------------------------------------------
!
         theyCross = TestPointsForCrossing( crv1inBox, nptsC1, crv2inBox, nptsC2 )
         IF(theyCross)     RETURN
         theyCross = TestPointsForCrossing( crv2inBox, nptsC2, crv1inBox, nptsC1 )

      END FUNCTION TwoCurvesAsPointsIntersect
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CollectArrayPointsInBox(array, bBox, collected, nCollected, collectedBox)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP), INTENT(IN)  :: array(:,0:)
         REAL(KIND=RP), INTENT(OUT) :: collected(:,0:)
         REAL(KIND=RP), INTENT(IN)  :: bBox(6)
         INTEGER      , INTENT(OUT) :: nCollected
         REAL(KIND=RP), INTENT(OUT) :: collectedBox(6)
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER :: j
         REAL(KIND=RP) :: xMin, xMax, yMin, yMax

         xMin =  HUGE(xMin)
         xMax = -HUGE(xMax)
         yMin =  HUGE(xMin)
         yMax = -HUGE(xMax)

         nCollected = 0
         collected  = 0.0_RP
         DO j = 0, SIZE(array,2)-1
            IF ( Point_IsInsideBox(p = array(:,j),bBox = bBox) )     THEN
               collected(:,nCollected) = array(:,j)

               xMax = MAX(collected(1,nCollected),xMax)
               xMin = MIN(collected(1,nCollected),xMin)
               yMax = MAX(collected(2,nCollected),yMax)
               yMin = MIN(collected(2,nCollected),yMin)

               nCollected = nCollected + 1
            END IF
         END DO
         nCollected = MAX(nCollected - 1,0) ! Gives the upper bound on the array.

         collectedBox(BBOX_TOP)    = yMax
         collectedBox(BBOX_BOTTOM) = yMin
         collectedBox(BBOX_LEFT)   = xMin
         collectedBox(BBOX_RIGHT)  = xMax

      END SUBROUTINE CollectArrayPointsInBox
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION TestPointsForCrossing(c1, n1, c2, n2)
         IMPLICIT NONE
!
!        ----------
!        Arguments
!        ----------
!
         INTEGER       :: n1, n2
         REAL(KIND=RP) :: c1(3,0:n1), c2(3,0:n2)
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER       :: i, j
         REAL(KIND=RP) :: bBox(6)

         bBox = 0.0_RP
         TestPointsForCrossing = .FALSE.

         DO i = 0, n1-1

            bBox(BBOX_LEFT)   = MIN(c1(1,i),c1(1,i+1))
            bBox(BBOX_BOTTOM) = MIN(c1(2,i),c1(2,i+1))
            bBox(BBOX_RIGHT)  = MAX(c1(1,i),c1(1,i+1))
            bBox(BBOX_TOP)    = MAX(c1(1,i),c1(1,i+1))

            DO j = 0, n2
              IF ( Point_IsInsideBox(p = c2(:,j),bBox = bBox) ) THEN
                 TestPointsForCrossing = .TRUE.
                 RETURN
              END IF
            END DO
         END DO

      END FUNCTION TestPointsForCrossing

   END MODULE MeshBoundaryMethodsModule

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
         TYPE  (FTLinkedListIterator), POINTER :: iterator => NULL()
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
            CALL iterator % initwithFTLinkedListClass( sizer % innerBoundariesList)
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
            CALL iterator % initwithFTLinkedListClass( sizer % interfaceBoundariesList)
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
      TYPE (FTLinkedList), POINTER :: list => NULL()
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
         CALL releaseFTLinkedList(list)
      END DO

      END SUBROUTINE AllocateBoundaryEdgesArray
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CollectBoundaryEdges( mesh, errorCode, useElementSides )
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
         TYPE( SMMesh )    :: mesh
         INTEGER           :: errorCode
         LOGICAL, OPTIONAL :: useElementSides
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (SMEdge)              , POINTER :: edge     => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         CLASS(FTObject)            , POINTER :: obj      => NULL()
         CLASS(FTLinkedList)        , POINTER :: edgeList => NULL()
         INTEGER                              :: curveID, curveSide, k
         LOGICAL                              :: elementSides

         errorCode = A_OK_ERROR_CODE

         elementSides = .FALSE.
         IF ( PRESENT(useElementSides) )     elementSides = useElementSides
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
!        ------------------------------------------------------------
!        Gather interior interface edges. On the background grid mesh
!        the interface edges are those that separate elements inside
!        an interface curve from those outside. After the nodes have
!        been moved onto the interface, the interface nodes are the
!        ones marked INSIDE, and the edges between them are gathered.
!        ------------------------------------------------------------
!
         IF ( elementSides )     THEN

            IF ( .NOT.ASSOCIATED(interfaceCurves) )     RETURN

            DO k = 1, SIZE(interfaceCurves)
               curveID  = interfaceCurves(k) % curveArray % id
               obj      => boundaryEdgesArray % objectAtIndex(curveID)
               edgeList => linkedListFromObject(obj)

               CALL CollectInterfaceEdgesFromElementSides( mesh, interfaceCurves(k) % curveArray, edgeList )
!
!              -----------------------------------------------------------
!              Remove dangling and chord edges so that the edge list forms
!              a closed loop that can be ordered.
!              -----------------------------------------------------------
!
               CALL PruneInterfaceEdges( edgeList )
               IF ( edgeList % COUNT() > 0 )     boundaryEdgesType(curveID) = INTERFACE_EDGES
            END DO

            RETURN
         END IF

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
      SUBROUTINE CollectInterfaceEdgesFromElementSides( mesh, curveArray, edgeList )
         USE Geometry
         USE ErrorTypesModule
         IMPLICIT NONE
!
!     -----------------------------------------------------------------
!     Find the edges of the background grid mesh that will be moved
!     onto an interface curve.
!
!     Each element is classified as inside or outside of the curve by
!     its centroid. If the centroid is close to the curve, the fraction
!     of the element that is inside decides. The interface edges are
!     the edges that separate an inside element from an outside one,
!     so only nodes next to the curve are moved onto it.
!
!     To guarantee that the edges form a single closed loop,
!     (1) inside or outside regions that are not connected to the
!         main region are reclassified, and
!     (2) an element with three or more interface edges, i.e. a one
!         element wide bump or notch, is reclassified, and
!     (3) at a "pinch" node, where inside elements touch only at that
!         node, the least certain element around it is reclassified.
!
!     On return, the nodes on the interface edges are marked INSIDE
!     and the other nodes associated with the curve are marked OUTSIDE.
!     -----------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh)                      :: mesh
         CLASS(SegmentedCurveArray), POINTER :: curveArray
         CLASS(FTLinkedList)       , POINTER :: edgeList
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, PARAMETER :: MAX_PINCH_ITERATIONS = 100

         TYPE (SMElementPtr), ALLOCATABLE :: elementForID(:)
         TYPE (SMEdgePtr)   , ALLOCATABLE :: meshEdges(:)
         LOGICAL            , ALLOCATABLE :: isInside(:), isInterfaceEdge(:), flippedAtCorner(:)
         REAL(KIND=RP)      , ALLOCATABLE :: insideFraction(:)
         INTEGER            , ALLOCATABLE :: interfaceEdgeCount(:), elementEdgeCount(:)
         TYPE (SMElement)   , POINTER     :: e => NULL(), eFlip => NULL()
         TYPE (SMEdge)      , POINTER     :: edge => NULL()
         TYPE (SMNode)      , POINTER     :: node => NULL()
         CLASS(FTObject)    , POINTER     :: obj => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         INTEGER                          :: maxElementID, maxNodeID, numEdges
         INTEGER                          :: i, j, k, m, pinchNodeID, iteration
         REAL(KIND=RP)                    :: centroid(3), hMin, h, bestScore, score
         REAL(KIND=RP)                    :: box(6)
         INTEGER, EXTERNAL                :: Loop
!
!        ---------------------------------
!        Index the elements by id, and the
!        edges that are shared by two
!        ---------------------------------
!
         maxElementID = 0
         iterator => mesh % elementsIterator
         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMElement(obj,e)
            maxElementID = MAX(maxElementID, e % id)
            CALL iterator % moveToNext()
         END DO

         ALLOCATE( elementForID(maxElementID), isInside(maxElementID), insideFraction(maxElementID) )
         ALLOCATE( elementEdgeCount(maxElementID), flippedAtCorner(maxElementID) )
         flippedAtCorner = .FALSE.
         isInside       = .FALSE.
         insideFraction = -1.0_RP

         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMElement(obj,e)
            IF ( .NOT.e % remove )     elementForID(e % id) % element => e
            CALL iterator % moveToNext()
         END DO

         maxNodeID = 0
         iterator => mesh % nodesIterator
         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMNode(obj,node)
            maxNodeID = MAX(maxNodeID, node % id)
            CALL iterator % moveToNext()
         END DO
         ALLOCATE( interfaceEdgeCount(maxNodeID) )

         numEdges = 0
         iterator => mesh % edgesIterator
         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMEdge(obj,edge)
            IF ( ASSOCIATED(edge % elements(1) % element) .AND. &
                 ASSOCIATED(edge % elements(2) % element) )     numEdges = numEdges + 1
            CALL iterator % moveToNext()
         END DO

         ALLOCATE( meshEdges(numEdges), isInterfaceEdge(numEdges) )
         i = 0
         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMEdge(obj,edge)
            IF ( ASSOCIATED(edge % elements(1) % element) .AND. &
                 ASSOCIATED(edge % elements(2) % element) )     THEN
               i = i + 1
               meshEdges(i) % edge => edge
            END IF
            CALL iterator % moveToNext()
         END DO
!
!        ----------------------------------------------------
!        Classify each element by its centroid. Elements with
!        a centroid close to the curve are decided by the
!        fraction of the element inside the curve.
!        ----------------------------------------------------
!
         box = curveArray % boundingBox

         DO i = 1, maxElementID
            e => elementForID(i) % element
            IF ( .NOT.ASSOCIATED(e) )     CYCLE

            centroid = 0.0_RP
            hMin     = HUGE(hMin)
            DO k = 1, 4
               centroid = centroid + 0.25_RP*e % nodes(k) % node % x
               h        = NORM2(e % nodes(Loop(k+1,4)) % node % x(1:2) - e % nodes(k) % node % x(1:2))
               hMin     = MIN(h, hMin)
            END DO

            IF ( centroid(1) < box(BBOX_LEFT)   - hMin .OR. centroid(1) > box(BBOX_RIGHT) + hMin .OR. &
                 centroid(2) < box(BBOX_BOTTOM) - hMin .OR. centroid(2) > box(BBOX_TOP)   + hMin )     CYCLE

            isInside(i) = ACWindingFunction( centroid, curveArray % x, curveArray % nSegments-1 ) /= 0

            IF ( distanceToSegmentedCurve( centroid, curveArray ) < 0.25_RP*hMin )     THEN
               insideFraction(i) = elementInsideFraction( e, curveArray )
               isInside(i)       = insideFraction(i) > 0.5_RP
            END IF
         END DO
!
!        -------------------------------------------------
!        Make the inside and outside regions each a single
!        connected region, then remove any pinch nodes
!        -------------------------------------------------
!
         DO iteration = 1, MAX_PINCH_ITERATIONS

            CALL MergeDisconnectedRegions( elementForID, isInside, meshEdges )
!
!           -------------------------------------------
!           Interface edges separate inside and outside
!           -------------------------------------------
!
            interfaceEdgeCount = 0
            DO j = 1, numEdges
               edge => meshEdges(j) % edge
               isInterfaceEdge(j) = isInside(edge % elements(1) % element % id) .NEQV. &
                                    isInside(edge % elements(2) % element % id)
               IF ( isInterfaceEdge(j) )     THEN
                  DO k = 1, 2
                     m = edge % nodes(k) % node % id
                     interfaceEdgeCount(m) = interfaceEdgeCount(m) + 1
                  END DO
               END IF
            END DO
!
!           --------------------------------------------------------------
!           An element with three or more interface edges is a one element
!           wide bump or notch. All its nodes would be moved onto a short
!           piece of the curve, so switch its side.
!           --------------------------------------------------------------
!
            elementEdgeCount = 0
            DO j = 1, numEdges
               IF ( .NOT.isInterfaceEdge(j) )     CYCLE
               edge => meshEdges(j) % edge
               DO k = 1, 2
                  m = edge % elements(k) % element % id
                  elementEdgeCount(m) = elementEdgeCount(m) + 1
               END DO
            END DO
            IF ( ANY(elementEdgeCount > 2) )     THEN
               WHERE ( elementEdgeCount > 2 )     isInside = .NOT.isInside
               pinchNodeID = -1
               CYCLE
            END IF
!
!           ------------------------------------------------------------
!           An element whose two interface edges meet at a node keeps
!           three nodes on the curve. If the curve between the outer two
!           bends toward the element, as at a reflex corner, the element
!           has to wrap the corner and folds once its nodes are on the
!           curve. Switch its side (once) so that the interface runs
!           through its fourth node instead, which then takes the corner.
!           ------------------------------------------------------------
!
            IF ( flipCornerWrappingElements( curveArray, elementForID, meshEdges, isInterfaceEdge, &
                                             isInside, flippedAtCorner ) )     THEN
               pinchNodeID = -1
               CYCLE
            END IF
!
!           -----------------------------------------------------
!           A node with more than two interface edges is a pinch.
!           Reclassify the least certain element that touches it
!           -----------------------------------------------------
!
            pinchNodeID = 0
            DO j = 1, maxNodeID
               IF ( interfaceEdgeCount(j) > 2 )     THEN
                  pinchNodeID = j
                  EXIT
               END IF
            END DO
            IF ( pinchNodeID == 0 )     EXIT

            eFlip     => NULL()
            bestScore =  HUGE(bestScore)
            DO j = 1, numEdges
               IF ( .NOT.isInterfaceEdge(j) )     CYCLE
               edge => meshEdges(j) % edge
               IF ( edge % nodes(1) % node % id /= pinchNodeID .AND. &
                    edge % nodes(2) % node % id /= pinchNodeID )     CYCLE

               DO k = 1, 2
                  e => edge % elements(k) % element
                  IF ( insideFraction(e % id) < 0.0_RP )     &
                     insideFraction(e % id) = elementInsideFraction( e, curveArray )
                  score = ABS(insideFraction(e % id) - 0.5_RP)
                  IF ( score < bestScore )     THEN
                     bestScore =  score
                     eFlip     => e
                  END IF
               END DO
            END DO
            isInside(eFlip % id) = .NOT.isInside(eFlip % id)

         END DO

         IF ( pinchNodeID /= 0 )     THEN
            CALL ThrowErrorExceptionOfType( "CollectInterfaceEdgesFromElementSides", &
                                            "Unable to remove all bumps and pinch nodes on interface "&
                                            //TRIM(curveArray % boundaryName()), &
                                            FT_ERROR_WARNING )
         END IF
!
!        ------------------------------------------------------------------
!        Mark the nodes: INSIDE means on the interface. Then save the edges
!        ------------------------------------------------------------------
!
         iterator => mesh % nodesIterator
         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMNode(obj,node)
            IF ( node % bCurveChainID == curveArray % id )     node % bCurveSide = OUTSIDE
            CALL iterator % moveToNext()
         END DO

         DO j = 1, numEdges
            IF ( .NOT.isInterfaceEdge(j) )     CYCLE
            edge => meshEdges(j) % edge
            DO k = 1, 2
               edge % nodes(k) % node % bCurveSide    = INSIDE
               edge % nodes(k) % node % bCurveChainID = curveArray % id
            END DO
            edge % edgeType = ON_INTERFACE
            obj => edge
            CALL edgeList % add(obj)
         END DO
!
!        ----------------------------
!        Release all the local memory
!        ----------------------------
!
         DEALLOCATE(elementForID, meshEdges, isInside, isInterfaceEdge)
         DEALLOCATE(insideFraction, interfaceEdgeCount, elementEdgeCount, flippedAtCorner)

      END SUBROUTINE CollectInterfaceEdgesFromElementSides
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION flipCornerWrappingElements( curveArray, elementForID, meshEdges, &
                                                   isInterfaceEdge, isInside, flippedAtCorner )
         USE Geometry
         IMPLICIT NONE
!
!     ------------------------------------------------------------------
!     For CollectInterfaceEdgesFromElementSides: an element whose two
!     interface edges meet at a node keeps three nodes on the curve. If
!     the curve between the outer two bends toward the element, as at a
!     reflex corner, switch the side of the element (once, recorded in
!     flippedAtCorner). True if any element was switched.
!     ------------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SegmentedCurveArray), POINTER :: curveArray
         TYPE (SMElementPtr)                 :: elementForID(:)
         TYPE (SMEdgePtr)                    :: meshEdges(:)
         LOGICAL                             :: isInterfaceEdge(:), isInside(:), flippedAtCorner(:)
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, ALLOCATABLE   :: edgesOfElement(:,:), numFound(:)
         TYPE (SMEdge), POINTER :: edgeA => NULL(), edgeB => NULL()
         TYPE (SMNode), POINTER :: nodeP => NULL(), nodeA => NULL(), nodeB => NULL(), nodeF => NULL()
         TYPE (SMElement), POINTER :: e => NULL()
         INTEGER                :: id, kA, kB, kk, j, k, maxElementID, numEdges
         REAL(KIND=RP), PARAMETER :: MIN_FLIP_CORNER_ANGLE = 90.0_RP ! convex side, degrees
         LOGICAL                :: hasCorner
         REAL(KIND=RP)          :: corner(2), cornerAngle, aImage(2), bImage(2)

         flipCornerWrappingElements = .FALSE.
         maxElementID = SIZE(elementForID)
         numEdges     = SIZE(meshEdges)
         ALLOCATE( edgesOfElement(2,maxElementID), numFound(maxElementID) )
         numFound = 0
         DO j = 1, numEdges
            IF ( .NOT.isInterfaceEdge(j) )     CYCLE
            DO k = 1, 2
               id = meshEdges(j) % edge % elements(k) % element % id
               numFound(id) = numFound(id) + 1
               IF( numFound(id) <= 2 )     edgesOfElement(numFound(id),id) = j
            END DO
         END DO

         DO id = 1, maxElementID
            IF ( numFound(id) /= 2 .OR. flippedAtCorner(id) )     CYCLE
            edgeA => meshEdges(edgesOfElement(1,id)) % edge
            edgeB => meshEdges(edgesOfElement(2,id)) % edge
!
!           ---------------------------------------------
!           The shared node and the two nodes on either side
!           ---------------------------------------------
!
            nodeP => NULL()
            DO kA = 1, 2
               DO kB = 1, 2
                  IF ( edgeA % nodes(kA) % node % id == edgeB % nodes(kB) % node % id )     THEN
                     nodeP => edgeA % nodes(kA) % node
                     nodeA => edgeA % nodes(3-kA) % node
                     nodeB => edgeB % nodes(3-kB) % node
                  END IF
               END DO
            END DO
            IF ( .NOT.ASSOCIATED(nodeP) )     CYCLE

            e => elementForID(id) % element
            DO kk = 1, 4
               nodeF => e % nodes(kk) % node
               IF ( nodeF % id /= nodeP % id .AND. nodeF % id /= nodeA % id .AND. &
                    nodeF % id /= nodeB % id )     EXIT
            END DO

            CALL FindCornerTowardPoint( nodeA % x, nodeB % x, nodeF % x, curveArray, &
                                        hasCorner, corner, cornerAngle, aImage, bImage )
            IF ( .NOT.hasCorner )     CYCLE
!
!           ----------------------------------------------------------
!           Switching the element adds an element to the convex side,
!           which does not fit into a sharp corner. If the shared node
!           is on the other side of the curve and one of its neighbors
!           has its image next to the corner, the corner is already
!           taken by that neighbor's node, so leave the element as is.
!           ----------------------------------------------------------
!
            IF ( cornerAngle < MIN_FLIP_CORNER_ANGLE .AND. &
                 ((ACWindingFunction( nodeP % x, curveArray % x, curveArray % nSegments-1 ) /= 0) &
                  .NEQV. isInside(id)) )     THEN
               IF ( NORM2(aImage - corner) < 0.5_RP*NORM2(nodeA % x(1:2) - nodeP % x(1:2)) .OR. &
                    NORM2(bImage - corner) < 0.5_RP*NORM2(nodeB % x(1:2) - nodeP % x(1:2)) )     CYCLE
            END IF

            isInside(id)               = .NOT.isInside(id)
            flippedAtCorner(id)        = .TRUE.
            flipCornerWrappingElements = .TRUE.
         END DO

         DEALLOCATE( edgesOfElement, numFound )

      END FUNCTION flipCornerWrappingElements
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FindCornerTowardPoint( a, b, f, curveArray, hasCorner, corner, cornerAngle, aImage, bImage )
         USE Geometry, ONLY: angleBetween
         IMPLICIT NONE
!
!     -----------------------------------------------------------------
!     Find whether the piece of the segmented curve between the points
!     a' and b' nearest to a and b has a corner, sharper than
!     MAX_TOWARD_CORNER_ANGLE, that points toward f, i.e. whether a quad
!     with nodes f, a', corner, b' would have a reflex angle at the
!     corner. cornerAngle is the angle between a' and b' seen from
!     the corner, on the side away from f, in degrees.
!     -----------------------------------------------------------------
!
         REAL(KIND=RP)                       :: a(3), b(3), f(3)
         CLASS(SegmentedCurveArray), POINTER :: curveArray
         LOGICAL                             :: hasCorner
         REAL(KIND=RP)                       :: corner(2), cornerAngle, aImage(2), bImage(2)
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP), PARAMETER :: MAX_TOWARD_CORNER_ANGLE = 160.0_RP ! degrees
         INTEGER                  :: n, j, jStart, jEnd, jMax, i
         REAL(KIND=RP)            :: sA, sB, sTmp, p1(2), p2(2), chord(2), side
         REAL(KIND=RP)            :: depth, depthMax, u(2), v(2)

         hasCorner   = .FALSE.
         corner      = 0.0_RP
         cornerAngle = 180.0_RP
         n = curveArray % nSegments

         CALL NearestPointOnSegmentedCurve( a, curveArray, sA, aImage )
         CALL NearestPointOnSegmentedCurve( b, curveArray, sB, bImage )
         p1 = aImage
         p2 = bImage
         IF ( sB < sA )     THEN
            sTmp = sA
            sA   = sB
            sB   = sTmp
            p1 = bImage
            p2 = aImage
         END IF
         chord = p2 - p1
         IF ( NORM2(chord) == 0.0_RP )     RETURN
         side = SIGN(1.0_RP, chord(1)*(f(2) - p1(2)) - chord(2)*(f(1) - p1(1)))
!
!        ---------------------------------------------------------
!        Vertices strictly between the two images, the shorter way
!        around the closed curve
!        ---------------------------------------------------------
!
         IF ( sB - sA <= 0.5_RP*n )     THEN
            jStart = FLOOR(sA) + 1
            jEnd   = CEILING(sB) - 1
         ELSE
            jStart = FLOOR(sB) + 1
            jEnd   = CEILING(sA) - 1 + n
         END IF

         depthMax = 0.0_RP
         jMax     = -1
         DO i = jStart, jEnd
            j     = MODULO(i, n)
            depth = side*(chord(1)*(curveArray % x(2,j) - p1(2)) - chord(2)*(curveArray % x(1,j) - p1(1)))
            IF ( depth > depthMax )     THEN
               depthMax = depth
               jMax     = j
            END IF
         END DO
         IF ( jMax < 0 )     RETURN

         u = aImage - curveArray % x(1:2,jMax)
         v = bImage - curveArray % x(1:2,jMax)
         IF ( NORM2(u) == 0.0_RP .OR. NORM2(v) == 0.0_RP )     RETURN
         cornerAngle = angleBetween(u, v)
         hasCorner   = cornerAngle < MAX_TOWARD_CORNER_ANGLE
         corner      = curveArray % x(1:2,jMax)

      END SUBROUTINE FindCornerTowardPoint
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE NearestPointOnSegmentedCurve( p, curveArray, s, pImage, distance )
         IMPLICIT NONE
!
!     -----------------------------------------------------------------
!     Point on the segments of a segmented curve nearest to p, and its
!     location s = j + fraction along segment j (from x(:,j) to x(:,j+1))
!     and (optionally) its distance from p
!     -----------------------------------------------------------------
!
         REAL(KIND=RP)                       :: p(3), s, pImage(2)
         CLASS(SegmentedCurveArray), POINTER :: curveArray
         REAL(KIND=RP), OPTIONAL             :: distance

         INTEGER       :: j
         REAL(KIND=RP) :: a(2), d(2), t, lengthSquared, dist, distMin

         distMin = HUGE(distMin)
         s       = 0.0_RP
         pImage  = curveArray % x(1:2,0)
         DO j = 0, curveArray % nSegments - 1
            a = curveArray % x(1:2,j)
            d = curveArray % x(1:2,j+1) - a
            lengthSquared = DOT_PRODUCT(d,d)
            t = 0.0_RP
            IF ( lengthSquared > 0.0_RP )     t = MAX(0.0_RP, MIN(1.0_RP, DOT_PRODUCT(p(1:2) - a, d)/lengthSquared))
            dist = NORM2(p(1:2) - a - t*d)
            IF ( dist < distMin )     THEN
               distMin = dist
               s       = j + t
               pImage  = a + t*d
            END IF
         END DO
         IF ( PRESENT(distance) )     distance = distMin

      END SUBROUTINE NearestPointOnSegmentedCurve
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MergeDisconnectedRegions( elementForID, isInside, meshEdges )
         IMPLICIT NONE
!
!     ---------------------------------------------------------------
!     Elements connected through shared edges and on the same side of
!     an interface form a region. Keep the largest inside region and
!     the largest outside region and switch the side of all others,
!     so that a single interface loop separates the two.
!     ---------------------------------------------------------------
!
         TYPE (SMElementPtr) :: elementForID(:)
         LOGICAL             :: isInside(:)
         TYPE (SMEdgePtr)    :: meshEdges(:)
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, ALLOCATABLE :: parent(:), regionSize(:)
         INTEGER              :: i, j, id1, id2, r1, r2, largestRoot, pass, n
         LOGICAL              :: side

         ALLOCATE( parent(SIZE(isInside)), regionSize(SIZE(isInside)) )

         n = SIZE(parent)
         DO pass = 1, 2
            side = (pass == 1)

            DO i = 1, n
               parent(i) = i
            END DO

            DO j = 1, SIZE(meshEdges)
               id1 = meshEdges(j) % edge % elements(1) % element % id
               id2 = meshEdges(j) % edge % elements(2) % element % id
               IF ( (isInside(id1) .EQV. side) .AND. (isInside(id2) .EQV. side) )     THEN
                  r1 = findRoot(id1, parent, n)
                  r2 = findRoot(id2, parent, n)
                  IF ( r1 /= r2 )     parent(r2) = r1
               END IF
            END DO

            regionSize = 0
            DO i = 1, n
               IF ( .NOT.ASSOCIATED(elementForID(i) % element) )     CYCLE
               IF ( isInside(i) .NEQV. side )     CYCLE
               r1 = findRoot(i, parent, n)
               regionSize(r1) = regionSize(r1) + 1
            END DO
            IF ( MAXVAL(regionSize) == 0 )     CYCLE
            largestRoot = MAXLOC(regionSize, DIM = 1)

            DO i = 1, n
               IF ( .NOT.ASSOCIATED(elementForID(i) % element) )     CYCLE
               IF ( isInside(i) .NEQV. side )     CYCLE
               IF ( findRoot(i, parent, n) /= largestRoot )     isInside(i) = .NOT.side
            END DO
         END DO
!
!        ----------------------------
!        Release all the local memory
!        ----------------------------
!
         DEALLOCATE(parent, regionSize)

      END SUBROUTINE MergeDisconnectedRegions
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION findRoot( id, parent, n )
         INTEGER               :: id, n
         INTEGER, DIMENSION(n) :: parent
         findRoot = id
         DO WHILE ( parent(findRoot) /= findRoot )
            parent(findRoot) = parent(parent(findRoot))
            findRoot         = parent(findRoot)
         END DO
      END FUNCTION findRoot
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION elementInsideFraction( e, curveArray )
         USE Geometry
         IMPLICIT NONE
!
!     ---------------------------------------------------------
!     Approximate the fraction of the area of an element inside
!     a curve by sampling points of the bilinear map
!     ---------------------------------------------------------
!
         TYPE (SMElement)          , POINTER :: e
         CLASS(SegmentedCurveArray), POINTER :: curveArray
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, PARAMETER :: NUM_SAMPLES = 5
         INTEGER            :: i, j, numInside
         REAL(KIND=RP)      :: u, v, p(3)

         numInside = 0
         DO j = 1, NUM_SAMPLES
            v = (j - 0.5_RP)/NUM_SAMPLES
            DO i = 1, NUM_SAMPLES
               u = (i - 0.5_RP)/NUM_SAMPLES
               p = (1.0_RP - u) * (1.0_RP - v) * e % nodes(1) % node % x &
                  + u * (1.0_RP - v) * e % nodes(2) % node % x           &
                  + u * v * e % nodes(3) % node % x                      &
                  + (1.0_RP - u)* v * e % nodes(4) % node % x
               IF ( ACWindingFunction( p, curveArray % x, curveArray % nSegments-1 ) /= 0 ) &
                  numInside = numInside + 1
            END DO
         END DO
         elementInsideFraction = REAL(numInside, KIND=RP)/REAL(NUM_SAMPLES**2, KIND=RP)

      END FUNCTION elementInsideFraction
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION distanceToSegmentedCurve( p, curveArray )
         IMPLICIT NONE
!
!     -------------------------------------------------------
!     Shortest distance from the point p to the line segments
!     of a segmented curve
!     -------------------------------------------------------
!
         REAL(KIND=RP)                       :: p(3)
         CLASS(SegmentedCurveArray), POINTER :: curveArray
         REAL(KIND=RP)                       :: s, pImage(2)

         CALL NearestPointOnSegmentedCurve( p, curveArray, s, pImage, distanceToSegmentedCurve )

      END FUNCTION distanceToSegmentedCurve
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PruneInterfaceEdges( edgeList )
!
!     -----------------------------------------------------------------
!     A thin feature of an interface curve, like a sharp tip, can leave
!     the interface edges in a configuration that cannot be ordered
!     into a single closed loop:
!
!     (1) A node marked INSIDE that is connected to the other inside
!         nodes by only one interface edge, i.e. a dangling spike.
!         The node is marked OUTSIDE and its edge is removed.
!     (2) An edge whose nodes are both INSIDE but where neither
!         neighboring element has an OUTSIDE node, so it does not
!         separate the materials. Such a chord gives its two nodes
!         three or more interface edges. The edge is removed.
!
!     Repeat until every node is shared by exactly two edges or no
!     further edge can be removed.
!     -----------------------------------------------------------------
!
         IMPLICIT NONE

         CLASS(FTLinkedList), POINTER :: edgeList
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (SMEdgePtr)           , ALLOCATABLE :: edgeArray(:)
         TYPE (SMEdge)              , POINTER     :: edge => NULL()
         TYPE (SMNode)              , POINTER     :: node => NULL(), danglingNode => NULL()
         CLASS(FTLinkedListIterator), POINTER     :: iterator => NULL()
         CLASS(FTObject)            , POINTER     :: obj => NULL()
         INTEGER                    , ALLOCATABLE :: nodeCount(:,:), countForNodeID(:)
         INTEGER                                  :: numEdges, i, k, maxNodeID
         LOGICAL                                  :: edgeRemoved, foundDanglingEdge

         edgeRemoved = .TRUE.
         DO WHILE ( edgeRemoved )
            edgeRemoved = .FALSE.
!
!           -------------------------------------------
!           Copy the current list into an array so that
!           the nodes can be compared pairwise
!           -------------------------------------------
!
            numEdges = edgeList % COUNT()
            IF ( numEdges == 0 )     RETURN
            ALLOCATE(edgeArray(numEdges), nodeCount(2,numEdges))

            ALLOCATE(iterator)
            CALL iterator % initWithFTLinkedListClass(edgeList)
            CALL iterator % setToStart()
            i = 1
            DO WHILE ( .NOT.iterator % isAtEnd() )
               obj => iterator % object()
               CALL castToSMEdge(obj,edge)
               edgeArray(i) % edge => edge
               i = i + 1
               CALL iterator % moveToNext()
            END DO
            CALL releaseFTLinkedListIteratorClass(iterator)
!
!           ----------------------------------------------
!           Count the number of edges that share each node
!           ----------------------------------------------
!
            maxNodeID = 0
            DO i = 1, numEdges
               DO k = 1, 2
                  maxNodeID = MAX(maxNodeID, edgeArray(i) % edge % nodes(k) % node % id)
               END DO
            END DO
            ALLOCATE(countForNodeID(maxNodeID))

            countForNodeID = 0
            DO i = 1, numEdges
               DO k = 1, 2
                  node => edgeArray(i) % edge % nodes(k) % node
                  countForNodeID(node % id) = countForNodeID(node % id) + 1
               END DO
            END DO
            DO i = 1, numEdges
               DO k = 1, 2
                  nodeCount(k,i) = countForNodeID(edgeArray(i) % edge % nodes(k) % node % id)
               END DO
            END DO
!
!           -------------------------------
!           First look for a dangling edge
!           -------------------------------
!
            foundDanglingEdge = .FALSE.
            DO i = 1, numEdges
               DO k = 1, 2
                  IF ( nodeCount(k,i) == 1 )     THEN
                     edge         => edgeArray(i) % edge
                     danglingNode => edge % nodes(k) % node
                     danglingNode % bCurveSide = OUTSIDE

                     edgeRemoved  = .TRUE.
                     foundDanglingEdge = .TRUE.
                     EXIT
                  END IF
               END DO

               IF (foundDanglingEdge) EXIT
            END DO
!
!           -------------------------------------------
!           then look for a chord between branch points
!           -------------------------------------------
!
            IF ( .NOT.edgeRemoved )     THEN
               DO i = 1, numEdges
                  IF ( nodeCount(1,i) < 3 .OR. nodeCount(2,i) < 3 )     CYCLE
                  IF ( edgeBordersOutsideNode(edgeArray(i) % edge) )   CYCLE
                  edge        => edgeArray(i) % edge

                  edgeRemoved = .TRUE.
                  EXIT
               END DO
            END IF
            DEALLOCATE(edgeArray, nodeCount, countForNodeID)

            IF ( edgeRemoved )     THEN
               edge % edgeType = INSIDE
               obj => edge
               CALL edgeList % remove(obj)
            END IF
         END DO

      END SUBROUTINE PruneInterfaceEdges
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION edgeBordersOutsideNode( edge )
!
!     ------------------------------------------------------
!     True if either element that shares the edge has a node
!     marked OUTSIDE of the interface curve
!     ------------------------------------------------------
!
         IMPLICIT NONE
         TYPE (SMEdge), POINTER :: edge
         INTEGER                :: j, k

         edgeBordersOutsideNode = .FALSE.
         DO j = 1, 2
            IF ( .NOT.ASSOCIATED(edge % elements(j) % element) )     CYCLE
            DO k = 1, 4
               IF ( edge % elements(j) % element % nodes(k) % node % bCurveSide == OUTSIDE )     THEN
                  edgeBordersOutsideNode = .TRUE.
                  RETURN
               END IF
            END DO
         END DO

      END FUNCTION edgeBordersOutsideNode
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
         CLASS(FTLinkedListIterator), POINTER     :: iterator       => NULL()
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
            CALL iterator % initWithFTLinkedListClass(edgeList)
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
            CALL releaseFTLinkedListIteratorClass(self = iterator)
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
            CALL releaseFTLinkedListClass(self = sortedEdges)
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
         CLASS(FTLinkedListIterator)  , POINTER :: iterator => NULL()
         CLASS(FTLinkedListRecord)    , POINTER :: cr => NULL()
         TYPE (SMEdge)                , POINTER :: currentEdge => NULL(), prevEdge => NULL()
         INTEGER                                :: nNodes, id, idP1, idP2, j
         CLASS(FTObject)              , POINTER :: obj => NULL()

         NULLIFY(GatheredNodes)
         IF ( .NOT.ASSOCIATED(list) )     RETURN

         nNodes = list % COUNT()
         ALLOCATE( GatheredNodes(nNodes) )
         ALLOCATE(iterator)
         CALL iterator % initwithFTLinkedListClass(list)
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

         CALL releaseFTLinkedListIteratorClass(self = iterator)

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
         CLASS(FTLinkedListIterator), POINTER     :: edgeListIterator => NULL()

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
            CALL edgeListIterator % initwithFTLinkedListClass(edgeList)

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
               CALL releaseFTLinkedListIteratorClass(self = edgeListIterator)
               RETURN
            END IF

            chain => model % chainWithID(chainID)
!
!           --------------------------------------------------------
!           Bail if the chain for this edge curve ID cannot be found
!           --------------------------------------------------------
!
            IF ( .NOT.ASSOCIATED(chain) )     THEN
               WRITE(msg,*) "Chain with id ", chainID, " Not found in model"
               CALL ThrowErrorExceptionOfType("LocateEdgeImagesOnBoundaries",msg,FT_ERROR_FATAL)
               CALL releaseFTLinkedListIteratorClass(self = edgeListIterator)
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
            CALL releaseFTLinkedListIteratorClass(self = edgeListIterator)

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
         TYPE (SMNode)                  , POINTER :: node => NULL()
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
         tStart  = 0.0_RP
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
            IF(tEnd > 1.0_RP) tEnd = 1.0_RP
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

         CLASS (FTLinkedListIterator), POINTER :: iterator => NULL()
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

            IF ( numBoundaries > 0 )     THEN
!
!              ----------------------------------------------------------
!              Nodes pinned to joints (whereOnBoundary = 0 or 1) are kept
!              fixed, also on interface curves
!              ----------------------------------------------------------
!
               DO j = 1, numBoundaries

                  obj => boundaryEdgesArray % objectAtIndex(j)
                  CALL cast(obj,currentEdgeList)
                  IF(.NOT. ASSOCIATED(currentEdgeList)) THEN
                     WRITE(stderr,*)  "Unnassociated edgelist in SetNodeActiveStatus number",j
                     CYCLE
                  END IF

                  CALL iterator % setLinkedListClass(currentEdgeList)
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
         CALL releaseFTLinkedListIteratorClass(self = iterator)
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
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
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
         CALL iterator % initwithFTLinkedListClass(boundaryNodesList)
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
!
!           ----------------------------------------------------------------
!           A node that is not pinned to a joint (whereOnBoundary = 0 or 1)
!           and has run into the end of its curve stays where it was, rather
!           than collapsing onto the node pinned at the joint.
!           ----------------------------------------------------------------
!
            IF ( tOld > 0.0_RP .AND. tOld < 1.0_RP )     THEN
               IF( AlmostEqual(tNew, 0.0_RP) .OR. AlmostEqual(tNew, 1.0_RP) )     tNew = tOld
            END IF
            pNew = c % positionAt(tNew)

            currentNode % x                = pNew
            currentNode % whereOnBoundary  = tNew
            currentNode % distToBoundary   = 0.0_RP
            currentNode % gWhereOnBoundary = chain % ChainTForCurveTInCurve(tNew,c)

            CALL iterator % moveToNext()
         END DO

         CALL releaseFTLinkedListIteratorClass(self = iterator)

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
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CollectBoundaryAndInterfaceNodes(allNodesIterator,boundaryNodesList)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (FTLinkedListIterator), POINTER :: allNodesIterator
         CLASS(FTLinkedList)        , POINTER :: boundaryNodesList
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (SMNode)  , POINTER :: currentNode => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
!
!        ------------------------------------------------------
!        Loop through all the nodes and add those whose
!        distance to a boundary is zero to the boundaryNodeList
!        ------------------------------------------------------
!
         CALL allNodesIterator % setToStart()
         DO WHILE ( .NOT.allNodesIterator % isAtEnd() )
            obj => allNodesIterator % object()
            CALL castToSMNode(obj,currentNode)
            IF ( IsOnBoundaryCurve(currentNode) .AND. &
                 currentNode%distToBoundary == 0.0_RP )     THEN
               CALL boundaryNodesList % add(obj)
            END IF
            CALL allNodesIterator % moveToNext()
         END DO

      END SUBROUTINE CollectBoundaryAndInterfaceNodes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SortBoundaryNodesToChains(nodesIterator, numBoundaryChains, chainNodesArray)
!
!     ---------------------------------------------------------------------------
!     Gather up all the boundary nodes and arrange them in arrays by chainID. The
!     returned chainNodesArray contains pointers to all the nodes that fall along
!     a given curve chain. Thus, chainNodesArray(j) % array is an ordered array
!     of all nodes along boundary chain j.
!     ---------------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                                 :: numBoundaryChains
         TYPE(FTLinkedListIterator), POINTER     :: nodesIterator
         TYPE(JaggedNodeArray)                   :: chainNodesArray(:)
!
!        ---------------
!        Local Variables
!        ---------------
!
         TYPE(LinkedListPtr)        , POINTER :: boundaryNodesArray(:)
         CLASS(FTLinkedList)        , POINTER :: boundaryNodesList => NULL()
         CLASS(FTLinkedList)        , POINTER :: list
         CLASS(FTLinkedListIterator), POINTER :: boundaryNodesIterator
         CLASS(FTLinkedListIterator), POINTER :: listIterator
         TYPE (SMNode)              , POINTER :: currentNode => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()
         CLASS(FTLinkedListRecord)  , POINTER :: previousRecord, currentRecord
         INTEGER                              :: id, j, i, k, n
         REAL(KIND=RP)                        :: prevT, currentT
         LOGICAL                              :: reOrder
         TYPE(SMNode)               , POINTER :: tmpNode
!
!        -----------------------------------------------------
!        Temporary storage for the nodes along each curve chain
!        -----------------------------------------------------
!
         ALLOCATE(boundaryNodesArray(numBoundaryChains))
         DO id = 1, numBoundaryChains
            ALLOCATE(boundaryNodesArray(id) % list)
            CALL boundaryNodesArray(id) % list % init()
         END DO
!
!        ------------------------------
!        Collect all the boundary nodes
!        ------------------------------
!
         ALLOCATE(boundaryNodesList)
         CALL boundaryNodesList % init()
         CALL CollectBoundaryAndInterfaceNodes( nodesIterator, boundaryNodesList )
!
!        --------------------------------------------------------
!        Separate into a single node list for each boundary chain
!        --------------------------------------------------------
!
         ALLOCATE(boundaryNodesIterator)
         CALL boundaryNodesIterator % initwithFTLinkedListClass(boundaryNodesList)
         CALL boundaryNodesIterator % setToStart()

         DO WHILE ( .NOT.boundaryNodesIterator % isAtEnd() )
            obj => boundaryNodesIterator % object()
            CALL castToSMNode(obj,currentNode)
            id = currentNode % bCurveChainID
            CALL boundaryNodesArray(id) % list % add(obj)
            CALL boundaryNodesIterator % moveToNext()
         END DO
!
!        ------------------------------------------------------------------------
!        The nodes in each chain will be ordered, but do not necessarily start
!        at parameter value of zero. Since the nodes describe a closed (periodic)
!        curve, we can just reset the pointers in the linked lists so that the
!        parameter values are strictly increasing.
!        ------------------------------------------------------------------------
!
         ALLOCATE(listIterator)
         CALL listIterator % init()
         DO id = 1, SIZE(boundaryNodesArray)
            list => boundaryNodesArray(id) % list
            CALL listIterator % setLinkedListClass(list)

            previousRecord => listIterator % currentRecord()
            obj            => listIterator % object()
            CALL castToSMNode(obj,currentNode)
            prevT = currentNode % gWhereOnBoundary
            CALL listIterator % moveToNext()

            reOrder = .FALSE.
            DO WHILE ( .NOT.listIterator % isAtEnd() )
               currentRecord => listIterator % currentRecord()
               obj           => listIterator % object()
               CALL castToSMNode(obj,currentNode)
               currentT = currentNode % gWhereOnBoundary
!
!              -----------------------------------------------
!              If the parameter, t, is starting over, then
!              the list needs to be re-ordered. Stop here and
!              do the re-ordering after exit
!              -----------------------------------------------
!
               IF ( currentT < prevT )     THEN ! The parameter t is starting over
                  reOrder = .TRUE.
                  EXIT
               END IF

               previousRecord => currentRecord
               CALL listIterator % moveToNext()
            END DO

            IF ( reOrder )     THEN
               list % tail % next    => list % head
               list % head           => currentRecord
               list % tail           => previousRecord
               previousRecord % next => NULL()
            END IF
!
!           ------------------------------------
!           Copy each chain into an array
!           *Note that these are weak references
!            and can go away at any time*
!           ------------------------------------
!
            ALLOCATE( chainNodesArray(id) % array(list % count()))
            CALL listIterator % setToStart()
            j = 0
            DO WHILE ( .NOT.listIterator % isAtEnd() )
               j = j + 1
               obj => listIterator % object()
               CALL castToSMNode(obj,currentNode)
               chainNodesArray(id) % array(j) % node => currentNode
               CALL listIterator % moveToNext()
            END DO
!
!           --------------------------------------------------------------------------
!           Ensure the boundary nodes in a chain are ordered. Particularly important
!           for any interfaceBoundaries. This uses a simple insertion sort. Code is
!           included to do a quickSort on this data at the end of the module, should
!           there be a switch desired. But in early testing on the meshes that we
!           generate, the insertion sort wins in cpu time, so we're going with that.
!           -------------------------------------------------------------------------
!
            ! TODO: Maybe add a Circulation check here
            ! So this only happens when necessary?
            n = SIZE(chainNodesArray(id) % array)
!            CALL QuickSort(chainNodesArray(id) % array, 1, n)

            DO i = 2, n
               tmpNode => chainNodesArray(id) % array(i) % node

               k = i - 1
               currentT = tmpNode % gWhereOnBoundary
               DO WHILE (chainNodesArray(id) % array(k) % node % gWhereOnBoundary > currentT)
                  chainNodesArray(id) % array(k+1) % node => chainNodesArray(id) % array(k) % node
                  k = k - 1
                  IF(k == 0) EXIT
               END DO

               chainNodesArray(id) % array(k+1) % node => tmpNode
            END DO
         END DO
!
!        -------
!        Cleanup
!        -------
!
         DO id = 1, numBoundaryChains
            CALL releaseFTLinkedListClass(boundaryNodesArray(id) % list)
         END DO
         DEALLOCATE( boundaryNodesArray )

         CALL releaseFTLinkedListIteratorClass(listIterator)
         CALL releaseFTLinkedListIteratorClass(boundaryNodesIterator)
         CALL releaseFTLinkedListClass(boundaryNodesList)

      END SUBROUTINE SortBoundaryNodesToChains
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SpreadCollapsedBoundaryLocations( list, model )
!        ----------------------------------------------------------------------
!        Guards against collapsed or reversed boundary node images before the
!        boundary elements are generated.
!
!        Each node on the ragged (background grid) boundary has an "image": its
!        projection onto the boundary curve, stored as the chain parameter
!        `gWhereOnBoundary`. Where the grid and curve disagree (sharp bends,
!        features smaller than a grid cell), the images of neighboring nodes
!        can land almost on top of each other or in the wrong order along the
!        curve. The boundary element built from such a pair is degenerate or
!        inverted (e.g. a bow tie).
!
!        For each movable node (ROW_SIDE only; joint-related nodes stay
!        pinned), the image is moved to the parametric midpoint of its two
!        neighbor images when either
!          - an image is not ahead of its neighbor along the curve (dt <= 0), or
!          - the distance to a neighboring image is less than minRatio (0.4)
!            times the length of the corresponding ragged grid edge.
!        ----------------------------------------------------------------------
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
         TYPE (SMNode)                , POINTER :: node => NULL()

         INTEGER                                :: j, jm, jp, n
         REAL(KIND=RP)                          :: t0, tm, tp, dtm, dtp, span, tNew
         REAL(KIND=RP)                          :: xm(3), x0(3), xp(3)
         REAL(KIND=RP)                          :: pm(3), p0(3), pp(3)
         REAL(KIND=RP)                          :: lRaggedM, lRaggedP, lImageM, lImageP

         REAL(KIND=RP), PARAMETER               :: minRatio  = 0.4_RP ! tunable

         INTEGER, EXTERNAL :: Loop

         nodeArray => GatheredNodes(list)
         n =  SIZE(nodeArray)
         chain => model % chainWithID(nodeArray(1) % node % bCurveChainID)

         DO j = 1, n
            node => nodeArray(j) % node
!
!           ----------------------------------------------------------
!           Only ROW_SIDE targets are movable. Joint-related nodes
!           (ROW_END, ROW_CORNER, ROW_REVERSAL) are pinned to corners.
!           ----------------------------------------------------------
!
            IF( node % nodeType /= ROW_SIDE )     CYCLE
!
!           -------------------------------------------------------
!           Avoid moving any smooth joint-related nodes on ROW_SIDE
!           -------------------------------------------------------
!
            IF( AlmostEqual(node % whereOnBoundary, 0.0_RP) .OR. &
                AlmostEqual(node % whereOnBoundary, 1.0_RP) )     CYCLE

            jm = Loop(j-1,n)
            jp = Loop(j+1,n)

            t0 = node % gWhereOnBoundary
            tm = nodeArray(jm) % node % gWhereOnBoundary
            tp = nodeArray(jp) % node % gWhereOnBoundary

            dtm  = wrapDifference(t0 - tm)
            dtp  = wrapDifference(tp - t0)
            span = wrapDifference(tp - tm)
!
!           -----------------------------------------------------
!           If the neighbors themselves are out of order there is
!           nothing sensible to center between. Leave it alone.
!           -----------------------------------------------------
!
            IF( span <= 0.0_RP )     CYCLE
!
!           ------------------------------------------------
!           Lengths of the ragged edges the images came from
!           ------------------------------------------------
!
            xm = nodeArray(jm) % node % x
            x0 = node % x
            xp = nodeArray(jp) % node % x
            lRaggedM = NORM2(x0 - xm)
            lRaggedP = NORM2(xp - x0)
!
!           ------------------------------------
!           Lengths between the projected images
!           ------------------------------------
!
            pm = chain % positionAt(tm)
            p0 = chain % positionAt(t0)
            pp = chain % positionAt(tp)
            lImageM = NORM2(p0 - pm)
            lImageP = NORM2(pp - p0)

            IF( dtm <= 0.0_RP .OR. dtp <= 0.0_RP .OR.           &
               lImageM < minRatio*lRaggedM .OR.                 &
               lImageP < minRatio*lRaggedP )     THEN
!
!              --------------------------------------
!              Center the image between its neighbors
!              --------------------------------------
!
               tNew = MODULO(tm + 0.5_RP*span, 1.0_RP)
               node % gWhereOnBoundary = tNew
               node % whereOnBoundary  = chain % curveTForChainT(tNew)
               node % bCurveID         = chain % myCurveIDs(chain % curveNumberForLocation(tNew))
            END IF

         END DO

         RETURN
      END SUBROUTINE SpreadCollapsedBoundaryLocations

!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MoveInterfaceNodesToImages( list, model )
         USE SMModelClass
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        The interface nodes were moved onto the interface before their
!        curve locations were adjusted at the joints (FlagEndNodes) and
!        spread out (SpreadCollapsedBoundaryLocations).
!        Put the nodes at their (new) locations on the curve.
!        ---------------------------------------------------------------
!
         CLASS(FTLinkedList), POINTER :: list
         TYPE (SMModel)     , POINTER :: model

         TYPE(SMNodePtr), DIMENSION(:), POINTER :: nodeArray => NULL()
         CLASS(SMChainedCurve)        , POINTER :: chain => NULL()
         CLASS(SMCurve)               , POINTER :: c => NULL()
         INTEGER                                :: j

         nodeArray => GatheredNodes(list)
         DO j = 1, SIZE(nodeArray)
            c => model % curveWithID(nodeArray(j) % node % bCurveID, chain)
            nodeArray(j) % node % x = c % positionAt(nodeArray(j) % node % whereOnBoundary)
         END DO
         DEALLOCATE( nodeArray )

      END SUBROUTINE MoveInterfaceNodesToImages
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION wrapDifference(d) RESULT(w)
!
!        ---------------------------------------------------------------
!        Wrap a parameter difference into (-0.5, 0.5] for a closed chain
!        ---------------------------------------------------------------
!
         REAL(KIND=RP) :: d, w
         w = d
         IF( w .GT.  0.5_RP ) w = w - 1.0_RP
         IF( w .LE. -0.5_RP ) w = w + 1.0_RP
      END FUNCTION wrapDifference
!
!////////////////////////////////////////////////////////////////////////
!
!      RECURSIVE SUBROUTINE QuickSort(nodeArray, left, right)
!        TYPE(SMNodePtr), INTENT(INOUT) :: nodeArray(:)
!        INTEGER        , INTENT(IN)    :: left, right
!
!        INTEGER               :: i, j
!        REAL(KIND=RP)         :: pivot
!        TYPE(SMNode), POINTER :: temp
!
!        IF (right - left <= 20) THEN
!            CALL InsertionSort(nodeArray, left, right)
!            RETURN
!        END IF
!
!        ! Median-of-three pivot selection
!        pivot = MedianOfThree(nodeArray(left)  % node % gWhereOnBoundary,         &
!                             (nodeArray(left)  % node % gWhereOnBoundary +        &
!                              nodeArray(left)  % node % gWhereOnBoundary)/2.0_RP, &
!                              nodeArray(right) % node % gWhereOnBoundary)
!        i = left
!        j = right
!
!        DO
!            DO WHILE (nodeArray(i) % node % gWhereOnBoundary < pivot)
!                i = i + 1
!            END DO
!
!            DO WHILE (nodeArray(j) % node % gWhereOnBoundary > pivot)
!                j = j - 1
!            END DO
!
!            IF (i <= j) THEN
!                temp => nodeArray(i) % node
!                nodeArray(i) % node => nodeArray(j) % node
!                nodeArray(j) % node => temp
!
!                i = i + 1
!                j = j - 1
!            END IF
!
!            IF (i > j) EXIT
!        END DO
!
!        IF (left < j)  CALL quickSort(nodeArray, left, j)
!        IF (i < right) CALL quickSort(nodeArray, i, right)
!
!    END SUBROUTINE QuickSort
!!
!!////////////////////////////////////////////////////////////////////////
!!
!    PURE FUNCTION MedianOfThree(x, y, z) RESULT(median)
!        REAL(KIND=RP), INTENT(IN) :: x, y, z
!        REAL(KIND=RP)             :: median
!
!        median = MAX(MIN(x, y), MIN(MAX(x, y), z))
!    END FUNCTION MedianOfThree
!!
!!////////////////////////////////////////////////////////////////////////
!!
!    SUBROUTINE InsertionSort(nodeArray, left, right)
!!
!!      ---------
!!      Arguments
!!      ---------
!!
!        TYPE(SMNodePtr), INTENT(INOUT) :: nodeArray(:)
!        INTEGER, INTENT(IN)            :: left, right
!        REAL(KIND=RP)                  :: key
!!
!!       ---------------
!!       Local variables
!!       ---------------
!!
!        TYPE(SMNode), POINTER :: tmpNode
!        INTEGER               :: i, j
!
!        DO i = left + 1, right
!            tmpNode => nodeArray(i) % node
!            key     = tmpNode % gWhereOnBoundary
!            j       = i - 1
!
!            DO WHILE (j >= left)
!                IF (nodeArray(j) % node % gWhereOnBoundary <= key) EXIT
!
!                nodeArray(j + 1) % node => nodeArray(j) % node
!                j = j - 1
!            END DO
!
!            nodeArray(j + 1) % node => tmpNode
!        END DO
!
!    END SUBROUTINE InsertionSort

   END MODULE MeshBoundaryMethodsModule

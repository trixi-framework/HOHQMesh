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
!      BoundaryEdgeCleaning.f90
!      Created: September 16, 2013 9:37 AM
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
      Module BoundaryEdgeCleaningModule
      USE SMMeshClass
      USE SMModelClass
      USE ConnectionsModule
      IMPLICIT NONE
      CONTAINS
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CleanUpBoundaryCurves( mesh, model, errorCode )
      IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(SMMesh)            :: mesh
         TYPE (SMModel), POINTER :: model
         INTEGER                 :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                       :: j
         INTEGER                       :: numBoundaries
         CLASS(FTLinkedList), POINTER  :: list => NULL()
         CLASS(FTObject)    , POINTER  :: obj  => NULL()
!
!        ----------------
!        Make connections
!        ----------------
!
         CALL MakeElementToEdgeConnections( mesh )
         CALL MakeNodeToEdgeConnections   ( mesh )
         CALL MakeNodeToElementConnections( mesh, errorCode )

         numBoundaries = boundaryEdgesArray % COUNT()
!
!        ----------------------------------
!        Do the cleanup on "External" edges
!        ----------------------------------
!
         DO j = 1, numBoundaries
            IF( boundaryEdgesType(j) == BOUNDARY_EDGES ) THEN
               obj  => boundaryEdgesArray % objectAtIndex(j)
               list => linkedListFromObject(obj)
               CALL CleanUpBoundaryEdges( list )
            END IF
         END DO
!
!        ------------------------------------------------
!        Now do the same for the internal interface edges
!        where we map nodes to the boundaries
!        ------------------------------------------------
!
         DO j = 1, numBoundaries
            IF ( boundaryEdgesType(j) == INTERFACE_EDGES )     THEN
               CALL cast(boundaryEdgesArray % objectAtIndex(j),list)
               CALL MoveInterfaceNodesToBoundary(list,model)
            END IF
         END DO

         CALL DoLazyDelete( mesh )
!
!        ---------------------------------------
!        Delete temporaries - no longer in sync.
!        ---------------------------------------
!
         CALL deallocateElementToEdgeConnections
         CALL deallocateNodeToElementConnections
         CALL deallocateNodeToEdgeConnections

      END SUBROUTINE CleanUpBoundaryCurves

!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RemoveDetachedElements( mesh, numberRemoved )
         IMPLICIT NONE
!
!     ------------------------------------------------------------------
!     After the elements outside of the boundaries are removed, the rest
!     must form a single region whose boundary edges can be ordered into
!     loops. Near a sharp tip, or where a boundary curve is narrower than
!     the grid, some elements can be left on their own, connected to the
!     rest at most through a node. Keep only the largest region of
!     elements connected through shared edges, and at a node where the
!     elements around it fall into more than one such group (a pinch),
!     keep only the largest group. Repeat until neither is left.
!     ------------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(SMMesh) :: mesh
         INTEGER      :: numberRemoved
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, PARAMETER :: MAX_PASSES = 1000
         TYPE(SMElementPtr), ALLOCATABLE :: elements(:)
         INTEGER           , ALLOCATABLE :: ids(:,:), nbr(:,:), start(:), list(:), fill(:), parent(:), regionSize(:)
         LOGICAL           , ALLOCATABLE :: removeIt(:)
         LOGICAL                         :: isPinch
         CLASS(FTObject)   , POINTER     :: obj => NULL()
         TYPE(SMElement)   , POINTER     :: e => NULL()
         INTEGER                         :: nE, maxNodeID, k, m, j, i, a, b, f, r, pass, largest, numRemovedNow, mNext
         INTEGER, EXTERNAL               :: Loop

         numberRemoved = 0
         DO pass = 1, MAX_PASSES
!
!           -------------------------------
!           Element nodes and node->element
!           -------------------------------
!
            nE = mesh % elements % COUNT()
            IF ( nE == 0 )     RETURN
            ALLOCATE( elements(nE), ids(4,nE), nbr(4,nE), parent(nE), regionSize(nE), removeIt(nE) )
            k = 0
            CALL mesh % elementsIterator % setToStart()
            DO WHILE ( .NOT.mesh % elementsIterator % isAtEnd() )
               obj => mesh % elementsIterator % object()
               CALL castToSMElement(obj,e)
               k = k + 1
               elements(k) % element => e
               DO m = 1, 4
                  ids(m,k) = e % nodes(m) % node % id
               END DO
               CALL mesh % elementsIterator % moveToNext()
            END DO
            maxNodeID = MAXVAL(ids)
            ALLOCATE( start(maxNodeID+1), list(4*nE), fill(maxNodeID) )
            start = 0
            DO k = 1, nE
               DO m = 1, 4
                  start(ids(m,k)+1) = start(ids(m,k)+1) + 1
               END DO
            END DO
            start(1) = 1
            DO j = 1, maxNodeID
               start(j+1) = start(j+1) + start(j)
            END DO
            fill = start(1:maxNodeID)
            DO k = 1, nE
               DO m = 1, 4
                  list(fill(ids(m,k))) = k
                  fill(ids(m,k))       = fill(ids(m,k)) + 1
               END DO
            END DO
!
!           ---------------------------------------------------
!           Neighbor across side m (nodes m, m+1), 0 if none
!           ---------------------------------------------------
!
            nbr = 0
            DO k = 1, nE
               DO m = 1, 4
                  a = ids(m,k)
                  b = ids(Loop(m+1,4),k)
                  DO j = start(a), start(a+1) - 1
                     f = list(j)
                     IF ( f == k )     CYCLE
                     IF ( ANY(ids(:,f) == b) )     nbr(m,k) = f
                  END DO
               END DO
            END DO
!
!           ------------------------------------------
!           Keep the largest edge-connected region only
!           ------------------------------------------
!
            DO k = 1, nE
               parent(k) = k
            END DO
            DO k = 1, nE
               DO m = 1, 4
                  IF ( nbr(m,k) > 0 )     CALL JoinRoots( k, nbr(m,k), parent )
               END DO
            END DO
            regionSize = 0
            DO k = 1, nE
               r = rootOf(k, parent)
               regionSize(r) = regionSize(r) + 1
            END DO
            largest = MAXLOC(regionSize, DIM = 1)
            DO k = 1, nE
               removeIt(k) = rootOf(k, parent) /= largest
            END DO
!
!           --------------------------------------------------------------
!           If it is one region, look for the first pinch node: group the
!           elements at the node by the sides through the node they share
!           --------------------------------------------------------------
!
            IF ( .NOT.ANY(removeIt) )     THEN
               DO i = 1, maxNodeID
                  IF ( start(i+1) - start(i) < 2 )     CYCLE
                  DO j = start(i), start(i+1) - 1
                     parent(list(j)) = list(j)
                  END DO
                  DO j = start(i), start(i+1) - 1
                     k = list(j)
                     DO m = 1, 4
                        IF ( nbr(m,k) == 0 )     CYCLE
                        mNext = Loop(m+1,4)
                        IF ( ids(m,k) /= i .AND. ids(mNext,k) /= i )     CYCLE
                        CALL JoinRoots( k, nbr(m,k), parent )
                     END DO
                  END DO
                  regionSize(list(start(i):start(i+1)-1)) = 0
                  DO j = start(i), start(i+1) - 1
                     r = rootOf(list(j), parent)
                     regionSize(r) = regionSize(r) + 1
                  END DO
                  largest = 0
                  DO j = start(i), start(i+1) - 1
                     r = rootOf(list(j), parent)
                     IF ( largest == 0 )     THEN
                        largest = r
                     ELSE IF ( regionSize(r) > regionSize(largest) )     THEN
                        largest = r
                     END IF
                  END DO
                  isPinch = .FALSE.
                  DO j = start(i), start(i+1) - 1
                     IF ( rootOf(list(j), parent) /= largest )     THEN
                        removeIt(list(j)) = .TRUE.
                        isPinch           = .TRUE.
                     END IF
                  END DO
                  IF ( isPinch )     EXIT
               END DO
            END IF
!
!           ------------------------------------------------------------
!           The nodes of a removed element are now on the boundary. Let
!           those that do not know it yet know which curve they are near
!           ------------------------------------------------------------
!
            numRemovedNow = 0
            DO k = 1, nE
               IF ( .NOT.removeIt(k) )     CYCLE
               e => elements(k) % element
               e % remove    = .TRUE.
               numRemovedNow = numRemovedNow + 1
               DO m = 1, 4
                  IF ( e % nodes(m) % node % bCurveChainID == UNDEFINED )     CYCLE
                  DO j = 1, 4
                     IF ( e % nodes(j) % node % bCurveChainID /= UNDEFINED )     CYCLE
                     e % nodes(j) % node % bCurveChainID = e % nodes(m) % node % bCurveChainID
                     e % nodes(j) % node % bCurveSide    = e % nodes(m) % node % bCurveSide
                  END DO
                  EXIT
               END DO
            END DO
            DEALLOCATE( elements, ids, nbr, parent, regionSize, removeIt, start, list, fill )

            IF ( numRemovedNow == 0 )     EXIT
            numberRemoved = numberRemoved + numRemovedNow
            CALL DoLazyDelete( mesh )
         END DO

      END SUBROUTINE RemoveDetachedElements
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION rootOf( k, parent )
         IMPLICIT NONE
         INTEGER :: k, parent(:)
         rootOf = k
         DO WHILE ( parent(rootOf) /= rootOf )
            rootOf = parent(rootOf)
         END DO
      END FUNCTION rootOf
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE JoinRoots( k, m, parent )
         IMPLICIT NONE
         INTEGER :: k, m, parent(:)
         INTEGER :: rk, rm
         rk = rootOf(k, parent)
         rm = rootOf(m, parent)
         IF ( rk < rm )     THEN
            parent(rm) = rk
         ELSE IF ( rm < rk )     THEN
            parent(rk) = rm
         END IF
      END SUBROUTINE JoinRoots
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CleanUpBoundaryEdges( list )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList), POINTER :: list

         CALL RemoveCloseElements( list )
         CALL RemoveBumpOuts( list )

      END SUBROUTINE CleanUpBoundaryEdges
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RemoveCloseElements( boundaryEdgeList )
         USE ProgramGlobals, ONLY:edgeLengthFactor
!
!     ----------------------------------------------------------------
!     Remove those elements that have edges that are too close to their
!     boundaries. This will expose new boundary edges, which will be
!     added to the newBoundaryEdge list and then to the boundary edge
!     after the elements have been removed.
!     ----------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList), POINTER :: boundaryEdgeList
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedList)        , POINTER :: newlyExposedBoundaryEdges  => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         TYPE (SMElement)           , POINTER :: e => NULL()
         TYPE (SMEdge)              , POINTER :: currentEdge, newBoundaryEdge, edge
         TYPE (SMNode)              , POINTER :: node1, node2
         CLASS(FTObject)            , POINTER :: obj
         INTEGER                              :: k, chainID, side
         REAL(KIND=RP)                        :: elementSize, x1(3), x2(3)
!
!        -------------------------------------------------
!        Keep a preliminary list of the new boundary edges
!        -------------------------------------------------
!
         ALLOCATE(newlyExposedBoundaryEdges)
         CALL newlyExposedBoundaryEdges % init()
!
!        ----------------------------------------
!        Step through the edges for this boundary
!        ----------------------------------------
!
         ALLOCATE(iterator)
         CALL iterator % initWithFTLinkedListClass(boundaryEdgeList)
         CALL iterator % setToStart()

         obj => iterator % object()
         CALL castToSMEdge(obj,currentEdge)
         chainID = currentEdge % nodes(1) % node % bCurveChainID
         side    = currentEdge % nodes(1) % node % bCurveSide

         DO WHILE ( .NOT.iterator % isAtEnd() )

            obj => iterator % object()
            CALL castToSMEdge(obj,currentEdge)

            IF( .NOT.currentEdge % remove )     THEN !Already did this one
!
!              ---------------------
!              Find the element size
!              ---------------------
!
               e           => currentEdge % elements(1) % element !Boundary edge only has one element association.
               elementSize = 0.0_RP
               DO k = 1, 4
                  edge        => edgesForElements(k,e%id) % edge
                  x1          = edge % nodes(1) % node % x
                  x2          = edge % nodes(2) % node % x
                  elementSize = MAX( elementSize, (x1(1) - x2(1))**2 + (x1(2) - x2(2))**2 )
               END DO
               elementSize = edgeLengthFactor*SQRT(elementSize)
!
!              -------------------------------------------------
!              Target element and edges for removal if any nodes
!              are too close.
!              -------------------------------------------------
!
               node1 => currentEdge % nodes(1) % node
               node2 => currentEdge % nodes(2) % node

               IF( node1 % distToBoundary < elementSize      .OR.  &
                   node2 % distToBoundary < elementSize )    THEN

                   e % remove = .true.
!
!                  -----------------------------------------------
!                  Target each of this element's edges for removal
!                  -----------------------------------------------
!
                   DO k = 1, 4
                      IF( ASSOCIATED(edgesForElements(k,e % id) % edge % elements(1) % element) .AND. &
                          ASSOCIATED(edgesForElements(k,e % id) % edge % elements(2) % element) )     THEN !Edge k is currently SHARED

                          IF( edgesForElements(k,e % id) % edge % elements(1) % element % remove .AND.  &
                              edgesForElements(k,e % id) % edge % elements(2) % element % remove )     THEN !Both elements are slated for removal

                              edgesForElements(k,e % id) % edge % remove = .true.

                          ELSE ! Temporarily save the edge

                              newBoundaryEdge => edgesForElements(k,e % id) % edge
                              obj             => newBoundaryEdge
                              CALL newlyExposedBoundaryEdges % add(obj)

                              edgesForElements(k,e % id) % edge % remove        = .false.
                              newboundaryEdge % nodes(1) % node % bCurveChainID = chainID
                              newboundaryEdge % nodes(2) % node % bCurveChainID = chainID
                              newboundaryEdge % nodes(1) % node % bCurveSide    = side
                              newboundaryEdge % nodes(2) % node % bCurveside    = side

                          END IF
                      ELSE
                         edgesForElements(k,e % id) % edge % remove = .true.
                      END IF

                   END DO
                END IF
            END IF

            CALL iterator % moveToNext()
         END DO !WHILE
!
!        ---------------------------
!        Add the newly exposed edges
!        ---------------------------
!
         IF(newlyExposedBoundaryEdges % COUNT() > 0)     THEN
            CALL boundaryEdgeList % makeCircular(.FALSE.)
            CALL boundaryEdgeList % addObjectsFromList(newlyExposedBoundaryEdges)
            CALL boundaryEdgeList % makeCircular(.TRUE.)
         END IF
!
!        --------------------------------------------------
!        Now actually delete the edges slated to be removed
!        from the edge list
!        --------------------------------------------------
!
         CALL removeMarkedEdges(iterator)
!
         CALL releaseFTLinkedListIteratorClass(iterator)
         CALL releaseFTLinkedListClass(newlyExposedBoundaryEdges)

      END SUBROUTINE RemoveCloseElements
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RemoveBumpOuts( boundaryEdgeList )
!
!     ----------------------------------------------------------------
!     A bumpout has three exterior edges
!     !BUG: This will crash if the exterior boundaries are different
!     TODO: Need to check that all three edges face the same boundary curve.
!     ----------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList) , POINTER :: boundaryEdgeList
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedList)        , POINTER      :: newlyExposedBoundaryEdges => NULL()
         TYPE (SMElement)           , POINTER      :: e => NULL()
         TYPE (SMEdge)              , POINTER      :: currentEdge => NULL(), newBoundaryEdge => NULL()
         CLASS(FTLinkedListIterator), POINTER      :: iterator => NULL()
         CLASS(FTObject)            , POINTER      :: obj => NULL()
         INTEGER                                   :: k, nB, interiorEdgeNumber
         CHARACTER(LEN=ERROR_EXCEPTION_MSG_LENGTH) :: msg
!
!        ---------------------------------------------
!        Keep a preliminary list of new boundary edges
!        ---------------------------------------------
!
         ALLOCATE(newlyExposedBoundaryEdges)
         CALL newlyExposedBoundaryEdges % init()

         ALLOCATE(iterator)
         CALL iterator % initWithFTLinkedListClass(boundaryEdgeList)
         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )

            obj => iterator % object()
            CALL castToSMEdge(obj,currentEdge)

            IF( .NOT.currentEdge % remove )     THEN ! Haven't done this one yet

               e           => currentEdge % elements(1) % element

               IF( .NOT.e % remove )     THEN
                  nB = 0
                  interiorEdgeNumber = -1
                  DO k = 1, 4
                     IF( edgesForElements(k,e % id) % edge % edgeType == ON_BOUNDARY )     THEN
                        nB = nB + 1
                     ELSE
                        interiorEdgeNumber = k
                     END IF
                  END DO
                  IF ( interiorEdgeNumber < 0 )     THEN
                     WRITE(msg,*) "InteriorEdgeNumber not found. All edges appear to be boundaries"
                     CALL ThrowErrorExceptionOfType("RemoveBumpOuts", msg, FT_ERROR_FATAL)
                     RETURN
                  END IF
!
!                 -----------------------------------------------------
!                 A bumpout has three exterior edges. Best to remove,
!                 unless they are not associated with the same boundary
!                 curve. TODO: Need to add this test!!!
!                 -----------------------------------------------------
!
                  IF( nB == 3 )     THEN
!
!                    ------------------------------------------------
!                    Lazily remove the element and its exterior edges
!                    ------------------------------------------------
!
                     e % remove = .true.
                     DO k = 1, 4
                        IF( k /= interiorEdgeNumber ) edgesForElements(k,e % id) % edge % remove = .true.
                     END DO
!
!                    -----------------------------------------------------------
!                    Make the remaining edge a boundary edge. Set its remaining
!                    element to the first position.
!                    -----------------------------------------------------------
!
                     newBoundaryEdge => edgesForElements(interiorEdgeNumber,e % id) % edge
                     IF( ASSOCIATED(newBoundaryEdge % elements(2) % element, e ) )    THEN
                        NULLIFY( newBoundaryEdge % elements(2) % element )
                     ELSE
                        newBoundaryEdge % elements(1) % element => newBoundaryEdge % elements(2) % element
                        NULLIFY( newBoundaryEdge % elements(2) % element )
                     END IF
!
!                    -------------------------------------------------
!                    Add the remaining element edge to the temp
!                    list Also, mark this edge now as a boundary edge.
!                    -------------------------------------------------
!
                     newBoundaryEdge % edgeType = ON_BOUNDARY
                     newBoundaryEdge % remove   = .false.

                     obj => newBoundaryEdge
                     CALL boundaryEdgeList % add(obj)

                  END IF
               END IF
            END IF

            CALL iterator % moveToNext()
         END DO
!
!        ---------------------------
!        Add the newly exposed edges
!        ---------------------------
!
         IF(newlyExposedBoundaryEdges % COUNT() > 0)     THEN
            CALL boundaryEdgeList % makeCircular(.FALSE.)
            CALL boundaryEdgeList % addObjectsFromList(newlyExposedBoundaryEdges)
            CALL boundaryEdgeList % makeCircular(.TRUE.)
         END IF
!
!        --------------------------------------------------
!        Now actually delete the edges slated to be removed
!        from the edge list
!        --------------------------------------------------
!
         CALL removeMarkedEdges(iterator)

         CALL releaseFTLinkedListIteratorClass(iterator)
         CALL releaseFTLinkedListClass(newlyExposedBoundaryEdges)

      END SUBROUTINE RemoveBumpOuts
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MoveInterfaceNodesToBoundary( boundaryEdgeList, model )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList), POINTER  :: boundaryEdgeList
         TYPE (SMModel)     , POINTER  :: model
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)            , POINTER :: obj         => NULL()
         TYPE (SMEdge)              , POINTER :: currentEdge => NULL()
         TYPE (SMNode)              , POINTER :: node        => NULL()
         CLASS(SMCurve)             , POINTER :: cEnd        => NULL()
         CLASS(SMChainedCurve)      , POINTER :: chain       => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator    => NULL()
         INTEGER                              :: k
         REAL(KIND=RP)                        :: t

         ALLOCATE(iterator)
         CALL iterator % initWithFTLinkedListClass(boundaryEdgeList)
         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )

            obj => iterator % object()
            CALL castToSMEdge(obj,currentEdge)

            DO k = 1, 2
               node                  => currentEdge % nodes(k) % node
               IF(node % distToBoundary == 0.0_RP)     CYCLE

               t                     =  node % whereOnBoundary
               cEnd                  => model % curveWithID(node % bCurveID, chain)
               node % x              =  cEnd % positionAt(t)
               node % distToBoundary =  0.0_RP
            END DO

            CALL iterator % moveToNext()
         END DO
         CALL releaseFTLinkedListIteratorClass(iterator)

      END SUBROUTINE MoveInterfaceNodesToBoundary
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE removeMarkedEdges(iterator)
         IMPLICIT NONE
!
!        ----------------------------------------------
!        Actually delete the edges slated to be removed
!        from the edge list
!        ----------------------------------------------
!
         CLASS(FTLinkedListIterator), POINTER :: iterator
         CLASS(FTobject)            , POINTER :: obj         => NULL()
         TYPE (SMEdge)              , POINTER :: currentEdge => NULL()
         LOGICAL                              :: takeStep

         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            takeStep = .TRUE.
            CALL castToSMEdge(obj,currentEdge)
            IF( currentEdge % remove )     THEN
                CALL iterator % removeCurrentRecord()
                takeStep = .FALSE.
            END IF
            IF(takeStep) CALL iterator % moveToNext()
         END DO

      END SUBROUTINE removeMarkedEdges
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE smoothBoundaryEdges
         USE MeshBoundaryMethodsModule
         IMPLICIT NONE
         INTEGER :: j
         CLASS(FTObject)    , POINTER :: obj  => NULL()
         CLASS(FTLinkedList), POINTER :: list => NULL()

         IF ( boundarySmoothingPasses > 0 )     THEN
            DO j = 1, SIZE(boundaryEdgesType)
               IF( boundaryEdgesType(j) == INTERFACE_EDGES )     CYCLE
               obj => boundaryEdgesArray % objectAtIndex(j)
               CALL cast(obj,list)
               CALL SmoothEdgeListNodes( list, boundarySmoothingPasses )
            END DO
         END IF

      END SUBROUTINE smoothBoundaryEdges

      END MODULE BoundaryEdgeCleaningModule

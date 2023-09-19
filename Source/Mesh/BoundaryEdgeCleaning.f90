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
         TYPE (FTLinkedListIterator), POINTER :: iterator => NULL()
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
         CALL iterator % initWithFTLinkedList(boundaryEdgeList)
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
         CALL releaseFTLinkedListIterator(iterator)
         CALL releaseFTLinkedList(newlyExposedBoundaryEdges)

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
         CLASS(SMElement)           , POINTER      :: e => NULL()
         TYPE (SMEdge)              , POINTER      :: currentEdge => NULL(), newBoundaryEdge => NULL()
         TYPE (FTLinkedListIterator), POINTER      :: iterator => NULL()
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
         CALL iterator % initWithFTLinkedList(boundaryEdgeList)
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

         CALL releaseFTLinkedListIterator(iterator)
         CALL releaseFTLinkedList(newlyExposedBoundaryEdges)

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
         TYPE (FTLinkedListIterator), POINTER :: iterator    => NULL()
         INTEGER                              :: k
         REAL(KIND=RP)                        :: t

         ALLOCATE(iterator)
         CALL iterator % initWithFTLinkedList(boundaryEdgeList)
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
         CALL releaseFTLinkedListIterator(iterator)

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
         TYPE (FTLinkedListIterator), POINTER :: iterator
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

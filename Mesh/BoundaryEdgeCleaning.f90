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
      USE ConectionsModule
      IMPLICIT NONE 
      CONTAINS 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CleanUpBoundaryCurves( mesh, model ) 
      IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(SMMesh)            :: mesh
         CLASS(SMModel), POINTER :: model
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
         CALL MakeNodeToElementConnections( mesh )
         
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
               CALL CleanUpBoundaryEdges( list, mesh, model )
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
               obj => boundaryEdgesArray % objectAtIndex(j)
               CALL cast(obj,list)
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
      SUBROUTINE CleanUpBoundaryEdges( list, mesh, model ) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(SMMesh)                 :: mesh
         CLASS(FTLinkedList), POINTER :: list
         CLASS(SMModel)     , POINTER :: model
         
         CALL RemoveCloseElements( list, model )
         CALL RemoveBumpOuts( list )
         
      END SUBROUTINE CleanUpBoundaryEdges
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RemoveCloseElements( boundaryEdgeList, model )
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
         CLASS(SMModel)     , POINTER :: model
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedList)        , POINTER :: newlyExposedBoundaryEdges  => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         CLASS(SMElement)           , POINTER :: e => NULL()
         CLASS(SMEdge)              , POINTER :: currentEdge, newBoundaryEdge, edge
         CLASS(SMNode)              , POINTER :: node1, node2
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
         CALL cast(obj,currentEdge)
         chainID = currentEdge % nodes(1) % node % bCurveChainID
         side    = currentEdge % nodes(1) % node % bCurveSide
         
         DO WHILE ( .NOT.iterator % isAtEnd() )
         
            obj => iterator % object()
            CALL cast(obj,currentEdge)
            
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
         CALL iterator % release()
         CALL newlyExposedBoundaryEdges % release()
         IF (iterator % isUnreferenced() )        DEALLOCATE(iterator)
         IF (newlyExposedBoundaryEdges % isUnreferenced()) DEALLOCATE(newlyExposedBoundaryEdges) 

      END SUBROUTINE RemoveCloseElements
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RemoveBumpOuts( boundaryEdgeList )
!
!     ----------------------------------------------------------------
!     A bumpout has three exterior edges
!     !BUG: This will crash if the exterior boundaries are different
!     Need to check that all three edges face the same boundary curve.
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
         CLASS(FTLinkedList)        , POINTER :: newlyExposedBoundaryEdges => NULL()
         CLASS(SMElement)           , POINTER :: e => NULL()
         CLASS(SMEdge)              , POINTER :: currentEdge => NULL(), newBoundaryEdge => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()
         INTEGER                              :: k, nB, interiorEdgeNumber
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
            CALL cast(obj,currentEdge)
            
            IF( .NOT.currentEdge % remove )     THEN ! Haven't done this one yet
            
               e           => currentEdge % elements(1) % element
               
               IF( .NOT.e % remove )     THEN
                  nB = 0
                  DO k = 1, 4 
                     IF( edgesForElements(k,e % id) % edge % edgeType == ON_BOUNDARY )     THEN
                        nB = nB + 1                     
                     ELSE
                        interiorEdgeNumber = k
                     END IF
                  END DO
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
         
         CALL iterator % release()
         CALL newlyExposedBoundaryEdges % release()
         IF (newlyExposedBoundaryEdges % isUnreferenced()) DEALLOCATE(newlyExposedBoundaryEdges) 
         IF (iterator % isUnreferenced())         DEALLOCATE(iterator)

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
         CLASS(SMModel)     , POINTER  :: model
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)            , POINTER :: obj         => NULL()
         CLASS(SMEdge)              , POINTER :: currentEdge => NULL()
         CLASS(SMNode)              , POINTER :: node        => NULL()
         CLASS(SMCurve)             , POINTER :: cEnd        => NULL()
         CLASS(SMChainedCurve)      , POINTER :: chain       => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator    => NULL()
         INTEGER                              :: k
         REAL(KIND=RP)                        :: t
         
         ALLOCATE(iterator)
         CALL iterator % initWithFTLinkedList(boundaryEdgeList)
         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
         
            obj => iterator % object()
            CALL cast(obj,currentEdge)
            
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
         CALL iterator % release()
         DEALLOCATE(iterator)
         
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
         CLASS(SMEdge)              , POINTER :: currentEdge => NULL()
         LOGICAL                              :: takeStep

         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            takeStep = .TRUE.
            CALL cast(obj,currentEdge)
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

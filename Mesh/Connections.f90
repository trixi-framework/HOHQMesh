!
!////////////////////////////////////////////////////////////////////////
!
!      Connections.f90
!      Created: September 12, 2013 3:59 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module ConectionsModule
         USE SMMeshObjectsModule
         USE SMMeshClass
         USE FTLinkedListClass
         IMPLICIT NONE 
!
!        ---------------------------------------------------------
!        Storage for connecting mesh objects to other mesh objects
!        All references are weak, meaning that these arrays do not
!        own (increase the reference count) of any objects they
!        reference.
!        ---------------------------------------------------------
!
         TYPE( SMEdgePtr    ), DIMENSION(:,:), ALLOCATABLE :: edgesForNodes
         TYPE( SMElementPtr ), DIMENSION(:,:), ALLOCATABLE :: elementsForNodes
         TYPE( SMEdgePtr    ), DIMENSION(:,:), ALLOCATABLE :: edgesForElements
         INTEGER             , DIMENSION(:)  , ALLOCATABLE :: numEdgesForNodes
         INTEGER             , DIMENSION(:)  , ALLOCATABLE :: numElementsForNode
!
!        ========
         CONTAINS
!        ========
! 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE makeElementToEdgeConnections( mesh )
      IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(SMMesh) :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMEdge)              , POINTER :: edge => NULL()
         CLASS(SMElement)           , POINTER :: e => NULL()
         INTEGER                              :: side, k, id, numElements
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()
!
!        -----------------------------------------------------
!        Make sure that the temporary array has been allocated
!        Note that an element has 4 edges.
!        -----------------------------------------------------
!
         CALL deallocateElementToEdgeConnections
         
         numElements = mesh % elements % COUNT()
         ALLOCATE( edgesForElements(4,numElements) )
         
         CALL renumberObjects( mesh , ELEMENTS)
         
         iterator => mesh % edgesIterator
         CALL iterator % setToStart()
         
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL cast(obj,edge)
            DO k = 1, 2 
               IF( ASSOCIATED(edge % elements(k) % element) )    THEN
                  e     => edge % elements(k) % element
                  side  =  edge % elementSide(k)
                  id    = e % id
                  edgesForElements(side,id) % edge => edge
               END IF
            END DO
           
            CALL iterator % moveToNext()
         END DO
         
      END SUBROUTINE makeElementToEdgeConnections
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE deallocateElementToEdgeConnections
         IMPLICIT NONE
         IF( ALLOCATED(edgesForElements)) DEALLOCATE(edgesForElements)
      END SUBROUTINE deallocateElementToEdgeConnections 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE makeNodeToElementConnections( mesh )
!
!     --------------------------------------------------------
!     Collect which elements are used by each node and store 
!     in the global temporary array *elementsForNodes*. TO
!     avoid a memory leak, be sure to deallocate the ALLOCATED
!     arrays.
!     --------------------------------------------------------
!
      IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(SMMesh) :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMNode)               , POINTER :: node => NULL()
         CLASS(SMElement)            , POINTER :: e => NULL()
         INTEGER                               :: k, id, numNodes
         CLASS(FTLinkedListIterator), POINTER  :: iterator => NULL()
         CLASS(FTObject)            , POINTER  :: obj => NULL()
!
!        -----------------------------------------------------
!        Make sure that the temporary array has been allocated
!        Note that the maximum valence of a node using the
!        grid based method is 8.
!        -----------------------------------------------------
!
         CALL deallocateNodeToElementConnections
         
         numNodes = mesh % nodes % COUNT()
         ALLOCATE( elementsForNodes(8,numNodes) )
         ALLOCATE( numElementsForNode(numNodes) )
         numElementsForNode = 0
         
         CALL renumberObjects(mesh,NODES)
         CALL renumberObjects(mesh,ELEMENTS)
         
         iterator => mesh % elementsIterator
         CALL iterator % setToStart()
         
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL cast(obj,e)
            DO k = 1, 4 
               obj  => e % nodes % objectAtIndex(k)
               CALL cast(obj,node)
               
               id   = node % id
               numElementsForNode(id) = numElementsForNode(id) + 1
               IF ( numElementsForNode(id) > 8 )     THEN
                  PRINT *, "Valence too high for node ",id
                  STOP
               END IF 
               
               elementsForNodes(numElementsForNode(id),id) % element => e
            END DO
           
            CALL iterator % moveToNext()
         END DO
         
      END SUBROUTINE makeNodeToElementConnections
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE deallocateNodeToElementConnections
         IMPLICIT NONE
         IF( ALLOCATED(numElementsForNode)) DEALLOCATE(numElementsForNode)
         IF( ALLOCATED(elementsForNodes  )) DEALLOCATE(elementsForNodes)
      END SUBROUTINE deallocateNodeToElementConnections 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE makeNodeToEdgeConnections( mesh )
!
!     --------------------------------------------------------
!     Create pointers from each node to each edge that uses
!     that node. Store in the temporary array edgesForNodes and
!     keep track of the number of edges for a given node with
!     numEdgesForNodes. The valence of a node can be determined
!     from this quantity.
!     --------------------------------------------------------
!
      IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(SMMesh) :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMNode)  , POINTER             :: node     => NULL()
         CLASS(SMEdge)  , POINTER             :: edge     => NULL()
         CLASS(FTObject), POINTER             :: obj      => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         INTEGER                              :: k, id, numNodes
         
!
!        -----------------------------------------------------
!        Make sure that the temporary array has been allocated
!        Note that the maximum valence of a node using the
!        grid based method is 8.
!        -----------------------------------------------------
!
         CALL deallocateNodeToEdgeConnections
         
         CALL renumberObjects(mesh,NODES)
         
         numNodes = mesh % nodes % COUNT()
         ALLOCATE( edgesForNodes(8,numNodes) )
         ALLOCATE( numEdgesForNodes(numNodes) )
         numEdgesForNodes = 0
         
         iterator => mesh % edgesIterator
         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL cast(obj,edge)
            DO k = 1, 2 
               node                                          => edge % nodes(k) % node
               id                                            =  node % id
               numEdgesForNodes(id)                          =  numEdgesForNodes(id) + 1
               edgesForNodes(numEdgesForNodes(id),id) % edge => edge
            END DO
            
            CALL iterator % moveToNext()
           
         END DO
         
      END SUBROUTINE makeNodeToEdgeConnections
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE deallocateNodeToEdgeConnections
         IMPLICIT NONE
         IF( ALLOCATED(numEdgesForNodes  )) DEALLOCATE(numEdgesForNodes)
         IF( ALLOCATED(edgesForNodes     )) DEALLOCATE(edgesForNodes)
      END SUBROUTINE deallocateNodeToEdgeConnections
      END MODULE ConectionsModule

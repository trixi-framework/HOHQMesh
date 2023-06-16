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
! HOHQMesh contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend,
!    https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
! * `fmin`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler,
!    Computer Methods for Mathematical Computations, 1977
! * `spline`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler,
!    Computer Methods for Mathematical Computations, 1977
! * `seval`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler,
!    Computer Methods for Mathematical Computations, 1977
!
! --- End License
!
!////////////////////////////////////////////////////////////////////////
!
!      Connections.f90
!      Created: September 12, 2013 3:59 PM
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
      Module ConnectionsModule
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

         INTEGER                             , PARAMETER   :: maxValence = 11
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
      SUBROUTINE makeNodeToElementConnections( mesh, errorCode )
      USE MeshOutputMethods, ONLY : WriteSkeletonToTecplot
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
         INTEGER      :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMNode)               , POINTER  :: node => NULL()
         CLASS(SMElement)            , POINTER  :: e    => NULL()
         INTEGER                                :: k, id, numNodes
         CLASS(FTLinkedListIterator), POINTER   :: iterator => NULL()
         CLASS(FTObject)            , POINTER   :: obj => NULL()
!
!        -----------------------------------------------------
!        Make sure that the temporary array has been allocated
!        Note that the maximum valence of a node using the
!        grid based method is set to 11.
!        -----------------------------------------------------
!
         errorCode = NONE
         CALL deallocateNodeToElementConnections

         numNodes = mesh % nodes % COUNT()
         ALLOCATE( elementsForNodes(maxValence,numNodes) )
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
               node => e % nodes(k) % node

               id   = node % id
               numElementsForNode(id) = numElementsForNode(id) + 1
               IF ( numElementsForNode(id) > maxValence )     THEN
                  IF(printMessage)     THEN
                     PRINT *, " "
                     PRINT *, "**************************************************************************"
                     PRINT *, "Valence ",numElementsForNode(id), " too high for node ",id, " x = ",node % x
                     PRINT *, "Plot the file 'DebugPlot.tec' to check on the mesh topology"
                     PRINT *, "**************************************************************************"
                     PRINT *, " "
                     CALL WriteSkeletonToTecplot( mesh, "DebugPlot.tec" )
                  END IF
                  errorCode = VALENCE_TOO_HIGH_ERROR_CODE
                  CALL deallocateNodeToElementConnections
                  RETURN
               END IF

               elementsForNodes(numElementsForNode(id),id) % element => e
            END DO
            IF(errorCode > A_OK_ERROR_CODE )     EXIT
            CALL iterator % moveToNext()
         END DO

         IF(errorCode > A_OK_ERROR_CODE ) CALL deallocateNodeToElementConnections

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
!        grid based method is maxValence.
!        -----------------------------------------------------
!
         CALL deallocateNodeToEdgeConnections

         CALL renumberObjects(mesh,NODES)

         numNodes = mesh % nodes % COUNT()
         ALLOCATE( edgesForNodes(maxValence,numNodes) )
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
      END MODULE ConnectionsModule

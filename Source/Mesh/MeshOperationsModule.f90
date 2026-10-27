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
!      MeshOperationsModule.f90
!      Created: August 21, 2013 1:08 PM
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
      Module MeshOperationsModule
      USE SMConstants
      USE QuadTreeGridClass
      USE SMMeshObjectsModule
      USE FTobjectClass
      USE FTLinkedListClass
      USE FTLinkedListIteratorClass
      USE SMMeshClass
      USE ProgramGlobals
      IMPLICIT NONE
!
!     ========
      CONTAINS
!     ========
!
!@mark -
!
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckMeshForDuplicateNodes( mesh )
      IMPLICIT NONE
      TYPE (SMMesh)  :: mesh
!
!     ---------------------------------------------------------------
!     This routine is intended for debugging purposes ONLY. It checks
!     the integrity of the mesh by looking for two nodes closer
!     than 1e-10 in each coordinate. The nodes are sorted by x
!     so that each node is only compared with the nodes that
!     follow it within that distance in x.
!     ---------------------------------------------------------------
!
      TYPE (SMNode)  , POINTER                     :: node => NULL()
      CLASS(FTObject), POINTER                     :: obj  => NULL()
      REAL(KIND=RP)  , DIMENSION(:,:), ALLOCATABLE :: xNodes
      INTEGER        , DIMENSION(:)  , ALLOCATABLE :: ids, levels, order
      REAL(KIND=RP)  , DIMENSION(3)                :: d
      INTEGER                                      :: numNodes, a, b, i, j
      LOGICAL                                      :: duplicateWasFound

      IF(printMessage) PRINT *, "Testing for duplicate nodes..."
!
!     ---------------------------------
!     Copy the node locations to arrays
!     ---------------------------------
!
      numNodes = mesh % nodes % COUNT()
      ALLOCATE( xNodes(3,numNodes), ids(numNodes), levels(numNodes), order(numNodes) )
      numNodes = 0
      CALL mesh % nodesIterator % setToStart()
      DO WHILE( .NOT.mesh % nodesIterator % isAtEnd())
         obj => mesh % nodesIterator % object()
         CALL castToSMNode(obj,node)
         numNodes           = numNodes + 1
         xNodes(:,numNodes) = node % x
         ids(numNodes)      = node % id
         levels(numNodes)   = node % level
         CALL mesh % nodesIterator % moveToNext()
      END DO
!
!     ---------------------------------------------------
!     Compare each node with the following ones in x that
!     are closer than the tolerance
!     ---------------------------------------------------
!
      CALL HeapSortIndexByKey( xNodes(1,1:numNodes), order(1:numNodes) )

      duplicateWasFound = .FALSE.
      DO a = 1, numNodes
         i = order(a)
         DO b = a + 1, numNodes
            j = order(b)
            IF( xNodes(1,j) - xNodes(1,i) >= 1.0E-10_RP )     EXIT
            d = ABS(xNodes(:,j) - xNodes(:,i))
            IF( MAXVAL(d) < 1.0E-10_RP )     THEN
               IF(printMessage) PRINT *, "Duplicate Node at ",xNodes(:,j), " with ids", ids(i), ids(j), &
                                         " and level ", levels(j)
               duplicateWasFound = .TRUE.
            END IF
         END DO
      END DO

      IF(printMessage)     THEN
         IF( .NOT.duplicateWasFound ) PRINT *, "No duplicate nodes found. Mesh OK"
         PRINT *, "Test complete."
      END IF
!
!     --------------------
!     Release local memory
!     --------------------
!
      DEALLOCATE( xNodes, ids, levels, order )

      END SUBROUTINE CheckMeshForDuplicateNodes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE HeapSortIndexByKey( key, idx )
         IMPLICIT NONE
!
!        --------------------------------------------------
!        Heap sort: on return key(idx(:)) is in ascending
!        order. The key array itself is not changed.
!        --------------------------------------------------
!
         REAL(KIND=RP), INTENT(IN)    :: key(:)
         INTEGER      , INTENT(INOUT) :: idx(:)
         INTEGER                      :: n, k, tmp

         n = SIZE(key)
         DO k = 1, n
            idx(k) = k
         END DO
         DO k = n/2, 1, -1
            CALL SiftDown(k, n, key, idx)
         END DO
         DO k = n, 2, -1
            tmp    = idx(1)
            idx(1) = idx(k)
            idx(k) = tmp
            CALL SiftDown(1, k-1, key, idx)
         END DO

      END SUBROUTINE HeapSortIndexByKey
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SiftDown( start, last, key, idx )
         IMPLICIT NONE
         INTEGER      , INTENT(IN)    :: start, last
         REAL(KIND=RP), INTENT(IN)    :: key(:)
         INTEGER      , INTENT(INOUT) :: idx(:)
!
         INTEGER :: root, child, tmp ! Local variables
!
         root = start
         DO WHILE ( 2*root <= last )
            child = 2*root
            IF ( child < last )     THEN
               IF ( key(idx(child)) < key(idx(child+1)) )     child = child + 1
            END IF
            IF ( key(idx(root)) < key(idx(child)) )     THEN
               tmp = idx(root)
               idx(root) = idx(child)
               idx(child) = tmp
               root = child
            ELSE
               RETURN
            END IF
         END DO
      END SUBROUTINE SiftDown

      END MODULE MeshOperationsModule

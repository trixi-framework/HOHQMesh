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
!     ------------------------------------------------------------
!     This routine is intended for debugging purposes ONLY because
!     it does a simple linear search through ALL of the nodes. It
!     is intended to check for the integrity of the mesh, but not
!     for use on grids of large size or anything else.
!     ------------------------------------------------------------
!
      CLASS(SMNode)            , POINTER :: node     => NULL()
      CLASS(FTObject)          , POINTER :: obj      => NULL()
      CLASS(FTLinkedListRecord), POINTER :: llRecord => NULL()
     
      REAL(KIND=RP), DIMENSION(3)    :: xTest, x, d
      INTEGER                        :: idTest, id
      LOGICAL                        :: duplicateWasFound
!      
      
      IF(printMessage) PRINT *, "Testing for duplicate nodes..."
      
      duplicateWasFound = .false.
      CALL mesh % nodesIterator % setToStart()
      DO WHILE( .NOT.mesh % nodesIterator % isAtEnd())
         obj => mesh % nodesIterator % object()
         CALL castToSMNode(obj,node)
         xTest = node % x; idTest = node % id
!
!        ----------------------------------
!        Now search through remaining nodes
!        ----------------------------------
!
         llRecord => mesh % nodesIterator % currentRecord()
         llRecord => llRecord % next
         DO WHILE( ASSOCIATED(llREcord) )
            obj => llRecord % recordObject
            CALL castToSMNode(obj,node)
            
            x    = node % x; id   = node % id
            d    = ABS(x-xTest)
            
            IF( MAXVAL(d) < 1.0d-10 )     THEN
               IF(printMessage) PRINT *, "Duplicate Node at ",x, " with ids", idTest, id, " and level ", node % level
               duplicateWasFound = .true.
            END IF
            
            llRecord => llrecord % next
         END DO  
         
         CALL mesh % nodesIterator % moveToNext() 
      END DO 
      
      IF(printMessage)     THEN 
         IF( .NOT.duplicateWasFound ) PRINT *, "No duplicate nodes found. Mesh OK"
         PRINT *, "Test complete."
      END IF 
      
      END SUBROUTINE CheckMeshForDuplicateNodes

      END MODULE MeshOperationsModule

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
!      QuadTreeGridGenerator.f90
!      Created: August 21, 2013 12:05 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   Module QuadTreeGridGeneratorModule
      USE MeshSizerClass
      USE QuadTreeGridClass
      USE QuadTreeTemplateOperations
      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      INTEGER, PARAMETER :: CONFORMING = 0, NON_CONFORMING = 1
!
!     ========
      CONTAINS
!     ========
!
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GenerateGridWithSizerAndType( grid, sizer, gridType )
!
!     -------------------------------------------------------
!     This is the main entry to generating a "grid".
!
!     grid     = root quad grid, already constructed by the project
!     sizer    = already constructed sizer object
!     gridType = either NON_CONFORMING or CONFORMING
!     
!     On return, grid has been refined according to the sizer
!     and grid type
!     -------------------------------------------------------
!
      IMPLICIT NONE
!
!      ---------
!      Arguments
!      ---------
!
      TYPE (QuadTreeGrid), POINTER :: grid
      CLASS(MeshSizer)             :: sizer
      INTEGER                      :: gridType
      
      IF ( gridType == NON_CONFORMING )     THEN
         CALL GenerateNCGridWithSizer( grid, sizer )
      ELSE
         CALL GenerateConformingGridWithSizer( grid, sizer )
      END IF

      END SUBROUTINE GenerateGridWithSizerAndType
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GenerateNCGridWithSizer( grid, sizer ) 
      IMPLICIT NONE 
!
!      ---------
!      Arguments
!      ---------
!
      TYPE (QuadTreeGrid), POINTER :: grid
      CLASS(MeshSizer)             :: sizer
      
      CALL generateNonconformingQuadTree(grid = grid,sizer = sizer)
      IF(catch() .AND. (maximumErrorSeverity() == FT_ERROR_FATAL)) RETURN 
      CALL ConstructQuads( grid )
      
      END SUBROUTINE GenerateNCGridWithSizer
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GenerateConformingGridWithSizer( grid, sizer ) 
      IMPLICIT NONE 
!
!      ---------
!      Arguments
!      ---------
!
      TYPE (QuadTreeGrid), POINTER :: grid
      CLASS(MeshSizer)             :: sizer
      
      CALL generateNonconformingQuadTree(grid = grid,sizer = sizer)
      IF(catch() .AND. (maximumErrorSeverity() == FT_ERROR_FATAL)) RETURN 
      CALL DoLevelOperation( grid, REMOVE_HANGING_NODES_OPERATION )
      CALL DeleteDuplicateNodesForGrid( grid )
      CALL ConstructQuadsWithTemplates( grid )
      
      END SUBROUTINE GenerateConformingGridWithSizer
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE generateNonconformingQuadTree(grid,sizer)  
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (QuadTreeGrid), POINTER :: grid
         CLASS(MeshSizer)             :: sizer
         INTEGER :: k
      
         CALL RefineGrid_ToSizeFunction_( grid, sizer )
         IF(catch() .AND. (maximumErrorSeverity() == FT_ERROR_FATAL)) RETURN 
         CALL DeleteDuplicateNodesForGrid( grid )
         
         IF( refinementType == REFINEMENT_2 )     THEN
!
!        -----------------------------------------------------------------------------------------------
!        TODO: The following is a hack to make sure that grids differ at most by one level. It is redone
!        to ensure that every jump of more than one level is removed. Maybe a better way of traversing
!        the tree would be more efficient. So far, this doesn't affect the cpu time much.
!        -----------------------------------------------------------------------------------------------
!
            DO k = 1, highestLevel-1
               CALL DoLevelOperation( grid, FLATTEN_NODE_LEVELS_OPERATION )
               CALL DeleteDuplicateNodesForGrid( grid )
            END DO
            CALL DeleteDuplicateNodesForGrid( grid )
         END IF
      
      END SUBROUTINE generateNonconformingQuadTree

   END MODULE QuadTreeGridGeneratorModule

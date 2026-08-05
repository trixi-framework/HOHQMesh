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
!      MeshSmoother.f90
!      Created: May 29, 2014 at 3:38 PM 
!      By: David Kopriva  
!
!      BASE CLASS FOR SMOOTHERS
!
!////////////////////////////////////////////////////////////////////////
!
      Module MeshSmootherClass
      USE SMMeshClass
      USE SMModelClass
      USE MeshBoundaryMethodsModule
      IMPLICIT NONE 
      
      TYPE :: MeshSmoother
!         
!        ========
         CONTAINS
!        ========
!         
         PROCEDURE  :: smoothMesh
         FINAL      :: destruct
      END TYPE MeshSmoother
!
!     ========
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destruct( self )
         IMPLICIT NONE
         TYPE (MeshSmoother)   :: self
      END SUBROUTINE destruct
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE smoothMesh( self, mesh, model, errorCode )
         IMPLICIT NONE
         CLASS (MeshSmoother)          :: self
         TYPE  (SMMesh)      , POINTER :: mesh
         TYPE  (SMModel)     , POINTER :: model
         INTEGER                       :: errorCode
      END SUBROUTINE smoothMesh
      
      END MODULE MeshSmootherClass

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
!      LibObjectAccessors.f90
!      Created: June 17, 2021 at 1:20 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module ProjectInterfaceAccessors 
      USE MeshProjectClass
      USE InteropUtilitiesModule
      USE ISO_C_BINDING
      IMPLICIT NONE
!
!     ________
      CONTAINS 
!     --------
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE HML_SetMeshFileName( cPtr, cFileName)  BIND(c)
         IMPLICIT NONE  
         TYPE(c_ptr)                          :: cPtr
         CHARACTER(KIND=c_char), DIMENSION(*) :: cFileName
         
         CHARACTER(len=:), ALLOCATABLE :: fFileName
         TYPE( MeshProject ), POINTER  :: project
         
         CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
         fFileName = c_to_f_string(c_string = cFileName )
         
         project % runParams % MeshFileName = fFileName
      END SUBROUTINE HML_SetMeshFileName
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION HML_MeshFileName(cPtr) RESULT(c_string)
         IMPLICIT NONE  
         TYPE(c_ptr)                                       :: cPtr
         TYPE( MeshProject )   , POINTER                   :: project
         CHARACTER(KIND=c_char), DIMENSION(:), ALLOCATABLE :: c_string
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH)           :: fileName
         
         CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
         fileName = project % runParams % MeshFileName
         c_string = f_to_c_string(fileName)
         
      END FUNCTION HML_MeshFileName
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE HML_SetMeshFileFormat( cPtr, cString)  
         IMPLICIT NONE  
         TYPE(c_ptr)                          :: cPtr
         CHARACTER(KIND=c_char), DIMENSION(*) :: cString
         
         CHARACTER(len=:), ALLOCATABLE :: fileFormat
         TYPE( MeshProject ), POINTER  :: project
         
         CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
         fileFormat = c_to_f_string(c_string = cString )
         
         SELECT CASE ( fileFormat )
            CASE( "ISM" ) 
               project % runParams % meshFileFormat = ISM
            CASE ("ISM-v2")
               project % runParams % meshFileFormat = ISM2
            CASE ("ISM_MM")
               project % runParams % meshFileFormat = ISM_MM
            CASE DEFAULT 
         END SELECT 
      END SUBROUTINE HML_SetMeshFileFormat
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION HML_MeshFileFormat(self)  RESULT(fileFormat)
         IMPLICIT NONE  
         CLASS (MeshProject)                       :: self
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH)   :: fileFormat
         
         SELECT CASE ( self % runParams % meshFileFormat )
            CASE( ISM ) 
               fileFormat = "ISM"
            CASE (ISM2)
               fileFormat = "ISM-v2"
            CASE (ISM_MM)
               fileFormat = "ISM-MM"
            CASE DEFAULT 
         END SELECT 
      END FUNCTION HML_MeshFileFormat

!//////////////////////////////////////////////////////////////////////// 
!
!> Getter for number of nodes in the mesh
!>
      INTEGER FUNCTION HML_NumberOfNodes(cPtr)   BIND(C)
         IMPLICIT NONE  
         TYPE(c_ptr)                  :: cPtr
         TYPE( MeshProject ), POINTER :: project
         
         CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
         HML_NumberOfNodes = project % mesh % nodes % count()
      END FUNCTION HML_NumberOfNodes
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Getter for number of elements in the mesh
!>
      INTEGER FUNCTION HML_NumberOfElements(cPtr)   BIND(C)
         IMPLICIT NONE  
         TYPE(c_ptr)                  :: cPtr
         TYPE( MeshProject ), POINTER :: project
         
         CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
         HML_NumberOfElements = project % mesh % elements % count()
      END FUNCTION HML_NumberOfElements
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Getter for number of edges in the mesh
!>
      INTEGER FUNCTION HML_NumberOfEdges(cPtr)   BIND(C)
         IMPLICIT NONE  
         TYPE(c_ptr)                  :: cPtr
         TYPE( MeshProject ), POINTER :: project
         
         CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
         HML_NumberOfEdges = project % mesh % edges % count()
      END FUNCTION HML_NumberOfEdges
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE HML_NodeLocations(cPtr, locationsArray, N)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(c_ptr)                  :: cPtr
         INTEGER                      :: N
         REAL(KIND=C_DOUBLE)          :: locationsArray(3,N)
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE( MeshProject ), POINTER :: project
         
      END SUBROUTINE HML_NodeLocations
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE HML_2DElementConnectivity(cPtr, connectivityArray, N)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(c_ptr)                  :: cPtr
         INTEGER                      :: N
         REAL(KIND=C_DOUBLE)          :: connectivityArray(4,N)
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE( MeshProject ), POINTER :: project
         
      END SUBROUTINE HML_2DElementConnectivity
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE HML_2DEdgeConnectivity(cPtr, connectivityArray, N)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(c_ptr)                  :: cPtr
         INTEGER                      :: N
         REAL(KIND=C_DOUBLE)          :: connectivityArray(2,N)
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE( MeshProject ), POINTER :: project
         
      END SUBROUTINE HML_2DEdgeConnectivity
   END Module ProjectInterfaceAccessors

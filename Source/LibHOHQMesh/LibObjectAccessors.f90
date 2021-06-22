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
      USE HMLConstants
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
      INTEGER(C_INT) FUNCTION DefaultCharacterLength() BIND(C)
         IMPLICIT NONE  
         DefaultCharacterLength = DEFAULT_CHARACTER_LENGTH
      END FUNCTION DefaultCharacterLength
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
      INTEGER(C_INT) FUNCTION HML_NumberOfNodes(cPtr)   BIND(C)
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
      INTEGER(C_INT) FUNCTION HML_NumberOfElements(cPtr)   BIND(C)
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
      INTEGER(C_INT) FUNCTION HML_NumberOfEdges(cPtr)   BIND(C)
         IMPLICIT NONE  
         TYPE(c_ptr)                  :: cPtr
         TYPE( MeshProject ), POINTER :: project
         
         CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
         HML_NumberOfEdges = project % mesh % edges % count()
      END FUNCTION HML_NumberOfEdges
!
!//////////////////////////////////////////////////////////////////////// 
! 
!> Gets the physical space node locations (x,y,z) for all the nodes
!>
      SUBROUTINE HML_NodeLocations(cPtr, locationsArray, N, errFlag)  BIND(C)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(c_ptr)          :: cPtr
         INTEGER(C_INT)       :: N
         REAL(KIND=C_DOUBLE)  :: locationsArray(3,N)
         INTEGER(C_INT)       :: errFlag
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE( MeshProject )        , POINTER :: project
         TYPE( SMMesh)              , POINTER :: mesh
         TYPE( FTLinkedListIterator), POINTER :: iterator
         CLASS(FTObject)            , POINTER :: obj
         CLASS(SMNode)              , POINTER :: node
         INTEGER                              :: j
!
!        ---------------
!        Check on errors
!        ---------------
!
         errFlag = HML_ERROR_NONE 
        
         CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
         mesh => project % mesh
         IF ( .NOT. ASSOCIATED(mesh) )     THEN
            errFlag = HML_ERROR_NO_OBJECT_FOR_REQUEST 
            RETURN 
         END IF 

         IF ( N < project % mesh % nodes % count() )     THEN
            errFlag = HML_ERROR_MEMORY_SIZE 
            RETURN 
         END IF 
!
!        ---------------------------
!        Gather the requested values
!        ---------------------------
!
         iterator => mesh % nodesIterator
         CALL iterator % setToStart()
         j = 1
         DO WHILE( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL cast(obj,node)
            locationsArray(:,j) = node % x
            CALL iterator % movetoNext()
            j = j + 1
         END DO 
         
      END SUBROUTINE HML_NodeLocations
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE HML_2DElementConnectivity(cPtr, connectivityArray, N, errFlag)    BIND(C)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(c_ptr)     :: cPtr
         INTEGER(C_INT)  :: N
         INTEGER(C_INT)  :: connectivityArray(4,N)
         INTEGER(C_INT)  :: errFlag
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE( MeshProject )        , POINTER :: project
         TYPE( SMMesh)              , POINTER :: mesh
         TYPE( FTLinkedListIterator), POINTER :: iterator
         CLASS(FTObject)            , POINTER :: obj
         CLASS(SMElement)           , POINTER :: e
         CLASS(SMNode)              , POINTER :: node
         INTEGER                              :: j,k
!
!        ---------------
!        Check on errors
!        ---------------
!
         errFlag = HML_ERROR_NONE 
        
         CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
         mesh => project % mesh
         IF ( .NOT. ASSOCIATED(mesh) )     THEN
            errFlag = HML_ERROR_NO_OBJECT_FOR_REQUEST 
            RETURN 
         END IF 

         IF ( N < project % mesh % elements % count() )     THEN
            errFlag = HML_ERROR_MEMORY_SIZE 
            RETURN 
         END IF 
!
!        ---------------------------
!        Gather the requested values
!        ---------------------------
!
         iterator => mesh % elementsIterator
         CALL iterator % setToStart()
         j = 1
         DO WHILE( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL cast(obj,e)
            DO k = 1, 4
               obj => e % nodes % objectAtIndex(k)
               CALL castToSMNode(obj, node)
               connectivityArray(k,j) = node % id
            END DO  
            CALL iterator % movetoNext()
            j = j + 1
         END DO 
         
      END SUBROUTINE HML_2DElementConnectivity
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE HML_2DEdgeConnectivity(cPtr, connectivityArray, N, errFlag)  BIND(C)
         USE MeshOutputMethods, ONLY:gatherEdgeInfo
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(c_ptr)     :: cPtr
         INTEGER(C_INT)  :: N
         INTEGER(C_INT)  :: connectivityArray(6,N)
         INTEGER(C_INT)  :: errFlag
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE( MeshProject )        , POINTER :: project
         TYPE( SMMesh)              , POINTER :: mesh
         TYPE( FTLinkedListIterator), POINTER :: iterator
         CLASS(FTObject)            , POINTER :: obj
         CLASS(SMEdge)              , POINTER :: e
         INTEGER                              :: j
         INTEGER                              :: edgeInfo(6)
!
!        ---------------
!        Check on errors
!        ---------------
!
         errFlag = HML_ERROR_NONE 
        
         CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
         mesh => project % mesh
         IF ( .NOT. ASSOCIATED(mesh) )     THEN
            errFlag = HML_ERROR_NO_OBJECT_FOR_REQUEST 
            RETURN 
         END IF 

         IF ( N < project % mesh % edges % count() )     THEN
            errFlag = HML_ERROR_MEMORY_SIZE 
            RETURN 
         END IF 
!
!        ---------------------------
!        Gather the requested values
!        ---------------------------
!
         iterator => mesh % edgesIterator
         CALL iterator % setToStart()
         j = 1
         DO WHILE( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL cast(obj,e)
            CALL gatherEdgeInfo(edge = e,info = edgeInfo)
            connectivityArray(:,j) = edgeInfo
            CALL iterator % movetoNext()
            j = j + 1
         END DO 
         
      END SUBROUTINE HML_2DEdgeConnectivity
   END Module ProjectInterfaceAccessors

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
!      INTEGER(C_INT) FUNCTION HML_DefaultCharacterLength() BIND(C)
!      INTEGER(C_INT) FUNCTION HML_BoundaryNameLength() BIND(C)
!      INTEGER(C_INT) FUNCTION HML_NumberOfNodes(cPtr, errFlag)   BIND(C)
!      INTEGER(C_INT) FUNCTION HML_NumberOfElements(cPtr, errFlag)   BIND(C)
!      INTEGER(C_INT) FUNCTION HML_NumberOfEdges(cPtr, errFlag)   BIND(C)
!
!      SUBROUTINE HML_SetMeshFileName( cPtr, cFileName, errFlag)  BIND(c)
!      SUBROUTINE HML_SetPlotFileName( cPtr, cFileName, errFlag)  BIND(c)
!      SUBROUTINE HML_MeshFileName(cPtr, nameAsCString, strBuf, errFlag) BIND(C)
!      SUBROUTINE HML_PlotFileName(cPtr, nameAsCString, strBuf, errFlag) BIND(C)
!      SUBROUTINE HML_SetMeshFileFormat( cPtr, cString, errFlag)     BIND(C)
!      SUBROUTINE HML_NodeLocations(cPtr, locationsArray, N, errFlag)  BIND(C)
!      SUBROUTINE HML_SetPolynomialOrder(cPtr, n, errFlag)  BIND(C)
!      SUBROUTINE HML_PolynomialOrder(cPtr, p, errFlag)    BIND(C) 
!      SUBROUTINE HML_2DElementConnectivity(cPtr, connectivityArray, N, errFlag)    BIND(C)
!      SUBROUTINE HML_2DElementBoundaryNames(cPtr, namesArray, N, errFlag)    BIND(C)
!      SUBROUTINE HML_2DElementBoundaryPoints(cPtr, boundaryPoints, p, N, errFlag)    BIND(C)
!      SUBROUTINE HML_2DElementEdgeFlag(cPtr, curveFlag, N, errFlag)    BIND(C)
!      SUBROUTINE HML_2DEdgeConnectivity(cPtr, connectivityArray, N, errFlag)  BIND(C)
!
!      SUBROUTINE HML_MeshFileFormat(cPtr, nameAsCString, strBuf, errFlag) BIND(C)
!      SUBROUTINE MeshCheck(project,N, errFlag)  
!
!////////////////////////////////////////////////////////////////////////
!
      Module ProjectInterfaceAccessors 
      USE MeshProjectClass
      USE InteropUtilitiesModule
      USE ProjectInterfaceActions
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
!> For string allocation, returns the allocated character length for fortran 
!> strings
!>
      INTEGER(C_INT) FUNCTION HML_DefaultCharacterLength() BIND(C)
         IMPLICIT NONE  
         HML_DefaultCharacterLength = DEFAULT_CHARACTER_LENGTH
      END FUNCTION HML_DefaultCharacterLength
!
!//////////////////////////////////////////////////////////////////////// 
! 
!> For string allocation, returns the allocated character length for the 
!> boundary name strings
!>
      INTEGER(C_INT) FUNCTION HML_BoundaryNameLength() BIND(C)
         IMPLICIT NONE  
         HML_BoundaryNameLength = LENGTH_OF_BC_STRING
      END FUNCTION HML_BoundaryNameLength
!
!//////////////////////////////////////////////////////////////////////// 
! 
!> Reset the path to the mesh file from that set by the control file
!>
      SUBROUTINE HML_SetMeshFileName( cPtr, cFileName, errFlag)  BIND(c)
         IMPLICIT NONE  
         TYPE(c_ptr)                          :: cPtr
         CHARACTER(KIND=c_char), DIMENSION(*) :: cFileName
         INTEGER(C_INT), INTENT(OUT)          :: errFlag
         
         CHARACTER(len=:), ALLOCATABLE :: fFileName
         TYPE( MeshProject ), POINTER  :: project
         
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         fFileName = c_to_f_string(c_string = cFileName )
         
         project % runParams % MeshFileName = fFileName
      END SUBROUTINE HML_SetMeshFileName
!
!//////////////////////////////////////////////////////////////////////// 
! 
!> Reset the path to the mesh file from that set by the control file
!>
      SUBROUTINE HML_SetPlotFileName( cPtr, cFileName, errFlag)  BIND(c)
         IMPLICIT NONE  
         TYPE(c_ptr)                          :: cPtr
         CHARACTER(KIND=c_char), DIMENSION(*) :: cFileName
         INTEGER(C_INT), INTENT(OUT)          :: errFlag
         
         CHARACTER(len=:), ALLOCATABLE :: fFileName
         TYPE( MeshProject ), POINTER  :: project
         
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         fFileName = c_to_f_string(c_string = cFileName )
         
         project % runParams % plotFileName = fFileName
      END SUBROUTINE HML_SetPlotFileName
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Get the current path to which the mesh file is to be written
!> On entry, strBuf is the allocated buffer size for the cString. 
!> On return, it is the actual length.
!> An error is returned if the string is truncated.
!>
      SUBROUTINE HML_MeshFileName(cPtr, nameAsCString, strBuf, errFlag) BIND(C)
         IMPLICIT NONE  
         TYPE(c_ptr)                                :: cPtr
         INTEGER(C_INT), INTENT(OUT)                :: errFlag
         INTEGER(C_INT), INTENT(INOUT)              :: strBuf ! Includes termination
         TYPE( MeshProject )   , POINTER            :: project
         CHARACTER(KIND=c_char), DIMENSION(strBuf)  :: nameAsCString
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH)    :: fileName
         INTEGER(C_INT)                             :: fNameLength
         
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         fileName = project % runParams % MeshFileName
         fNameLength = LEN_TRIM(fileName)
         IF( fNameLength > strBuf-1)     THEN
            errFlag = HML_ERROR_STRING_TRUNCATED
            strBuf = fNameLength
         END IF  
         
         CALL f_to_c_stringSub(fString = fileName,      &
                               cString = nameAsCString, &
                               cStrLen = strBuf)
         
      END SUBROUTINE HML_MeshFileName
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Get the current path to which the mesh file is to be written
!> On entry, strBuf is the allocated buffer size for the cString. 
!> On return, it is the actual length.
!> An error is returned if the string is truncated.
!>
      SUBROUTINE HML_PlotFileName(cPtr, nameAsCString, strBuf, errFlag) BIND(C)
         IMPLICIT NONE  
         TYPE(c_ptr)                                :: cPtr
         INTEGER(C_INT), INTENT(OUT)                :: errFlag
         INTEGER(C_INT), INTENT(INOUT)              :: strBuf ! Includes termination
         TYPE( MeshProject )   , POINTER            :: project
         CHARACTER(KIND=c_char), DIMENSION(strBuf)  :: nameAsCString
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH)    :: fileName
         INTEGER(C_INT)                             :: fNameLength
         
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         fileName = project % runParams % plotFileName
         fNameLength = LEN_TRIM(fileName)
         IF( fNameLength > strBuf-1)     THEN
            errFlag = HML_ERROR_STRING_TRUNCATED
            strBuf = fNameLength
         END IF  
         
         CALL f_to_c_stringSub(fString = fileName,      &
                               cString = nameAsCString, &
                               cStrLen = strBuf)
         
      END SUBROUTINE HML_PlotFileName
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Reset the mesh file format, "ISM", "ISM-v2" or "ISM_MM" from that
!> set in the control file.
!>
      SUBROUTINE HML_SetMeshFileFormat( cPtr, cString, errFlag)     BIND(C)
         IMPLICIT NONE  
         TYPE(c_ptr)                          :: cPtr
         CHARACTER(KIND=c_char), DIMENSION(*) :: cString
         INTEGER(C_INT), INTENT(OUT)          :: errFlag
         
         CHARACTER(len=:), ALLOCATABLE :: fileFormat
         TYPE( MeshProject ), POINTER  :: project
         
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         fileFormat = c_to_f_string(c_string = cString )
         
         SELECT CASE ( fileFormat )
            CASE( "ISM" ) 
               project % runParams % meshFileFormat = ISM
            CASE ("ISM-v2")
               project % runParams % meshFileFormat = ISM2
            CASE ("ISM_MM")
               project % runParams % meshFileFormat = ISM_MM
            CASE ("ABAQUS")
               project % runParams % meshFileFormat = ABAQUS
            CASE DEFAULT 
         END SELECT 
      END SUBROUTINE HML_SetMeshFileFormat
!
!//////////////////////////////////////////////////////////////////////// 
! 
!> Get the mesh file format the project will use to write a mesh file
!>
!      FUNCTION HML_MeshFileFormat(self) RESULT(fileFormat)
!         IMPLICIT NONE  
!         CLASS (MeshProject)                       :: self
!         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH)   :: fileFormat
!         
!         SELECT CASE ( self % runParams % meshFileFormat )
!            CASE( ISM ) 
!               fileFormat = "ISM"
!            CASE (ISM2)
!               fileFormat = "ISM-v2"
!            CASE (ISM_MM)
!               fileFormat = "ISM-MM"
!            CASE (ABAQUS)
!               fileFormat = "ABAQUS"
!            CASE DEFAULT 
!         END SELECT 
!      END FUNCTION HML_MeshFileFormat
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Get the mesh file format the project will use to write a mesh file
!> On entry, strBuf is the allocated buffer size for the cString. 
!> On return, it is the actual length.
!> An error is returned if the string is truncated.
!>
      SUBROUTINE HML_MeshFileFormat(cPtr, nameAsCString, strBuf, errFlag) BIND(C)
         IMPLICIT NONE  
         TYPE(c_ptr)                                :: cPtr
         INTEGER(C_INT), INTENT(OUT)                :: errFlag
         INTEGER(C_INT), INTENT(INOUT)              :: strBuf ! Includes termination
         TYPE( MeshProject )   , POINTER            :: project
         CHARACTER(KIND=c_char), DIMENSION(strBuf)  :: nameAsCString
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH)    :: fileFormat
         INTEGER(C_INT)                             :: fFormatLength
         
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         SELECT CASE ( project % runParams % meshFileFormat )
            CASE( ISM ) 
               fileFormat = "ISM"
            CASE (ISM2)
               fileFormat = "ISM-v2"
            CASE (ISM_MM)
               fileFormat = "ISM-MM"
            CASE (ABAQUS)
               fileFormat = "ABAQUS"
            CASE DEFAULT 
         END SELECT 
         
         fFormatLength = LEN_TRIM(fileFormat)
         IF( fFormatLength > strBuf-1)     THEN
            errFlag = HML_ERROR_STRING_TRUNCATED
            strBuf = fFormatLength
         END IF  
         
         CALL f_to_c_stringSub(fString = fileFormat,      &
                               cString = nameAsCString, &
                               cStrLen = strBuf)
         
      END SUBROUTINE HML_MeshFileFormat

!//////////////////////////////////////////////////////////////////////// 
!
!> Get the number of nodes in the mesh
!>
      INTEGER(C_INT) FUNCTION HML_NumberOfNodes(cPtr, errFlag)   BIND(C)
         IMPLICIT NONE  
         TYPE(c_ptr)                  :: cPtr
         TYPE( MeshProject ), POINTER :: project
         INTEGER(C_INT), INTENT(OUT)  :: errFlag
         
         HML_NumberOfNodes = NONE
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         HML_NumberOfNodes = project % mesh % nodes % count()
      END FUNCTION HML_NumberOfNodes
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Get the number of elements in the mesh
!>
      INTEGER(C_INT) FUNCTION HML_NumberOfElements(cPtr, errFlag)   BIND(C)
         IMPLICIT NONE  
         TYPE(c_ptr)                  :: cPtr
         TYPE( MeshProject ), POINTER :: project
         INTEGER(C_INT), INTENT(OUT)  :: errFlag
         
         HML_NumberOfElements = NONE
         errFlag              = HML_ERROR_NONE
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         HML_NumberOfElements = project % mesh % elements % count()
      END FUNCTION HML_NumberOfElements
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Get the number of edges in the mesh
!>
      INTEGER(C_INT) FUNCTION HML_NumberOfEdges(cPtr, errFlag)   BIND(C)
         IMPLICIT NONE  
         TYPE(c_ptr)                  :: cPtr
         INTEGER(C_INT), INTENT(OUT)  :: errFlag
         TYPE( MeshProject ), POINTER :: project
         
         HML_NumberOfEdges = NONE 
         errFlag           = HML_ERROR_NONE
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         HML_NumberOfEdges = project % mesh % edges % count()
      END FUNCTION HML_NumberOfEdges
!
!//////////////////////////////////////////////////////////////////////// 
! 
!> Fill an array of dimension (3,numberOfNodes) with the physical space node locations (x,y,z)
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
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
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
!> Override the polynomial order specified in the control file. If
!> the mesh has already been generated, then re-compute the boundary
!> information where it is used.
!>
      SUBROUTINE HML_SetPolynomialOrder(cPtr, n, errFlag)  BIND(C)
         USE MeshGenerationMethods, ONLY:CompleteElementConstruction
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(c_ptr)    :: cPtr
         INTEGER(C_INT) :: n
         INTEGER(C_INT) :: errFlag
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE( MeshProject ) , POINTER :: project
         CLASS( MeshProject ), POINTER :: projAsClass
         TYPE( SMMesh)       , POINTER :: mesh
          
         
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         projAsClass => project
         IF ( .NOT. IsMeshProjectPtr(projAsClass) )     THEN
               errFlag = HML_ERROR_NOT_A_PROJECT
               RETURN 
         END IF

         IF ( N < project % mesh % elements % count() )     THEN
            errFlag = HML_ERROR_MEMORY_SIZE 
            RETURN 
         END IF 
         
         IF ( n /= project % runParams % polynomialOrder )     THEN
            mesh => project % mesh
            IF ( .NOT. ASSOCIATED(mesh) )     THEN
               project % runParams % polynomialOrder = n 
               RETURN 
            END IF
            IF ( project % meshIsGenerated )     THEN
               project % runParams % polynomialOrder = n 
               CALL CompleteElementConstruction(project)
            END IF 
         END IF 
          
      END SUBROUTINE HML_SetPolynomialOrder
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE HML_PolynomialOrder(cPtr, p, errFlag)   BIND(C)  
         IMPLICIT NONE  
         TYPE(c_ptr)                  :: cPtr
         INTEGER(C_INT)               :: p, errFlag
         TYPE( MeshProject ), POINTER :: project
         
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         p = project % runParams % polynomialOrder
         
      END SUBROUTINE HML_PolynomialOrder
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Fill an array, dimension (4,N), for a 2D quad element with the node IDs 
!> for the four corners for each of the N elements
!>
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
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         CALL MeshCheck(project,N ,errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         mesh => project % mesh
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
!> Fill an array, dimension (LENGTH_OF_BC_STRING+1,4,N),e  for a 2D quad element with the 
!> names of the boundaries a side is on. Interior edges have boundary names = "---"
!>
      SUBROUTINE HML_2DElementBoundaryNames(cPtr, namesArray, N, errFlag)    BIND(C)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(c_ptr)       :: cPtr
         INTEGER(C_INT)    :: N
         CHARACTER(KIND=c_char)  :: namesArray(LENGTH_OF_BC_STRING+1,4,N)
         INTEGER(C_INT)    :: errFlag
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
         INTEGER                              :: i, j, k
         INTEGER                              :: strLen
         CHARACTER(LEN=LENGTH_OF_BC_STRING)   :: bcString
!
!        ---------------
!        Check on errors
!        ---------------
!
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         CALL MeshCheck(project,N ,errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         mesh => project % mesh
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
               bcString = e % boundaryInfo % bCurveName(k)
               strLen   = LEN_TRIM(bcString)
               DO i = 1, strLen
                  namesArray(i,k,j) = bcString(i:i) 
               END DO 
               namesArray(strLen+1,k,j)  = c_null_char
            END DO 
            
            CALL iterator % movetoNext()
            j = j + 1
         END DO 
         
      END SUBROUTINE HML_2DElementBoundaryNames
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Fill an array, dimension (3,p+1,4,N), for a 2D quad element, where 
!> N is the number of elements and p is the polynomial order.
!>
      SUBROUTINE HML_2DElementBoundaryPoints(cPtr, boundaryPoints, p, N, errFlag)    BIND(C)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(c_ptr)       :: cPtr
         INTEGER(C_INT)    :: N, p
         REAL(C_DOUBLE)    :: boundaryPoints(3,p+1,4,N)
         INTEGER(C_INT)    :: errFlag
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
         INTEGER                              :: j, k, nE
!
!        ---------------
!        Check on errors
!        ---------------
!
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         CALL MeshCheck(project,N ,errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         mesh => project % mesh
!
!        ---------------------------
!        Gather the requested values
!        ---------------------------
!
         iterator => mesh % elementsIterator
         CALL iterator % setToStart()
         nE = 1
         DO WHILE( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL cast(obj,e)
            
            DO k = 1, 4
               DO j = 0, p 
                  boundaryPoints(:,j+1,k,nE) =  e % boundaryInfo % x(:,j,k)
               END DO
            END DO
            
            CALL iterator % movetoNext()
            nE = nE + 1
         END DO 
         
      END SUBROUTINE HML_2DElementBoundaryPoints
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Fill an array, dimension (4,N), for a 2D quad element, where 
!> N is the number of elements. The value (k,e) is either 
!>    0  if side k of element e is a straight line OR
!>    1  if side k of element e is defined by some other curve
!>
      SUBROUTINE HML_2DElementEdgeFlag(cPtr, curveFlag, N, errFlag)    BIND(C)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(c_ptr)       :: cPtr
         INTEGER(C_INT)    :: N
         INTEGER(C_INT)    :: curveFlag(4,N)
         INTEGER(C_INT)    :: errFlag
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
         INTEGER                              :: k, nE
!
!        ---------------
!        Check on errors
!        ---------------
!
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         CALL MeshCheck(project,N ,errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         mesh => project % mesh
!
!        ---------------------------
!        Gather the requested values
!        ---------------------------
!
         iterator => mesh % elementsIterator
         CALL iterator % setToStart()
         nE = 1
         DO WHILE( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL cast(obj,e)
            
            DO k = 1, 4
               curveFlag(k,nE) =  e % boundaryInfo % bCurveFlag(k)
            END DO
            
            CALL iterator % movetoNext()
            nE = nE + 1
         END DO 
         
      END SUBROUTINE HML_2DElementEdgeFlag
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Fill an array of dimension (6,numberOfEdges) with the connectivity 
!> needed for the ISM-v2 mesh file format. For edge j,
!>            connectivityArray(1,j) = start node id
!>            connectivityArray(2,j) = end node id
!>            connectivityArray(3,j) = left element id
!>            connectivityArray(4,j) = right element id (or 0, if a boundary edge)
!>            connectivityArray(5,j) = element side for left element
!>            connectivityArray(6,j) = element side for right element signed for direction (or 0 for boundary edge)
!> 
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
         CALL ptrToProject(cPtr = cPtr, proj = project, errFlag = errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         
         CALL MeshCheck(project,N ,errFlag)
         IF(errFlag /= HML_ERROR_NONE)     RETURN 
         mesh => project % mesh
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
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE MeshCheck(project,N, errFlag)  
         IMPLICIT NONE  
         TYPE( MeshProject ), POINTER :: project
         TYPE( SMMesh)      , POINTER :: mesh
         INTEGER(C_INT)               :: N
         INTEGER(C_INT)               :: errFlag
         
         mesh => project % mesh
         IF ( .NOT. ASSOCIATED(mesh) )     THEN
            errFlag = HML_ERROR_NO_OBJECT_FOR_REQUEST 
            RETURN 
         END IF 

         IF ( N < project % mesh % elements % count() )     THEN
            errFlag = HML_ERROR_MEMORY_SIZE 
            RETURN 
         END IF 
      END SUBROUTINE MeshCheck
      
   END Module ProjectInterfaceAccessors

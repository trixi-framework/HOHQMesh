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
!      ProjectFront.f90
!      Created: June 15, 2021 at 10:53 AM 
!      By: David Kopriva  
!
!      Front end interface to a MeshProject
!
!////////////////////////////////////////////////////////////////////////
!
! How should errors be reported? Translate to error codes? String messages?
!
!>
!> This module provides the interfaces to the MeshProject objects
!>
!
!   FUNCTION   HML_AllocProject() BIND(C) RESULT(cPtr)
!   SUBROUTINE HML_ReleaseProject(cPtr, errFlag)   BIND(C)
!   SUBROUTINE HML_InitWithControlFile(cPtr, cFileName, errFlag) BIND(C)
!   SUBROUTINE HML_InitWithDictionary(cPtr, cPtrToDict, errFlag) BIND(C)
!   SUBROUTINE HML_GenerateMesh(cPtr, errFlag)   BIND(C)
!   SUBROUTINE HML_WriteMesh(cPtr, errFlag)   BIND(C)
!   SUBROUTINE HML_WritePlotFile(cPtr, errFlag)   BIND(C)
!
!   SUBROUTINE StopOnError(errorFlag)  
!   LOGICAL FUNCTION IsMeshProjectPtr(ptr)
!   LOGICAL FUNCTION CptrIsProjectPtr(cPtr)  
!   SUBROUTINE ptrToProject(cPtr, proj, errFlag)  
!
   Module ProjectInterfaceActions
   USE MeshProjectClass
   USE FTValueDictionaryClass
   USE ControlFileReaderClass
   USE HOHQMeshModule
   USE SharedExceptionManagerModule
   USE MeshController3D
   USE InteropUtilitiesModule
   USE HMLConstants
   USE ContainerInterfaceActions
   USE ISO_C_BINDING
   
   IMPLICIT NONE
!
!  --------
   CONTAINS
!  --------
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Function that returns a c_ptr pointer to a new project
!>
   FUNCTION HML_AllocProject() BIND(C) RESULT(cPtr)
      IMPLICIT NONE
      TYPE( MeshProject ), POINTER :: proj
      TYPE(c_ptr)                  :: cPtr
      ALLOCATE(proj)
      CALL proj % FTObject % init()
      cPtr = C_LOC(proj)
   END FUNCTION HML_AllocProject
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Call to close out/kill a MeshProject and release all its memory
!> CloseProject returns an error flag to tell whether or not the operation
!> is successful. 
!>
   SUBROUTINE HML_ReleaseProject(cPtr, errFlag)   BIND(C)
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)                   :: cPtr
      INTEGER(C_INT), INTENT(OUT)   :: errFlag
!
!     ---------------
!     Local Variables
!     ---------------
!
      TYPE  ( MeshProject ), POINTER :: proj
      CLASS ( MeshProject ), POINTER :: projAsClass
      INTEGER                        :: rc
      
      CALL ptrToProject(cPtr = cPtr, proj = proj, errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
      projAsClass => proj
      
      rc = proj % refCount()
      IF ( rc > 1 )     THEN
         errFlag = HML_ERROR_MULTIPLE_REFERENCES 
      ELSE 
         CALL releaseMeshProject(projAsClass)
         IF(ASSOCIATED(projAsClass)) errFlag = HML_ERROR_DEALLOCATION
         cPtr = c_null_ptr
      END IF 

   END SUBROUTINE HML_ReleaseProject

!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION IsMeshProjectPtr(ptr)
         IMPLICIT NONE  
         CLASS ( MeshProject ), POINTER :: ptr
         SELECT TYPE (p => ptr)
            TYPE IS(MeshProject)
               IsMeshProjectPtr = .TRUE.
            CLASS DEFAULT
               IsMeshProjectPtr = .FALSE.
         END SELECT
      END FUNCTION IsMeshProjectPtr
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION CptrIsProjectPtr(cPtr)  
         IMPLICIT NONE
         TYPE(c_ptr) :: cPtr
!
         TYPE( MeshProject )  , POINTER     :: project
         CLASS ( MeshProject ), POINTER     :: projAsClass
         
         CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
         
         projAsClass      => project
         CptrIsProjectPtr = .FALSE.
         
         IF ( IsMeshProjectPtr(projAsClass) )     THEN
               CptrIsProjectPtr = .TRUE.
         END IF 

      END FUNCTION CptrIsProjectPtr
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE ptrToProject(cPtr, proj, errFlag)  
      IMPLICIT NONE  
      TYPE(c_ptr)                    :: cPtr
      TYPE( MeshProject )  , POINTER :: proj
      CLASS ( MeshProject ), POINTER :: projAsClass
      INTEGER(C_INT)                 :: errFlag
      
      errFlag     = HML_ERROR_NONE
      IF ( .NOT. C_ASSOCIATED(cPtr) )     THEN
         errFlag = HML_ERROR_NULL_POINTER
         return 
      END IF 
      
      CALL C_F_POINTER(cPtr = cPtr, FPTR = proj)
      projAsClass => proj
      IF ( .NOT. IsMeshProjectPtr(projAsClass) )     THEN
            errFlag = HML_ERROR_NOT_A_PROJECT
            proj => NULL()
      END IF 
      
   END SUBROUTINE ptrToProject
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Initializer given a control file name
!>
   SUBROUTINE HML_InitWithControlFile(cPtr, cFileName, errFlag) BIND(C)
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)                          :: cPtr
      CHARACTER(KIND=c_char), DIMENSION(*) :: cFileName
      INTEGER(C_INT)                       :: errFlag
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE(c_ptr)                           :: cPtrToProjectDict
      TYPE( MeshProject )     , POINTER     :: proj
      TYPE (FTValueDictionary), POINTER     :: projectDict => NULL()
      CLASS(FTObject)         , POINTER     :: obj
      TYPE (FTValueDictionary), POINTER     :: modelDict, controlDict
      CHARACTER(len=:)        , ALLOCATABLE :: fFileName
      
      CALL ptrToProject(cPtr = cPtr, proj = proj, errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
      
      fFileName = c_to_f_string(c_string = cFileName )
      CALL ReadControlFile(fFileName, projectDict)
      cPtrToProjectDict = C_LOC(projectdict)
      CALL HML_InitWithDictionary(cPtr = cPtr,cPtrToDict = cPtrToProjectDict,errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
    
   END SUBROUTINE HML_InitWithControlFile
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Initializer given a control file dictionary
!>
   SUBROUTINE HML_InitWithDictionary(cPtr, cPtrToDict, errFlag) BIND(C)
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)      :: cPtr       ! Project to initialize
      TYPE(c_ptr)      :: cPtrToDict !Dictionary to initialize with
      INTEGER(C_INT)   :: errFlag
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE( MeshProject )     , POINTER     :: proj
      TYPE (FTValueDictionary), POINTER     :: projectDict => NULL()
      CLASS(FTObject)         , POINTER     :: obj
      TYPE (FTValueDictionary), POINTER     :: modelDict, controlDict
      
      CALL ptrToProject(cPtr = cPtr, proj = proj, errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
      CALL ptrToDictionary(cPtr = cPtrToDict,dict = projectDict,errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
      
      CALL proj % initWithDictionary( projectDict )
      CALL trapExceptions !Abort on fatal exceptions   
      
      IF ( proj % shouldGenerate3DMesh )     THEN
         obj            => projectDict % objectForKey(key = "MODEL")
         modelDict      => valueDictionaryFromObject(obj)
         CALL modelDict % retain()
         CALL Check3DMeshParametersIntegrity(controlDict, modelDict) 
         CALL releaseFTValueDictionary(modelDict)
      END IF 
      
      CALL trapExceptions !Abort on fatal exceptions
    
   END SUBROUTINE HML_InitWithDictionary
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Causes the mesh to be generated
!>
   SUBROUTINE HML_GenerateMesh(cPtr, errFlag)   BIND(C)
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)    :: cPtr
      INTEGER(C_INT) :: errFlag
!
!     ---------------
!     Local Variables
!     ---------------
!
      TYPE ( MeshProject )    , POINTER :: project
      CLASS ( MeshProject )   , POINTER :: projAsClass
!
!     -----
!     Other
!     -----
!         
      INTEGER                           :: errorCode

      CALL ptrToProject(cPtr = cPtr,proj = project,errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
      projAsClass => project
!
!     -----------------
!     Generate the mesh
!     -----------------
!
      CALL GenerateQuadMesh(projAsClass, errorCode)
      CALL trapExceptions !Abort on fatal exceptions
!
      CALL CheckMeshForDuplicateNodes(projAsClass % mesh)
!
!     -----------------------------------------------
!     Perform transformations on 2D mesh if requested
!     -----------------------------------------------
!
      CALL Perform2DMeshTransformations(projAsClass)
!
!     -----------------------------------------
!     Generate a 3D Extrusion mesh if requested
!     -----------------------------------------
!         
      IF ( project % shouldGenerate3DMesh )     THEN
         CALL generate3DMesh(project = projAsClass)
      END IF 
 
   END SUBROUTINE HML_GenerateMesh
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Writes the mesh file
!>
   SUBROUTINE HML_WriteMesh(cPtr, errFlag)   BIND(C)
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)                                     :: cPtr
      INTEGER(C_INT)                                  :: errFlag
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE( MeshProject )  , POINTER     :: project
      CLASS ( MeshProject ), POINTER     :: projAsClass
      
      CALL ptrToProject(cPtr = cPtr,proj = project,errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
      projAsClass => project
      
      CALL WriteMeshFile(projAsClass, projAsClass % shouldGenerate3DMesh)

   END SUBROUTINE HML_WriteMesh
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Writes a tecplot file
!>
   SUBROUTINE HML_WritePlotFile(cPtr, errFlag)   BIND(C)
      IMPLICIT NONE  
      TYPE(c_ptr)                    :: cPtr
      INTEGER(C_INT)                 :: errFlag
      TYPE( MeshProject )  , POINTER :: project
      CLASS ( MeshProject ), POINTER :: projAsClass
      
      CALL ptrToProject(cPtr = cPtr,proj = project,errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
      projAsClass => project
      
      CALL WritePlotFile(projAsClass, projAsClass % shouldGenerate3DMesh)

   END SUBROUTINE HML_WritePlotFile
!
!> A convenience method for stopping if one of the library functions 
!> returns an error
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE StopOnError(errorFlag)  
      IMPLICIT NONE  
      INTEGER :: errorFlag
      
      IF ( errorFlag > HML_ERROR_NONE )     THEN
         PRINT *, "Error : ", errorFlag 
         ERROR STOP 
      END IF 
   END SUBROUTINE StopOnError
   
   END Module ProjectInterfaceActions

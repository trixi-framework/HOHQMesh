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
   Module ProjectInterfaceActions
   USE MeshProjectClass
   USE FTValueDictionaryClass
   USE ControlFileReaderClass
   USE HOHQMeshModule
   USE SharedExceptionManagerModule
   USE MeshController3D
   USE InteropUtilitiesModule
   USE ISO_C_BINDING
   
   IMPLICIT NONE
!
!  -----------
!  Error Codes
!  -----------
!
!> Error Flag for no error
   INTEGER, PARAMETER :: HML_ERROR_NONE = 0 
!
!> Error Flag when a project has more than one reference and cannot be deallocated
   INTEGER, PARAMETER :: HML_ERROR_MULTIPLE_REFERENCES = 1
!
!> Error Flag if for some reason a MeshProject cannot be deallocated
   INTEGER, PARAMETER :: HML_ERROR_DEALLOCATION = 2  
!
!> Error Flag if a routine is passed a c_ptr that doesn't resolve to a MeshProject
   INTEGER, PARAMETER :: HML_ERROR_NOT_A_PROJECT = 3  
!
!  --------
   CONTAINS
!  --------
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Function that returns a c_ptr pointer to a new project
!>
   FUNCTION HML_NewProject() BIND(C) RESULT(cPtr)
      IMPLICIT NONE
      TYPE( MeshProject ), POINTER :: proj
      TYPE(c_ptr)                  :: cPtr
      ALLOCATE(proj)
      cPtr = C_LOC(proj)
   END FUNCTION HML_NewProject
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Call to close out/kill a MeshProject and release all its memory
!> CloseProject returns an error flag to tell whether or not the operation
!> is successful. 
!>
   SUBROUTINE HML_CloseProject(cPtr, errFlag)   BIND(C)
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)                   :: cPtr
      INTEGER, INTENT(OUT)          :: errFlag
!
!     ---------------
!     Local Variables
!     ---------------
!
      TYPE  ( MeshProject ), POINTER :: proj
      CLASS ( MeshProject ), POINTER :: projAsClass
      INTEGER                        :: rc
      
      errFlag = HML_ERROR_NONE
      IF( .NOT.C_ASSOCIATED(cPtr))     RETURN 
!
!     ------------------
!     Convert to project
!     ------------------
!
      CALL C_F_POINTER(cPtr = cPtr, FPTR = proj)
      projAsClass => proj
      IF(.NOT. IsMeshProjectPtr(projAsClass))     THEN
         errFlag = HML_ERROR_NOT_A_PROJECT
         RETURN 
      END IF 
      
      rc = proj % refCount()
      IF ( rc > 1 )     THEN
         errFlag = HML_ERROR_MULTIPLE_REFERENCES 
      ELSE 
         CALL releaseMeshProject(projAsClass)
         IF(ASSOCIATED(projAsClass)) errFlag = HML_ERROR_DEALLOCATION
         cPtr = c_null_ptr
      END IF 

   END SUBROUTINE HML_CloseProject
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Initializer given a control file name
!>
   SUBROUTINE HML_InitWithControlFile(cPtr, fileName, errFlag)
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)      :: cPtr
      CHARACTER(LEN=*) :: fileName
      INTEGER          :: errFlag
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE( MeshProject )     , POINTER :: proj
      TYPE (FTValueDictionary), POINTER :: projectDict => NULL()
      CLASS(FTObject)         , POINTER :: obj
      TYPE (FTValueDictionary), POINTER :: modelDict, controlDict
      LOGICAL                           :: shouldGenerate3D  = .FALSE.
      CLASS ( MeshProject )   , POINTER :: projAsClass

      errFlag     = HML_ERROR_NONE

      CALL C_F_POINTER(cPtr = cPtr, FPTR = proj)
      projAsClass => proj
      IF ( .NOT. IsMeshProjectPtr(projAsClass) )     THEN
            errFlag = HML_ERROR_NOT_A_PROJECT
            RETURN 
      END IF 
      
      CALL ReadControlFile(fileName, projectDict)
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
    
   END SUBROUTINE HML_InitWithControlFile
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
      TYPE(c_ptr)   :: cPtr
      INTEGER       :: errFlag
!
!     ---------------
!     Local Variables
!     ---------------
!
      LOGICAL                           :: didGenerate3DMesh
      TYPE ( MeshProject )    , POINTER :: project
      TYPE (FTValueDictionary), POINTER :: projectDict
      CLASS ( MeshProject )   , POINTER :: projAsClass
!
!     -----
!     Other
!     -----
!         
      INTEGER                           :: errorCode
! 
      errFlag     = HML_ERROR_NONE

      CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
      projAsClass => project
      IF ( .NOT. IsMeshProjectPtr(projAsClass) )     THEN
            errFlag = HML_ERROR_NOT_A_PROJECT
            RETURN 
      END IF 
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
      TYPE(c_ptr)                    :: cPtr
      INTEGER                        :: errFlag
      TYPE( MeshProject )  , POINTER :: project
      CLASS ( MeshProject ), POINTER :: projAsClass
      
      errFlag     = HML_ERROR_NONE

      CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
      projAsClass => project
      IF ( .NOT. IsMeshProjectPtr(projAsClass) )     THEN
            errFlag = HML_ERROR_NOT_A_PROJECT
            RETURN 
      END IF 
      
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
      INTEGER                        :: errFlag
      TYPE( MeshProject )  , POINTER :: project
      CLASS ( MeshProject ), POINTER :: projAsClass
      
      errFlag     = HML_ERROR_NONE

      CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
      projAsClass => project
      IF ( .NOT. IsMeshProjectPtr(projAsClass) )     THEN
            errFlag = HML_ERROR_NOT_A_PROJECT
            RETURN 
      END IF 
      
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

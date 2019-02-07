!
!////////////////////////////////////////////////////////////////////////
!
!      3DMeshController.f90
!      Created: April 1, 2013 3:16 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module MeshController3D
      USE FTValuedictionaryClass
      USE SimpleSweepModule
      USE MeshProjectClass
      USE SMMeshObjectsModule
      USE HexMeshObjectsModule
      USE ErrorTypesModule

      IMPLICIT NONE
! 
!---------------------------------------------------------------------
! This module Coordinates the generation of a 3D Mesh from the 2D Mesh 
!---------------------------------------------------------------------
!
!     ---------
!     Constants
!     ---------
!
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: SM_3D_ALGORITHM_CHOICE_KEY = "AlgorithmChoice"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: SM_ELEMENT_TYPE_KEY        = "elementType"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: SM_GENERATE3D_MESH_KEY     = "generate3DMesh"
!
!     --------
!     The mesh
!     --------
!
      TYPE(StructuredHexMesh) :: hex8Mesh
!
!     ======== 
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION shouldGenerate3DMesh(controlDict)
!
!     ---------------------------------------------
!     See if we should generate a 3D mesh, as given
!     by the presense of the Define3DMesh block
!     ---------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTValueDictionary), POINTER :: controlDict
!
!        ---------------
!        Local Variables
!        ---------------
!
         CLASS(FTObject)           , POINTER :: obj
         CLASS(FTValueDictionary)  , POINTER :: runParamsDict
         CHARACTER(DEFAULT_CHARACTER_LENGTH) :: str
         
          
         shouldGenerate3DMesh = .FALSE.
         
         IF ( controlDict % containsKey(key = MESH_PARAMETERS_KEY) )     THEN
         
            obj => controlDict % objectForKey(key = MESH_PARAMETERS_KEY) 
            runParamsDict => valueDictionaryFromObject(obj)
            IF ( runParamsDict % containsKey( ELEMENT_TYPE_KEY) )     THEN
               str = runParamsDict % stringValueForKey(key = ELEMENT_TYPE_KEY, &
                                                       requestedLength = DEFAULT_CHARACTER_LENGTH) 
               IF(TRIM(ADJUSTL(str)) == "hex") shouldGenerate3DMesh = .TRUE.
            END IF 
            
         ELSE
            CALL ThrowErrorExceptionOfType(poster = "shouldGenerate3DMesh", &
                                           msg = TRIM(MESH_PARAMETERS_KEY) // "not found in control file",&
                                           typ = FT_ERROR_FATAL)
            RETURN
         END IF 

      END FUNCTION shouldGenerate3DMesh
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE generate3DMesh(controlDict, project)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTValueDictionary), POINTER :: controlDict
         TYPE(MeshProject)                 :: project
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)           , POINTER :: obj
         CLASS(FTValueDictionary)  , POINTER :: generatorDict
         
         INTEGER                                :: algorithmChoice = NONE
         CHARACTER(LEN=LINE_LENGTH)             :: meshAlgorithm, algorithmName
         CHARACTER(LEN=ERROR_MSG_STRING_LENGTH) :: msg
!
!        ----------
!        Interfaces
!        ----------
!
         LOGICAL, EXTERNAL :: ReturnOnFatalError
!
!
!        ----------------------------------------
!        Get the included 3D generator dictionary
!        and check its integrity.
!        ----------------------------------------
!
         
         IF ( controlDict % containsKey(key = SIMPLE_EXTRUSION_BLOCK_KEY) )     THEN
         
            obj             => controlDict % objectForKey(key = SIMPLE_EXTRUSION_BLOCK_KEY)
            generatorDict   => valueDictionaryFromObject(obj) 
            algorithmChoice = SIMPLE_EXTRUSION_ALGORITHM
            CALL CheckSimpleExtrusionBlock(dict = generatorDict)
            IF(ReturnOnFatalError())     RETURN 
            
         ELSE IF ( controlDict % containsKey(key = SIMPLE_ROTATION_ALGORITHM_KEY) )     THEN 
         
            obj             => controlDict % objectForKey(key = SIMPLE_ROTATION_ALGORITHM_KEY)
            generatorDict   => valueDictionaryFromObject(obj) 
            algorithmChoice = SIMPLE_ROTATION_ALGORITHM
            CALL CheckSimpleRotationBlock(dict = generatorDict)
            IF(ReturnOnFatalError())     RETURN 
            
         ELSE
            CALL ThrowErrorExceptionOfType(poster = "generate3DMesh", &
                                           msg = "No generator for 3D mesh found in control file", &
                                           typ = FT_ERROR_FATAL)
            RETURN 
         END IF 
!
!        ---------------------
!        Generate the Hex mesh
!        ---------------------
!
         SELECT CASE ( algorithmChoice )
            CASE( SIMPLE_EXTRUSION_ALGORITHM, SIMPLE_ROTATION_ALGORITHM )
               CALL PerformSimpleMeshSweep( project, hex8Mesh, generatorDict, algorithmChoice)
            CASE DEFAULT
            CALL ThrowErrorExceptionOfType(poster = "generate3DMesh", &
                                           msg = "unknown generator for 3D mesh found in control file", &
                                           typ = FT_ERROR_FATAL)
         END SELECT 
         
      END SUBROUTINE generate3DMesh

   END Module MeshController3D 
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
         
         IF ( controlDict % containsKey(key = SIMPLE_EXTRUSION_BLOCK_KEY) .OR. &
              controlDict % containsKey(key = SIMPLE_ROTATION_BLOCK_KEY) )     THEN
         
               shouldGenerate3DMesh = .TRUE.
         END IF 

      END FUNCTION shouldGenerate3DMesh
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE Check3DMeshParametersIntegrity( controlDict )  
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTValueDictionary), POINTER :: controlDict
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)           , POINTER :: obj
         CLASS(FTValueDictionary)  , POINTER :: generatorDict
         INTEGER                             :: algorithmChoice
!
!        ----------
!        Interfaces
!        ----------
!
         LOGICAL, EXTERNAL :: ReturnOnFatalError
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
      END SUBROUTINE Check3DMeshParametersIntegrity
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE generate3DMesh( controlDict, project )
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
         INTEGER                             :: numberOfLayers
         
         INTEGER                             :: algorithmChoice = NONE
!
!        ----------
!        Interfaces
!        ----------
!
         LOGICAL, EXTERNAL :: ReturnOnFatalError
!
!        ----------------------------------------
!        Get the included 3D generator dictionary
!        ----------------------------------------
!
         IF ( controlDict % containsKey(key = SIMPLE_EXTRUSION_BLOCK_KEY) )     THEN
         
            obj             => controlDict % objectForKey(key = SIMPLE_EXTRUSION_BLOCK_KEY)
            generatorDict   => valueDictionaryFromObject(obj) 
            algorithmChoice = SIMPLE_EXTRUSION_ALGORITHM
            
         ELSE IF ( controlDict % containsKey(key = SIMPLE_ROTATION_ALGORITHM_KEY) )     THEN 
         
            obj             => controlDict % objectForKey(key = SIMPLE_ROTATION_ALGORITHM_KEY)
            generatorDict   => valueDictionaryFromObject(obj) 
            algorithmChoice = SIMPLE_ROTATION_ALGORITHM
            
         END IF
!
!        ---------------------------------------------------------------------
!        Allocate memory for the hex mesh. Since the extrusion is 
!        structured in the new direction, the data structures can be accessed
!        as arrays. We also know exactly how many nodes and elements are to be
!        created, so we don't need pointers for most things.
!        ---------------------------------------------------------------------
!
         ALLOCATE(project % hexMesh)
         numberOfLayers = generatorDict % integerValueForKey( SIMPLE_SWEEP_SUBDIVISIONS_KEY )
         CALL InitializeStructuredHexMesh(hexMesh              = project % hexMesh,                   &
                                          numberOf2DNodes      = project % mesh % nodes % count(),    &
                                          numberOfQuadElements = project % mesh % elements % count(), &
                                          numberOfLayers       = numberOfLayers,                      &
                                          N                    = project % mesh % polynomialOrder)
!
!        ---------------------
!        Generate the Hex mesh
!        ---------------------
!
         SELECT CASE ( algorithmChoice )
            CASE( SIMPLE_EXTRUSION_ALGORITHM, SIMPLE_ROTATION_ALGORITHM )
               CALL PerformSimpleMeshSweep( project, generatorDict, algorithmChoice)
            CASE DEFAULT
            CALL ThrowErrorExceptionOfType(poster = "generate3DMesh", &
                                           msg = "unknown generator for 3D mesh found in control file", &
                                           typ = FT_ERROR_FATAL)
         END SELECT 
         
      END SUBROUTINE generate3DMesh

   END Module MeshController3D 
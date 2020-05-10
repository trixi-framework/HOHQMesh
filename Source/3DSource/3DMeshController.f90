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
      USE CurveSweepClass
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
         TYPE (FTValueDictionary), POINTER :: controlDict
!
!        ---------------
!        Local Variables
!        ---------------
!         
         shouldGenerate3DMesh = .FALSE.
         
         IF ( controlDict % containsKey(key = SIMPLE_EXTRUSION_BLOCK_KEY) .OR. &
              controlDict % containsKey(key = SIMPLE_ROTATION_BLOCK_KEY)  .OR. &
              controlDict % containsKey(key = SWEEP_CURVE_CONTROL_KEY))     THEN
         
               shouldGenerate3DMesh = .TRUE.
         END IF 

      END FUNCTION shouldGenerate3DMesh
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE Check3DMeshParametersIntegrity( controlDict, modelDict )  
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (FTValueDictionary), POINTER :: controlDict, modelDict
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)           , POINTER :: obj
         CLASS(FTValueDictionary)  , POINTER :: generatorDict
         INTEGER                             :: algorithmChoice
         INTEGER                             :: numberOf3DGenerators
!
!        ----------
!        Interfaces
!        ----------
!
         LOGICAL, EXTERNAL :: ReturnOnFatalError
!
!        --------------------------------
!        Only allow one 3D mesh generator
!        --------------------------------
!
         numberOf3DGenerators = 0
         IF(controlDict % containsKey(key = SIMPLE_EXTRUSION_BLOCK_KEY))    numberOf3DGenerators = numberOf3DGenerators + 1
         IF(controlDict % containsKey(key = SIMPLE_ROTATION_ALGORITHM_KEY)) numberOf3DGenerators = numberOf3DGenerators + 1
         IF(controlDict % containsKey(key = SWEEP_CURVE_CONTROL_KEY))       numberOf3DGenerators = numberOf3DGenerators + 1
         IF ( numberOf3DGenerators > 1  )     THEN
            CALL ThrowErrorExceptionOfType(poster = "generate3DMesh", &
                                           msg    = "Too many 3D mesh generators specified in control file", &
                                           typ    = FT_ERROR_FATAL)
            RETURN 
         END IF 
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
            
         ELSE IF ( controlDict % containsKey(key = SIMPLE_ROTATION_ALGORITHM_KEY) )     THEN 
         
            obj             => controlDict % objectForKey(key = SIMPLE_ROTATION_ALGORITHM_KEY)
            generatorDict   => valueDictionaryFromObject(obj) 
            algorithmChoice = SIMPLE_ROTATION_ALGORITHM
            CALL CheckSimpleRotationBlock(dict = generatorDict)
            
         ELSE IF ( controlDict % containsKey(key = SWEEP_CURVE_CONTROL_KEY) )     THEN 
         
            obj             => controlDict % objectForKey(key = SWEEP_CURVE_CONTROL_KEY)
            generatorDict   => valueDictionaryFromObject(obj) 
            algorithmChoice = SWEEP_ALGORITHM
            CALL CheckCurveSweepBlock(controlDict = generatorDict, &
                                      modelDict   = modelDict)            
         ELSE
            CALL ThrowErrorExceptionOfType(poster = "generate3DMesh", &
                                           msg = "No generator for 3D mesh found in control file", &
                                           typ = FT_ERROR_FATAL)
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
         TYPE (FTValueDictionary), POINTER :: controlDict
         TYPE(MeshProject)                 :: project
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)           , POINTER   :: obj
         CLASS(FTValueDictionary)  , POINTER   :: generatorDict
         INTEGER                               :: numberOfLayers
         CHARACTER(LEN=STRING_CONSTANT_LENGTH) :: subdivisionsKey
         INTEGER                               :: algorithmChoice = NONE
         INTEGER                               :: pMutation, rotAxis
         INTEGER                               :: rotMap(3) = [3, 3, 1]
         REAL(KIND=RP)                         :: dz, h
         TYPE(CurveSweeper)                    :: sweeper
!
!        ----------
!        Interfaces
!        ----------
!
         LOGICAL, EXTERNAL :: ReturnOnFatalError

!
!        --------------------------------------------
!        Get the 3D generator dictionary and set up 
!        algorithm dependent quantities for extrusion
!        --------------------------------------------
!
         IF ( controlDict % containsKey(key = SIMPLE_EXTRUSION_BLOCK_KEY) )     THEN
         
            obj             => controlDict % objectForKey(key = SIMPLE_EXTRUSION_BLOCK_KEY)
            generatorDict   => valueDictionaryFromObject(obj) 
            algorithmChoice = SIMPLE_EXTRUSION_ALGORITHM
            subdivisionsKey = SIMPLE_SWEEP_SUBDIVISIONS_KEY
            
            numberOfLayers = generatorDict % integerValueForKey( subdivisionsKey )
            pMutation      = generatorDict % integerValueForKey(SIMPLE_SWEEP_DIRECTION_KEY)
            h              = generatorDict % doublePrecisionValueForKey( SIMPLE_EXTRUSION_HEIGHT_KEY )
            dz             = h/generatorDict % integerValueForKey( subdivisionsKey )
            
         ELSE IF ( controlDict % containsKey(key = SIMPLE_ROTATION_ALGORITHM_KEY) )     THEN 
         
            obj             => controlDict % objectForKey(key = SIMPLE_ROTATION_ALGORITHM_KEY)
            generatorDict   => valueDictionaryFromObject(obj) 
            algorithmChoice = SIMPLE_ROTATION_ALGORITHM
            subdivisionsKey = SIMPLE_SWEEP_SUBDIVISIONS_KEY
            
            numberOfLayers = generatorDict % integerValueForKey( subdivisionsKey )
            pMutation      = generatorDict % integerValueForKey(SIMPLE_SWEEP_DIRECTION_KEY)
            rotAxis        = pMutation
            pMutation      = rotMap(pMutation)
 
            h  = PI * generatorDict % doublePrecisionValueForKey( SIMPLE_ROTATION_ANGLE_KEY )
            dz = h/generatorDict % integerValueForKey( subdivisionsKey )
           
         ELSE IF ( controlDict % containsKey(key = SWEEP_CURVE_CONTROL_KEY) )     THEN 
         
            obj             => controlDict % objectForKey(key = SWEEP_CURVE_CONTROL_KEY)
            generatorDict   => valueDictionaryFromObject(obj) 
            algorithmChoice = SWEEP_ALGORITHM
            subdivisionsKey = CURVE_SWEEP_SUBDIVISIONS_KEY
             
            numberOfLayers = generatorDict % integerValueForKey( subdivisionsKey )* &
                             project % model % sweepCurve % numberOfCurvesInChain
            pMutation      = 3
            dz             = 1.0_RP/numberOfLayers
         ELSE
            CALL ThrowErrorExceptionOfType(poster = "generate3DMesh", &
                                           msg    = "unknown generator for 3D mesh found in control file", &
                                           typ    = FT_ERROR_FATAL)
            RETURN 
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
         CALL InitializeStructuredHexMesh(hexMesh              = project % hexMesh,                   &
                                          numberOf2DNodes      = project % mesh % nodes % count(),    &
                                          numberOfQuadElements = project % mesh % elements % count(), &
                                          numberOfLayers       = numberOfLayers,                      &
                                          N                    = project % mesh % polynomialOrder)
!
!        ---------------------------------------------------
!        Permute the mesh for the simple sweeps if necessary
!        ---------------------------------------------------
!
         IF ( pMutation < 3 )     THEN
            CALL permuteMeshDirection(project % mesh, pMutation)
         END IF 
!
!        ---------------------
!        Generate the Hex mesh
!        ---------------------
!
         CALL PerformSimpleMeshSweep( project, pMutation, dz, generatorDict)
!
!        ------------------------------
!        Rotate the mesh when requested
!        ------------------------------
!
         IF ( algorithmChoice == SIMPLE_ROTATION_ALGORITHM )     THEN
         
            CALL RotateAll(mesh    = project % hexMesh,                     &
                           N       = project % runParams % polynomialOrder, &
                           rotAxis = rotAxis)
                           
         ELSE IF(algorithmChoice == SWEEP_ALGORITHM)     THEN 
         
            CALL ConstructCurveSweeper(self  = sweeper,                      &
                                  sweepCurve = project % model % sweepCurve, &
                                  scaleCurve = project % model % scaleCurve)
                                  
            CALL applyParallelSweepTransform(self    = sweeper,                       &
                                     mesh    = project % hexMesh,                     &
                                     dt      = dz,                                    &
                                     N       = project % runParams % polynomialOrder)
                                     
            CALL destructCurveSweeper(self = sweeper)
         END IF 
        
      END SUBROUTINE generate3DMesh
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE permuteMeshDirection(mesh,pmutation)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMMesh) :: mesh
         INTEGER       :: pmutation
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject), POINTER  :: obj
         CLASS(SMNode)  , POINTER  :: node
         REAL(KIND=RP)             :: x(3)
         
         CALL mesh % nodesIterator % setToStart()
         DO WHILE( .NOT. mesh % nodesIterator % isAtEnd() )
         
            obj => mesh % nodesIterator % object()
            CALL castToSMNode(obj,node)
            
            x = permutePosition(x = node % x,pmutation = pmutation)
            node % x = x
            
            CALL mesh % nodesIterator % moveToNext()
         END DO 
      END SUBROUTINE permuteMeshDirection

   END Module MeshController3D 
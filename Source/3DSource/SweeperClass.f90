!
!////////////////////////////////////////////////////////////////////////
!
!      SweeperClass.f90
!      Created: April 10, 2020 at 12:57 PM 
!      By: David Kopriva  
!
!      Class for sweeping a 2D mesh into hexahedra by following a curve.
!
!      SWEEP_ALONG_CURVE contains the keys
!         subdivisions
!         start surface name
!         end surface name
!
!
!////////////////////////////////////////////////////////////////////////
!
      Module CurveSweepClass
         USE FTValueDictionaryClass
         USE SMConstants
         USE ProgramGlobals
         USE FTExceptionClass
         USE SharedExceptionManagerModule
         USE HexMeshObjectsModule
         USE ErrorTypesModule
         USE SMChainedCurveClass
         USE Geometry3DModule
         IMPLICIT NONE
         
         TYPE CurveSweeper
            CLASS(SMChainedCurve), POINTER :: sweepCurve
            CLASS(SMChainedCurve), POINTER :: scaleCurve
            TYPE(AffineTransform)          :: affineTransformer
            TYPE(ScaleTransform)           :: scaleTransformer
         END TYPE CurveSweeper

         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: SWEEP_CURVE_CONTROL_KEY        = "SWEEP_ALONG_CURVE"
         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: SWEEP_CURVE_BLOCK_KEY          = "SWEEP_CURVE"
         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: SCALE_CURVE_BLOCK_KEY          = "SCALE_CURVE"
         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: CURVE_SWEEP_SUBDIVISIONS_KEY   = "subdivisions"
         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: CURVE_SWEEP_STARTNAME_KEY      = "start surface name"
         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: CURVE_SWEEP_ENDNAME_KEY        = "end surface name"
         
         REAL(KIND=RP), PRIVATE :: derivativeDT = 1.0D-6 !TODO: Put in preferences
! 
!        ========
         CONTAINS  
!        ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
         SUBROUTINE ConstructCurveSweeper(self, sweepCurve, scaleCurve)  
            IMPLICIT NONE  
            TYPE( CurveSweeper)            :: self
            CLASS(SMChainedCurve), POINTER :: sweepCurve
            CLASS(SMChainedCurve), POINTER :: scaleCurve
            
            self % sweepCurve => sweepCurve
            CALL self % sweepCurve % retain()
            self % scaleCurve => scaleCurve
            CALL self % scaleCurve % retain()
            
            CALL ConstructIdentityScaleTransform( self = self % scaleTransformer)
            CALL ConstructIdentityAffineTransform(self = self % affineTransformer)
              
         END SUBROUTINE ConstructCurveSweeper
!
!//////////////////////////////////////////////////////////////////////// 
! 
         SUBROUTINE destructCurveSweeper(self)
            IMPLICIT NONE  
            TYPE( CurveSweeper)         :: self
            
            CALL releaseChainedCurve(self = self % sweepCurve)
            CALL releaseChainedCurve(self = self % scaleCurve)
            
         END SUBROUTINE destructCurveSweeper
!
!//////////////////////////////////////////////////////////////////////// 
! 
         SUBROUTINE CheckCurveSweepBlock(controlDict, modelDict)  
            IMPLICIT NONE  
!
!              ---------
!              Arguments
!              ---------
!
               CLASS(FTValueDictionary) :: controlDict, modelDict
!
!              ---------------
!              Local variables
!              ---------------
!
               INTEGER      , EXTERNAL                :: GetIntValue
               REAL(KIND=RP), EXTERNAL                :: GetRealValue
               CHARACTER( LEN=LINE_LENGTH ), EXTERNAL :: GetStringValue
!
!              ----------------------------------------
!              Make sure the model has a curve to sweep
!              scaleCurve is optional.
!              ----------------------------------------
!
               IF ( .NOT. modelDict % containsKey(key = SWEEP_CURVE_BLOCK_KEY) )     THEN
                  CALL ThrowErrorExceptionOfType(poster = "CheckCurveSweepBlock", &
                                                 msg    = "key " // TRIM(SWEEP_CURVE_BLOCK_KEY) // &
                                                          " not found in model for sweeping", &
                                                 typ    = FT_ERROR_FATAL) 
               END IF
!
!              ------------
!              Subdivisions
!              ------------
!
               IF ( .NOT. controlDict % containsKey(key = CURVE_SWEEP_SUBDIVISIONS_KEY) )     THEN
                  CALL ThrowErrorExceptionOfType(poster = "CheckCurveSweepBlock", &
                                                 msg    = "key " // TRIM(CURVE_SWEEP_SUBDIVISIONS_KEY) // &
                                                          " not found in sweep block", &
                                                 typ    = FT_ERROR_FATAL) 
               END IF
!
!              -------------------
!              Bottom surface name
!              -------------------
!
               IF ( .NOT. controlDict % containsKey(key = CURVE_SWEEP_STARTNAME_KEY) )     THEN
                  CALL ThrowErrorExceptionOfType(poster = "CheckCurveSweepBlock", &
                                                 msg    = "key " // TRIM(CURVE_SWEEP_STARTNAME_KEY) // &
                                                           " not found in sweep block", &
                                                 typ    = FT_ERROR_FATAL) 
               END IF
!
!              ----------------
!              Top surface name
!              ----------------
!
               IF ( .NOT. controlDict % containsKey(key = CURVE_SWEEP_ENDNAME_KEY) )     THEN
                  CALL ThrowErrorExceptionOfType(poster = "CheckCurveSweepBlock", &
                                                 msg = "key " // TRIM(CURVE_SWEEP_ENDNAME_KEY) // &
                                                 " not found in sweep block", &
                                                 typ = FT_ERROR_FATAL) 
               END IF
            END SUBROUTINE CheckCurveSweepBlock
!
!//////////////////////////////////////////////////////////////////////// 
! 
            SUBROUTINE applySweepTransform(self, hexMesh, N)  
               IMPLICIT NONE
               TYPE(CurveSweeper)      :: self
               TYPE(StructuredHexMesh) :: hexMesh
               INTEGER                 :: N
              
               
            END SUBROUTINE applySweepTransform
       END Module CurveSweepClass

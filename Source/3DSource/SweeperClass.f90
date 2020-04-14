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

         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: SWEEP_CURVE_CONTROL_KEY          = "SWEEP_ALONG_CURVE"
         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: SWEEP_CURVE_BLOCK_KEY            = "SWEEP_CURVE"
         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: SWEEP_SCALE_FACTOR_EQN_BLOCK_KEY = "SWEEP_SCALE_FACTOR_EQN"
         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: CURVE_SWEEP_SUBDIVISIONS_KEY     = "subdivisions"
         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: CURVE_SWEEP_STARTNAME_KEY        = "start surface name"
         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: CURVE_SWEEP_ENDNAME_KEY          = "end surface name"
         
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
            IF(ASSOCIATED(sweepCurve)) CALL self % sweepCurve % retain()
            
            self % scaleCurve => scaleCurve
            IF(ASSOCIATED(scaleCurve))   CALL self % scaleCurve % retain()
            
            CALL ConstructIdentityScaleTransform( self = self % scaleTransformer)
            CALL ConstructIdentityAffineTransform(self = self % affineTransformer)
              
         END SUBROUTINE ConstructCurveSweeper
!
!//////////////////////////////////////////////////////////////////////// 
! 
         SUBROUTINE destructCurveSweeper(self)
            IMPLICIT NONE  
            TYPE( CurveSweeper)         :: self
            
            IF(ASSOCIATED(self % sweepCurve)) CALL releaseChainedCurve(self = self % sweepCurve)
            IF(ASSOCIATED(self % scaleCurve)) CALL releaseChainedCurve(self = self % scaleCurve)
            
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
            SUBROUTINE applySweepTransform(self, mesh, dt, N)  
            !TODO needs implementation of scaling
               IMPLICIT NONE
!
!              ---------
!              Arguments
!              ---------
!
               TYPE(CurveSweeper)      :: self
               TYPE(StructuredHexMesh) :: mesh
               INTEGER                 :: N
               REAL(KIND=RP)           :: dt, t, t0
!
!              ---------------
!              Local variables
!              ---------------
!
               REAL(KIND=RP) :: direction(3), r(3), newX(3)
               INTEGER       :: l, m, k, j, i
               REAL(KIND=RP) :: zHat(3) = [0.0_RP, 0.0_RP, 1.0_RP]
!
!              -----------------------------------------------------
!              Transform the nodes. Each level is dt higher than the 
!              previous.
!              -----------------------------------------------------
!
               DO l = 0, SIZE(mesh % nodes,2)-1
                  t = l*dt
                  r         = self % sweepCurve % positionAt(t)
                  direction = self % sweepCurve % tangentAt(t)
                  CALL ConstructAffineTransform(self = self % affineTransformer, &
                                                translation = r, &
                                                startDirection = zHat, &
                                                newDirection = direction)
                                                
                  DO m = 1, SIZE(mesh % nodes,1)
                     newX = PerformAffineTransform(x = mesh % nodes(m,l) % x,transformation = self % affineTransformer)
                     mesh % nodes(m,l) % x = newX
                  END DO   
                  
               END DO  
!
!              ------------------------------------------------------
!              Transform the internal DOFs
!              We will assume that the curve is curved, otherwise the
!              simple extrusion would be used. So then all faces
!              will be curved, too
!              ------------------------------------------------------
!
               DO l = 1, mesh % numberOfLayers             ! level
                  t0 = l*dt
               
                  DO m = 1, mesh % numberOfQuadElements    ! element on original quad mesh
                     mesh % elements(m,l) % bFaceFlag = ON
                     DO k = 0, N 
                        t = t0 + dt*0.5_RP*(1.0_RP - COS(k*PI/N))
                        r         = self % sweepCurve % positionAt(t)
                        direction = self % sweepCurve % tangentAt(t)
                        CALL ConstructAffineTransform(self           = self % affineTransformer, &
                                                      translation    = r, &
                                                      startDirection = zHat, &
                                                      newDirection   = direction)
                        DO j = 0, N 
                           DO i = 0, N 
                              newX = PerformAffineTransform(x              = mesh % elements(m,l) % x(:,i,j,k), &
                                                            transformation = self % affineTransformer)
                              mesh % elements(m,l) % x(:,i,j,k) = newX
                           END DO 
                        END DO 
                     END DO 
                  END DO 
               END DO
              
            END SUBROUTINE applySweepTransform
       END Module CurveSweepClass

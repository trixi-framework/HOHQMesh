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
         USE Frenet
         IMPLICIT NONE
         
         TYPE CurveSweeper
            CLASS(SMChainedCurve), POINTER :: sweepCurve
            CLASS(SMChainedCurve), POINTER :: scaleCurve
            TYPE(RotationTransform)        :: RotationTransformer
            TYPE(ScaleTransform)           :: scaleTransformer
         END TYPE CurveSweeper

         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: SWEEP_CURVE_CONTROL_KEY          = "SWEEP_ALONG_CURVE"
         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: SWEEP_CURVE_BLOCK_KEY            = "SWEEP_CURVE"
         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: SWEEP_SCALE_FACTOR_EQN_BLOCK_KEY = "SWEEP_SCALE_FACTOR"
         CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: CURVE_SWEEP_SUBDIVISIONS_KEY     = "subdivisions per segment"
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
            
            CALL ConstructIdentityScaleTransform   (self = self % scaleTransformer)
            CALL ConstructIdentityRotationTransform(self = self % RotationTransformer)
              
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
            SUBROUTINE rotateCylinder(self, mesh, dt, N)
!
!           -----------------------------------------------------------------
!           Take the swept skeleton (always in the \hat z direction) and
!           rotate it to be along the initial direction of the sweeping curve
!           This could be used to replace the simple sweep method for alternate
!           normal directions.
!           -----------------------------------------------------------------
!
               IMPLICIT NONE
!
!              ---------
!              Arguments
!              ---------
!
               TYPE(CurveSweeper)      :: self
               TYPE(StructuredHexMesh) :: mesh
               INTEGER                 :: N
               REAL(KIND=RP)           :: dt
!
!              ---------------
!              Local variables
!              ---------------
!
               REAL(KIND=RP) :: t, t0, f(3)
               REAL(KIND=RP) :: direction(3), r(3), p0(3), p1(3)
               INTEGER       :: l, m, k, j, i
               REAL(KIND=RP) :: zHat(3) = [0.0_RP, 0.0_RP, 1.0_RP]
               REAL(KIND=RP) :: zero(3) = [0.0_RP, 0.0_RP, 0.0_RP]
!
!              -----------------------------------------------------
!              Transform the nodes. Each level is dt higher than the 
!              previous.
!              -----------------------------------------------------
!
               t         = 0.0_RP
               direction = self % sweepCurve % tangentAt(t)
               
               CALL ConstructRotationTransform(self           = self % RotationTransformer, &
                                               rotationPoint  = zero,                       &
                                               startDirection = zHat,                       &
                                               newDirection   = direction)

               DO l = 0, mesh % numberofLayers ! level in the hex mesh
                  t = l*dt
                  r = t*direction
!
!                 ------------------------------
!                 Apply rotation and translation
!                 ------------------------------
!
                  DO m = 1, SIZE(mesh % nodes,1) ! Nodes in the quad mesh
                     p0    = mesh % nodes(m,l) % x
                     p0(3) = 0.0_RP ! Move back to original z plane
                     p1    = PerformRotationTransform(x = p0 ,transformation = self % RotationTransformer)
                     
                     mesh % nodes(m,l) % x = p1 + r
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
!              ----------------------
!              For each element level
!              ----------------------
!
               DO l = 1, mesh % numberOfLayers 
                  t0 = (l-1)*dt
!
!                 ---------------------------
!                 For each element at level l
!                 ---------------------------
!
                  DO m = 1, mesh % numberOfQuadElements    ! element on original quad mesh
!
!                    -------------------------------------------------------------------------
!                    For each Chebyshev node in the vertical direction in element m at level l
!                    -------------------------------------------------------------------------
!
                     DO k = 0, N
                     
                        t = t0 + dt*0.5_RP*(1.0_RP - COS(k*PI/N))
                        r = t*direction
!
!                      ---------------------------------------------------------------------
!                      For each point i,j at level k within element m at level l in hex mesh
!                      ---------------------------------------------------------------------
!
!                      ------------------------------
!                      Apply rotation and translation
!                      ------------------------------
!
                       DO j = 0, N 
                           DO i = 0, N 
                              p0    = mesh % elements(m,l) % x(:,i,j,k)
                              p0(3) = 0.0_RP ! Move point back to original z-plane
                              p1    = PerformRotationTransform(x = p0 ,transformation = self % RotationTransformer)
                              
                              mesh % elements(m,l) % x(:,i,j,k) = p1 + r
                           END DO 
                        END DO 

                     END DO 
                  END DO 
               END DO
              
            END SUBROUTINE rotateCylinder
!
!//////////////////////////////////////////////////////////////////////// 
! 
            SUBROUTINE applyParallelSweepTransform(self, mesh, dt, N)
               USE Frenet
               IMPLICIT NONE
!
!              ---------
!              Arguments
!              ---------
!
               TYPE(CurveSweeper)      :: self
               TYPE(StructuredHexMesh) :: mesh
               INTEGER                 :: N
               REAL(KIND=RP)           :: dt
!
!              ---------------
!              Local variables
!              ---------------
!
               REAL(KIND=RP)     :: t, t0, f(3)
               REAL(KIND=RP)     :: direction(3), r(3), p0(3), p1(3), refDirection(3)
               INTEGER           :: l, m, k, j, i
               REAL(KIND=RP)     :: zHat(3) = [0.0_RP, 0.0_RP, 1.0_RP]
               REAL(KIND=RP)     :: zero(3) = [0.0_RP, 0.0_RP, 0.0_RP]
               TYPE(FrenetFrame) :: refFrame, frame, prevFrame
               LOGICAL           :: isDegenerate
!
!              --------------------------------------------------------
!              The reference frame will either be the first one, or the
!              first non-default frame.
!              --------------------------------------------------------
!
               isDegenerate = .TRUE.
               
               DO l = 0, mesh % numberofLayers
                 t = l*dt
                 CALL ComputeFrenetFrame(frame        = refFrame,          &
                                         t            = t,                 &
                                         curve        = self % sweepCurve, &
                                         isDegenerate = isDegenerate)
                   IF(.NOT.isDegenerate)   EXIT 
               END DO
!
!              -------------------------------------------------------
!              Rotate the swept cylinder make the first face normal to
!              the sweep curve
!              -------------------------------------------------------
!
               CALL rotateCylinder(self = self,mesh = mesh,dt = dt,N = N)
!
!              -----------------------------------------------------
!              Transform the nodes. Each level is dt higher than the 
!              previous.
!              -----------------------------------------------------
!
               prevFrame = refFrame
               refDirection = self % sweepCurve % tangentAt(0.0_RP)
               
               DO l = 0, mesh % numberofLayers ! level in the hex mesh
               
                  t = l*dt
                  r = self % sweepCurve % positionAt(t)
                  
                  CALL ComputeParallelFrame(t        = t,                &
                                            curve    = self % sweepCurve,&
                                            frame    = frame,            &
                                            refFrame = prevFrame)
              
                  CALL ConstructParallelTransportRotation(rotationTransformer = self % RotationTransformer, &
                                                          refDirection        = refDirection,                       &
                                                          rotationPoint       = zero,                       &
                                                          frame               = frame,                      &
                                                          refFrame            = refFrame,                   &
                                                          isDegenerate        = isDegenerate)
                  prevFrame = frame
                  
                  IF ( ASSOCIATED( self % scaleCurve) )     THEN
                     f = self % scaleCurve % positionAt(t)
                     CALL ConstructScaleTransform(self   = self % scaleTransformer, &
                                                  origin = r,                       &
                                                  normal = direction,               &
                                                  factor = f(1))
                  END IF 
!
!                 ------------------------------
!                 Apply rotation and translation
!                 ------------------------------
!
                  DO m = 1, SIZE(mesh % nodes,1) ! Nodes in the quad mesh
                     p0    = mesh % nodes(m,l) % x - t*refDirection
                     p1    = PerformRotationTransform(x              = p0 , &
                                                      transformation = self % RotationTransformer)
                     
                     mesh % nodes(m,l) % x = p1 + r
                  END DO
!
!                 -----------------------
!                 Apply scaling transform
!                 -----------------------
!
                  IF ( ASSOCIATED( self % scaleCurve) )     THEN
                      DO m = 1, SIZE(mesh % nodes,1) ! Points in the quad mesh
                        p0 = PerformScaleTransformation(x              = mesh % nodes(m,l) % x, &
                                                        transformation = self % scaleTransformer)
                        mesh % nodes(m,l) % x = p0
                      END DO
                 END IF 
                 
               END DO
!
!              ------------------------------------------------------
!              Transform the internal DOFs
!              We will assume that the curve is curved, otherwise the
!              simple extrusion would be used. So then all faces
!              will be curved, too
!              ------------------------------------------------------
!
!              ----------------------
!              For each element level
!              ----------------------
!
               prevFrame = refFrame

               DO l = 1, mesh % numberOfLayers 
                  t0 = (l-1)*dt
!
!                 ---------------------------
!                 For each element at level l
!                 ---------------------------
!
                  DO m = 1, mesh % numberOfQuadElements    ! element on original quad mesh
                     mesh % elements(m,l) % bFaceFlag = ON
!
!                    -------------------------------------------------------------------------
!                    For each Chebyshev node in the vertical direction in element m at level l
!                    -------------------------------------------------------------------------
!
                     DO k = 0, N
                     
                        t = t0 + dt*0.5_RP*(1.0_RP - COS(k*PI/N))
                        r         = self % sweepCurve % positionAt(t)
                        
                        CALL ComputeParallelFrame(t        = t,                &
                                                  curve    = self % sweepCurve,&
                                                  frame    = frame,            &
                                                  refFrame = prevFrame)
                        CALL ConstructParallelTransportRotation(rotationTransformer = self % RotationTransformer, &
                                                                refDirection        = refDirection,                       &
                                                                rotationPoint       = zero,                       &
                                                                frame               = frame,                      &
                                                                refFrame            = refFrame,                   &
                                                                isDegenerate        = isDegenerate)
                        prevFrame = frame
                        
                        IF ( ASSOCIATED( self % scaleCurve) )     THEN
                          f = self % scaleCurve % positionAt(t)
                          CALL ConstructScaleTransform(self   = self % scaleTransformer, &
                                                       origin = r,                       &
                                                       normal = direction,               &
                                                       factor = f(1))
                       END IF 
!
!                      ---------------------------------------------------------------------
!                      For each point i,j at level k within element m at level l in hex mesh
!                      ---------------------------------------------------------------------
!
!                      ------------------------------
!                      Apply rotation and translation
!                      ------------------------------
!
                       DO j = 0, N 
                           DO i = 0, N 
                              p0    = mesh % elements(m,l) % x(:,i,j,k) - t*refDirection
                              p1    = PerformRotationTransform(x = p0 ,transformation = self % RotationTransformer)
                              
                              mesh % elements(m,l) % x(:,i,j,k) = p1 + r
                           END DO 
                        END DO 
!
!                       -------------
!                       Apply scaling
!                       -------------
!
                        IF ( ASSOCIATED( self % scaleCurve) )     THEN
                           DO j = 0, N 
                              DO i = 0, N 
                                 p0 = PerformScaleTransformation(x              = mesh % elements(m,l) % x(:,i,j,k), &
                                                                 transformation = self % scaleTransformer)
                                 mesh % elements(m,l) % x(:,i,j,k) = p0
                              END DO 
                           END DO 
                        END IF 

                     END DO 
                  END DO 
               END DO
              
            END SUBROUTINE applyParallelSweepTransform
!
!//////////////////////////////////////////////////////////////////////// 
! 
 
         SUBROUTINE ConstructParallelTransportRotation( rotationTransformer, refDirection,  &
                                                        rotationPoint, frame, refFrame, isDegenerate )
            IMPLICIT NONE
!
!           ---------
!           arguments
!           ---------
!
            TYPE(RotationTransform) :: rotationTransformer
            REAL(KIND=RP)           :: refDirection(3), rotationPoint(3)
            TYPE(FrenetFrame)       :: frame, refFrame
            LOGICAL                 :: isDegenerate
!
!           ---------------
!           Local variables
!           ---------------
!
            REAL(KIND=RP) :: rotMat(3,3)
            REAL(KIND=RP) :: R(3,3)
            REAL(KIND=RP) :: cosTheta, theta, sinTheta
            REAL(KIND=RP) :: d1, d2
            REAL(KIND=RP) :: B(3)
!
!           ------------------------------
!           Compute the rotation transform
!           ------------------------------
!
            CALL ConstructRotationTransform(self           = rotationTransformer, &
                                            rotationPoint  = rotationPoint, &
                                            startDirection = refDirection, &
                                            newDirection   = frame % tangent)
            IF(isDegenerate) RETURN 
!
!           ------------------------------------------------------------
!           Find the angle by which the transportVector would be rotated
!           ------------------------------------------------------------
!
!             theta    = FrameAngle(frame1 = refFrame, frame2 = frame)
!             cosTheta = COS(theta)
!             sinTheta = SIN(theta)
!             CALL Cross3D(u = refFrame % tangent,v = frame % tangent,cross = B)
!             CALL Norm3D(u = B,norm = d1)
!             
!             IF(d1 < zeroNormSize)     RETURN
!             B = B/d1
!
!           -----------------------------------------------
!            Compute rotation matrix to counteract the twist
!            -----------------------------------------------
!
!             CALL RotationMatrixWithNormalAndAngle(nHat     = B, &
!                                                   cosTheta = cosTheta,     &
!                                                   SinTheta = sinTheta,     &
!                                                   R        = R)
!             rotMat = MATMUL(MATRIX_A = R, MATRIX_B = parallelRotation % rotMatrix)
!             R      = rotMat
!             parallelRotation % rotMatrix = R
!
!            ---------------
!            Combine the two
!           ---------------
!
!             rotMat = MATMUL(MATRIX_A = R, MATRIX_B = rotationTransformer % rotMatrix)
!             rotationTransformer % rotMatrix = rotMat
             
         END SUBROUTINE ConstructParallelTransportRotation
         
       END Module CurveSweepClass

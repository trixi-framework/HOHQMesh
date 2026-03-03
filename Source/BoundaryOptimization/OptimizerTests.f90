!
!////////////////////////////////////////////////////////////////////////
!
!      OptimizerTests.f90
!      Created: November 22, 2025 at 8:58 AM 
!      By: David Kopriva  
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE polynomialApproximationTest
!
!  -------------------------------------------------------------------
!  Approximate simple basis functions with a three segment polynomial
!  This gives an example of how to use the optimizer and multiSegment
!  modal curve classes. The return value fail can be used for testing.
!  -------------------------------------------------------------------
!
      USE CurveOptimization
      USE SMParametricEquationCurveClass
      USE FTAssertions
      IMPLICIT NONE
            
      INTEGER                                  :: optType = H1_NORM! L2_NORM or H1_NORM
      
      CLASS(SMCurve)                 , POINTER :: crv
      TYPE(SMParametricEquationCurve), POINTER :: simpleCurve
      CLASS(SMCurve)                 , POINTER :: optimizedCurve
      CLASS(MultiSegmentModalCurve)  , POINTER :: msmCurve
      TYPE(GaussQuadratureType)                :: gQuad
      TYPE(OptimizerOptions)                   :: options
      
      INTEGER                    :: nSegments
      INTEGER                    :: polyOrder
      REAL(KIND=RP), ALLOCATABLE :: cuts(:)
      REAL(KIND=RP), ALLOCATABLE :: errors(:,:)
      CHARACTER(LEN=64)          :: xEqn, yEqn, zEqn
      REAL(KIND=RP)              :: tol = 1.0d-7
      
      CALL SetDefaultOptions(options)
      options % whichNorm = optType
!
!     --------------------
!     Curve to approximate
!     --------------------
!
      xEqn = "x(t) = 2*t-1"                   ! mapped L_0
      yEqn = "y(t) = 0.5*(3*(2*t-1)^2 - 1)"   ! mapped L_1
      zEqn = "z(t) = 0.0"
      
      ALLOCATE(simpleCurve)
      CALL simpleCurve % initWithEquationsNameAndID(xEqn, yEqn, zEqn, curveName = "simpleCurve", id = 1)
      crv => simpleCurve
!
!     ----------
!     Optimizing
!     ----------
!
      nSegments       = 3
      polyOrder       = 4
      ALLOCATE(cuts(0:nSegments))
      cuts  = [0.0_RP, 0.3_RP, 0.6_RP, 1.0_RP] ! for SMCurves, t\in [0,1]
      
      CALL OptimizeCurve(curve              = crv,                   &
                         polyOrder          = polyOrder,             &
                         cuts               = cuts,                  &
                         options            = options,               &
                         newName            = "OptimizedSimpleCurve",&
                         newID              = simpleCurve % id() + 1,&
                         optimized          = optimizedCurve)
!
!     --------------
!     Compute errors
!     --------------
!
      CALL castToMultiSegmentCurve(optimizedCurve, msmCurve)
      CALL ConstructGaussQuadrature(gQuad, 2*polyOrder)
      CALL SegmentErrors(exact          = crv, &
                         segmentedCurve = msmCurve, &
                         gQuad          = gQuad, &
                         errors         = errors)
      CALL FTAssert(MAXVAL(errors(:,USER_NORM)) .le. tol, msg = "PolynomialApproximationTest")
! 
!     --------
!     Clean up
!     --------
!
      CALL releaseBaseCurve(optimizedCurve)
      CALL releasePECurve(simpleCurve)
       
   END SUBROUTINE PolynomialApproximationTest
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE BlobCurveTest()
!
!  -------------------------------------------------------------------
!  Ensure that the marching optimization is consistent with previous
!  results.
!  -------------------------------------------------------------------
!
      USE CurveOptimization
      USE SMParametricEquationCurveClass
      USE FTAssertions
      IMPLICIT NONE
!
!     --------------
!     Test paramters
!     --------------
!
      INTEGER                                :: optType    = H1_NORM
      INTEGER                                :: polyOrder  = 6
      INTEGER                                :: smoothness = 2 ! C^2 smoothness
      REAL(KIND=RP)                          :: toler      = 0.1_RP
      REAL(KIND=RP)                          :: testTol    = 1.0d-10
!
!     ---------------
!     Local Variables
!     ---------------
!
      CLASS(SMCurve)                 , POINTER :: crv
      TYPE(SMParametricEquationCurve), POINTER :: blobCurve
      CLASS(SMCurve)                 , POINTER :: optimizedCurve
      CLASS(MultiSegmentModalCurve)  , POINTER :: msmCurve
      TYPE(GaussQuadratureType)                :: gQuad
      TYPE(OptimizerOptions)                   :: options
      CHARACTER(LEN=64)                        :: xEqn, yEqn, zEqn
      REAL(KIND=RP), ALLOCATABLE               :: errors(:,:)
!
!     ------------------------------------------------------
!     If algorithms change, these values will have to change
!     ------------------------------------------------------
!
      REAL(KIND=RP), DIMENSION(0:14) :: targetCuts = [0.0000000000000000d0, 8.1259187636458827d-002, 0.15044572349103003d0, &
                                                      0.22076257764491855d0,0.29159412481275299d0,   0.36570638182942034d0, &
                                                      0.43543102026059471d0,0.50459342895656389d0,     0.58656359480732634d0, &
                                                      0.66014983994203658d0,0.72936755789328434d0,   0.79961783088021776d0, &
                                                      0.87133167735314354d0,0.95003285344413990d0,   1.0000000000000000d0]     
      REAL(KIND=RP), DIMENSION(14) :: targetErrs = [6.5714942701127529d-002, 9.2755197027968445d-002, 9.4310979537382669d-002, &
                                                    8.9842680375006354d-002, 8.4840427295390153d-002, 9.1118686454459363d-002, &
                                                    8.9961818708732377d-002, 6.6410585969885208d-002, 8.5121858774796985d-002, &
                                                    9.4304839514197394d-002, 9.3096462053713372d-002, 8.7238042196269891d-002, &
                                                    6.2575411268049178d-002, 2.5420763854962714d-002]
!
!     --------------
!     Set up options
!     --------------
!
      CALL SetDefaultOptions(options)
      options % internalConstraint = smoothness
      options % toler              = toler
      options % whichNorm          = optType
!
!     -------------------
!     Create Winters Blob
!     -------------------
!
      xEqn = "x(t) = 4*cos(2*pi*t) - 3/5*cos(8*pi*t)^3"
      yEqn = "y(t) = 4*sin(2*pi*t) - 0.5*sin(11*pi*t)^2"
      zEqn = "z(t) = 0.0"
      ALLOCATE(blobCurve)
      CALL blobCurve % initWithEquationsNameAndID(xEqn, yEqn, zEqn, curveName = "blobCurve", id = 1)
      crv => blobCurve
!
!     ------------------------------------
!     Find the optimal curve approximation
!     ------------------------------------
!
      CALL OptimizeCurveByMarching(curve              = crv,             &
                                   polyOrder          = polyOrder,       &
                                   options            = options,         &
                                   newName            = "BlobTest",      &
                                   newID              = crv % id() + 1,  &
                                   optimized          = optimizedCurve)
!
!     --------------
!     Compute errors
!     --------------
!
      CALL castToMultiSegmentCurve(optimizedCurve, msmCurve)
      CALL ConstructGaussQuadrature(gQuad, 2*polyOrder)
      CALL SegmentErrors(exact          = crv, &
                         segmentedCurve = msmCurve, &
                         gQuad          = gQuad, &
                         errors         = errors)      
!
!     ------------
!     Check errors ! REPLACE WITH EXCEPTIONS IN FINAL CODE
!     ------------
!
      CALL FTAssert(MAXVAL(ABS(errors(:,USER_NORM) - targetErrs)) .le. testTol, msg = "Errors dont match")
      CALL FTAssert(MAXVAL(ABS(targetCuts - msmCurve % cuts))     .le. testTol, msg = "Segments dont match")
!
!     --------
!     Clean up
!     --------
!
      CALL releaseBaseCurve(optimizedCurve)
      CALL releasePECurve(blobCurve)
       
   END SUBROUTINE BlobCurveTest
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE CircleTest()
!
!  -------------------------------------------------------------------
!  Ensure that the marching optimization is consistent with previous
!  results.
!  -------------------------------------------------------------------
!
      USE CurveOptimization
      USE SMParametricEquationCurveClass
      USE FTAssertions
      IMPLICIT NONE
!
!     --------------
!     Test paramters
!     --------------
!
      INTEGER                                :: optType    = H1_NORM
      INTEGER                                :: polyOrder  = 6
      INTEGER                                :: smoothness = 2 ! C^2 smoothness
      REAL(KIND=RP)                          :: toler      = 0.10_RP
!
!     ---------------
!     Local Variables
!     ---------------
!
      CLASS(SMCurve)                 , POINTER :: crv
      TYPE(SMParametricEquationCurve), POINTER :: circleCurve
      CLASS(SMCurve)                 , POINTER :: optimizedCurve
      CLASS(MultiSegmentModalCurve)  , POINTER :: msmCurve
      TYPE(OptimizerOptions)                   :: options
      CHARACTER(LEN=64)                        :: xEqn, yEqn, zEqn
      REAL(KIND=RP)                            :: arcL
!
!     --------------
!     Set up options
!     --------------
!
      CALL SetDefaultOptions(options)
      options % internalConstraint = smoothness
      options % toler              = toler
      options % whichNorm          = optType
!
!     ---------------
!     Create a Circle
!     ---------------
!
      xEqn = "x(t) = 4*cos(2*pi*t)"
      yEqn = "y(t) = 4*sin(2*pi*t)"
      zEqn = "z(t) = 0.0"
      ALLOCATE(circleCurve)
      CALL circleCurve % initWithEquationsNameAndID(xEqn, yEqn, zEqn, curveName = "circleCurve", id = 1)
      crv => circleCurve
!
!     ------------------------------------
!     Find the optimal curve approximation
!     ------------------------------------
!
      CALL OptimizeCurveByMarching(curve              = crv,             &
                                   polyOrder          = polyOrder,       &
                                   options            = options,         &
                                   newName            = "CircleTest",    &
                                   newID              = crv % id() + 1,  &
                                   optimized          = optimizedCurve)
!
!     ------------------
!     Compute arc length
!     ------------------
!
      CALL castToMultiSegmentCurve(optimizedCurve, msmCurve)
      arcL = msmCurve % arcLength()
      CALL FTAssertEqual(expectedValue = 8.0_RP*PI, &
                         actualValue   = arcL,  &
                         relTol        = 1.0d-4, &
                         msg           = "Arc Length estimation")
!
!     --------
!     Clean up
!     --------
!
      CALL releaseBaseCurve(optimizedCurve)
      CALL releasePECurve(circleCurve)
       
   END SUBROUTINE CircleTest
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE SmoothnessCheck
      USE FTAssertions
      USE ConstrainedMultiH1Optimization
      IMPLICIT NONE
      
      CALL FTAssert(spotCheckSmoothnessConditionsIsOK(),msg = "Smoothness Continuity array")

   END SUBROUTINE SmoothnessCheck
!
!//////////////////////////////////////////////////////////////////////// 
! 
!   SUBROUTINE BlobCurveTestSetup()
!!
!!  -------------------------------------------------------------------
!!  Compute and wite out the parameters needed for the BlobCurveTest
!!  Uncomment and use to generate new results to compare against if 
!!  algorithms change.
!!  -------------------------------------------------------------------
!!
!      USE CurveOptimization
!      USE SMParametricEquationCurveClass
!      IMPLICIT NONE
!      
!      INTEGER                                :: optType    = H1_NORM
!      INTEGER                                :: polyOrder  = 6
!      INTEGER                                :: smoothness = 2 ! C^2 smoothness
!      REAL(KIND=RP)                          :: toler      = 0.1_RP
!!
!      CLASS(SMCurve)                 , POINTER :: crv
!      TYPE(SMParametricEquationCurve), POINTER :: blobCurve
!      CLASS(SMCurve)                 , POINTER :: optimizedCurve
!      CLASS(MultiSegmentModalCurve)  , POINTER :: msmCurve
!      TYPE(GaussQuadratureType)                :: gQuad
!      TYPE(OptimizerOptions)                   :: options
!      CHARACTER(LEN=64)                        :: xEqn, yEqn, zEqn
!      CHARACTER(LEN=8)                         :: exampleName = "BlobTest"
!      INTEGER                                  :: j, nSegments
!      REAL(KIND=RP)                            :: exact(3), apprx(3)
!      REAL(KIND=RP), ALLOCATABLE               :: errors(:)
!      CHARACTER(LEN=256)                       :: str
!      
!!
!!     --------------
!!     Set up options
!!     --------------
!!
!      CALL SetDefaultOptions(options)
!      options % internalConstraint = smoothness
!      options % toler              = toler
!      options % whichNorm          = optType
!!
!!     -------------------
!!     Create Winters Blob
!!     -------------------
!!
!      xEqn = "x(t) = 4*cos(2*pi*t) - 3/5*cos(8*pi*t)^3"
!      yEqn = "y(t) = 4*sin(2*pi*t) - 0.5*sin(11*pi*t)^2"
!      zEqn = "z(t) = 0.0"
!      ALLOCATE(blobCurve)
!      CALL blobCurve % initWithEquationsNameAndID(xEqn, yEqn, zEqn, curveName = "blobCurve", id = 1)
!      crv => blobCurve
!!
!!     ------------------------------------
!!     Find the optimal curve approximation
!!     ------------------------------------
!!
!      CALL OptimizeCurveByMarching(curve              = crv,             &
!                                   polyOrder          = polyOrder,       &
!                                   options            = options,         &
!                                   newName            = "BlobTest",      &
!                                   newID              = crv % id() + 1,  &
!                                   optimized          = optimizedCurve)
!!
!!     --------------
!!     Compute errors
!!     --------------
!!
!      CALL castToMultiSegmentCurve(optimizedCurve, msmCurve)
!      CALL ConstructGaussQuadrature(gQuad, 2*polyOrder)
!      CALL SegmentErrors(exact          = crv, &
!                         segmentedCurve = msmCurve, &
!                         gQuad          = gQuad, &
!                         errors         = errors)      
!!
!      str = TRIM(exampleName) // "Segments.txt"
!      OPEN(11, FILE = str)
!         WRITE(11,*) msmCurve % cuts
!         WRITE(11,*) errors
!      CLOSE(11)
!!
!!     --------
!!     Clean up
!!     --------
!!
!      CALL releaseBaseCurve(optimizedCurve)
!      CALL releasePECurve(blobCurve)
!       
!   END SUBROUTINE BlobCurveTestSetup


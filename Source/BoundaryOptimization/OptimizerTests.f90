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
      INTEGER      , ALLOCATABLE :: breakIndices(:)
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
      cuts         = [0.0_RP, 0.3_RP, 0.6_RP, 1.0_RP] ! for SMCurves, t\in [0,1]
      breakIndices = [0]
      
      CALL OptimizeCurve(curve              = crv,                   &
                         polyOrder          = polyOrder,             &
                         cuts               = cuts,                  &
                         breakIndices       = breakIndices,          &
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
      INTEGER      , ALLOCATABLE               :: breakIndices(:)
!
!     ------------------------------------------------------
!     If algorithms change, these values will have to change
!     ------------------------------------------------------
!
      REAL(KIND=RP), DIMENSION(0:14) :: targetCuts = [0.0000000000000000d0, 8.1265428894348749d-002, 0.15045634145597994d0, &
                                                      0.22077160987832684d0,0.29160351735901074d0,   0.36571566262080718d0, &
                                                      0.43544793978560420d0,0.50460410822852930d0,   0.58656579827221011d0, &
                                                      0.66015641233663280d0,0.72937541831030550d0,   0.79962654506078590d0, &
                                                      0.87133812999416993d0,0.93566906499708491d0,   1.0000000000000000d0]     
      REAL(KIND=RP), DIMENSION(14) :: targetErrs = [6.5701630229967944D-002, 9.2756354534509713D-002, 9.4318888450774865D-002, &
                                                    8.9851776388901142D-002, 8.4840637330823726D-002, 9.1113354770013449D-002, &
                                                    8.9982194651519853D-002, 6.6437188888605309D-002, 8.5125338466645842D-002, &
                                                    9.4312319154085611D-002, 9.2294223211097523D-002, 7.9708219542325251D-002, &
                                                    6.9615382669664438D-002, 6.0416666913070079D-002]
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
                                   breaks             = [0.0_RP,1.0_RP], &
                                   breakIndices       = breakIndices,    &
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
!     Check errors
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
      INTEGER                    , ALLOCATABLE :: breakIndices(:)
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
                                   breaks             = [0.0_RP,1.0_RP], &
                                   breakindices       = breakIndices,    &
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


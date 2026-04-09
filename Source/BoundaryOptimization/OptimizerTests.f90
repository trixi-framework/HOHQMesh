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
      REAL(KIND=RP), DIMENSION(0:14) :: targetCuts = [0.0000000000000000d0, 8.1265579824703116d-002, 0.15045640903455607d0, &
                                                      0.22077167956244118d0, 0.29160346317898733d0 , 0.36571564576560278d0, &
                                                      0.43544797674686886d0, 0.50460408747686858d0 , 0.58656581476102343d0, &
                                                      0.66015650714092378d0, 0.72937554977781693d0 , 0.79962664950202822d0, &
                                                      0.87133825221559236d0, 0.93566912610779118d0 , 1.0000000000000000d0]     
      REAL(KIND=RP), DIMENSION(14) :: targetErrs   = [6.5701104198703258D-002, 9.2755933527799317D-002, 9.4318673445075371D-002, &
                                                      8.9851528325016500D-002, 8.4840512767012335D-002, 9.1113375058104948D-002, &
                                                      8.9982141522456996D-002, 6.6437128781457827D-002, 8.5125422961208327D-002, &
                                                      9.4312401609089630D-002, 9.2294314749049741D-002, 7.9708234689768759D-002, &
                                                      6.9615262724751242D-002, 6.0416402664441364D-002]

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
   SUBROUTINE BlobBreaksTest()
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
      REAL(KIND=RP)                          :: toler      = 0.1_RP, testTol = 1.0d-10
      REAL(KIND=RP)                          :: testBreaks(0:3) = [0.0_RP, 0.4_RP, 0.7_RP, 1.0_RP]
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
      REAL(KIND=RP), ALLOCATABLE               :: optimizedCuts(:)
      INTEGER      , ALLOCATABLE               :: breakIndices(:)
!
!     ------------------------------------------------------
!     If algorithms change, these values will have to change
!     ------------------------------------------------------
!
      REAL(KIND=RP) :: targetCuts(14) = [0.0000000000000000d0 , 9.7012998027394590D-002,  0.18597895426900293d0, &
                                         0.2747807572302729d0 , 0.33739037861513144d0  , 0.40000000000000002d0,  &
                                         0.48775051059386526d0, 0.57664143749005436d0  , 0.63832071874502216d0,  &
                                         0.69999999999999996d0, 0.78923224973352135d0  , 0.87992823103602991d0,  &
                                         0.93996411551800996d0, 1.0000000000000000d0]
      INTEGER       :: targetIndices(3) = [6, 10, 14]
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
      CALL ConstructGaussQuadrature(gQuad, 4*polyOrder) ! For error computation. The 4 is arbitrary
      CALL FindOptimizedCuts(crv, polyOrder, testBreaks, options, gQuad, optimizedCuts, breakIndices)
!
!     ------------
!     Check errors
!     ------------
!
      CALL FTAssert(MAXVAL(ABS(optimizedCuts - targetCuts)) .le. testTol, msg = "Segments dont match")
      CALL FTAssert(MAXVAL(ABS(targetIndices - breakIndices)) == 0, msg = "Break Indices dont match")
!
!     --------
!     Clean up
!     --------
!
      CALL releasePECurve(blobCurve)
       
   END SUBROUTINE BlobBreaksTest
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
   SUBROUTINE SegmentedCurveCheck
!
!  -------------------------------------------------
!  Compute optimal segments for a half circle domain
!  -------------------------------------------------
!
      USE SMConstants
      USE SMLineClass
      USE SMChainedCurveClass
      USE SMEllipticArcClass
      USE CurveOptimization
      USE FTExceptionClass
      USE SharedExceptionManagerModule
      USE FTAssertions
      USE, INTRINSIC :: iso_fortran_env, only : stderr => ERROR_UNIT 
      IMPLICIT NONE
!
!     --------
!     Geometry
!     --------
!
      CLASS(SMLine)        , POINTER :: line
      CLASS(SMEllipticArc) , POINTER :: circle
      CLASS(SMChainedCurve), POINTER :: chain
      CLASS(SMCurve)       , POINTER :: curvePtr => NULL()
      CLASS(FTObject)      , POINTER :: obj
!
!     ------------
!     Optimization
!     ------------
!
      TYPE(GaussQuadratureType)  :: gQuad
      TYPE(OptimizerOptions)     :: options
      REAL(KIND=RP), ALLOCATABLE :: optimizedCuts(:)
      INTEGER      , ALLOCATABLE :: breakIndices(:)
      REAL(KIND=RP)              :: testBreaks(0:2)   = [0.0_RP, 0.5_RP, 1.0_RP]
      REAL(KIND=RP)              :: expectedCuts(8) = [0.0000000000000000d0, 8.5660458319249122d-002,  0.17132061069400731d0, &
                                                       0.25698120097987087d0, 0.34264177103695337d0, 0.42829034087238133d0,   &
                                                       0.50000000000000000d0, 1.0000000000000000d0]
      INTEGER                    :: expectedIndices(2) = [7, 8]
!
!     --------------
!     Test paramters
!     --------------
!
      INTEGER       :: optType    = H1_NORM
      INTEGER       :: polyOrder  = 3
      INTEGER       :: smoothness = 0 ! C^0 smoothness
      REAL(KIND=RP) :: toler      = 0.0010_RP, testTol = 1.0d-10
!
!     -----
!     Other
!     -----
!      
      TYPE(FTException), POINTER :: exception
      INTEGER                    :: errorSeverity = FT_ERROR_NONE
      
      CALL SetDefaultOptions(options)
      options % internalConstraint = smoothness
      options % toler              = toler
      options % whichNorm          = optType
!
!     --------
!     Geometry
!     --------
!
      ALLOCATE(line)
      CALL line % initWithStartEndNameAndID(xStart = [-1.0_RP,0.0_RP,0.0_RP], &
                                            xEnd   = [1.0_RP,0.0_RP,0.0_RP],  &
                                            cName  = "line", id = 1)
      ALLOCATE(circle)
      CALL circle % initWithParametersNameAndID(center     = [0.0_RP, 0.0_RP, 0.0_RP], &
                                                radius     = 1.0_RP,                   &
                                                startAngle = 0.0_RP,                   &
                                                endAngle   = PI,                       &
                                                cName      = "circle",                 &
                                                id         = 2) 
      ALLOCATE(chain)
      CALL chain % initChainWithNameAndID(chainName = "chain",id = 3)
!      
      curvePtr => circle
      CALL chain % addCurve(curvePtr)
      obj => circle
      CALL release(obj)
      
      curvePtr => line
      CALL chain % addCurve(curvePtr)
      obj => line
      CALL release(obj)
!      
      CALL chain % complete(innerOrOuterCurve = OUTER,chainMustClose = .TRUE.)
      
      IF ( catch() )     THEN
         WRITE(stderr,*) 
         WRITE(stderr,*)  "------------------------------------------------------------------"
         WRITE(stderr,*) 
         WRITE(stderr,*)  "The following errors were found when constructing the check:"
         
         DO
            exception => popLastException()
            IF ( .NOT.ASSOCIATED(exception) )     EXIT
            CALL exception % printDescription(stderr)
            errorSeverity = MAX(errorSeverity, exception % severity())
         END DO
         WRITE(stderr,*) 
         WRITE(stderr,*)  "------------------------------------------------------------------"
         WRITE(stderr,*) 
         
         IF ( errorSeverity > FT_ERROR_WARNING )     THEN
            ERROR STOP "The Errors were Fatal. Cannot generate optimized curve." 
         END IF 
      END IF 
!
!     -------------
!     Optimizations
!     -------------
!
      curvePtr => chain
      CALL ConstructGaussQuadrature(gQuad, 4*polyOrder) ! For error computation. The 4 is arbitrary
      CALL FindOptimizedCuts(curvePtr, polyOrder, testBreaks, options, gQuad, optimizedCuts, breakIndices)
!
!     ------------
!     Check errors
!     ------------
!
      CALL FTAssert(MAXVAL(ABS(optimizedCuts - expectedCuts)) .le. testTol, msg = "Segments dont match")
      CALL FTAssert(MAXVAL(ABS(expectedIndices - breakIndices)) == 0      , msg = "Break Indices dont match")

      CALL releaseChainedCurve(chain)
      
   END SUBROUTINE SegmentedCurveCheck


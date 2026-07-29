! MIT License
!
! Copyright (c) 2010-present David A. Kopriva and other contributors: AUTHORS.md
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
! FTObjectLibrary contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend,
! https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
!
! --- End License
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
      CALL castToMultiSegmentModalCurve(optimizedCurve, msmCurve)
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
!     ---------------
!     Test parameters
!     ---------------
!
      INTEGER                                :: optType    = H1_NORM
      INTEGER                                :: polyOrder  = 6
      INTEGER                                :: smoothness = 2 ! C^2 smoothness
      REAL(KIND=RP)                          :: toler      = 0.1_RP
      REAL(KIND=RP)                          :: testTol    = 2.0d-9
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
      REAL(KIND=RP), DIMENSION(0:14) :: targetCuts = [0.0000000000000000d0, 8.1265069873694071d-002, 0.15045565047752610d0, &
                                                      0.22077110332899968d0, 0.29160293158998757d0 , 0.36571518148058291d0, &
                                                      0.43544720244301666d0, 0.50460363752397241d0 , 0.58656575126088406d0, &
                                                      0.66015636956282953d0, 0.72937540809332357d0 , 0.79962651199484203d0, &
                                                      0.87133816457876168d0, 0.93566908228937584d0 , 1.0000000000000000d0]
      REAL(KIND=RP), DIMENSION(14) :: targetErrs   = [6.5702408339004884d-002, 9.2756204392096850d-002, 9.4318406193022147d-002, &
                                                      8.9851311172061341d-002, 8.4840693387210162d-002, 9.1113841663074166d-002, &
                                                      8.9981484563422739d-002, 6.6436074374897389d-002, 8.5125646477200778d-002, &
                                                      9.4312464884582489d-002, 9.2294291838550244d-002, 7.9708305374154340d-002, &
                                                      6.9615346724282365d-002, 6.0416555870896144d-002]
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
      CALL castToMultiSegmentModalCurve(optimizedCurve, msmCurve)
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
!     ---------------
!     Test parameters
!     ---------------
!
      INTEGER                                :: optType    = H1_NORM
      INTEGER                                :: polyOrder  = 6
      INTEGER                                :: smoothness = 2 ! C^2 smoothness
      REAL(KIND=RP)                          :: toler      = 0.1_RP, testTol = 2.0d-9
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
      REAL(KIND=RP) :: targetCuts(0:13) = [0.0000000000000000d0 , 9.7013000086126913d-002, 0.18597895538838266d0, &
                                           0.27478075822849279d0, 0.33739037911424141d0  , 0.40000000000000002d0, &
                                           0.48775051048802515d0, 0.57664143712361982d0  , 0.63832071856180495d0, &
                                           0.69999999999999996d0, 0.78923224968415717d0  , 0.87992823095351413d0, &
                                           0.93996411547675207d0, 1.0000000000000000d0]
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
!     ---------------
!     Test parameters
!     ---------------
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
      TYPE(GaussQuadratureType)                :: gQuad
      CHARACTER(LEN=64)                        :: xEqn, yEqn, zEqn
      REAL(KIND=RP)                            :: arcL
      INTEGER                    , ALLOCATABLE :: breakIndices(:)
      REAL(KIND=RP)              , ALLOCATABLE :: aLengths(:)

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
!     ------------------------
!     Compute total arc length
!     ------------------------
!
      CALL ConstructGaussQuadrature(gQuad, 4*polyOrder) ! The 4 is arbitrary
      CALL castToMultiSegmentModalCurve(optimizedCurve, msmCurve)
      arcL = msmCurve % arcLength(gQuad)
      CALL FTAssertEqual(expectedValue = 8.0_RP*PI, &
                         actualValue   = arcL,  &
                         relTol        = 1.0d-4, &
                         msg           = "Arc Length estimation")
!
!     -------------------------------
!     Compute arc lengths of segments
!     -------------------------------
!
      CALL curveSegmentLengths(crv, polyOrder, [0.0_RP, 0.5_RP, 1.0_RP], &
                               options, gQuad, aLengths)
      CALL FTAssertEqual(expectedValue = 4.0_RP*PI, &
                         actualValue   = aLengths(1),  &
                         relTol        = 1.0d-4, &
                         msg           = "Arc Length estimation 1")
      CALL FTAssertEqual(expectedValue = 4.0_RP*PI, &
                         actualValue   = aLengths(2),  &
                         relTol        = 1.0d-4, &
                         msg           = "Arc Length estimation 2")
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
      TYPE (SMChainedCurve), POINTER :: chain
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
      REAL(KIND=RP)              :: expectedCuts(0:7) = [0.0000000000000000d0 , 8.5660589005650267d-002, 0.17132074027521632d0, &
                                                         0.25698133016711511d0, 0.34264190234426545d0  , 0.42829047404568898d0, &
                                                         0.50000000000000000d0, 1.0000000000000000d0]
      INTEGER                    :: expectedIndices(2) = [7, 8]

!
!     ---------------
!     Test parameters
!     ---------------
!
      INTEGER       :: optType    = H1_NORM
      INTEGER       :: polyOrder  = 3
      INTEGER       :: smoothness = 0 ! C^0 smoothness
      REAL(KIND=RP) :: toler      = 0.0010_RP, testTol = 2.0d-9
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


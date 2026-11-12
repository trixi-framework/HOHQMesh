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
!      CurveOptimization.f90
!      Created: November 20, 2025 at 11:10 AM
!      By: David Kopriva
!
!  Compute an optimized approximation to a curve.
!
!  There are three main entry points:
!
!
!  (1) SUBROUTINE OptimizeCurve(curve, polyOrder, cuts, options, newName, newID, optimized)
!
!      Use this if the location of the segment boundaries given in cuts is known.
!      Returns an SMCurve subclass "optimized".
!
!  (2) SUBROUTINE OptimizeCurveWithSubdivisions(curve, polyOrder, options, newName, newID, optimized)
!
!      Use this to automatically compute the segments so that the error is is within the desired
!      tolerance, toler. Returns an SMCurve subclass "optimized". Uses subdivision algorithm
!      *** THIS ROUTINE IS INEFFICIENT COMPARED TO THE NEXT. It IS INCLUDED BUT COMMENTED OUT FOR SOME FUTURE NEED ***
!
!  (3) SUBROUTINE OptimizeCurveByMarching(curve, polyOrder, options, newName, newID, optimized)
!
!      Use this to automatically compute the segments so that the error is is within the desired
!      tolerance, toler. Returns an SMCurve subclass "optimized". Uses the marching algorithm
!
!
!////////////////////////////////////////////////////////////////////////
!
   Module CurveOptimization
   USE SMCurveClass
   USE ConstrainedMultiH1Optimization
   USE MultiSegmentModalCurveClass
   IMPLICIT NONE

   INTEGER, PARAMETER, PRIVATE :: MAX_DEPTH = 10
   INTEGER, PARAMETER          :: USER_NORM = 1, MAX_NORM   = 2, MAX_NORM_DERIV = 3
!
!  ========
   CONTAINS
!  ========
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE OptimizeCurve(curve, polyOrder, cuts, breakIndices, &
                            options, newName, newID, optimized)
!
!  ----------------------------------------------------
!  Given an SMCurve, return and optimal multi-segment
!  curve that approximates it
!  ----------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(SMCurve), POINTER :: curve              ! The curve to be approximated
      CLASS(SMCurve), POINTER :: optimized          ! The new approximation to be returned
      INTEGER                 :: polyOrder          ! polynomial order of the approximation
      REAL(KIND=RP)           :: cuts(0:)           ! Segment boundaries of the new approximation
      CHARACTER(LEN=*)        :: newName            ! Name of the new curve
      INTEGER                 :: newID              ! ID of the new curve
      TYPE(OptimizerOptions)  :: options            ! parameters for the approximation
      INTEGER, ALLOCATABLE    :: breakIndices(:)    ! index in optimizedCuts where break boundaries occur
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE(MultiH1Optimizer)                   :: multiOptimizer
      TYPE(MultiSegmentModalCurve)   , POINTER :: MSMCurve

      INTEGER                                  :: quadratureOrder
      INTEGER                                  :: nSegments
      REAL(KIND=RP), ALLOCATABLE               :: optimalCoefficients(:,:,:)
!
!     ---------------
!     Optimizer setup
!     ---------------
!
      nSegments       = SIZE(cuts) - 1
      quadratureOrder = 2*polyOrder

      CALL multiOptimizer % construct(                                 &
                                      N            = polyOrder,        &
                                      M            = quadratureOrder,  &
                                      nSegments    = nSegments,        &
                                      cuts         = cuts,             &
                                      breakIndices = breakIndices,     &
                                      options      = options)
!
!     ------------------
!     Find optimal curve
!     ------------------
!
      ALLOCATE(optimalCoefficients(0:polyOrder,2,nSegments), source = 0.0_RP)
      CALL multiOptimizer % Optimize(curve, optimalCoefficients)
!
!     --------------------------------------------------------------
!     Create a multiSegmentModalCurve from the computed coefficients
!     --------------------------------------------------------------
!
      ALLOCATE(MSMCurve)
      CALL MSMCurve % constructMultiSegmentModalCurve(curve, cuts, optimalCoefficients, newName, newID)

      optimized => MSMCurve

   END SUBROUTINE OptimizeCurve
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE OptimizeCurveByMarching(curve, polyOrder, breaks, breakIndices, &
                                      options, newName, newID, optimized)
!
!  ----------------------------------------------------------------------------------
!  March along the curve and use bisection and secant to find the length of each
!  segment so that the H^1 error is less than the tolerance allowed for that segment.
!  Clean up at the end by applying the global optimization
!  ----------------------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(SMCurve), POINTER :: curve              ! The curve to be approximated
      CLASS(SMCurve), POINTER :: optimized          ! The new approximation
      INTEGER                 :: polyOrder          ! polynomial order of the new approximation
      REAL(KIND=RP)           :: breaks(0:)         ! Location of forced cut points, starting with t = 0.0
                                                    ! If none, use breaks = [0.0_RP,1.0_RP]
      CHARACTER(LEN=*)        :: newName            ! Name of the new curve
      INTEGER                 :: newID              ! ID of the new curve
      TYPE(OptimizerOptions)  :: options            ! parameters for the approximation
      INTEGER, ALLOCATABLE    :: breakIndices(:)    ! index in optimizedCuts where break boundaries occur
!
!     ---------------
!     Local variables
!     ---------------
!
      REAL(KIND=RP), ALLOCATABLE             :: optimizedCuts(:)
      CLASS(SMCurve)               , POINTER :: optimizedCurve
      CLASS(MultiSegmentModalCurve), POINTER :: msmCurve
      TYPE(GaussQuadratureType)              :: gQuad
      TYPE(OptimizerOptions)                 :: savedOptions   ! parameters for the marching algorithm
      REAL(KIND=RP), ALLOCATABLE             :: errors(:,:)
      REAL(KIND=RP)                          :: e, errorRatio
      INTEGER                                :: n


      CALL ConstructGaussQuadrature(gQuad, 4*polyOrder) ! For error computation. The 4 is arbitrary
!
!     -----------------------------------------------------------------------
!     Save the options because for marching only end constraints will be used
!     -----------------------------------------------------------------------
!
      savedOptions            = options
      options % endConstraint = FIXED_CONSTRAINT

      DO n = 1,3 ! This is a try/try again to account for constraints not being included in marching.
!
!        ---------------------------
!        Find the optimized segments
!        ---------------------------
!
         CALL FindOptimizedCuts(curve, polyOrder, breaks, options, gQuad, &
                                optimizedCuts, breakIndices)
!
!        ------------------------------------------------------------
!        Construct the global polynomial using the optimized segments
!        ------------------------------------------------------------
!
         CALL OptimizeCurve(curve              = curve,              &
                            polyOrder          = polyOrder,          &
                            cuts               = optimizedCuts,      &
                            breakIndices       = breakIndices,       &
                            options            = savedOptions,       &
                            newName            = newName,            &
                            newID              = newID,              &
                            optimized          = optimizedCurve)
!
!        --------------------------------------------------------------------------
!        See if the error is within the requested tolerance. Since the marching can
!        only use the FIXED_CONSTRAINT option, and since errors increase as the
!        constraint order increases, the error might not be. If not, march again
!        with a tolerance that is smaller by the amount that the error is too large.
!        --------------------------------------------------------------------------
!
         CALL castToMultiSegmentModalCurve(optimizedCurve, msmCurve)
         CALL SegmentErrors(exact          = curve,    &
                            segmentedCurve = msmCurve, &
                            gQuad          = gQuad,    &
                            errors         = errors,   &
                            whichNorm      = options % whichNorm)
         e = MAXVAL(errors(:,USER_NORM))
         IF ( e .LE. savedOptions % toler )     EXIT

         errorRatio = savedOptions % toler/e
         options % toler = options % toler*errorRatio
!
         IF ( n < 3 )     THEN
            DEALLOCATE(errors)
            DEALLOCATE(optimizedCuts)
            CALL releaseMultiSegmentModalCurve(msmCurve)
         END IF

      END DO

      options   =  savedOptions
      optimized => optimizedCurve
!
   END SUBROUTINE OptimizeCurveByMarching
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE FindOptimizedCuts(curve, polyOrder, breaks, options, gQuad, &
                                optimizedCuts, breakIndices)
!
!  ----------------------------------------------------------------------------------
!  March along the curve to find the length of each
!  segment so that the H^1 error is less than the tolerance allowed for that segment.
!  ----------------------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(SMCurve), POINTER   , INTENT(IN)  :: curve              ! The curve to be approximated
      INTEGER                   , INTENT(IN)  :: polyOrder          ! polynomial order of the new approximation
      REAL(KIND=RP)             , INTENT(IN)  :: breaks(0:)         ! t Location of forced break points, starting with t = 0.0
!                                                                     If none, use [0.0_RP,1.0_RP]
      TYPE(OptimizerOptions)   ,  INTENT(IN)  :: options            ! parameters for the approximation
      TYPE(GaussQuadratureType),  INTENT(IN)  :: gQuad
      REAL(KIND=RP), ALLOCATABLE, INTENT(OUT) :: optimizedCuts(:)   ! t location of segment boundaries starting with t = 0
      INTEGER      , ALLOCATABLE, INTENT(OUT) :: breakIndices(:)    ! index in optimizedCuts where break boundaries occur
!
!     ---------------
!     Local variables
!     ---------------
!
      REAL(KIND=RP), ALLOCATABLE :: rangeCuts(:)
      INTEGER                    :: nBreaks
      INTEGER                    :: j
      REAL(KIND=RP)              :: limit = 1.0E-14_RP
      REAL(KIND=RP), ALLOCATABLE :: tmpA(:)

      nBreaks = SIZE(breaks) - 1
!
!     -------------------------------------------------------------
!     There will be at least one, and possibly more forced segments
!     -------------------------------------------------------------
!
      CALL FindOptimizedCutsInRange(curve, polyOrder, [breaks(0), breaks(1)-Limit], &
                                    options, gQuad, tmpA)
      breakIndices = [SIZE(tmpA) - 1] ! tmpA holds cuts 0:K, so the break is cut K
      tmpA(UBOUND(tmpA)) = breaks(1)
      IF(nBreaks == 1) THEN
         ALLOCATE(optimizedCuts(0:SIZE(tmpA)-1))
         optimizedCuts(0:) = tmpA
         RETURN
      END IF
!
!     ----------------------------
!     Otherwise, continue marching
!     ----------------------------
!
      DO j = 2, nBreaks
         CALL FindOptimizedCutsInRange(curve, polyOrder, [breaks(j-1)+limit, breaks(j)-limit], &
                                       options, gQuad, rangeCuts)
         breakIndices  = [breakIndices,breakIndices(j-1) + SIZE(rangeCuts) - 1]
         rangeCuts(UBOUND(rangeCuts)) = breaks(j)
         tmpA = [tmpA, rangeCuts(1:)]
         DEALLOCATE(rangeCuts)
      END DO
      ALLOCATE(optimizedCuts(0:SIZE(tmpA)-1))
      optimizedCuts(0:) = tmpA

   END SUBROUTINE FindOptimizedCuts
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE FindOptimizedCutsInRange(curve, polyOrder, tRange, options, gQuad, optimizedCuts)
!
!  ----------------------------------------------------------------------------------
!  March along the curve to find the length of each
!  segment so that the H^1 error is less than the tolerance allowed for that segment.
!  ----------------------------------------------------------------------------------
!
      USE ErrorTypesModule
      USE FTExceptionClass, ONLY: FT_ERROR_WARNING
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(SMCurve), POINTER    :: curve              ! The curve to be approximated
      INTEGER                    :: polyOrder          ! polynomial order of the new approximation
      REAL(KIND=RP)              :: tRange(2)          ! Location of forced cut points, starting with t = 0.0
      TYPE(OptimizerOptions)     :: options            ! parameters for the approximation
      TYPE(GaussQuadratureType)  :: gQuad
      REAL(KIND=RP), ALLOCATABLE :: optimizedCuts(:)
!
!     ---------------
!     Local variables
!     ---------------
!
      REAL(KIND=RP)                          :: cuts(0:1)
      REAL(KIND=RP)                          :: fR, h, h2
      REAL(KIND=RP)                          :: tL, tCut, tR
      REAL(KIND=RP)                          :: minStep
      REAL(KIND=RP)                          :: e, hStar, cFactor, targetError
      INTEGER                                :: k, nCuts
      CHARACTER(LEN=2*DEFAULT_CHARACTER_LENGTH) :: msg
!
!     ---------------------------------------------------------------
!     Segments shorter than minStep are not resolved by the marching.
!     It is relative to the range so that the result does not depend
!     on how many curves are in the chain.
!     ---------------------------------------------------------------
!
      REAL(KIND=RP), PARAMETER               :: MIN_STEP_FRACTION = 1.0E-7_RP

      ALLOCATE(optimizedCuts(0:1))
      optimizedCuts = tRange  ! will be added to as marching progresses

      tL      = tRange(1)
      tR      = tRange(2)
      cuts    = [tL, tR] ! = [t_{k-1},t_k]
      minStep = MIN_STEP_FRACTION*(tR - tL)
!
!     ------------------------------------------------------------------
!     If the approximation already satisfies the approximation tolerance
!     then the marchingFunction will be negative. Positive means
!     the tolerance is not met and a search must be done
!     ------------------------------------------------------------------
!
      fR = marchingFunction(curve, polyOrder, cuts, options, gQuad)
      IF(fR .le. 0.0_RP) RETURN
!
!     ---------------------------------------
!     Loop until hit the end at t = tRange(2)
!     ---------------------------------------
!
      k = 0 ! Segment number
      DO
         tCut = iterate(tL, tR, fR, minStep, curve, polyOrder, options, gQuad)
!
!        ------------------------------------------------------------
!        No segment of resolvable length starting at tL satisfies the
!        tolerance. Stop here rather than add zero length segments.
!        ------------------------------------------------------------
!
         IF ( tCut - tL < minStep )     THEN
            WRITE(msg,'(A,ES10.3,A,F9.6,A)') "The tolerance ", options % toler,   &
                                " cannot be met by a segment starting at t = ", tL, &
                                ". The tolerance may be too small for this curve."
            CALL ThrowErrorExceptionOfType(poster = "FindOptimizedCutsInRange", &
                                           msg    = msg,                        &
                                           typ    = FT_ERROR_WARNING)
            EXIT
         END IF
!
!        -------------------------------------------------------------
!        The segment reaches the end of the range to within what can be
!        resolved. Don't leave a sliver.
!        -------------------------------------------------------------
!
         IF ( tR - tCut < minStep )     EXIT
!
!        ----------------------------------------------------
!        Converged...add this point to the cuts, and prepare
!        for the next.
!        We're done if the remaining segment has small enough
!        errors
!        ----------------------------------------------------
!
         k = k + 1
         CALL addCut(cuts = optimizedCuts, valueToAdd = tCut, atIndex = k)
         tL   = tCut
         cuts = [tL,tR]

         fR = marchingFunction(curve, polyOrder, cuts, options, gQuad)
         IF(fR .le. 0.0_RP)    EXIT

      END DO !Stepping
!
!     --------------------------------------------------------------------
!     The last segment may be very small since the marching knows nothing
!     about what lies ahead. Use the last error and the fact that the
!     the H^1 error should vary like h^{p} to estimate the segment size
!     needed to produce the desired error. Choose the smaller of this
!     segment size and half of the last two segments to balance the error
!     in the last two segments. Don't bother with changes that are less
!     than 25%, which is a completely arbitrary number.
!     --------------------------------------------------------------------
!
      nCuts = SIZE(optimizedCuts) - 1
      IF ( nCuts < 2 )     RETURN ! Only one segment, so there is nothing to balance

      h2          = optimizedCuts(nCuts) - optimizedCuts(nCuts-2)
      h           = optimizedCuts(nCuts) - optimizedCuts(nCuts-1)
      cuts        = [optimizedCuts(nCuts-1), optimizedCuts(nCuts)]
      targetError = options % safetyFactor*options % toler
      e           = marchingFunction(curve, polyOrder, cuts, options, gQuad) + targetError
      cFactor     = (targetError/(e + 1.0E-14_RP))**(1.0_RP/polyOrder)

      IF ( cFactor > 1.25_RP )     THEN
         hStar       = h*cFactor
         optimizedCuts(nCuts-1) = optimizedCuts(nCuts) - MIN(hStar, 0.5_RP*h2)
      END IF

   END SUBROUTINE FindOptimizedCutsInRange
!
!////////////////////////////////////////////////////////////////////////
!
   REAL(KIND=RP) FUNCTION iterate(tL, tR, fR, minStep, curve, polyOrder, options, gQuad)
!
!  -----------------------------------------------------------------------------------
!  Find the end, t, of the longest segment [tL,t] in [tL,tR] whose error is within the
!  tolerance, i.e. find the root of the marchingFunction, f, which is negative when
!  the tolerance is met.
!
!  The search keeps a bracket [a,b] with f(a) <= 0 < f(b), and always returns a, the
!  side that satisfies the tolerance. It starts with a = tL, where the segment has no
!  length and no error, and with b = tR, where the caller has found f = fR > 0.
!  If no point within the tolerance is found, the result is tL, which the caller
!  must check for.
!
!  The error grows like a high power of the segment length, so the secant through
!  the ends of the starting bracket says nothing useful about where the root is.
!  Therefore the segment is first halved until it meets the tolerance, which gives a
!  bracket no more than a factor of two wide with computed values at both ends.
!  After that, new points come from the Pegasus method, a fast modification of
!  Regula Falsi. See
!
!     Dowell, M., Jarratt, P. (1972)
!     The "Pegasus" method for computing the root of an equation.
!     https://doi.org/10.1007/BF01932959
!
!  A Pegasus point is used only if it falls inside of the bracket, and bisection
!  is used for the next point any time a step leaves more than three quarters of the
!  bracket, so the bracket shrinks to minStep in a bounded number of steps.
!  -----------------------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      REAL(KIND=RP)             :: tL, tR             ! The range to search
      REAL(KIND=RP)             :: fR                 ! marchingFunction on [tL,tR]. Must be > 0
      REAL(KIND=RP)             :: minStep            ! Smallest bracket size to resolve
      CLASS(SMCurve), POINTER   :: curve              ! The curve to be approximated
      INTEGER                   :: polyOrder          ! polynomial order of the new approximation
      TYPE(OptimizerOptions)    :: options            ! parameters for the approximation
      TYPE(GaussQuadratureType) :: gQuad
!
!     ---------------
!     Local variables
!     ---------------
!
      REAL(KIND=RP)   :: cuts(0:1)
      REAL(KIND=RP)   :: a, b, c          ! The bracket and the new point
      REAL(KIND=RP)   :: fA, fC           ! marchingFunction at a and c
      REAL(KIND=RP)   :: gA, gB           ! Pegasus scaled function values, for interpolation only
      REAL(KIND=RP)   :: targetError, bracketSize
      INTEGER         :: n, lastMoved
      LOGICAL         :: bisectNext
!
!     -----------------------------------------------------------------
!     Stop when the error at a is within F_TOLERANCE of the target, as
!     a fraction of the target. The bracket shrinks by at least a
!     quarter every other step, so MAX_ITERATIONS is only a backstop.
!     -----------------------------------------------------------------
!
      REAL(KIND=RP), PARAMETER :: F_TOLERANCE    = 1.0E-3_RP
      INTEGER      , PARAMETER :: MAX_ITERATIONS = 200
      INTEGER      , PARAMETER :: NEITHER = 0, MOVED_A = 1, MOVED_B = 2

      targetError = options % safetyFactor * options % toler

      a  = tL; fA = -targetError ! No length, so no error. See marchingFunction
      b  = tR
      gA = fA; gB = fR

      cuts       = [tL, tR]
      lastMoved  = NEITHER
      bisectNext = .FALSE.

      DO n = 1, MAX_ITERATIONS

         bracketSize = b - a
         IF ( bracketSize <= minStep )              EXIT
         IF ( fA >= -F_TOLERANCE*targetError )      EXIT
!
!        ----------------------------------------------------------------
!        Choose the next point. Bisect while a is still at tL, i.e. until
!        some segment has met the tolerance, and after any step that left
!        more than 3/4 of the bracket. Otherwise use the Pegasus point,
!        the intersection of the secant line with the t axis.
!        ----------------------------------------------------------------
!
         IF ( bisectNext .OR. (a <= tL) )     THEN
            c = a + 0.5_RP*bracketSize       ! Bisection step
         ELSE
            c = (gB*a - gA*b)/(gB - gA)      ! Pegasus step
         END IF

!        The point must be strictly inside the bracket. If not, e.g. from
!        rounding or a NaN (which fails both comparisons), bisect instead.
         IF ( .NOT.((c > a) .AND. (c < b)) )   c = a + 0.5_RP*bracketSize

         cuts(1) = c
         fC      = marchingFunction(curve, polyOrder, cuts, options, gQuad)
!
!        ---------------------------------------------------------------
!        Update the bracket. A NaN counts as not meeting the tolerance.
!        When the same end is replaced twice in a row, Pegasus scales the
!        function value at the end that stays.
!        ---------------------------------------------------------------
!
         IF ( fC <= 0.0_RP )     THEN
            IF ( lastMoved == MOVED_A )     THEN
               IF( gA + fC < 0.0_RP ) gB = gB*gA/(gA + fC)
            END IF
            a  = c; fA = fC; gA = fC
            lastMoved = MOVED_A
         ELSE
            IF ( lastMoved == MOVED_B )     THEN
               IF( gB + fC > 0.0_RP ) gA = gA*gB/(gB + fC)
            END IF
            b  = c; gB = fC
            lastMoved = MOVED_B
         END IF
!
!        ------------------------------------------------------------------
!        The test is against 3/4 and not 1/2 of the bracket on purpose. A
!        bisection step leaves exactly half, to within rounding, so a test
!        against 1/2 is decided by the last bit. The points chosen, and so
!        the cuts, would then differ from one platform to another.
!        ------------------------------------------------------------------
!
         bisectNext = (b - a) > 0.75_RP*bracketSize

      END DO

      iterate = a

   END FUNCTION iterate
!
!////////////////////////////////////////////////////////////////////////
!
   FUNCTION marchingFunction(curve, polyOrder, cuts, options, gQuad) RESULT(f)
      IMPLICIT NONE
!
!     ----------
!     Arguments
!     ----------
!
      CLASS(SMCurve), POINTER    :: curve              ! The curve to be approximated
      INTEGER                    :: polyOrder          ! polynomial order of the new approximation
      REAL(KIND=RP)              :: cuts(0:1)
      TYPE(OptimizerOptions)     :: options            ! parameters for the marching algorithm
      TYPE(GaussQuadratureType)  :: gQuad
!
!     ------------
!     Return value
!     ------------
!
      REAL(KIND=RP) :: f
!
!     ---------------
!     Local variables
!     ---------------
!
      REAL(KIND=RP), ALLOCATABLE             :: errors(:,:)
      CALL curveSegmentErrors(curve, polyOrder, cuts, options, gQuad, errors)
      f = errors(1,USER_NORM) - options % safetyFactor*options % toler

   END FUNCTION marchingFunction
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE curveSegmentErrors(curve, polyOrder, cuts, options, gQuad, errors)
      IMPLICIT NONE
!
!     ----------
!     Arguments
!     ----------
!
      CLASS(SMCurve), POINTER    :: curve              ! The curve to be approximated
      INTEGER                    :: polyOrder          ! polynomial order of the new approximation
      REAL(KIND=RP)              :: cuts(0:1)
      TYPE(GaussQuadratureType)  :: gQuad
      REAL(KIND=RP), ALLOCATABLE :: errors(:,:)
!
!     ---------------
!     Local variables
!     ---------------
!
      CLASS(SMCurve)               , POINTER :: optimizedCurve
      CLASS(MultiSegmentModalCurve), POINTER :: msmCurve
      TYPE(OptimizerOptions)                 :: options  ! parameters for the marching algorithm
      INTEGER, ALLOCATABLE                   :: breakIndices(:)

      breakIndices = [0]
      CALL OptimizeCurve(curve        = curve,                   &
                         polyOrder    = polyOrder,               &
                         cuts         = cuts,                    &
                         breakIndices = breakIndices,            &
                         options      = options,                 &
                         newName      = "tmp",                   &
                         newID        = 0,                       &
                         optimized    = optimizedCurve)

      CALL castToMultiSegmentModalCurve(optimizedCurve, msmCurve)
      CALL SegmentErrors(exact          = curve,    &
                         segmentedCurve = msmCurve, &
                         gQuad          = gQuad,    &
                         errors         = errors,   &
                         whichNorm      = options % whichNorm)

      CALL releaseBaseCurve(optimizedCurve)

   END SUBROUTINE curveSegmentErrors
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE curveSegmentLengths(curve, polyOrder, cuts, options, gQuad, lengths)
!
!  -----------------------------------------------------
!  Collect the arc lengths of the optimal curve segments
!  -----------------------------------------------------
!
      IMPLICIT NONE
!
!     ----------
!     Arguments
!     ----------
!
      CLASS(SMCurve), POINTER    :: curve              ! The curve to be approximated
      INTEGER                    :: polyOrder          ! polynomial order of the new approximation
      REAL(KIND=RP)              :: cuts(0:)
      TYPE(GaussQuadratureType)  :: gQuad
      REAL(KIND=RP), ALLOCATABLE :: lengths(:)
!
!     ---------------
!     Local variables
!     ---------------
!
      CLASS(SMCurve)               , POINTER :: optimizedCurve
      CLASS(MultiSegmentModalCurve), POINTER :: msmCurve
      TYPE(OptimizerOptions)                 :: options  ! parameters for the marching algorithm
      INTEGER, ALLOCATABLE                   :: breakIndices(:)
      INTEGER                                :: nSegments
      INTEGER                                :: j
!
!     ------------------------------
!     find the optimal approximation
!     ------------------------------
!
      breakIndices = [0]
      CALL OptimizeCurve(curve        = curve,                   &
                         polyOrder    = polyOrder,               &
                         cuts         = cuts,                    &
                         breakIndices = breakIndices,            &
                         options      = options,                 &
                         newName      = "tmp",                   &
                         newID        = 0,                       &
                         optimized    = optimizedCurve)

      CALL castToMultiSegmentModalCurve(optimizedCurve, msmCurve)
!
!     ---------------------------------------
!     Compute the arc lengths of each segment
!     ---------------------------------------
!
      nSegments = msmCurve % nSegments
      ALLOCATE(lengths(nSegments))
      DO j = 1, nSegments
         lengths(j) = segmentArcLength(msmCurve, j ,gQuad % nodes, gQuad % weights)
      END DO
      CALL releaseBaseCurve(optimizedCurve)

   END SUBROUTINE curveSegmentLengths
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE SegmentErrors(exact, segmentedCurve, gQuad, errors, whichNorm)
!
!  -------------------------------------------------------------------
!  Compute the errors in each of the segments of a segmented curve.
!  The USER_NORM column is the L2 or H1 (default) error, according to
!  whichNorm, per unit arc length of the segment. It is the same
!  measure as reported by ComputeBoundaryErrors, so the tolerance
!  means the same thing when choosing segments as when checking them.
!  -------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(SMCurve)               , POINTER :: exact
      CLASS(MultiSegmentModalCurve), POINTER :: segmentedCurve
      TYPE(GaussQuadratureType)              :: gQuad
      REAL(KIND=RP), ALLOCATABLE             :: errors(:,:)
      INTEGER, OPTIONAL                      :: whichNorm
!
!     ----------------
!     Local variables
!     ----------------
!
      INTEGER       :: nSegments
      INTEGER       :: k
      INTEGER       :: normType

      normType = H1_NORM
      IF(PRESENT(whichNorm)) normType = whichNorm

      nSegments = segmentedCurve % nSegments
      ALLOCATE(errors(nSegments,3), source = 0.0_RP)

      DO k = 1, nSegments
         errors(k,:) =  segmentError(k, exact, segmentedCurve, gQuad, normType)
      END DO

   END SUBROUTINE SegmentErrors
!
!////////////////////////////////////////////////////////////////////////
!
   FUNCTION segmentError(k, exact, segmentedCurve, gQuad, whichNorm) RESULT(er)
!
!  ---------------------------------------------------------------------
!  Compute the errors on the k'th segment of a segmented curve. The
!  USER_NORM error is normalized by the arc length of the segment so
!  that, like the pointwise error, it scales as h^N and does not depend
!  on the length of the segment itself.
!  ---------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER                                :: k
      CLASS(SMCurve)               , POINTER :: exact
      CLASS(MultiSegmentModalCurve), POINTER :: segmentedCurve
      TYPE(GaussQuadratureType)              :: gQuad
      INTEGER                                :: whichNorm
      REAL(KIND=RP)                          :: er(3)
!
!     ----------------
!     Local variables
!     ----------------
!
      INTEGER       :: qOrder
      INTEGER       :: j
      REAL(KIND=RP) :: t, h, dsdt, arc
      REAL(KIND=RP) :: e, e0(3), e1(3), eMax, eMaxDeriv

      qOrder = gQuad % N

      e         = 0.0_RP
      eMax      = 0.0_RP
      eMaxDeriv = 0.0_RP
      h         = segmentedCurve % cuts(k) - segmentedCurve % cuts(k-1)
      dsdt      = 2.0_RP/h
      DO j = 0, qOrder
         t    = segmentedCurve % cuts(k-1) + h*0.5_RP*(gQuad % nodes(j) + 1.0_RP)
         e0   = (exact % positionAt(t)   - segmentedCurve % valueInSegment(k, t, which = LA_EVALUATE_FUNCTION))**2
         e1   = (exact % derivativeAt(t) - dsdt*segmentedCurve % valueInSegment(k, t, which = LA_EVALUATE_DERIVATIVE))**2

         IF ( whichNorm == L2_NORM )     THEN
            e = e + (e0(1) + e0(2))*gQuad % weights(j)
         ELSE
            e = e + (e0(1) + e0(2) + e1(1) + e1(2))*gQuad % weights(j)
         END IF
         eMax      = MAX(eMax,e0(1),e0(2))
         eMaxDeriv = MAX(eMaxDeriv,e1(1),e1(2))
      END DO
      arc                 = segmentArcLength(segmentedCurve, k, gQuad % nodes, gQuad % weights)
      er(USER_NORM)       = SQRT(0.5_RP*h*e/arc)
      er(MAX_NORM)        = SQRT(eMax)
      er(MAX_NORM_DERIV)  = SQRT(eMaxDeriv)

   END FUNCTION segmentError
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE addCut(cuts, valueToAdd, atIndex)
!
!     ----------------------------------------------------------
!     Cuts is dimensioned as (0:N), but there seems to be no way
!     with an allocatable to note that in the declaration here.
!     So work with shifted indices, shifted by 1
!     ----------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      REAL(KIND=RP), ALLOCATABLE :: cuts(:)
      REAL(KIND=RP) :: valueToAdd
      INTEGER       :: atIndex
!
!     ---------------
!     Local variables
!     ---------------
!
      REAL(KIND=RP), ALLOCATABLE :: newCuts(:)
      INTEGER                    :: N

      N = SIZE(cuts)
      ALLOCATE(newCuts(0:N))
      newCuts(0:atIndex-1)  = cuts(0:atIndex-1)
      newCuts(atIndex+1:)   = cuts(atIndex:)
      newCuts(atIndex)      = valueToAdd

      CALL MOVE_ALLOC(FROM = newCuts, TO = cuts)

   END SUBROUTINE addCut
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE AppendRealArray(base, arrayToAdd)
      IMPLICIT NONE
      REAL(KIND=RP), ALLOCATABLE :: base(:)
      REAL(KIND=RP)              :: arrayToAdd(:)
      base = [base,arrayToAdd]

   END SUBROUTINE AppendRealArray
!
!////////////////////////////////////////////////////////////////////////
!
!   SUBROUTINE OptimizeCurveWithSubdivisions(curve, polyOrder, options, newName, newID, optimized)
!      IMPLICIT NONE
!!
!!     ---------
!!     Arguments
!!     ---------
!!
!      CLASS(SMCurve), POINTER :: curve              ! The curve to be approximated
!      CLASS(SMCurve), POINTER :: optimized          ! The new approximation
!      INTEGER                 :: polyOrder          ! polynomial order of the new approximation
!      CHARACTER(LEN=*)        :: newName            ! Name of the new curve
!      INTEGER                 :: newID              ! ID of the new curve
!      TYPE(OptimizerOptions)  :: options            ! parameters for the approximation
!!
!!     ---------------
!!     Local variables
!!     ---------------
!!
!      INTEGER                                :: lev, j, i
!      INTEGER                                :: offset
!      INTEGER                                :: nSegments
!      REAL(KIND=RP), ALLOCATABLE             :: cuts(:)
!      CLASS(SMCurve)               , POINTER :: optimizedCurve
!      CLASS(MultiSegmentModalCurve), POINTER :: msmCurve
!      TYPE(GaussQuadratureType)              :: gQuad
!      REAL(KIND=RP), ALLOCATABLE             :: errors(:)
!      REAL(KIND=RP)                          :: tMid
!      LOGICAL                                :: subdivisionIsFinished
!
!      CALL ConstructGaussQuadrature(gQuad, 4*polyOrder)
!!
!!     ------------------
!!     Start at top level
!!     ------------------
!!
!      nSegments = 1
!      ALLOCATE(cuts(0:nSegments))
!      cuts = [0.0_RP, 1.0_RP]
!
!      CALL OptimizeCurve(curve     = curve,                       &
!                         polyOrder = polyOrder,                   &
!                         cuts      = cuts,                        &
!                         options   = options,                     &
!                         newName   = newName,                     &
!                         newID     = newID,                       &
!                         optimized = optimizedCurve)
!
!      CALL castToMultiSegmentModalCurve(optimizedCurve, msmCurve)
!      CALL SegmentErrors(exact          = curve,    &
!                         segmentedCurve = msmCurve, &
!                         gQuad          = gQuad,    &
!                         errors         = errors)
!      DO lev = 1, MAX_DEPTH
!         subdivisionIsFinished = .TRUE.
!!
!!        -------------------------------------
!!        Compute new subdivisions as necessary
!!        -------------------------------------
!!
!         offset = 0
!         DO j = 1, UBOUND(errors,1)
!            i = j + offset
!            IF ( errors(j) > options % toler )     THEN
!               tMid = 0.5_RP*(cuts(i) + cuts(i-1))
!               CALL addCut(cuts,valueToAdd = tMid, atIndex = i)
!               offset = offset + 1
!               subdivisionIsFinished = .FALSE.
!            END IF
!         END DO
!         IF(subdivisionIsFinished) EXIT
!!
!!        ----------------------------------
!!        Compute curves on new subdivisions
!!        ----------------------------------
!!
!         CALL releaseBaseCurve(optimizedCurve)
!         DEALLOCATE(errors)
!         CALL OptimizeCurve(curve              = curve,              &
!                            polyOrder          = polyOrder,          &
!                            cuts               = cuts,               &
!                            options            = options,            &
!                            newName            = newName,            &
!                            newID              = newID,              &
!                            optimized          = optimizedCurve)
!
!         CALL castToMultiSegmentModalCurve(optimizedCurve, msmCurve)
!         CALL SegmentErrors(exact          = curve,    &
!                            segmentedCurve = msmCurve, &
!                            gQuad          = gQuad,    &
!                            errors         = errors)
!
!      END DO
!      optimized => optimizedCurve
!
!   END SUBROUTINE OptimizeCurveWithSubdivisions

   END Module CurveOptimization

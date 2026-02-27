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
   INTEGER, PARAMETER, PRIVATE :: LEFT_SIDE = 0, RIGHT_SIDE = 1
   INTEGER, PARAMETER          :: USER_NORM = 1, MAX_NORM   = 2, MAX_NORM_DERIV = 3
!
!  ========
   CONTAINS
!  ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE OptimizeCurve(curve, polyOrder, cuts, options, newName, newID, optimized)
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
!     --------------
!     Optimzer setup
!     --------------
!
      nSegments       = SIZE(cuts) - 1
      quadratureOrder = 2*polyOrder
      
      CALL multiOptimizer % construct(                              &
                                      N         = polyOrder,        &
                                      M         = quadratureOrder,  &
                                      nSegments = nSegments,        &
                                      cuts      = cuts,             &
                                      options   = options)
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
      CALL MSMCurve % construct(curve, cuts, optimalCoefficients, newName, newID)

      optimized => MSMCurve

   END SUBROUTINE OptimizeCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE OptimizeCurveByMarching(curve, polyOrder, options, newName, newID, optimized)
!
!  -----------------------------------------------------------------------------------
!  March along the curve and use bisection and secant to find the length of each
!  segement so that the H^1 error is less than the tolerance allowed for that segment.
!  Clean up at the end by applying the global optimization
!  -----------------------------------------------------------------------------------
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
      CHARACTER(LEN=*)        :: newName            ! Name of the new curve
      INTEGER                 :: newID              ! ID of the new curve
      TYPE(OptimizerOptions)  :: options            ! parameters for the approximation
!
!     ---------------
!     Local variables
!     ---------------
!
      REAL(KIND=RP), ALLOCATABLE             :: optimizedCuts(:)
      CLASS(SMCurve)               , POINTER :: optimizedCurve
      CLASS(MultiSegmentModalCurve), POINTER :: msmCurve
      TYPE(GaussQuadratureType)              :: gQuad
      TYPE(OptimizerOptions)                 :: savedOptions  ! parameters for the marching algorithm
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
         CALL FindOptimizedCuts(curve, polyOrder, options, gQuad, optimizedCuts)
!
!        ------------------------------------------------------------
!        Construct the global polynomial using the optimized segments
!        ------------------------------------------------------------
!
         CALL OptimizeCurve(curve              = curve,              &
                            polyOrder          = polyOrder,          &
                            cuts               = optimizedCuts,      &
                            options            = savedOptions,       &
                            newName            = newName,            &
                            newID              = newID,              &
                            optimized          = optimizedCurve)
!
!        --------------------------------------------------------------------------
!        See if the error is within tolerance. Since the marching can only use the 
!        FIXED_CONSTRAINT option, and since errors increase as the constraint
!        order increases, then the error won't be within the tolerance. If not,
!        create a new tolerance that is small enough so that when done we should
!        be OK
!        --------------------------------------------------------------------------
!
         CALL castToMultiSegmentCurve(optimizedCurve, msmCurve)
         CALL SegmentErrors(exact          = curve,    &
                            segmentedCurve = msmCurve, &
                            gQuad          = gQuad,    &
                            errors         = errors)
         e = MAXVAL(errors(:,USER_NORM))
         IF ( e .le. options % toler )     EXIT

         errorRatio = options % toler/e
         options % toler = options % toler*errorRatio
!         IF(errorRatio < 1.0_RP) WRITE(0,*) "Error required adjusting", e, errorRatio, TRIM(curve % curveName())
!
         IF ( n < 3 )     THEN
            DEALLOCATE(errors)
            DEALLOCATE(optimizedCuts)
            CALL releaseMultiSegmentModalCurve(msmCurve)
         END IF 

      END DO

      optimized => optimizedCurve
!
   END SUBROUTINE OptimizeCurveByMarching
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE FindOptimizedCuts(curve, polyOrder, options, gQuad, optimizedCuts)
!
!  -----------------------------------------------------------------------------------
!  March along the curve to find the length of each
!  segement so that the H^1 error is less than the tolerance allowed for that segment.
!  -----------------------------------------------------------------------------------
!
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(SMCurve), POINTER    :: curve              ! The curve to be approximated
      INTEGER                    :: polyOrder          ! polynomial order of the new approximation
      TYPE(OptimizerOptions)     :: options            ! parameters for the approximation
      TYPE(GaussQuadratureType)  :: gQuad
      REAL(KIND=RP), ALLOCATABLE :: optimizedCuts(:)
!
!     ---------------
!     Local variables
!     ---------------
!
      LOGICAL                                :: steppingNotDone
      REAL(KIND=RP)                          :: cuts(0:1)
      REAL(KIND=RP)                          :: iterToler = 1.0d-6
      REAL(KIND=RP)                          :: fR, h, f
      REAL(KIND=RP)                          :: tL, tMid, tR
      INTEGER                                :: k

      ALLOCATE(optimizedCuts(0:1))
      optimizedCuts = [0.0_RP,1.0_RP]  ! will be added to as marching progresses
!
!     ---------------------------------
!     Loop until hit the end at t = 1.0
!     ---------------------------------
!
      tL              = 0.0_RP
      tR              = 1.0_RP
      cuts            = [tL, tR] ! = [t_{k-1},t_k]
      h               = tR - tL 
!
!     ------------------------------------------------------------------
!     If the approximation already satisfies the approximation tolerance
!     then the marchingfunction will be negative. Positive means 
!     the tolerance is not met and a search must be done
!     fR = marchingfunction(curve, polyOrder, cuts, options, gQuad)
!     ------------------------------------------------------------------
!
      fR = marchingfunction(curve, polyOrder, cuts, options, gQuad)
      IF(ABS(fR) .le. 0.0) RETURN 
!
!     --------------------------
!     Find the intervals in turn
!     --------------------------
!
      steppingNotDone = .TRUE.
      k               = 0 ! Segment number
      DO WHILE(steppingNotDone)
         k    = k + 1
         tMid = iterate(tL, tR, curve, polyOrder, options, gQuad, iterToler, RIGHT_SIDE)
!
!        -------------------------------------------------
!        converged... add this point to the segments array
!        and prepare for the next
!        -------------------------------------------------
!
         CALL addCut(cuts = optimizedCuts,valueToAdd = tMid, atIndex = k)
         tL   = tMid
         tR   = 1.0_RP
         cuts = [tL,tR]
!
!        ----------------------------------------------------
!        We're done if the remaining segment has small enough
!        errors
!        ----------------------------------------------------
!
         f = marchingFunction(curve, polyOrder, cuts, options, gQuad)
         IF(f .le. 0.0_RP)    EXIT  

      END DO !Stepping
!
!     --------------------------------------------------------------------
!     The last segment may be very small since the marching knows nothing
!     about what lies ahead. Find the last segment size by starting at the
!     right and moving left. Then choose something inbetween for the last
!     two segments
!     --------------------------------------------------------------------
!
!      tL   = 0.0_RP !optimizedCuts(UBOUND(optimizedCuts,1)-2) !Two segments away
!      tR   = 1.0_RP  
!      tMid = iterate(tL, tR, curve, polyOrder, options, gQuad, iterToler, LEFT_SIDE) !DEBUG
!!      k    = UBOUND(optimizedCuts,1)
!!      hNew = 1.0_RP - tMid
!!      h    = optimizedCuts(k-1) - optimizedCuts(k-2)
!      WRITE(0,*) optimizedCuts
!      WRITE(0,*) tMid
!!      IF ( hNew .ge. h )     THEN
!!         h = 0.5_RP*h 
!!      ELSE 
!!         h = hNew 
!!      END IF 
!!!      optimizedCuts(k-1) = 1.0_RP - h
!
   END SUBROUTINE FindOptimizedCuts
!
!//////////////////////////////////////////////////////////////////////// 
! 
   REAL(KIND=RP) FUNCTION iterate(tL, tR, curve, polyOrder, options, gQuad, iterToler, side)
!
!  -----------------------------------------------------------------------------------
!  Iterate to find the point t_{k+1} such that the error on [t_k,t_{k+1}] is less than
!  iterToler.
!  -----------------------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      REAL(KIND=RP)             :: tL, tR
      CLASS(SMCurve), POINTER   :: curve              ! The curve to be approximated
      INTEGER                   :: polyOrder          ! polynomial order of the new approximation
      TYPE(OptimizerOptions)    :: options            ! parameters for the approximation
      TYPE(GaussQuadratureType) :: gQuad
      REAL(KIND=RP)             :: iterToler
      INTEGER                   :: side
!
!     ---------------
!     Local variables
!     ---------------
!
      REAL(KIND=RP)   :: cuts(0:1)
      REAL(KIND=RP)   :: fL, fR, delta, s
      REAL(KIND=RP)   :: tN, tNm1, fN, fNm1, tNp1
      INTEGER         :: maxIterB = 6 ! Should get to 0.016
      INTEGER         :: maxIterS = 8 ! Should get to iterToler
      INTEGER         :: maxIter
      INTEGER         :: n
      LOGICAL         :: done
      
      cuts = [tL, tR] ! = [t_{k-1},t_k]
      iterate = bisect(maxIterB, side, done)
      IF ( done ) RETURN
!
!     -----------------------
!     Secant method to finish
!     -----------------------
!
      tN   = tR; fN    = fR
      tNm1 = tL; fNm1 = fL
      
      DO n = 1, maxIterS
         delta   = - (tn - tNm1)*fn/(fn - fNm1)
         s       = SIGN(1.0_RP,delta)
         delta   = s*MIN(ABS(0.1_RP*tN),ABS(delta))
         tNp1    = tn + delta
         tnm1    = tn
         fNm1    = fn

         IF ( ABS(delta) <= iterToler )     THEN
            iterate = tNp1
            RETURN 
         END IF 
 
         cuts(1) = tNp1
         fn      = marchingFunction(curve, polyOrder, cuts, options, gQuad)
         tn      = tNp1
      END DO 
!
!     ---------------------------------------------------------------
!     If secant doesn't converge, we get to here. Just use bisection
!     to finish the job.
!     ---------------------------------------------------------------
!
      maxIter = NINT(LOG((tR - tL)/iterToler)/LOG(2.0_RP))+1
      iterate = bisect(maxIter, side, done)
!
!     ========
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
   REAL(KIND=RP) FUNCTION bisect(maxIter, side, done)
!
!  -----------------------------------------------------------------------------------
!  Iterate to find the point t_{k+1} such that the error on [t_k,t_{k+1}] is less than
!  iterToler.
!  -----------------------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER                   :: maxIter
      LOGICAL                   :: done
      INTEGER                   :: side
!
!     ---------------
!     Local variables
!     ---------------
!
      REAL(KIND=RP)   :: tMid, h
      REAL(KIND=RP)   :: fMid
      INTEGER         :: n

!
!    -----------------------------------------------------
!    Iterate on error funtion until tolerance is satisfied
!    -----------------------------------------------------
!
      fL = - options % safetyFactor*options % toler ! the best case. See marchingFunction
!
!     ---------
!     Bisection
!     ---------
!
      done = .FALSE.
      DO n = 1, maxIter
         h       = tR - tL
         tMid    = tL + 0.5_RP*h
         cuts(side) = tMid 
         fMid = marchingFunction(curve, polyOrder, cuts, options, gQuad)

         IF(0.5_RP*ABS(h) .le. iterToler) THEN ! If marching R -> L, h < 0
            bisect = tMid
            done   = .TRUE.
            RETURN
         END IF
         
         IF ( fMid*fL < 0.0_RP )     THEN
            tR = tMid
            fR = fMid
         ELSE 
            tL = tMid
            fL = fMid
         END IF 
      END DO
      
      bisect = tMid
      
   END FUNCTION bisect
          
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
      CLASS(SMCurve)               , POINTER :: optimizedCurve
      CLASS(MultiSegmentModalCurve), POINTER :: msmCurve
      TYPE(OptimizerOptions)                 :: options  ! parameters for the marching algorithm
      REAL(KIND=RP), ALLOCATABLE             :: errors(:,:)

      CALL OptimizeCurve(curve     = curve,                   &
                         polyOrder = polyOrder,               &
                         cuts      = cuts,                    &
                         options   = options,                 &
                         newName   = "tmp",                   &
                         newID     = 0,                       &
                         optimized = optimizedCurve)

      CALL castToMultiSegmentCurve(optimizedCurve, msmCurve)
      CALL SegmentErrors(exact          = curve,    &
                         segmentedCurve = msmCurve, &
                         gQuad          = gQuad,    &
                         errors         = errors)

      f = errors(1,USER_NORM) - options % safetyFactor*options % toler
      CALL releaseBaseCurve(optimizedCurve)
      
   END FUNCTION marchingFunction
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE SegmentErrors(exact, segmentedCurve, gQuad, errors)
!
!  -------------------------------------------------------------------
!  Compute the H^1 errors in each of the segments of a segmented curve
!  -------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(SMCurve)              , POINTER :: exact
      TYPE(MultiSegmentModalCurve), POINTER :: segmentedCurve
      TYPE(GaussQuadratureType)             :: gQuad
      REAL(KIND=RP), ALLOCATABLE            :: errors(:,:)
!
!     ----------------
!     Local variables 
!     ----------------
!
      INTEGER       :: qOrder
      INTEGER       :: nSegments
      INTEGER       :: j, k
      REAL(KIND=RP) :: t, h, dsdt
      REAL(KIND=RP) :: e, e0(3), e1(3), eMax, eMaxDeriv
      
      nSegments = segmentedCurve % nSegments
      ALLOCATE(errors(nSegments,3), source = 0.0_RP)
      
      qOrder = gQuad % N
      
      DO k = 1, nSegments
         e         = 0.0_RP
         eMax      = 0.0_RP
         eMaxDeriv = 0.0_RP
         h         = segmentedCurve % cuts(k) - segmentedCurve % cuts(k-1)
         dsdt      = 2.0_RP/h
         DO j = 0, qOrder
            t    = segmentedCurve % cuts(k-1) + h*0.5_RP*(gQuad % nodes(j) + 1.0_RP)
            e0   = (exact % positionAt(t)   - segmentedCurve % valueInSegment(k, t, which = LA_EVALUATE_FUNCTION))**2
            e1   = (exact % derivativeAt(t) - dsdt*segmentedCurve % valueInSegment(k, t, which = LA_EVALUATE_DERIVATIVE))**2
            
            e         = e + (e0(1) + e0(2) + e1(1) + e1(2))*gQuad % weights(j)
            eMax      = MAX(eMax,e0(1),e0(2))
            eMaxDeriv = MAX(eMaxDeriv,e1(1),e1(2))
         END DO 
         errors(k,USER_NORM)       = SQRT(0.5_RP*h*e)
         errors(k,MAX_NORM)        = SQRT(eMax)
         errors(k,MAX_NORM_DERIV)  = SQRT(eMaxDeriv)
      END DO 
      
   END SUBROUTINE SegmentErrors
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
!      CALL castToMultiSegmentCurve(optimizedCurve, msmCurve)
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
!         CALL castToMultiSegmentCurve(optimizedCurve, msmCurve)
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

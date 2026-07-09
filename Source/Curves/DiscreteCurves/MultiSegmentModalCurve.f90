!
!////////////////////////////////////////////////////////////////////////
!
!      MultiSegmentModalCurve.f90
!      Created: October 18, 2025 at 1:39 PM 
!      By: David Kopriva  
!
!      A curve subclass that is defined in terms of multiple segments
!      of modally defined Legendre polynomials
!
!////////////////////////////////////////////////////////////////////////
!
   Module MultiSegmentModalCurveClass
      USE MultiSegmentCurveClass
      USE LegendreAlgorithms
      IMPLICIT NONE  
   
      TYPE, EXTENDS(MultiSegmentCurve) :: MultiSegmentModalCurve
         REAL(KIND=RP), ALLOCATABLE    :: segmentLengths(:)
         REAL(KIND=RP), ALLOCATABLE    :: segmentPoints(:,:)
         REAL(KIND=RP), ALLOCATABLE    :: coefs(:,:,:)
!
!        ========
         CONTAINS
!        ========
!         
         PROCEDURE :: ConstructMultiSegmentModalCurve
         FINAL     :: DestructMultiSegmentModalCurve
         PROCEDURE :: positionAt => EvaluateMultiSegmentModalCurve
         PROCEDURE :: derivativeAt => EvaluateMultiSegmentModalCurveD
         PROCEDURE :: derivativeInSegment => modalDerivativeInSegment
         PROCEDURE :: valueInSegment
         PROCEDURE :: className  => MSMCClassName
      END TYPE MultiSegmentModalCurve
!
!  ========      
   CONTAINS  
!  ========      
!
      SUBROUTINE castToMultiSegmentModalCurve(curve,cast)
!
!     -----------------------------------------------------
!     Cast the base class SMCurve to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(SMCurve)                , POINTER :: curve
         CLASS(MultiSegmentModalCurve) , POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => curve)
            CLASS IS(MultiSegmentModalCurve)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castToMultiSegmentModalCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ConstructMultiSegmentModalCurve(self, parentCurve, cuts, coefs, curveName, id )  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MultiSegmentModalCurve) :: self
         CLASS(SMCurve), POINTER       :: parentCurve
         REAL(KIND=RP)                 :: cuts(0:)
         REAL(KIND=RP)                 :: coefs(0:,:,:)
         CHARACTER(LEN=*)              :: curveName
         INTEGER                       :: id
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                       :: k, N
         REAL(KIND=RP), ALLOCATABLE    :: nodes(:), weights(:)
         INTEGER                       :: qOrder
         REAL(KIND=RP)                 :: xStart(3), xEnd(3)
         
         N = SIZE(coefs,1) - 1
         CALL self % MultiSegmentCurve % construct(cuts,N,curveName,id)
         self % coefs = coefs
!
!        ---------------
!        Computed values
!        ---------------
!
         ALLOCATE(self % segmentLengths(self % nSegments)    , SOURCE = 0.0_RP)
         ALLOCATE(self % segmentPoints (3,0:self % nSegments), SOURCE = 0.0_RP)
         qOrder = 2*self % polyOrder
         ALLOCATE(nodes(0:qOrder), weights(0:qOrder))
         CALL GaussLegendreNodesAndWeights( qOrder, nodes, weights )
         
         self % segmentPoints(:,0) = self % valueInSegment(1, self % cuts(0), which = LA_EVALUATE_FUNCTION)
         DO k = 1, self % nSegments 
            xStart = self % valueInSegment(k, self % cuts(k-1), which = LA_EVALUATE_FUNCTION)
            xEnd   = self % valueInSegment(k, self % cuts(k)  , which = LA_EVALUATE_FUNCTION)
            self % segmentPoints(:,k) = xEnd
            self % segmentLengths(k) = SQRT((xEnd(1) - xStart(1))**2 + (xEnd(2) - xStart(2))**2)
         END DO

      END SUBROUTINE ConstructMultiSegmentModalCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE DestructMultiSegmentModalCurve(self)  
         IMPLICIT NONE  
         TYPE(MultiSegmentModalCurve) :: self

      END SUBROUTINE DestructMultiSegmentModalCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseMultiSegmentModalCurve(self)  
         IMPLICIT NONE
         CLASS(MultiSegmentModalCurve), POINTER :: self
         CLASS(FTObject)              , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseMultiSegmentModalCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION EvaluateMultiSegmentModalCurve(self,t)  RESULT(x)
         IMPLICIT NONE  
         CLASS(MultiSegmentModalCurve) :: self
         REAL(KIND=RP)                 :: t
         REAL(KIND=RP)                 :: x(3)
         
         x = MultiSegmentModalCurveValue(self, t, LA_EVALUATE_FUNCTION)
         
      END FUNCTION EvaluateMultiSegmentModalCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION EvaluateMultiSegmentModalCurveD(self,t)  RESULT(x)
         IMPLICIT NONE  
         CLASS(MultiSegmentModalCurve) :: self
         REAL(KIND=RP)                 :: t
         REAL(KIND=RP)                 :: x(3)
         
         x = MultiSegmentModalCurveValue(self, t, LA_EVALUATE_DERIVATIVE)
         
      END FUNCTION EvaluateMultiSegmentModalCurveD
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION MultiSegmentModalCurveValue(self, t, which)  RESULT(x)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MultiSegmentModalCurve) :: self
         REAL(KIND=RP)                 :: t
         REAL(KIND=RP)                 :: x(3)
         INTEGER                       :: which ! = LA_EVALUATE_FUNCTION or LA_EVALUATE_DERIVATIVE
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER       :: k
         
         k = findInterval(self % cuts,t)
         x = valueInSegment(self, k ,t, which)
         
         IF ( which == LA_EVALUATE_DERIVATIVE )     THEN
            x = 2.0_RP*x/(self % cuts(k) - self % cuts(k-1)) 
         END IF 
          
      END FUNCTION MultiSegmentModalCurveValue
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION modalDerivativeInSegment(self, t, k)  RESULT(x)
!
!     ---------------------------------------------------------------
!     Returns the derivative in segment k as needed by the superclass
!     procedure to compute the arc length
!     ---------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MultiSegmentModalCurve) :: self
         REAL(KIND=RP)                 :: t
         REAL(KIND=RP)                 :: x(3)
         INTEGER                       :: k
!
         x = valueInSegment(self, k ,t, LA_EVALUATE_DERIVATIVE)

      END FUNCTION modalDerivativeInSegment
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION valueInSegment(self, k, t, which)  RESULT(x)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MultiSegmentModalCurve) :: self
         REAL(KIND=RP)                 :: t
         REAL(KIND=RP)                 :: x(3)
         INTEGER                       :: which ! = LA_EVALUATE_FUNCTION or LA_EVALUATE_DERIVATIVE
         INTEGER                       :: k
!
!        ---------------
!        Local Variables
!        ---------------
!
         REAL(KIND=RP) :: s
         
         x = 0.0_RP
         s = InvAffineMap(t0 = self % cuts(k-1),t1 = self % cuts(k),t = t)
         x(1) = LegendreSeries(x     = s, N = self % polyOrder, &
                               coefs = self % coefs(:,1,k),     &
                               which = which)      
         x(2) = LegendreSeries(x     = s, N = self % polyOrder, &
                               coefs = self % coefs(:,2,k),     &
                               which = which)      
          
      END FUNCTION valueInSegment
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> Class name returns a string with the name of the type of the object
!>
!>  ### Usage:
!>
!>        PRINT *,  obj % className()
!>        if( obj % className = ="Spline")
!>
      FUNCTION MSMCClassName(self)  RESULT(s)
         IMPLICIT NONE  
         CLASS(MultiSegmentModalCurve)              :: self
         CHARACTER(LEN=CLASS_NAME_CHARACTER_LENGTH) :: s
         
         s = "MultiSegmentModalCurve"
         IF( self % refCount() >= 0 ) CONTINUE 
 
      END FUNCTION MSMCClassName
!
!///////////////////////////////////////////////////////////////////////
!
!                     TESTS
!
!///////////////////////////////////////////////////////////////////////
!
! 
   LOGICAL FUNCTION MultiSegmentModalCurveIsOK()  
      IMPLICIT NONE
      
      CLASS(MultiSegmentModalCurve), POINTER :: self
      CLASS(SMCurve)               , POINTER :: parentCurve => NULL()
!
!     ----------------------------------------------------------
!     Polynomial with three modes and three segments
!     We don't care3 in this test if the segments are continuous
!     ----------------------------------------------------------
!
      REAL(KIND=RP) :: coefs(3,2,3)
      REAL(KIND=RP) :: cuts(0:3) = [0.0_RP, 0.4_RP, 0.7_RP, 1.0_RP]
      REAL(KIND=RP) :: t, tol = 1.0d-9
      INTEGER       :: k
      
      MultiSegmentModalCurveIsOK = .TRUE.
      
      coefs(:,1,1) = [1.0_RP, 0.0_RP, 2.0_RP]
      coefs(:,2,1) = [1.1_RP, 0.1_RP, 2.1_RP]
      coefs(:,1,2) = [0.0_RP, 0.0_RP, 2.0_RP]
      coefs(:,2,2) = [0.1_RP, 0.1_RP, 2.1_RP]
      coefs(:,1,3) = [1.0_RP, 0.0_RP, 0.0_RP]
      coefs(:,2,3) = [1.1_RP, 0.0_RP, 0.1_RP]
      
      ALLOCATE(self)
      CALL self % constructMultiSegmentModalCurve( parentCurve, cuts, coefs,"Test Curve",1)
!
!     ---------------
!     Check intervals
!     ---------------
!
      IF( findInterval(self % cuts, t = 0.25_RP) .NE. 1) MultiSegmentModalCurveIsOK = .FALSE.
      IF( findInterval(self % cuts, t = 0.50_RP) .NE. 2) MultiSegmentModalCurveIsOK = .FALSE.
      IF( findInterval(self % cuts, t = 0.80_RP) .NE. 3) MultiSegmentModalCurveIsOK = .FALSE.
!
!     ------------
!     Check values
!     ------------
!
      MultiSegmentModalCurveIsOK = .TRUE.
      DO k = 0, 5 
         t = cuts(0) + k*(cuts(3) - cuts(0))/5.0_RP
         MultiSegmentModalCurveIsOK = MAXVAL(ABS(self % positionAt(t) - testPolynomialAt(self,t))) < tol
         MultiSegmentModalCurveIsOK = MultiSegmentModalCurveIsOK .AND. MAXVAL(ABS(self % derivativeAt(t) - &   
                                      testPolynomialDerivAt(self,t))) < tol
      END DO 

      CALL releaseMultiSegmentModalCurve(self)
      
   END FUNCTION MultiSegmentModalCurveIsOK
!
!//////////////////////////////////////////////////////////////////////// 
! 
   FUNCTION testPolynomialAt(self, t)  RESULT(p)
      IMPLICIT NONE  
      TYPE(MultiSegmentModalCurve) :: self
      REAL(KIND=RP)                :: t
      INTEGER                      :: k
      REAL(KIND=RP)                :: p(3), s
      
      p = 0.0_RP
      k = findInterval(self % cuts, t)
      s = InvAffineMap(t0 = self % cuts(k-1),t1 = self % cuts(k),t = t)
      
      p(1) = self % coefs(0,1,k)*P0(s) + self % coefs(1,1,k)*P1(s) + self % coefs(2,1,k)*P2(s)
      p(2) = self % coefs(0,2,k)*P0(s) + self % coefs(1,2,k)*P1(s) + self % coefs(2,2,k)*P2(s)

   END FUNCTION testPolynomialAt
!
!//////////////////////////////////////////////////////////////////////// 
! 
   FUNCTION testPolynomialDerivAt(self, t)  RESULT(p)
      IMPLICIT NONE  
      TYPE(MultiSegmentModalCurve) :: self
      REAL(KIND=RP)                :: t
      INTEGER                      :: k
      REAL(KIND=RP)                :: p(3), s
      
      p = 0.0_RP
      k = findInterval(self % cuts, t)
      s = InvAffineMap(t0 = self % cuts(k-1),t1 = self % cuts(k),t = t)
      
      p(1) = self % coefs(0,1,k)*P0Prime(s) + self % coefs(1,1,k)*P1Prime(s) + &
             self % coefs(2,1,k)*P2Prime(s)
      p(2) = self % coefs(0,2,k)*P0Prime(s) + self % coefs(1,2,k)*P1Prime(s) + &
             self % coefs(2,2,k)*P2Prime(s)

   END FUNCTION testPolynomialDerivAt
   
   END Module MultiSegmentModalCurveClass

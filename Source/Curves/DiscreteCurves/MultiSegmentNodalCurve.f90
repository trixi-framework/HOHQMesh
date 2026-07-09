!
!////////////////////////////////////////////////////////////////////////
!
!      MultiSegmentNodalCurve.f90
!      Created: April 22, 2026 at 2:45 PM
!      From the MultiSegmentNodalCurve class
!      By: David Kopriva  
!
!      A curve subclass that is defined in terms of multiple segments
!      of nodally defined Lagrange polynomials
!
!////////////////////////////////////////////////////////////////////////
!
   Module MultiSegmentNodalCurveClass
      USE MultiSegmentCurveClass
      USE CurveInterpolantClass
      USE LegendreAlgorithms
      IMPLICIT NONE  
   
      TYPE, EXTENDS(MultiSegmentCurve) :: MultiSegmentNodalCurve
         TYPE(CurveInterpolant), ALLOCATABLE :: selfInterpolants(:) 
!
!        ========
         CONTAINS
!        ========
!         
         PROCEDURE :: ConstructMultiSegmentNodalCurve
         FINAL     :: DestructMultiSegmentNodalCurve
         PROCEDURE :: positionAt => EvaluateMultiSegmentNodalCurve
         PROCEDURE :: derivativeAt => EvaluateMultiSegmentNodalCurveD 
         PROCEDURE :: className  => MSNCClassName
         PROCEDURE :: derivativeInSegment => nodalDerivativeInSegment
      END TYPE MultiSegmentNodalCurve
!
!  ========      
   CONTAINS  
!  ========      
!
      SUBROUTINE castToMultiSegmentNodalCurve(curve,cast)
!
!     -----------------------------------------------------
!     Cast the base class SMCurve to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(SMCurve)                , POINTER :: curve
         CLASS(MultiSegmentNodalCurve) , POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => curve)
            CLASS IS(MultiSegmentNodalCurve)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castToMultiSegmentNodalCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ConstructMultiSegmentNodalCurve(self, parentCurve, cuts, N, curveName, id )  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MultiSegmentNodalCurve) :: self
         CLASS(SMCurve), POINTER       :: parentCurve
         REAL(KIND=RP)                 :: cuts(0:)
         INTEGER                       :: N
         CHARACTER(LEN=*)              :: curveName
         INTEGER                       :: id
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                    :: k, m
         REAL(KIND=RP), ALLOCATABLE :: chebyPoints(:), values(:,:), nodes(:)
         REAL(KIND=RP)              :: dt, t
!
!        ---------------------------
!        Parent class initialization
!        ---------------------------
!
         CALL self % MultiSegmentCurve % construct(cuts,N,curveName,id)
!
!        ---------------------------------------
!        Allocate Chebyshev Gauss-Lobatto points
!        ---------------------------------------
!
         ALLOCATE(chebyPoints(0:N))
         ALLOCATE(values(0:N,3))
         ALLOCATE(nodes(0:N))
         DO m = 0, N 
            chebyPoints(m) = 0.5_RP*(1.0_RP - COS(m*PI/N))
         END DO 
!
!        -----------------------
!        Compute the polynomials
!        -----------------------
!
         ALLOCATE(self % selfInterpolants(self % nSegments))
         DO k = 1, self % nSegments 
            dt = cuts(k) - cuts(k-1)
            DO m = 0, N 
               t           = cuts(k-1) + dt*chebyPoints(m)
               nodes(m)    = t
               values(m,:) = parentCurve % positionAt(t)
            END DO
            CALL ConstructCurveInterpolant( self % selfInterpolants(k), N, nodes, values)
         END DO 
!
      END SUBROUTINE ConstructMultiSegmentNodalCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE DestructMultiSegmentNodalCurve(self)  
         IMPLICIT NONE  
         TYPE(MultiSegmentNodalCurve) :: self

      END SUBROUTINE DestructMultiSegmentNodalCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseMultiSegmentNodalCurve(self)  
         IMPLICIT NONE
         class(MultiSegmentNodalCurve), POINTER :: self
         CLASS(FTObject)              , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseMultiSegmentNodalCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION EvaluateMultiSegmentNodalCurve(self,t)  RESULT(x)
         IMPLICIT NONE  
         CLASS(MultiSegmentNodalCurve) :: self
         REAL(KIND=RP)                 :: t
         REAL(KIND=RP)                 :: x(3)
         INTEGER                       :: k
         
         k = findInterval(self % cuts,t)
         CALL EvaluateAt(self % selfInterpolants(k),t,x)
         
      END FUNCTION EvaluateMultiSegmentNodalCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION EvaluateMultiSegmentNodalCurveD(self,t)  RESULT(x)
         IMPLICIT NONE  
         CLASS(MultiSegmentNodalCurve) :: self
         REAL(KIND=RP)                 :: t
         REAL(KIND=RP)                 :: x(3)
         INTEGER                       :: k
         
         k = findInterval(self % cuts,t)
         CALL derivative_AT(self % selfInterpolants(k),t,x)
         
      END FUNCTION EvaluateMultiSegmentNodalCurveD
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION nodalDerivativeInSegment(self, t, k)  RESULT(x)
!
!        ---------------------------------------------------------------
!        Returns the derivative in segment k as needed by the superclass
!        procedure to compute the arc length
!        ---------------------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(MultiSegmentNodalCurve) :: self
         REAL(KIND=RP)                 :: t
         REAL(KIND=RP)                 :: x(3)
         INTEGER                       :: k
         
         CALL derivative_AT(self % selfInterpolants(k),t,x)
         x = x*(self % cuts(k) - self % cuts(k-1))/2.0_RP
         
      END FUNCTION nodalDerivativeInSegment
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
      FUNCTION MSNCClassName(self)  RESULT(s)
         IMPLICIT NONE  
         CLASS(MultiSegmentNodalCurve)              :: self
         CHARACTER(LEN=CLASS_NAME_CHARACTER_LENGTH) :: s
         
         s = "MultiSegmentNodalCurve"
         IF( self % refCount() >= 0 ) CONTINUE 
 
      END FUNCTION MSNCClassName
   
   END Module MultiSegmentNodalCurveClass
!
!///////////////////////////////////////////////////////////////////////
!
!                     TESTS
!
!///////////////////////////////////////////////////////////////////////
!
! 
   SUBROUTINE MultiSegmentNodalCurveTest()  
      USE SMParametricEquationCurveClass
      USE MultiSegmentNodalCurveClass
      USE FTAssertions
      IMPLICIT NONE
      
      CLASS(MultiSegmentNodalCurve)   , POINTER :: self
      TYPE (SMParametricEquationCurve), POINTER :: parentCurve
      CLASS(SMCurve)                  , POINTER :: crv
      CHARACTER(LEN=EQUATION_STRING_LENGTH) :: sx = "f(t) = 2.1*t^3 + 1.4*t^2 + t + 3.13"
      CHARACTER(LEN=EQUATION_STRING_LENGTH) :: sy = "f(t) = t^3 + 2*t^2 + 3*t + 2.72"
      CHARACTER(LEN=EQUATION_STRING_LENGTH) :: sz = "f(t) = 0.0"
!
!     ----------------------------------------------
!     Polynomial with three modes and three segments
!     ----------------------------------------------
!
      REAL(KIND=RP) :: cuts(0:3) = [0.0_RP, 0.4_RP, 0.7_RP, 1.0_RP]
      REAL(KIND=RP) :: t, tol = 1.0d-12, eMax, dTol = 1.0d-6
      INTEGER       :: k, N = 3
!
!     ----------------------------
!     The curve to be approximated
!     ----------------------------
!
      ALLOCATE(parentCurve)
      CALL parentCurve % initWithEquationsNameAndID(xEqn      = sx,                &
                                                    yEqn      = sy,                &
                                                    zEqn      = sz,                &
                                                    curveName = "Parametric Curve",&
                                                    id        = 1)
      ALLOCATE(self)
      crv => parentCurve
      CALL self % ConstructMultiSegmentNodalCurve(crv, cuts, N, "multiSeg", 2 )
!
!     ---------------
!     Check intervals
!     ---------------
!
      CALL FTAssert(test = findInterval(self % cuts, t = 0.25_RP) == 1,msg = "First find interval failed")
      CALL FTAssert(test = findInterval(self % cuts, t = 0.50_RP) == 2,msg = "Second find interval failed")
      CALL FTAssert(test = findInterval(self % cuts, t = 0.80_RP) == 3,msg = "Third find interval failed")
!
!     ------------
!     Check values
!     ------------
!
      eMax = 0.0_RP
      DO k = 0, 25 
         t = cuts(0) + k*(cuts(3) - cuts(0))/25.0_RP
         eMax = MAX(eMax, MAXVAL(ABS(self % positionAt(t) - parentCurve % positionAt(t))) )
      END DO
      CALL FTAssert(test = eMax < tol, msg = "Nodal segmented curve evaluation failed")
!
!     -----------------
!     Check Derivatives
!     -----------------
!
      eMax = 0.0_RP
      DO k = 0, 10 
         t = cuts(0) + k*(cuts(3) - cuts(0))/10.0_RP
         eMax = MAX(eMax, MAXVAL(ABS(self % derivativeAt(t) - parentCurve % derivativeAt(t))) )
      END DO
      CALL FTAssert(test = eMax < dTol, msg = "Nodal segmented curve derivative evaluation failed")

      CALL releaseMultiSegmentNodalCurve(self)
      CALL releasePECurve(parentCurve)
      
   END SUBROUTINE MultiSegmentNodalCurveTest
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE NodalCircleTest()
!
!  -------------------------------------------------------------------
!  Ensure that the marching optimization is consistent with previous
!  results.
!  -------------------------------------------------------------------
!
      USE MultiSegmentNodalCurveClass
      USE SMParametricEquationCurveClass
      USE FTAssertions
      IMPLICIT NONE
!
!     --------------
!     Test paramters
!     --------------
!
      INTEGER                                :: polyOrder  = 6
!
!     ---------------
!     Local Variables
!     ---------------
!
      CLASS(SMCurve)                 , POINTER :: crv
      TYPE(SMParametricEquationCurve), POINTER :: circleCurve
      CLASS(MultiSegmentNodalCurve)  , POINTER :: msnCurve
      TYPE(GaussQuadratureType)                :: gQuad
      CHARACTER(LEN=64)                        :: xEqn, yEqn, zEqn
      REAL(KIND=RP)                            :: arcL
!
!     ----------------------------------------------
!     Polynomial with three modes and three segments
!     ----------------------------------------------
!
      REAL(KIND=RP) :: cuts(0:3) = [0.0_RP, 0.4_RP, 0.7_RP, 1.0_RP]
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
!     ---------------------------------
!     Create a multisegment nodal curve
!     ---------------------------------
!
      ALLOCATE(msnCurve)
      CALL msnCurve % ConstructMultiSegmentNodalCurve(crv, cuts, polyOrder, "multiSeg", 2 )
!
!     ------------------------
!     Compute total arc length
!     ------------------------
!
      CALL ConstructGaussQuadrature(gQuad, 4*polyOrder) ! The 4 is arbitrary
      arcL = msnCurve % arcLength(gQuad)
      CALL FTAssertEqual(expectedValue = 8.0_RP*PI, &
                         actualValue   = arcL,  &
                         relTol        = 1.0d-4, &
                         msg           = "Arc Length estimation")
!
!     --------
!     Clean up
!     --------
!
      CALL releaseMultiSegmentNodalCurve(msnCurve)
      CALL releasePECurve(circleCurve)
       
   END SUBROUTINE NodalCircleTest

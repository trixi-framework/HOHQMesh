!
!////////////////////////////////////////////////////////////////////////
!
!      LegendreAlgorithms.f90
!      Created: June 3, 2025 at 1:57 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   Module LegendreAlgorithms
      USE ProgramGlobals
      IMPLICIT NONE  

      INTEGER, PARAMETER     :: LA_EVALUATE_FUNCTION = 0, LA_EVALUATE_DERIVATIVE = 1

      REAL(KIND=RP), PRIVATE :: testCoefs(0:4) = [2.2_RP, 1.3_RP, 4.7_RP, 3.14_RP, 0.7_RP]
!
!     ======== 
      CONTAINS  
!     ======== 
!
!      Main entry points:
!
!      SUBROUTINE LegendreLobattoNodesAndWeights( N, x, w )
!         Computes the Lobatto points,x, and weights, w, for polynomial order N
!
!      SUBROUTINE LegendrePolyAndDerivative( N, x, L_N, LPrime_N )
!         Compute the Legendre polynomial of degree N and its derivative at a point x
!   
!      SUBROUTINE LegendrePolysAndDerivatives( N, x, L, LPrime )
!         Same as the above, but returns the arrays of L_k(x) and L'_k(x), k = 0, ..., N
!
!      REAL(KIND=RP) FUNCTION LegendreSeries( x, N, coefs, which )
!         Given the coefficents computes either \sum_{k=0}^N coefs(k)L_k(x) for which = LA_EVALUATE_FUNCTION
!         or the derivative \sum_{k=0}^N coefs(k)L'_k(x) for which = LA_EVALUATE_DERIVATIVE
!
!      FUNCTION gaussInnerProduct(a, b, weights, N)
!         Compute the Gauss-Lobatto innerproduct <a,b>_N for the weights compute from 
!         LegendreLobattoNodesAndWeights
!
!      SUBROUTINE LegendreCoefficients( N, vals, coefs) 
!         Given an array of values, compute the Lagrange modal coefficients
!         
!
!      Testing Routines:
!
!      LOGICAL FUNCTION legendreQuadratureIsOK()
!      LOGICAL FUNCTION gaussInnerProductIsOK()  
!      LOGICAL FUNCTION LegendrePolyAndDerivIsOK()  
!      LOGICAL FUNCTION legendreCoefsForTestAreOK()
!      LOGICAL FUNCTION legendreSeriesIsOK()
!
!////////////////////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE LegendrePolysAndDerivatives( N, x, L, LPrime )
!
!     ---------------------------------------------------------------------
!     Compute the Legendre Polynomials up to degree N and their derivatives
!     at the point x.
!     ---------------------------------------------------------------------
!
!     -----------------
!     Input parameters:
!     -----------------
!
      INTEGER      , INTENT(IN) :: N
      REAL(KIND=RP), INTENT(IN) :: x
!
!     ------------------
!     Output parameters:
!     ------------------
!
      REAL(KIND=RP), INTENT(OUT) :: L(0:N), LPrime(0:N)
!
!     ----------------
!     Local Variables:
!     ----------------
!
      INTEGER       :: k
     
      L(0)      = 1.0_rp
      LPrime(0) = 0.0_rp
      IF(N==0)    RETURN 
       
      L(1)      = x
      LPrime(1) = 1.0_rp
      IF( N == 1)  RETURN 

      DO k = 2, N
         L(k)        = ((2*k-1)*x*L(k-1) - (k-1)*L(k-2))/k
         LPrime(k)   = LPrime(k-2) + (2*k-1)*L(k-1)
      END DO
      
      END SUBROUTINE LegendrePolysAndDerivatives
!
!//////////////////////////////////////////////////////////////////////// 
! 
      REAL(KIND=RP) FUNCTION LegendreSeries( x, N, coefs, which )
!
!     --------------------------------------------------------------------
!     Compute the value of the Legendre series \sum_{k=0}^N coefs_k \phi_k
!     for \phi_k = L_k if which = LA_EVALUATE_FUNCTION and \phi_k = L'_k
!     if it is LA_EVALUATE_DERIVATIVE
!     --------------------------------------------------------------------
!
         IMPLICIT NONE  
         INTEGER      , INTENT(IN) :: N
         REAL(KIND=RP), INTENT(IN) :: x
         REAL(KIND=RP), INTENT(IN) :: coefs(0:N)
         INTEGER                   :: which
         
         REAL(KIND=RP) :: L(0:N), LPrime(0:N)
         
         CALL LegendrePolysAndDerivatives( N, x, L, LPrime )
         
         IF ( which == LA_EVALUATE_FUNCTION  )     THEN
            LegendreSeries = SumSeries( N, coefs, L) 
         ELSE 
            LegendreSeries = SumSeries( N, coefs, LPrime) 
         END IF 
         
      END FUNCTION LegendreSeries
!
!//////////////////////////////////////////////////////////////////////// 
! 
      REAL(KIND=RP) FUNCTION SumSeries( N, coefs, basis )  
         IMPLICIT NONE  
         INTEGER      , INTENT(IN) :: N
         REAL(KIND=RP), INTENT(IN) :: coefs(0:N), basis(0:N)
         
         REAL(KIND=RP) :: s
         INTEGER       :: k
         
         s = 0.0_RP
         DO k = 0, N 
            s = s +  coefs(k)*basis(k)
         END DO 
         SumSeries = s
         
      END FUNCTION SumSeries
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION gaussInnerProduct(a, b, weights, N)
!
!     -----------------------------------------
!     Compute the discrete inner product
!     <a,b>_N = \sum_{k=0}^N a(k)b(k)weights(k)
!     -----------------------------------------
!
         IMPLICIT NONE  
         INTEGER :: N
         REAL(KIND=RP) :: a(0:N), b(0:N), weights(0:N)
         REAL(KIND=RP) :: gaussInnerProduct
         
         INTEGER       :: j
         REAL(KIND=RP) :: s
         
         s = 0.0_RP
         DO j = 0,N 
            s = s + a(j)*b(j)*weights(j) 
         END DO 
         
         gaussInnerProduct = s
          
      END FUNCTION gaussInnerProduct
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION gaussQuadrature(a, weights, N)
!
!     -----------------------------------------
!     Compute the discrete inner product
!     <a,b>_N = \sum_{k=0}^N a(k)b(k)weights(k)
!     -----------------------------------------
!
         IMPLICIT NONE  
         INTEGER :: N
         REAL(KIND=RP) :: a(0:N), weights(0:N)
         REAL(KIND=RP) :: gaussQuadrature
         
         INTEGER       :: j
         REAL(KIND=RP) :: s
         
         s = 0.0_RP
         DO j = 0,N 
            s = s + a(j)*weights(j) 
         END DO 
         
         gaussQuadrature = s
          
      END FUNCTION gaussQuadrature
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE LegendreCoefficients( N, vals, coefs)
!
!     ---------------------------------------------------------------------------
!     Compute the Legendre modal coefficients coefs(k) = <vals,L_k>_N/||L_k||^2_N
!     ---------------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER      , INTENT(IN)  :: N
         REAL(KIND=RP), INTENT(IN)  :: vals(0:N)
         REAL(KIND=RP), INTENT(OUT) :: coefs(0:N)
!
!        ---------------
!        Local Variables
!        ---------------
!
         REAL(KIND=RP)              :: nodes(0:N), weights(0:N)
         INTEGER                   :: j,k
         REAL(KIND=RP)             :: L(0:N,0:N), LPrime(0:N,0:N)
!
!        ---------------
!        Basis functions
!        ---------------
!
         CALL LegendreLobattoNodesAndWeights( N, nodes, weights )
         DO j = 0, N 
            CALL LegendrePolysAndDerivatives(N, nodes(j), L(j,:), LPrime(j,:))
         END DO
!
!        ------------
!        Coefficients
!        ------------
!
         DO k = 0, N-1 
            coefs(k) = gaussInnerProduct(vals,L(:,k),weights,N)*(2.0_RP*k + 1.0_RP)/2.0_RP
         END DO 
         coefs(N) = gaussInnerProduct(vals,L(:,N),weights,N)/gaussInnerProduct(L(:,N),L(:,N),weights,N)
          
      END SUBROUTINE LegendreCoefficients
!
!////////////////////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE qAndLEvaluation( N, x, Q, Q_prime, L_N )
!
!     ---------------------------------------------------
!     Compute the function Q_N = L_{N+1} - L_{N-1} and
!     its derivative to find the Gauss-Lobatto points and
!     weights.
!     ---------------------------------------------------
!
!
!     -----------------
!     Input parameters:
!     -----------------
!
      INTEGER      , INTENT(IN) :: N
      REAL(KIND=RP), INTENT(IN) :: x
!
!     ------------------
!     Output parameters:
!     ------------------
!
      REAL(KIND=RP), INTENT(OUT) :: Q, Q_prime, L_N
!
!     ----------------
!     Local Variables:
!     ----------------
!
      INTEGER       :: k
      REAL(KIND=RP) :: L_kM1, L_kM2, LPrime_kM2, LPrime_kM1, L_k, LPrime_k
      
      IF( N == 0 )     THEN       !Should never be called
         L_N      = 1.0_rp
         Q = HUGE(Q)
         Q_Prime = Huge(Q_Prime)
      ELSE IF ( N == 1 )     THEN ! Should never be called
         L_N      = x
         Q = HUGE(Q)
         Q_Prime = Huge(Q_Prime)
      ELSE
         L_kM2 = 1.0_rp
         LPrime_kM2 = 0.0_rp
         L_kM1 = x
         LPrime_kM1 = 1.0_rp
         DO k = 2, N
            L_k        = ((2*k-1)*x*L_kM1 - (k-1)*L_kM2)/k
            LPrime_k   = LPrime_kM2 + (2*k-1)*L_kM1
            L_kM2      = L_kM1
            L_kM1      = L_k
            LPrime_kM2 = LPrime_kM1
            LPrime_kM1 = LPrime_k
         END DO
         k = N+1
         L_k        = ((2*k-1)*x*L_kM1 - (k-1)*L_kM2)/k
         LPrime_k   = LPrime_kM2 + (2*k-1)*L_kM1
         
         Q = L_k - L_kM2
         Q_prime = LPrime_k - LPrime_kM2
         L_N = L_kM1
      END IF
      
      END SUBROUTINE qAndLEvaluation
!
!////////////////////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE LegendreLobattoNodesAndWeights( N, x, w )
!
!     -------------------------------------------------------
!     Compute the Gauss-Lobatto-legendre quadrature nodes and
!     weights
!     -------------------------------------------------------
!
!     -----------------
!     Input parameters:
!     -----------------
!
      INTEGER, INTENT(IN) :: N
!
!     ------------------
!     Output parameters:
!     ------------------
!
      REAL(KIND=RP), DIMENSION(0:N), INTENT(OUT) :: x, w
!
!     ----------------
!     Local Variables:
!     ----------------
!
      REAL(KIND=RP) :: xj, Q, Q_prime, L_N, delta, tolerance, LPrime_NP1
      INTEGER       :: j, k, NDiv2
!
!     ----------
!     Constants:
!     ----------
!
      INTEGER, PARAMETER       :: noNewtonIterations = 10
      REAL(KIND=RP), PARAMETER :: toleranceFactor    = 4.0_RP
      
      tolerance = toleranceFactor*EPSILON(L_N)
      IF( N == 0 )     THEN ! Error - Must have at least two points, +/-1
         x(0) = 0.0_RP
         w(0) = 0.0_RP
         RETURN
      ELSE IF( N == 1 )     THEN
         x(0) = -1.0_RP
         w(0) =  0.5_RP
         x(1) =  1.0_RP
         w(1) =  w(0)
      ELSE
!
!        ---------
!        Endpoints
!        ---------
!
         x(0) = -1.0_RP
         w(0) =  2.0_RP/(N*(N+1))
         x(N) =  1.0_RP
         w(N) =  w(0)
!
!        ----------------------------------
!        Iterate on half the interior nodes
!        ----------------------------------
!
         NDiv2 = (N+1)/2
         DO j = 1, NDiv2-1
            xj = -COS( (j+0.25_RP)*PI/N - 3.0_RP/(8*N*PI*(j+0.25_RP)) )
            DO k = 0, noNewtonIterations
               CALL qAndLEvaluation( N, xj, Q, Q_prime, L_N )
               delta = -Q/Q_prime
               xj = xj + delta
               IF( ABS(delta) <=  tolerance*ABS(xj) )     EXIT
            END DO
            CALL qAndLEvaluation( N, xj, Q, Q_prime, L_N )
            x(j)   = xj
            w(j)   = 2.0_RP/(N*(N+1)*L_N**2)
            x(N-j) = -xj
            w(N-j) = w(j)
         END DO
      END IF
!
!     ---------------------------
!     Fill in middle if necessary
!     ---------------------------
!
      IF( MOD(N,2) == 0 )     THEN
         CALL LegendrePolyAndDerivative( N, 0.0_RP, L_N, LPrime_NP1 )
         x(N/2) = 0.0_RP
         w(N/2) = 2.0_RP/(N*(N+1)*L_N**2)
      END IF
      
      END SUBROUTINE LegendreLobattoNodesAndWeights
!
!////////////////////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE LegendrePolyAndDerivative( N, x, L_N, LPrime_N )
!
!     --------------------------------------------------------------
!     Compute the Legendre Polynomial of degree k and its derivative
!     --------------------------------------------------------------
!
!     -----------------
!     Input parameters:
!     -----------------
!
      INTEGER      , INTENT(IN) :: N
      REAL(KIND=RP), INTENT(IN) :: x
!
!     ------------------
!     Output parameters:
!     ------------------
!
      REAL(KIND=RP), INTENT(OUT) :: L_N, LPrime_N
!
!     ----------------
!     Local Variables:
!     ----------------
!
      INTEGER       :: k
      REAL(KIND=RP) :: L_NM1, L_NM2, LPrime_NM2, LPrime_NM1
      
      IF( N == 0 )     THEN
         L_N      = 1.0_rp
         LPrime_N = 0.0_rp
      ELSE IF ( N == 1 )     THEN
         L_N      = x
         LPrime_N = 1.0_rp
      ELSE
         L_NM2 = 1.0_rp
         LPrime_NM2 = 0.0_rp
         L_NM1 = x
         LPrime_NM1 = 1.0_rp
         DO k = 2, N
            L_N        = ((2*k-1)*x*L_NM1 - (k-1)*L_NM2)/k
            LPrime_N   = LPrime_NM2 + (2*k-1)*L_NM1
            L_NM2      = L_NM1
            L_NM1      = L_N
            LPrime_NM2 = LPrime_NM1
            LPrime_NM1 = LPrime_N
         END DO
      END IF
      
      END SUBROUTINE LegendrePolyAndDerivative
!
!     -------
!     Testing
!     -------
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION legendreQuadratureIsOK()
!
!     ---------------------------------------------------------------
!     The Legendre quadrature will be exact for polynomials of degree
!     2N-1. Checks up to N = 20, but we know it should be good at
!     leat to N = 2000 from other tests.
!     ---------------------------------------------------------------
!
         IMPLICIT NONE  
         REAL(KIND=RP), DIMENSION(:), ALLOCATABLE :: x, w, f, coefs
         INTEGER                                  :: N, j, m, nMax
         REAL(KIND=RP)                            :: sm, fInt, exact
         REAL(KIND=RP)                            :: maxError, maxSumError
         REAL(KIND=RP)                            :: tol = 1.0d-9

         nMax = 20
         ALLOCATE(x(0:nMax),w(0:nMax), f(0:nMax), coefs(0:2*nMax-1))
         
         maxError    = -1.0_RP
         maxSumError = -1.0_RP
         
         legendreQuadratureIsOK = .TRUE.
         
         DO N = 2, nMax
            
            CALL LegendreLobattoNodesAndWeights( N, x, w )
            CALL RANDOM_NUMBER(HARVEST = coefs)
!
!           --------------------------------
!           Check sum of weights should be 2
!           --------------------------------
!
            sm = 0.0_RP
            DO j = 0, N 
               sm = sm + w(j) 
            END DO
            maxSumError = MAX(maxSumError,ABS(sm-2.0_RP))
!
!           ----------------------------------------------------------
!           Lobatto quadrature is exact for polynomials to degree 2N-1
!           ----------------------------------------------------------
!
            DO j = 0,N
               f(j) = 0.0_RP
               DO m = 0, 2*N-1
                  f(j) = f(j) + coefs(m)*x(j)**m
               END DO 
            END DO
!
!           ---------------------------
!           Exact value of the integral
!           ---------------------------
!
            exact = 0.0_RP
            DO m = 0, 2*N-1
               exact = exact + coefs(m)*(1.0_RP - (-1.0)**(m+1))/(m+1)
            END DO
!
!           ----------------------
!           Compute the quadrature
!           ----------------------
!
            fInt = gaussQuadrature(f, w, N)
            
            maxError = MAX(maxError, ABS(fInt-exact))
            
            legendreQuadratureIsOK = legendreQuadratureIsOK .AND. maxError < tol .AND. maxSumError < tol

         END DO
          
      END FUNCTION legendreQuadratureIsOK
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION gaussInnerProductIsOK()  
        IMPLICIT NONE  
        INTEGER, PARAMETER :: N = 4
        REAL(KIND=RP)      :: a(0:N), b(0:N)
        REAL(KIND=RP)      :: nodes(0:N), weights(0:N)
        REAL(KIND=RP)      :: nrm, tol = 1.0d-8
        INTEGER            :: j
 
        CALL LegendreLobattoNodesAndWeights( N, nodes, weights )
!
!       --------------------------------------
!       Orthogonal polynomials shall give zero
!       --------------------------------------
!
        DO j = 0,N 
           a(j) = P3(nodes(j))
           b(j) = P2(nodes(j)) 
        END DO
        nrm = gaussInnerProduct(a,b,weights,N)
        gaussInnerProductIsOK = nrm < tol
!
!       -------------------------------
!       Same polynomial shall give norm
!       -------------------------------
!
        DO j = 0,N 
           a(j) = P3(nodes(j))
           b(j) = a(j) 
        END DO
        nrm = gaussInnerProduct(a,b,weights,N)
        gaussInnerProductIsOK = ABS(nrm - 2.0_RP/(2.0_RP*3.0_RP+1.0_RP)) < tol &
                                .AND. gaussInnerProductIsOK 

      END FUNCTION gaussInnerProductIsOK
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION LegendrePolyAndDerivIsOK()  
         IMPLICIT NONE  
         INTEGER      , PARAMETER  :: N = 4
         REAL(KIND=RP)             :: x, dx, tol = 1.0d-8
         REAL(KIND=RP)             :: L(0:N)     , LPrime(0:N)
         REAL(KIND=RP)             :: LExact(0:N), LPrimeExact(0:N)
         INTEGER                   :: j
         
         dx = 2.0_RP/5.0_RP
         LegendrePolyAndDerivIsOK = .TRUE.
         DO j = 0, 5 
            x = -1.0_RP + j*dx 
            CALL LegendrePolysAndDerivatives(N,x,L,LPrime)
            
            LExact(0) = P0(x)
            LExact(1) = P1(x)
            LExact(2) = P2(x)
            LExact(3) = P3(x)
            LExact(4) = P4(x)
            
            LegendrePolyAndDerivIsOK = MAXVAL(ABS(L - LExact)) < tol &
                                       .AND. LegendrePolyAndDerivIsOK
            
            LPrimeExact(0) = P0Prime(x)
            LPrimeExact(1) = P1Prime(x)
            LPrimeExact(2) = P2Prime(x)
            LPrimeExact(3) = P3Prime(x)
            LPrimeExact(4) = P4Prime(x)
            
            LegendrePolyAndDerivIsOK = MAXVAL(ABS(LPrime - LPrimeExact)) < tol &
                                       .AND. LegendrePolyAndDerivIsOK
         END DO 
        
      END FUNCTION LegendrePolyAndDerivIsOK
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION legendreCoefsForTestAreOK()
!
!     -------------------------------------------------------------------
!     The Legendre series will be exact for any polynomial of degree <= N
!     -------------------------------------------------------------------
!
         IMPLICIT NONE
         INTEGER, PARAMETER :: N = 4
         REAL(KIND=RP)      :: tol = 1.0d-8
         REAL(KIND=RP)      :: coefs(0:N)
        
         CALL LegendreCoefsForTestPolynomial(coefs)
         legendreCoefsForTestAreOK = MAXVAL(ABS(coefs - testCoefs)) < tol

      END FUNCTION legendreCoefsForTestAreOK
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION legendreCoefsAreOK()
!
!     -------------------------------------------------------------------
!     The Legendre series will be exact for any polynomial of degree <= N
!     -------------------------------------------------------------------
!
         IMPLICIT NONE
         REAL(KIND=RP) :: tol = 1.0d-8
         REAL(KIND=RP) :: coefs(0:4), vals(0:4)
         INTEGER       :: j
         REAL(KIND=RP) :: nodes(0:4), weights(0:4)
 
         CALL LegendreLobattoNodesAndWeights( 4, nodes, weights )
         DO j = 0, 4 
            vals(j) = testPolynomial(nodes(j))
         END DO 
        
         CALL LegendreCoefficients(4, vals, coefs)

         legendreCoefsAreOK = MAXVAL(ABS(coefs - testCoefs)) < tol

      END FUNCTION legendreCoefsAreOK
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION legendreSeriesIsOK()  
         IMPLICIT NONE  
         REAL(KIND=RP) :: tol = 1.0d-8
         REAL(KIND=RP) :: x, dx
         INTEGER       :: j
 
         legendreSeriesIsOK = .TRUE.
         dx = 2.0_RP/5.0_RP
         DO j = 0, 5 
            x = -1.0_RP + j*dx
            legendreSeriesIsOK = ABS(LegendreSeries(x,4,testCoefs,LA_EVALUATE_FUNCTION) &
                                    - testPolynomial(x)) < tol &
                                    .AND. legendreSeriesIsOK
         END DO 
         
      END FUNCTION legendreSeriesIsOK
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE LegendreCoefsForTestPolynomial(coefs)  
         IMPLICIT NONE
         INTEGER, PARAMETER :: N = 6
         REAL(KIND=RP)      :: coefs(0:4)
         REAL(KIND=RP)      :: a(0:N), b(0:N)
         REAL(KIND=RP)      :: nodes(0:N), weights(0:N)
         INTEGER            :: j,k
 
        CALL LegendreLobattoNodesAndWeights( N, nodes, weights )
!
!       -----------------------
!       Select the coefficients
!       -----------------------
!
        k = 0
        DO j = 0,N 
           a(j) = testPolynomial(nodes(j))
           b(j) = P0(nodes(j)) 
        END DO
        coefs(k) = gaussInnerProduct(a,b,weights,N)*(2.0_RP*k + 1.0_RP)/2.0_RP
        k = 1
        DO j = 0,N 
           b(j) = P1(nodes(j)) 
        END DO
        coefs(k) = gaussInnerProduct(a,b,weights,N)*(2.0_RP*k + 1.0_RP)/2.0_RP
        k = 2
        DO j = 0,N 
           b(j) = P2(nodes(j)) 
        END DO
        coefs(k) = gaussInnerProduct(a,b,weights,N)*(2.0_RP*k + 1.0_RP)/2.0_RP
        k = 3
        DO j = 0,N 
           b(j) = P3(nodes(j)) 
        END DO
        coefs(k) = gaussInnerProduct(a,b,weights,N)*(2.0_RP*k + 1.0_RP)/2.0_RP
        k = 4
        DO j = 0,N 
           b(j) = P4(nodes(j)) 
        END DO
        coefs(k) = gaussInnerProduct(a,b,weights,N)*(2.0_RP*k + 1.0_RP)/2.0_RP
 
      END SUBROUTINE LegendreCoefsForTestPolynomial
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION testPolynomial(x) RESULT(s)
!
!     ---------------------------------
!     Evaluate a polynomial of degree 4 
!     ---------------------------------
!
         IMPLICIT NONE 
         REAL(KIND=RP) :: x
         REAL(KIND=RP) :: P(0:4), s
         INTEGER       :: k
         
         P(0) = P0(x)
         P(1) = P1(x)
         P(2) = P2(x)
         P(3) = P3(x)
         P(4) = P4(x)
         
         s = 0.0_RP
         DO k = 0, 4 
            s = s + testCoefs(k)*P(k) 
         END DO 
                  
      END FUNCTION testPolynomial
!
!     -------------------------
!     Some Legendre polynomials
!     -------------------------
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION P0(x)  
         IMPLICIT NONE
         REAL(KIND=RP) :: x, P0
         P0 = 1.0_RP 
      END FUNCTION P0
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION P0Prime(x)  
         IMPLICIT NONE
         REAL(KIND=RP) :: x, P0Prime
         P0Prime = 0.0_RP 
      END FUNCTION P0Prime
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION P1(x)  
         IMPLICIT NONE
         REAL(KIND=RP) :: x, P1
         P1 = x 
      END FUNCTION P1
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION P1Prime(x)  
         IMPLICIT NONE
         REAL(KIND=RP) :: x, P1Prime
         P1Prime = 1.0_RP 
      END FUNCTION P1Prime
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION P2(x)  
         IMPLICIT NONE
         REAL(KIND=RP) :: x, P2
         P2 = 0.5_RP*(3.0_RP*x**2 - 1.0_RP) 
      END FUNCTION P2
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION P2Prime(x)  
         IMPLICIT NONE
         REAL(KIND=RP) :: x, P2Prime
         P2Prime = 0.5_RP*(2.0_RP*3.0_RP*x) 
      END FUNCTION P2Prime
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION P3(x)  
         IMPLICIT NONE
         REAL(KIND=RP) :: x, P3
         P3 = 0.5_RP*(5.0_RP*x**3 - 3.0_RP*x)
      END FUNCTION P3
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION P3Prime(x)  
         IMPLICIT NONE
         REAL(KIND=RP) :: x, P3Prime
         P3Prime = 0.5_RP*(3.0_RP*5.0_RP*x**2 - 3.0_RP)
      END FUNCTION P3Prime
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION P4(x)  
         IMPLICIT NONE
         REAL(KIND=RP) :: x, P4
         P4 = 0.125_RP*(35.0_RP*x**4 - 30.0_RP*x**2 + 3.0_RP) 
      END FUNCTION P4
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION P4Prime(x)  
         IMPLICIT NONE
         REAL(KIND=RP) :: x, P4Prime
         P4Prime = 0.125_RP*(4.0_RP*35.0_RP*x**3 - 2.0_RP*30.0_RP*x) 
      END FUNCTION P4Prime
   END Module LegendreAlgorithms

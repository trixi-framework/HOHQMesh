!
!////////////////////////////////////////////////////////////////////////
!
!      Interpolation.F95
!      Created: 2009-12-15 15:36:24 -0500 
!      By: David Kopriva  
!
!
!      Contains:
!               
!               SUBROUTINE InterpolatingPolynomialVector( x, N, nodes, weights, p )
!               REAL(KIND=RP) FUNCTION EvaluateLagrangePolyDerivative( j, x, N, nodes)
!
!               ALGORITHM 30: SUBROUTINE BarycentricWeights( N, x, w )
!               ALGORITHM 31: REAL(KIND=RP) FUNCTION LagrangeInterpolation( x, N, nodes, values, weights)
!               ALGORITHM 32: SUBROUTINE PolynomialInterpolationMatrix( N, M, oldNodes, weights, newNodes, T)
!               ALGORITHM 33: SUBROUTINE InterpolateToNewPoints( N, M, T, f, fInterp )
!               ALGORITHM 34: REAL(KIND=RP) FUNCTION LagrangeInterpolatingPolynomial( j, x, N, nodes )
!               ALGORITHM 35: 
!
!      
!////////////////////////////////////////////////////////////////////////
!
!
!  ******
   MODULE PolynomialInterpolationModule
!  ******
!
     USE SMConstants
     IMPLICIT NONE
     
     INTEGER, PARAMETER :: MXV_DIRECT = 1, MXV_TRANSPOSE = 2
!
!    ========
     CONTAINS
!    ========
!
!    /////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION LagrangeInterpolatingPolynomial( j, x, N, nodes ) RESULT(p)
!
!---------------------------------------------------------------------
! Compute L_j(x) of degree N whose zeros are at the nodes using direct
! evaluation.
!---------------------------------------------------------------------
!
      INTEGER                      , INTENT(IN) :: j     !! Which polynomial
      INTEGER                      , INTENT(IN) :: N     !! Polynomial order
      REAL(KIND=RP)                , INTENT(IN) :: x     !! evaluation point
      REAL(KIND=RP), DIMENSION(0:N), INTENT(IN) :: nodes !! Interpolation nodes
!
!     ---------------
!     Local Variables
!     ---------------
!
      INTEGER :: k
      
      IF ( j == 0 )     THEN
         p = (x - nodes(1))/(nodes(j) - nodes(1))
         DO k = 2, N 
            p = p*(x - nodes(k))/(nodes(j) - nodes(k))
         END DO
      ELSE
         p = (x - nodes(0))/(nodes(j) - nodes(0))
         DO k = 1, j-1 
            p = p*(x - nodes(k))/(nodes(j) - nodes(k))
         END DO
         DO k = j+1, N
            p = p*(x - nodes(k))/(nodes(j) - nodes(k))
         END DO
      END IF
   END FUNCTION LagrangeInterpolatingPolynomial
!
!    /////////////////////////////////////////////////////////////////
!
      SUBROUTINE InterpolatingPolynomialVector( x, N, nodes, weights, p )
!
!---------------------------------------------------------------------
! Compute L_j(x), j = 0, ..., N of degree N whose zeros are at the nodes 
! using barycentric form.
!---------------------------------------------------------------------
!
      INTEGER                      , INTENT(IN)  :: N       !! Polynomial order
      REAL(KIND=RP)                , INTENT(IN)  :: x       !! evaluation point
      REAL(KIND=RP), DIMENSION(0:N), INTENT(IN)  :: nodes   !! Interpolation nodes
      REAL(KIND=RP), DIMENSION(0:N), INTENT(IN)  :: weights !! Barycentric weights
      REAL(KIND=RP), DIMENSION(0:N), INTENT(INOUT) :: p
!
!     ---------------
!     Local Variables
!     ---------------
!
      INTEGER       :: j
      LOGICAL       :: xMatchesNode
      REAL(KIND=RP) :: d, t
!
!     ---------
!     Externals
!     ---------
!
      LOGICAL, EXTERNAL :: AlmostEqual
!
!     -------------------------------------
!     See if the evaluation point is a node
!     -------------------------------------
!
      xMatchesNode = .false.
      DO j = 0, N 
         p(j) = 0.0_RP
         IF( AlmostEqual( x, nodes(j) ) )     THEN
            p(j) = 1.0_RP
            xMatchesNode = .true.
         END IF
      END DO
      IF( xMatchesNode )     RETURN
!
!     ------------------------------
!     Evaluation point is not a node
!     ------------------------------
!
      d = 0.0_RP
      DO j = 0, N 
         t = weights(j)/( x - nodes(j) )
         p(j) = t
         d = d + t
      END DO
      DO j = 0, N 
         p(j) = p(j)/d
      END DO
      
   END SUBROUTINE InterpolatingPolynomialVector
!
! /////////////////////////////////////////////////////////////////////
!
!---------------------------------------------------------------------
!!    Compute at x the derivative of the lagrange polynomial L_j(x) of
!!    degree N whose zeros are given by the nodes(i)
!---------------------------------------------------------------------
!
      REAL(KIND=RP) FUNCTION EvaluateLagrangePolyDerivative( j, x, N, nodes)
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER, INTENT(IN)                       :: j !! Which poly
      INTEGER, INTENT(IN)                       :: N !! Order
      REAL(KIND=RP), DIMENSION(0:N), INTENT(IN) :: nodes !! Nodes
      REAL(KIND=RP)                , INTENT(IN) :: x !! Eval Pt
!
!     ---------------
!     Local Variables
!     ---------------
!
      INTEGER       :: l, m
      REAL(KIND=RP) :: hp, poly
!                                                                       
      hp = 0.0_RP
      DO l = 0,N 
         IF(l == j)     CYCLE
         poly = 1.0_RP
         DO m = 0,N 
            IF (m == l)     CYCLE
            IF (m == j)     CYCLE 
            poly = poly*(x - nodes(m))/(nodes(j) - nodes(m))
         END DO
         hp = hp + poly/(nodes(j) - nodes(l)) 
      END DO
      EvaluateLagrangePolyDerivative = hp
!                                                                       
      END FUNCTION EvaluateLagrangePolyDerivative
!
!////////////////////////////////////////////////////////////////////////
!
!     ----------------------------------------------------------------
!!    Compute the barycentric weights for polynomial interpolation
!     ----------------------------------------------------------------
!
      SUBROUTINE BarycentricWeights( N, x, w )
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER :: N
      REAL(KIND=RP), DIMENSION(0:N), INTENT(IN)  :: x
      REAL(KIND=RP), DIMENSION(0:N), INTENT(OUT) :: w
!
!     ---------------
!     Local Variables
!     ---------------
!
      INTEGER :: j, k
!      
      w = 1.0_RP
      DO j = 1, N
         DO k = 0, j-1
            w(k) = w(k)*(x(k) - x(j))
            w(j) = w(j)*(x(j) - x(k))
         END DO
      END DO
      w = 1.0_RP/w

      END SUBROUTINE BarycentricWeights
!
!     ////////////////////////////////////////////////////////////////
!
!     ------------------------------------------------------------------
!!    Compute the value of the interpolant using the barycentric formula
!     ------------------------------------------------------------------
!
      REAL(KIND=RP) FUNCTION LagrangeInterpolation( x, N, nodes, values, weights)
!
!     ---------
!     Arguments
!     ---------
!
      REAL(KIND=RP)                 :: x
      INTEGER                       :: N
      REAL(KIND=RP), DIMENSION(0:N) :: nodes
      REAL(KIND=RP), DIMENSION(0:N) :: values
      REAL(KIND=RP), DIMENSION(0:N) :: weights
!
!     ---------------
!     Local Variables
!     ---------------
!
      INTEGER       :: j
      REAL(KIND=RP) :: t, numerator, denominator
      
      LOGICAL, EXTERNAL :: AlmostEqual
      
      numerator   = 0.0_RP
      denominator = 0.0_RP
      DO j = 0, N
         IF( AlmostEqual( x, nodes(j) ) )    THEN
            LagrangeInterpolation = values(j)
            RETURN 
         END IF 
         t = weights(j)/( x - nodes(j) )
         numerator = numerator + t*values(j)
         denominator = denominator + t
      END DO
      LagrangeInterpolation = numerator/denominator

      END FUNCTION LagrangeInterpolation
!
!     ////////////////////////////////////////////////////////////////
!
!     ------------------------------------------------------------------
!!    Compute the value of the interpolant using the barycentric formula
!     ------------------------------------------------------------------
!
      REAL(KIND=RP) FUNCTION LagrangeInterpolantDerivative( x, N, nodes, values, weights)
!
!     ---------
!     Arguments
!     ---------
!
      REAL(KIND=RP)                 :: x
      INTEGER                       :: N
      REAL(KIND=RP), DIMENSION(0:N) :: nodes
      REAL(KIND=RP), DIMENSION(0:N) :: values
      REAL(KIND=RP), DIMENSION(0:N) :: weights
!
!     ---------------
!     Local Variables
!     ---------------
!
      INTEGER       :: j, i
      REAL(KIND=RP) :: t, numerator, denominator
      REAL(KIND=RP) :: p
      LOGICAL       :: atNode
      
      LOGICAL, EXTERNAL :: AlmostEqual
      
!
!     --------------------------
!     See if the point is a node
!     --------------------------
!
      atNode = .FALSE.
      numerator   = 0.0_RP
      DO j = 0, N 
         IF( AlmostEqual( x, nodes(j) ) )    THEN
            atNode      = .TRUE.
            p           = values(j)
            denominator = -weights(j)
            i           = j
            EXIT 
         END IF
      END DO
      
      IF ( atNode )     THEN
         DO j = 0, N
            IF( j == i )    CYCLE
            numerator = numerator + weights(j)*(p - values(j))/( x - nodes(j) )
         END DO
      ELSE
         p = LagrangeInterpolation( x, N, nodes, values, weights)
         denominator = 0.0_RP
         DO j = 0, N
            t = weights(j)/( x - nodes(j) )
            numerator = numerator + t*(p - values(j))/( x - nodes(j) )
            denominator = denominator + t
         END DO
      END IF
      LagrangeInterpolantDerivative = numerator/denominator

      END FUNCTION LagrangeInterpolantDerivative
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE InterpolateToNewPoints( N, M, T, f, fInterp )
!
!-------------------------------------------------------------------
!!    Use matrix Multiplication to interpolate between two sets of
!!    nodes.
!-------------------------------------------------------------------
!
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER                          , INTENT(IN)  :: N, M
      REAL(KIND=RP), DIMENSION(0:M,0:N), INTENT(IN)  :: T
      REAL(KIND=RP), DIMENSION(0:N)    , INTENT(IN)  :: f
      REAL(KIND=RP), DIMENSION(0:M)    , INTENT(OUT) :: fInterp
!
!     ---------------
!     Local Variables
!     ---------------
!
      INTEGER       :: i,j
      REAL(KIND=RP) :: tmp
      
      DO i = 0, M
         tmp = 0.0_RP
         DO j = 0, N
            tmp = tmp + T(i,j)*f(j)
         END DO 
         fInterp(i) = tmp
      END DO 
      
      END SUBROUTINE InterpolateToNewPoints
!
!
!  **********     
   END MODULE PolynomialInterpolationModule
!  **********     

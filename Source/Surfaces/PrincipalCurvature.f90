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
! --- End License
!
!////////////////////////////////////////////////////////////////////////
!
!      PrincipalCurvature.f90
!      Created: March 19, 2025 at 10:54 AM
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
   Module PrincipalCurvatureModule
   USE SMConstants
   IMPLICIT NONE
!
!-----------------------------------------------------------------------
! Two procedures are included with which to compute the maximum
! absolute value of the principal curvature at a point (x,y)
! of a surface z = f(x,y).
!
! (1) PrincipalCurvature(gradF, HessF)
!         REAL(KIND=RP) :: gradF(2)
!         REAL(KIND=RP) :: hessF(2,2)
!
!    The first takes the gradient and Hessian at the point (x,y) and
!    returns the result. Use this if the gradient and hessian can
!    be computed externally.
!
! (2) PrincipalCurvature(f, x, y)
!         REAL(KIND=RP), EXTERNAL :: f ! = f(x,y)
!
!    The second takes a function f(x,y) and uses a second order finite
!    difference approximation to computed the required derivatives.
!
!    TO USE THIS FORM, THE FUNCTION f(x,y) SHOULD BE DEFINED FOR ALL POINTS (X,Y)
!    No boundary point restriction is used in the finite difference approximation.
!
!  Five test routines are included:
!
!      SUBROUTINE ExactAndComputedSphereCurvature(x,y,exact,computed)
!      SUBROUTINE ExactAndComputedQuadraticCurvature(x,y,exact,computed)
!      SUBROUTINE ExactAndComputedQuadraticGradients(x,y,exact,computed)
!      SUBROUTINE ExactAndComputedQuadraticHessian(x,y,exact,computed)
!      SUBROUTINE ExactAndComputedHyParCurvature(x, y, exact, computed)
!
!   All should return results equal to a tolerance of 2d-8
!
! For formulas, see: https://web.mit.edu/hyperbook/Patrikalakis-Maekawa-Cho/node32.html
!-------------------------------------------------------------------
!
   REAL(KIND=RP), PRIVATE  :: gc_dx = 1.0d-4, gc_dx2 = 1.0d-8

   INTERFACE PrincipalCurvature
      MODULE PROCEDURE :: PrincipalCurvatureWithDerivs, PrincipalCurvatureWithFun
   END INTERFACE

   PRIVATE :: HyParSurf, SphereSurf, HyParSurfCurvature
   PRIVATE :: ComputeApproximateGrad, ComputeApproximateHessian
!
!  ========
   CONTAINS
!  ========
!
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE ComputeApproximateGrad(f, x, y, gradF)
      IMPLICIT NONE
      REAL(KIND=RP) :: f ! = f(x,y)
      REAL(KIND=RP) :: x, y
      REAL(KIND=RP) :: gradF(2)

      gradF(1) = 0.5_RP*(f(x + gc_dx, y) - f(x - gc_dx, y))/gc_dx
      gradF(2) = 0.5_RP*(f(x, y + gc_dx) - f(x, y - gc_dx))/gc_dx

   END SUBROUTINE ComputeApproximateGrad
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE ComputeApproximateHessian(f, x, y, hessian)
      IMPLICIT NONE
      REAL(KIND=RP) :: f ! = f(x,y)
      REAL(KIND=RP) :: x, y
      REAL(KIND=RP) :: hessian(2,2)
      REAL(KIND=RP) :: fAtXY

      fAtXY = f(x, y)
      hessian(1,1) = (f(x + gc_dx, y) - 2.0_RP*fAtXY + f(x - gc_dx, y))/gc_dx2
      hessian(2,2) = (f(x, y + gc_dx) - 2.0_RP*fAtXY + f(x, y - gc_dx))/gc_dx2
      hessian(1,2) = 0.25_RP*(f(x + gc_dx, y + gc_dx) - f(x - gc_dx, y + gc_dx) - &
                              f(x + gc_dx, y - gc_dx) + f(x - gc_dx, y - gc_dx))/gc_dx2
      hessian(2,1) = hessian(1,2)

   END SUBROUTINE ComputeApproximateHessian
!
!////////////////////////////////////////////////////////////////////////
!
   REAL(KIND=RP) FUNCTION PrincipalCurvatureWithDerivs(gradF, hessF)
      IMPLICIT NONE
      REAL(KIND=RP) :: gradF(2)
      REAL(KIND=RP) :: hessF(2,2)

      PrincipalCurvatureWithDerivs = MAXVAL(ABS(PrincipalCurvaturesWithDerivs(gradF, hessF)))

   END FUNCTION PrincipalCurvatureWithDerivs
!
!////////////////////////////////////////////////////////////////////////
!
   FUNCTION PrincipalCurvaturesWithDerivs(gradF, hessF)  RESULT(pc)
      IMPLICIT NONE
      REAL(KIND=RP) :: gradF(2)
      REAL(KIND=RP) :: hessF(2,2)
      REAL(KIND=RP) :: pc(2)
      REAL(KIND=RP) :: nrm, TM(2,2), TC(2,2)
      REAL(KIND=RP) :: k1, k2, H, K, detTM, detTC

      nrm = SQRT(1.0_RP + gradF(1)**2 + gradF(2)**2)

      TM(1,1) = 1.0_RP + gradF(1)**2
      TM(1,2) =          gradF(1)*gradF(2)
      TM(2,1) = TM(1,2)
      TM(2,2) = 1.0_RP + gradF(2)**2
      detTM   = TM(1,1)*TM(2,2) - TM(1,2)**2

      TC(1,1) = hessF(1,1)/nrm
      TC(1,2) = hessF(1,2)/nrm
      TC(2,1) = TC(1,2)
      TC(2,2) = hessF(2,2)/nrm
      detTC   = TC(1,1)*TC(2,2) - TC(1,2)**2

      K = detTC/detTM
      H = 0.5_RP*(TC(1,1)*TM(2,2) - 2.0_RP*TC(1,2)*TM(1,2) + TC(2,2)*TM(1,1))/detTM

      k1 = H + SQRT(H*H - K)
      k2 = H - SQRT(H*H - K)

      pc = [k1,k2]

   END FUNCTION PrincipalCurvaturesWithDerivs
!
!////////////////////////////////////////////////////////////////////////
!
   REAL(KIND=RP) FUNCTION PrincipalCurvatureWithFun(f, x, y)
      IMPLICIT NONE
      REAL(KIND=RP), EXTERNAL :: f ! = f(x,y)
      REAL(KIND=RP) :: x, y
      REAL(KIND=RP) :: gradF(2)
      REAL(KIND=RP) :: hessF(2,2)

      CALL ComputeApproximateGrad(f, x, y, gradF)
      CALL ComputeApproximateHessian(f, x, y, hessF)
      PrincipalCurvatureWithFun = PrincipalCurvatureWithDerivs(gradF, hessF)

   END FUNCTION PrincipalCurvatureWithFun
!
!////////////////////////////////////////////////////////////////////////
!
!            TEST FUNCTIONS
!
!////////////////////////////////////////////////////////////////////////
!
   REAL(KIND=RP) FUNCTION QuadSurf(x, y)
      IMPLICIT NONE
      REAL(KIND=RP) :: x, y

      QuadSurf = x**2 + y**2

   END FUNCTION QuadSurf
!
!////////////////////////////////////////////////////////////////////////
!
   REAL(KIND=RP) FUNCTION HyParSurf(x, y)
      IMPLICIT NONE
      REAL(KIND=RP) :: x, y

      HyParSurf = x*y

   END FUNCTION HyParSurf
!
!////////////////////////////////////////////////////////////////////////
!
   REAL(KIND=RP) FUNCTION SphereSurf(x, y)
      IMPLICIT NONE
      REAL(KIND=RP) :: x, y

      SphereSurf = SQRT(1.0_RP - x**2 - y**2)

   END FUNCTION SphereSurf
!
!////////////////////////////////////////////////////////////////////////
!
   FUNCTION HyParSurfCurvatures(x, y) RESULT(pc)
      IMPLICIT NONE
      REAL(KIND=RP) :: x, y
      REAL(KIND=RP) :: pc(2)
      REAL(KIND=RP) :: a, b, c

      a = SQRT((1.0_RP + x*x)*(1.0_RP + y*y))
      b = (x*x + y*y + 1.0_RP)**1.5_RP

      pc(1) = (-x*y + a)/b
      pc(2) = (-x*y - a)/b

   END FUNCTION HyParSurfCurvatures
!
!////////////////////////////////////////////////////////////////////////
!
   REAL(KIND=RP) FUNCTION HyParSurfCurvature(x, y)
      IMPLICIT NONE
      REAL(KIND=RP) :: x, y
      REAL(KIND=RP) :: pc(2)

      pc                 = HyParSurfCurvatures(x, y)
      HyParSurfCurvature = MAXVAL(ABS(pc))

   END FUNCTION HyParSurfCurvature
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE ExactAndComputedSphereCurvature(x, y, exact, computed)
      IMPLICIT NONE
      REAL(KIND=RP) :: x, y, exact, computed

      exact    = 1.0_RP
      computed = PrincipalCurvature(SphereSurf, x, y)

   END SUBROUTINE ExactAndComputedSphereCurvature
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE ExactAndComputedHyParCurvature(x, y, exact, computed)
      IMPLICIT NONE
      REAL(KIND=RP) :: x, y, exact, computed

      exact    = HyParSurfCurvature(x, y)
      computed = PrincipalCurvature(HyParSurf, x, y)

   END SUBROUTINE ExactAndComputedHyParCurvature
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE ExactAndComputedQuadraticGradients(x, y, exact, computed)
      IMPLICIT NONE
      REAL(KIND=RP) :: x, y
      REAL(KIND=RP) :: exact(2), computed(2)

      exact(1) = 2.0_RP*x
      exact(2) = 2.0_RP*y

      CALL ComputeApproximateGrad(QuadSurf, x, y, computed)

   END SUBROUTINE ExactAndComputedQuadraticGradients
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE ExactAndComputedQuadraticHessian(x, y, exact, computed)
      IMPLICIT NONE
      REAL(KIND=RP) :: x, y
      REAL(KIND=RP) :: exact(2,2), computed(2,2)

      exact      = 2.0
      exact(2,1) = 0.0
      exact(1,2) = 0.0

      CALL ComputeApproximateHessian(QuadSurf, x, y, computed)

   END SUBROUTINE ExactAndComputedQuadraticHessian

END Module PrincipalCurvatureModule

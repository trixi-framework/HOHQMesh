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
! HOHQMesh contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend, 
!    https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
! * `fmin`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
! * `spline`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
! * `seval`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
!
! --- End License
!
!////////////////////////////////////////////////////////////////////////
!
!      GaussianCurvature.f90
!      Created: November 23, 2021 at 10:15 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   Module GaussianCurvatureModule
   USE SMConstants
   IMPLICIT NONE  
! 
!------------------------------------------------------------------- 
! Two procedures are included with which to compute the Gaussian
! curvature at a point (x,y) of a surface z = f(x,y). 
!
! (1) GaussianCurvature(gradF, HessF)
!         REAL(KIND=RP) :: gradF(2)
!         REAL(KIND=RP) :: hessF(2,2)
!
!    The first takes the gradient and Hessian at the point (x,y) and
!    returns the result. Use this if the gradient and hessian can
!    be computed externally.
!
! (2) GaussianCurvature(f, x, y)
!         REAL(KIND=RP), EXTERNAL :: f ! = f(x,y)
!
!    The second takes a function f(x,y) and uses a second order finite
!    difference approximation to computed the required derivatives.
!
!    TO USE THIS FORM, THE FUNCTION f(x,y) SHOULD BE DEFINED FOR ALL POINTS (X,Y)
!    No boundary point restriction is used in the finite difference approximation.
!
!  Four test routines are included:
!
!      SUBROUTINE ExactAndComputedSphereCurvature(x,y,exact,computed)  
!      SUBROUTINE ExactAndComputedQuadraticCurvature(x,y,exact,computed)  
!      SUBROUTINE ExactAndComputedQuadraticGradients(x,y,exact,computed)
!      SUBROUTINE ExactAndComputedQuadraticHessian(x,y,exact,computed)
!
!   All should return results equal to a tolerance of 2d-8
!
!------------------------------------------------------------------- 
! 
   REAL(KIND=RP), PRIVATE  :: gc_dx = 1.0d-4, gc_dx2 = 1.0d-8
   
   INTERFACE GaussianCurvature
      MODULE PROCEDURE :: GaussianCurvatureWithDerivs, GaussianCurvatureWithFun
   END INTERFACE 
   
   PRIVATE :: Quadratic, Sphere, QuadraticCurvature
!
!  --------
   CONTAINS
!  --------
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
         
         fAtXY = f(x,y)
         hessian(1,1) = (f(x + gc_dx,y) - 2.0_RP*fAtXY + f(x - gc_dx,y))/gc_dx2
         hessian(2,2) = (f(x,y + gc_dx) - 2.0_RP*fAtXY + f(x,y - gc_dx))/gc_dx2
         hessian(1,2) = 0.25_RP*(f(x + gc_dx, y + gc_dx) - f(x - gc_dx, y + gc_dx) - &
                                 f(x + gc_dx, y - gc_dx) + f(x - gc_dx, y - gc_dx))/gc_dx2
         hessian(2,1) = hessian(1,2)
         
      END SUBROUTINE ComputeApproximateHessian
!
!//////////////////////////////////////////////////////////////////////// 
! 
      REAL(KIND=RP) FUNCTION GaussianCurvatureWithDerivs(gradF, hessF)  
         IMPLICIT NONE  
         REAL(KIND=RP) :: gradF(2)
         REAL(KIND=RP) :: hessF(2,2)
      
         GaussianCurvatureWithDerivs = (HessF(1,1)*hessF(2,2) - hessF(1,2)*hessF(2,1)) &
                             /(1.0_RP + gradF(1)**2 + gradF(2)**2)**2
      END FUNCTION GaussianCurvatureWithDerivs
!
!//////////////////////////////////////////////////////////////////////// 
! 
      REAL(KIND=RP) FUNCTION GaussianCurvatureWithFun(f, x, y)  
         IMPLICIT NONE  
         REAL(KIND=RP), EXTERNAL :: f ! = f(x,y)
         REAL(KIND=RP) :: x, y
         REAL(KIND=RP) :: gradF(2)
         REAL(KIND=RP) :: hessF(2,2)
         
         CALL ComputeApproximateGrad(f,x,y,gradF)
         CALL ComputeApproximateHessian(f,x,y,hessF)
         GaussianCurvatureWithFun = GaussianCurvatureWithDerivs(gradF,hessF)
         
      END FUNCTION GaussianCurvatureWithFun
!
!//////////////////////////////////////////////////////////////////////// 
!
!            TEST FUNCTIONS
!
!//////////////////////////////////////////////////////////////////////// 
! 
      REAL(KIND=RP) FUNCTION Quadratic(x,y)  
         IMPLICIT NONE  
         REAL(KIND=RP) :: x, y
         
         Quadratic = x**2 + y**2 + x*y
         
      END FUNCTION Quadratic
!
!//////////////////////////////////////////////////////////////////////// 
! 
      REAL(KIND=RP) FUNCTION Sphere(x,y)  
         IMPLICIT NONE  
         REAL(KIND=RP) :: x, y
         
         Sphere = SQRT(1.0_RP - x**2 - y**2)
         
      END FUNCTION Sphere
!
!//////////////////////////////////////////////////////////////////////// 
! 
      REAL(KIND=RP) FUNCTION QuadraticCurvature(x,y)
         IMPLICIT NONE  
         REAL(KIND=RP) :: x, y
         REAL(KIND=RP) :: fx, fy, fxx, fyy, fxy
         
         fx = 2.0_RP*x+y
         fy = 2.0_RP*y+x
         fxx = 2.0_RP
         fyy = 2.0_RP
         fxy = 1.0_RP
         
         QuadraticCurvature = (fxx*fyy - fxy**2)/(1.0_RP + fx**2 + fy**2)**2
      
      END FUNCTION QuadraticCurvature
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ExactAndComputedSphereCurvature(x,y,exact,computed)  
         IMPLICIT NONE  
         REAL(KIND=RP) :: x, y, exact, computed
         
         exact = 1.0_RP
         computed = GaussianCurvature(Sphere,x,y)
         
      END SUBROUTINE ExactAndComputedSphereCurvature
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ExactAndComputedQuadraticCurvature(x,y,exact,computed)  
         IMPLICIT NONE  
         REAL(KIND=RP) :: x, y, exact, computed
         
         exact    = QuadraticCurvature(x,y)
         computed = GaussianCurvature(Quadratic,x,y)
         
      END SUBROUTINE ExactAndComputedQuadraticCurvature
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ExactAndComputedQuadraticGradients(x,y,exact,computed)
         IMPLICIT NONE  
         REAL(KIND=RP) :: x, y
         REAL(KIND=RP) :: exact(2), computed(2)
         
         exact(1) = 2.0_RP*x+y
         exact(2) = 2.0_RP*y+x
         
         CALL ComputeApproximateGrad(Quadratic, x, y, computed)
         
      END SUBROUTINE ExactAndComputedQuadraticGradients
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ExactAndComputedQuadraticHessian(x,y,exact,computed)
         IMPLICIT NONE  
         REAL(KIND=RP) :: x, y
         REAL(KIND=RP) :: exact(2,2), computed(2,2)
         
         exact      = 2.0
         exact(2,1) = 1.0
         exact(1,2) = 1.0
         
         CALL ComputeApproximateHessian(Quadratic, x, y, computed)
         
      END SUBROUTINE ExactAndComputedQuadraticHessian
      
   END Module GaussianCurvatureModule

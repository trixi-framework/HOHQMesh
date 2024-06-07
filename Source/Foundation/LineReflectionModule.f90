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
!      LineReflectionModule.f90
!      Created: May 28, 2024 at 9:23 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   Module LineReflectionModule 
      USE SMConstants
      IMPLICIT NONE  
!
!  ========
   CONTAINS
!  ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ComputeLineCoefs(x0,x1,a,b,c)
!
!     -------------------------------------------------------
!     Given two points x0, x1, in the plane,
!     compute the coefficients
!     a, b, c for the line ax + by + c = 0 that goes through
!     the two points. Note that the coefficients are not
!     unique. The equation can be multiplied by any constant.
!     -------------------------------------------------------
!
         IMPLICIT NONE  
         REAL(KIND=RP), INTENT(IN)  :: x0(3), x1(3)
         REAL(KIND=RP), INTENT(OUT) :: a, b, c
         
         a = x0(2) - x1(2)
         b = x1(1) - x0(1)
         c = x1(2)*x0(1) - x0(2)*x1(1)
         
      END SUBROUTINE ComputeLineCoefs
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION reflectAboutLine(x,a,b,c)  RESULT(r)
!
!        --------------------------------------------
!        Reflect the point x about the line given by 
!        ax + by + c = 0 in the plane. From
!        https://math.stackexchange.com/questions/1013230/how-to-find-coordinates-of-reflected-point
!        --------------------------------------------
!
         USE SMConstants
         IMPLICIT NONE  
         REAL(KIND=RP) :: x(3), r(3)
         REAL(KIND=RP) :: a,b,c, d
         
         d = 2.0_RP*(a*x(1) + b*x(2) + c)/(a**2 + b**2)
         
         r(1) = x(1) - a*d
         r(2) = x(2) - b*d
         r(3) = 0.0_RP
         
      END FUNCTION reflectAboutLine
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE GetReflectionTestPoints(x0,x1)
!
!     -----------------------------------------
!     Two points along the line ax + by + c = 0
!     where the coefficents are defined in 
!     GetTestLineCoefficients. Used for testing
!     -----------------------------------------
!
         IMPLICIT NONE  
         REAL(KIND=RP), INTENT(OUT) :: x0(3), x1(3)
         REAL(KIND=RP)              :: a, b, c
         
         CALL GetTestLineCoefficients(a,b,c)
         
         x0(1) = 0.0_RP
         x0(2) = -(c + a*x0(1))/b
         x0(3) = 0.0_RP
         
         x1(1) = 1.0_RP
         x1(2) = -(c + a*x1(1))/b
         x1(3) = 0.0_RP
         
      END SUBROUTINE GetReflectionTestPoints
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION lineCoefficientTestError()  RESULT(e)
!
!        ---------------------------------------------------------
!        Compute two points along a line ax+by+c = 0 and 
!        reverse to compute a,b,c. The results should be the same 
!        to within a constant, returning e \approx 0. 
!        ---------------------------------------------------------
!
         IMPLICIT NONE  
         REAL(KIND=RP) :: x0(3), x1(3)
         REAL(KIND=RP) :: a, b, c
         REAL(KIND=RP) :: aT, bT, cT
         REAL(KIND=RP) :: e, r1, r2, r
         
         CALL GetReflectionTestPoints(x0,x1)
         CALL ComputeLineCoefs(x0,x1,a,b,c)
         CALL GetTestLineCoefficients(aT,bT,cT)
!
!        ----------------------------------------------------
!        The coefficients can be scaled differently, but must
!        all have the same scale
!        ----------------------------------------------------
!
         r1 = MAX(ABS(at),ABS(bt),ABS(ct))
         r2 = MAX(ABS(a),ABS(b),ABS(c))
         r  = r1/r2
         
         e = MAX(ABS(r*a-aT), ABS(r*b-BT), ABS(r*c-cT))

      END FUNCTION lineCoefficientTestError
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION lineReflectionTestError() RESULT(e)
!
!        ------------------------------------------------------
!        Reflect a point about a line and compare to the exact.
!        ------------------------------------------------------
!
         IMPLICIT NONE  
         REAL(KIND=RP) :: x0(3), x1(3), p(3), pr(3)
         REAL(KIND=RP) :: a, b, c, e
         REAL(KIND=RP) :: pre(3) = [0.0_RP,1.5_RP,0.0_RP]
         
         x0 = [0.0_RP,0.5_RP,0.0_RP]
         x1 = [1.0_RP,1.5_RP,0.0_RP]
         CALL ComputeLineCoefs(x0,x1,a,b,c)
         
         p  = [1.0,0.5,0.0]
         pr = reflectAboutLine(p,a,b,c)
         e = MAXVAL(ABS(pr-pre))
         
      END FUNCTION lineReflectionTestError
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE GetTestLineCoefficients(a,b,c)
!
!        -----------------------------------------------
!        For testing purposes, set Coefficients for a line
!        ax+by+c = 0
!        -----------------------------------------------
!
         IMPLICIT NONE  
         REAL(KIND=RP), INTENT(OUT) :: a, b, c
         
         a =  3.0_RP
         b =  4.0_RP
         c =  7.0_RP
         
      END SUBROUTINE GetTestLineCoefficients
   
   END Module LineReflectionModule

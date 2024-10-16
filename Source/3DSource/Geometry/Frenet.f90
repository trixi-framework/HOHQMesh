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
!      Frenet.f90
!      Created: April 30, 2020 at 2:00 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module Frenet 
      USE SMCurveClass
      USE Geometry3DModule
      IMPLICIT NONE  
      
      TYPE FrenetFrame
         REAL(KIND=RP) :: tangent(3)
         REAL(KIND=RP) :: normal(3)
         REAL(KIND=RP) :: coNormal(3)
      END TYPE FrenetFrame
      
      REAL(KIND=RP), PARAMETER :: zeroNormSize = 1.d-7
! 
!     ========
      CONTAINS
!     ========
!  
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ComputeFrenetFrame(frame, t, curve, isDegenerate)  
         IMPLICIT NONE
!
!        ---------
!        arguments
!        ---------
!
         CLASS(SMCurve)     :: curve
         TYPE(FrenetFrame)  :: frame
         REAL(KIND=RP)      :: t
         LOGICAL            :: isDegenerate
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP) :: x(3), xp(3), xpp(3)
         REAL(KIND=RP) :: d1
         REAL(KIND=RP) :: tangent(3), B(3), Nhat(3)

         x   = curve % positionAt(t)
         xp  = curve % tangentAt(t)
         xpp = curve % secondDerivativeAt(t)
!
!        --------------
!        Tangent vector
!        --------------
!
         tangent = xp
         CALL Normalize(tangent)
!
!        ---------------
!        Binormal vector
!        ---------------
!
         CALL Cross3D(u = tangent,v = xpp,cross = B)
         CALL Norm3D(u = B,norm = d1)
         isDegenerate = .TRUE.
         IF ( d1 .gt. zeroNormSize )     THEN
            CALL Normalize(B)
            isDegenerate = .FALSE.
         END IF 
!
!        -------------
!        Normal vector
!        -------------
!            
         CALL Cross3D(u = B, v = tangent, cross = Nhat)
         
         frame % coNormal = B
         frame % normal   = Nhat
         frame % tangent  = tangent

      END SUBROUTINE ComputeFrenetFrame
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ComputeParallelFrame(t, curve, frame, refFrame)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(FrenetFrame), INTENT(IN ) :: refFrame
         TYPE(FrenetFrame), INTENT(OUT) :: frame
         REAL(KIND=RP)                  :: t
         CLASS(SMCurve)                 :: curve
!
!        ---------------
!        Local Variables
!        ---------------
!
         REAL(KIND=RP) :: d1
         LOGICAL       :: isDegenerate
         
         CALL ComputeFrenetFrame(frame        = frame, &
                                 t            = t,     &
                                 curve        = curve, &
                                 isDegenerate = isDegenerate)
         
         IF ( isDegenerate )     THEN
            frame % coNormal = refFrame % coNormal
            frame % normal   = refFrame % normal
         END IF 
         
         CALL Dot3D(u = refFrame % coNormal,v = frame % coNormal, dot = d1)
         IF(d1 < -zeroNormSize)     THEN
            frame % coNormal = -frame % coNormal
            CALL Cross3D(u = frame % coNormal, v = frame % tangent, cross = frame % normal)
         ENDIF 
         
      END SUBROUTINE ComputeParallelFrame
!
!//////////////////////////////////////////////////////////////////////// 
! 
      REAL(KIND=RP) FUNCTION FrameAngle(frame1, frame2)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(FrenetFrame) :: frame1, frame2
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP) :: cosTheta
         
         CALL Dot3D(u = frame1 % normal ,v = frame2 % normal, dot = cosTheta)
         cosTheta = MIN(cosTheta, 1.0_RP)
         
         FrameAngle = ACOS(cosTheta)
         
      END FUNCTION FrameAngle
      
      END Module Frenet

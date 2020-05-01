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
      
      REAL(KIND=RP), PARAMETER, PRIVATE :: zeroNormSize = 1.d-7
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
         REAL(KIND=RP) :: d1, d2, d3, theta, cosTheta
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
         IF ( d1 .ge. 1.d-7 )     THEN
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
         REAL(KIND=RP) :: B1(3), B2(3)
         REAL(KIND=RP) :: d1
         LOGICAL       :: isDegenerate
         
         CALL ComputeFrenetFrame(frame        = frame, &
                                 t            = t,     &
                                 curve        = curve, &
                                 isDegenerate = isDegenerate)

         CALL Norm3D(u = frame % coNormal, norm = d1)
         
         IF ( isDegenerate )     THEN
            frame % coNormal = refFrame % coNormal
            frame % normal   = refFrame % normal
         END IF 
         
         CALL Dot3D(u = refFrame % coNormal,v = frame % coNormal, dot = d1)
         IF(d1 < 0.0_RP)     THEN
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

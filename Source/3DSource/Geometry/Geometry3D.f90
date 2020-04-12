!
!////////////////////////////////////////////////////////////////////////
!
!      Geometry3D.f90
!      Created: April 9, 2013 4:12 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module Geometry3DModule 
      USE SMConstants
      IMPLICIT NONE 
      
      TYPE AffineTransform
         REAL(KIND=RP)    :: translation(3)
         REAL(KIND=RP)    :: rotMatrix(3,3)
         LOGICAL, PRIVATE :: isIdentityTransform
      END TYPE AffineTransform
      
      TYPE ScaleTransform
         REAL(KIND=RP)    :: origin(3)
         REAL(KIND=RP)    :: scaleFactor
         LOGICAL, PRIVATE :: isIdentityScale
      END TYPE ScaleTransform
      
      REAL(KIND=RP), PRIVATE :: vectorDifferenceTolerance = 1.0d-2
!
!     ========
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ConstructIdentityAffineTransform(self)  
         IMPLICIT NONE  
         TYPE(AffineTransform) :: self
         
         self % translation    = 0.0_RP
         self % rotMatrix      = 0.0
         self % rotMatrix(1,1) = 1.0_RP
         self % rotMatrix(2,2) = 1.0_RP
         self % rotMatrix(3,3) = 1.0_RP
         self % isIdentityTransform = .TRUE.
      END SUBROUTINE ConstructIdentityAffineTransform
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ConstructAffineTransform(self, translation, startDirection, newDirection)  
         IMPLICIT NONE  
         TYPE(AffineTransform)  :: self
         REAL(KIND=RP)          :: translation(3)
         REAL(KIND=RP)          :: startDirection(3)
         REAL(KIND=RP)          :: newDirection(3)
         
         self % translation = translation
         CALL RotationMatrix(old = startDirection, &
                             new = newDirection,   &
                             R = self % rotMatrix)
         self % isIdentityTransform = .FALSE.
      END SUBROUTINE ConstructAffineTransform
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION PerformAffineTransform(x,transformation)  RESULT(y)
         IMPLICIT NONE
         REAL(KIND=RP)         :: x(3)
         REAL(KIND=RP)         :: y(3)
         TYPE(AffineTransform) :: transformation
         
         IF ( transformation % isIdentityTransform )     THEN
            y = x 
         ELSE 
            CALL AffineTransformWithRAndShift(xOld        = x,                            &
                                              xNew        = y,                            &
                                              rotMatrix   = transformation % rotMatrix,   &
                                              shiftVector = transformation % translation)
         END IF 

      END FUNCTION PerformAffineTransform
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION isIdentityTransform(self)  
         IMPLICIT NONE  
         TYPE(AffineTransform)  :: self
         isIdentityTransform = self % isIdentityTransform
      END FUNCTION isIdentityTransform
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ConstructIdentityScaleTransform(self)
         IMPLICIT NONE  
         TYPE(ScaleTransform) :: self
         self % origin      = 0.0_RP
         self % scaleFactor = 1.0_RP
         self % isIdentityScale = .TRUE.
      END SUBROUTINE ConstructIdentityScaleTransform
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ConstructScaleTransform(self, origin, factor)
         IMPLICIT NONE
         TYPE(ScaleTransform) :: self
         REAL(KIND=RP)        :: origin(3)
         REAL(KIND=RP)        :: factor
         self % origin      = origin
         self % scaleFactor = factor
         self % isIdentityScale = .FALSE.
      END SUBROUTINE ConstructScaleTransform
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION PerformScaleTransformation(x, transformation)  RESULT(y)
         IMPLICIT NONE  
         TYPE(ScaleTransform) :: transformation
         REAL(KIND=RP)        :: x(3)
         REAL(KIND=RP)        :: y(3)
         
         y = transformation % scaleFactor*(x- transformation % origin) + transformation % origin
         
      END FUNCTION PerformScaleTransformation
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION isIdentityScale(self)  
         IMPLICIT NONE  
         TYPE(ScaleTransform) :: self
         isIdentityScale = self % isIdentityScale
      END FUNCTION isIdentityScale
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE Cross3D(u,v,cross)
         IMPLICIT NONE
         REAL(KIND=RP), INTENT(IN)  :: u(3), v(3)
         REAL(KIND=RP), INTENT(OUT) :: cross(3)
         
         cross(1) =   u(2)*v(3) - v(2)*u(3)
         cross(2) = -(u(1)*v(3) - v(1)*u(3))
         cross(3) =   u(1)*v(2) - v(1)*u(2)
      END SUBROUTINE Cross3D
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE Dot3D(u,v,dot)  
         IMPLICIT NONE  
         REAL(KIND=RP), INTENT(IN)  :: u(3), v(3)
         REAL(KIND=RP), INTENT(OUT) :: dot
         
         dot = u(1)*v(1) + u(2)*v(2) + u(3)*v(3)
         
      END SUBROUTINE Dot3D
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE Norm3D(u,norm)  
         IMPLICIT NONE  
         REAL(KIND=RP), INTENT(IN)  :: u(3)
         REAL(KIND=RP), INTENT(OUT) :: norm
         
         norm = u(1)*u(1) + u(2)*u(2) + u(3)*u(3)
         norm = SQRT(norm)
         
      END SUBROUTINE Norm3D
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE RotationMatrix(old,new,R)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP), INTENT(IN)  :: old(3), new(3)
         REAL(KIND=RP), INTENT(OUT) :: R(3,3)
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP) :: u(3) , v(3)
         REAL(KIND=RP) :: norm, cosTheta, sinTheta
         REAL(KIND=RP) :: rotVec(3), cross(3)
!
!        -----------------------
!        Do nothing if old = new
!        -----------------------
!
         IF(MAXVAL(ABS(old-new)) < vectorDifferenceTolerance)     THEN 
            R = 0.0_RP
            R(1,1) = 1; R(2,2) = 1; R(3,3) = 1
            RETURN 
         END IF 
!
!        -------------------------------------
!        Compute the rotation vector and angle
!        -------------------------------------
!
         CALL Cross3D(old,new,rotVec)
         CALL Norm3D(rotVec,norm)
         rotVec = rotVec/norm
         
         CALL Norm3D(old,norm)
         u = old/norm
         CALL Norm3D(new,norm)
         v = new/norm
         CALL Dot3D(u,v,cosTheta)
         
         CALL Cross3D(u,v,cross)
         CALL Norm3D(cross,sinTheta)
!
!        ---------------------------
!        Compute the rotation matrix
!        ---------------------------
!
         R(1,1) = cosTheta + rotVec(1)**2*(1.0_RP - costheta)
         R(1,2) = rotVec(1)*rotVec(2)*(1.0_RP - costheta) - rotVec(3)*sinTheta
         R(1,3) = rotVec(1)*rotVec(3)*(1.0_RP - costheta) + rotVec(2)*sinTheta
         
         R(2,1) = rotVec(2)*rotVec(1)*(1.0_RP - costheta) + rotVec(3)*sinTheta
         R(2,2) = cosTheta + rotVec(2)**2*(1.0_RP - costheta)
         R(2,3) = rotVec(2)*rotVec(3)*(1.0_RP - costheta) - rotVec(1)*sinTheta
         
         R(3,1) = rotVec(3)*rotVec(1)*(1.0_RP - costheta) - rotVec(2)*sinTheta
         R(3,2) = rotVec(3)*rotVec(2)*(1.0_RP - costheta) + rotVec(1)*sinTheta
         R(3,3) = cosTheta + rotVec(3)**2*(1.0_RP - costheta)
      END SUBROUTINE RotationMatrix
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE AffineTransformWithRAndShift(xOld,xNew,rotMatrix,shiftVector) 
!
!        --------------------------------
!        Perform the transformation
!        \vec x'prime = R\vec x  + \vec b
!        --------------------------------
! 
         IMPLICIT NONE
         REAL(KIND=RP), DIMENSION(3)   :: xOld, xNew, shiftVector
         REAL(KIND=RP), DIMENSION(3,3) :: rotMatrix
         
         xNew(1) = rotMatrix(1,1)*xOld(1) + rotMatrix(1,2)*xOld(2) + rotMatrix(1,3)*xOld(3)
         xNew(2) = rotMatrix(2,1)*xOld(1) + rotMatrix(2,2)*xOld(2) + rotMatrix(2,3)*xOld(3)
         xNew(3) = rotMatrix(3,1)*xOld(1) + rotMatrix(3,2)*xOld(2) + rotMatrix(3,3)*xOld(3)
         
         xNew = xNew + shiftVector
         
      END SUBROUTINE AffineTransformWithRAndShift
      
      END Module Geometry3DModule

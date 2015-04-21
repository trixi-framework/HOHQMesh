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
! 
      CONTAINS
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
      SUBROUTINE AffineTransform(xOld,xNew,rotMatrix,shiftVector) 
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
         
      END SUBROUTINE AffineTransform
      
      END Module Geometry3DModule

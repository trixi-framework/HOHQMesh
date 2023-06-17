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
!      Geometry3D.f90
!      Created: April 9, 2013 4:12 PM
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
      Module Geometry3DModule
      USE SMConstants
      IMPLICIT NONE

      TYPE RotationTransform
         REAL(KIND=RP)    :: rotationPoint(3)
         REAL(KIND=RP)    :: rotMatrix(3,3)
         LOGICAL          :: isIdentityRotation
      END TYPE RotationTransform

      TYPE TranslationTransform
         REAL(KIND=RP)    :: translation(3)
      END TYPE TranslationTransform

      TYPE ScaleTransform
         REAL(KIND=RP)    :: origin(3)
         REAL(KIND=RP)    :: normal(3)
         REAL(KIND=RP)    :: scaleFactor
         LOGICAL, PRIVATE :: isIdentityScale
      END TYPE ScaleTransform

      REAL(KIND=RP), PRIVATE :: vectorDifferenceTolerance = 1.0d-3
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
!                 ROTATIONS
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructIdentityRotationTransform(self)
         IMPLICIT NONE
         TYPE(RotationTransform) :: self

         self % rotMatrix      = 0.0
         self % rotMatrix(1,1) = 1.0_RP
         self % rotMatrix(2,2) = 1.0_RP
         self % rotMatrix(3,3) = 1.0_RP
         self % isIdentityRotation = .TRUE.
         self % rotationPoint = 0.0_RP
      END SUBROUTINE ConstructIdentityRotationTransform
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructRotationTransform(self, rotationPoint, startDirection, newDirection)
         IMPLICIT NONE
         TYPE(RotationTransform)  :: self
         REAL(KIND=RP)            :: rotationPoint(3)
         REAL(KIND=RP)            :: startDirection(3)
         REAL(KIND=RP)            :: newDirection(3)

         self % rotationPoint = rotationPoint

         CALL RotationMatrixWithTwoVectors(old = startDirection, &
                                           new = newDirection,   &
                                           R   = self % rotMatrix)
         !TODO The following doesn't work when the vectors are in the opposite direction
         IF(MAXVAL(ABS(startDirection-newDirection)) < vectorDifferenceTolerance)     THEN
            self % isIdentityRotation = .TRUE.
         ELSE
            self % isIdentityRotation = .FALSE.
         END IF

      END SUBROUTINE ConstructRotationTransform
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION PerformRotationTransform(x,transformation)  RESULT(y)
         IMPLICIT NONE
         REAL(KIND=RP)           :: x(3)
         REAL(KIND=RP)           :: y(3)
         TYPE(RotationTransform) :: transformation

         IF ( transformation % isIdentityRotation )     THEN
            y = x
         ELSE
            CALL RotationTransformWithRAndShift(xOld        = x,                              &
                                                xNew        = y,                              &
                                                rotMatrix   = transformation % rotMatrix,     &
                                                rotationPoint = transformation % rotationPoint)
         END IF

      END FUNCTION PerformRotationTransform
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION isIdentityRotation(self)
         IMPLICIT NONE
         TYPE(RotationTransform)  :: self
         isIdentityRotation = self % isIdentityRotation
      END FUNCTION isIdentityRotation
!
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RotationTransformWithRAndShift(xOld,xNew,rotMatrix,rotationPoint)
!
!        --------------------------------
!        Perform the transformation
!        \vec x'prime = R\vec x  + \vec b
!        --------------------------------
!
         IMPLICIT NONE
         REAL(KIND=RP), DIMENSION(3)   :: xOld, xNew, rotationPoint, xS
         REAL(KIND=RP), DIMENSION(3,3) :: rotMatrix

         xS = xOld - rotationPoint

         xNew(1) = rotMatrix(1,1)*xS(1) + rotMatrix(1,2)*xS(2) + rotMatrix(1,3)*xS(3)
         xNew(2) = rotMatrix(2,1)*xS(1) + rotMatrix(2,2)*xS(2) + rotMatrix(2,3)*xS(3)
         xNew(3) = rotMatrix(3,1)*xS(1) + rotMatrix(3,2)*xS(2) + rotMatrix(3,3)*xS(3)

         xNew = xNew + rotationPoint

      END SUBROUTINE RotationTransformWithRAndShift
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RotationMatrixWithTwoVectors(old,new,R)
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
            R(1,1) = 1.0_RP; R(2,2) = 1.0_RP; R(3,3) = 1.0_RP
            RETURN
         END IF
         R = 0.0_RP
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
         CALL RotationMatrixWithNormalAndAngle(nHat     = rotVec,    &
                                               cosTheta = cosTheta,  &
                                               SinTheta = sinTheta,  &
                                               R        = R)

      END SUBROUTINE RotationMatrixWithTwoVectors
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RotationMatrixWithNormalAndAngle(nHat,cosTheta, SinTheta, R)
         IMPLICIT NONE
         REAL(KIND=RP) :: nHat(3)
         REAL(KIND=RP) :: cosTheta, sinTheta
         REAL(KIND=RP) :: R(3,3)

         R(1,1) = cosTheta + nHat(1)**2*(1.0_RP - cosTheta)
         R(1,2) = nHat(1)*nHat(2)*(1.0_RP - cosTheta) - nHat(3)*sinTheta
         R(1,3) = nHat(1)*nHat(3)*(1.0_RP - cosTheta) + nHat(2)*sinTheta

         R(2,1) = nHat(2)*nHat(1)*(1.0_RP - cosTheta) + nHat(3)*sinTheta
         R(2,2) = cosTheta + nHat(2)**2*(1.0_RP - cosTheta)
         R(2,3) = nHat(2)*nHat(3)*(1.0_RP - cosTheta) - nHat(1)*sinTheta

         R(3,1) = nHat(3)*nHat(1)*(1.0_RP - cosTheta) - nHat(2)*sinTheta
         R(3,2) = nHat(3)*nHat(2)*(1.0_RP - cosTheta) + nHat(1)*sinTheta
         R(3,3) = cosTheta + nHat(3)**2*(1.0_RP - cosTheta)

      END SUBROUTINE RotationMatrixWithNormalAndAngle
!
!////////////////////////////////////////////////////////////////////////
!
!                 SCALING
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructIdentityScaleTransform(self)
         IMPLICIT NONE
         TYPE(ScaleTransform) :: self

         self % origin      = 0.0_RP
         self % normal      = [0.0_RP,0.0_RP,1.0_RP]
         self % scaleFactor = 1.0_RP
         self % isIdentityScale = .TRUE.

      END SUBROUTINE ConstructIdentityScaleTransform
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructScaleTransform(self, origin, normal, factor)
         IMPLICIT NONE
         TYPE(ScaleTransform) :: self
         REAL(KIND=RP)        :: origin(3), normal(3)
         REAL(KIND=RP)        :: factor

         self % origin      = origin
         self % normal      = normal
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
         REAL(KIND=RP)        :: y(3), xPerp(3), xDotN
!
!        ---------------
!        Move the origin
!        ---------------
!
         y = x - transformation % origin
!
!        -------------------------
!        Get trangential component
!        -------------------------
!
         CALL Dot3D(u = transformation % normal,v = y,dot = xDotN)
         xPerp = y - xDotN*transformation % normal
!
!        ------------------------------------------------------------------------
!        Scale the tangential component, add back normal component and shift back
!        to current location
!        ------------------------------------------------------------------------
!
         y = transformation % scaleFactor*xPerp + xDotN*transformation % normal
         y = y + transformation % origin

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
!                 TRANSLATIONS
!
!////////////////////////////////////////////////////////////////////////
!
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructTranslationTransform(self, translation)
         IMPLICIT NONE
         TYPE(TranslationTransform) :: self
         REAL(KIND=RP)              :: translation(3)

         self % translation = translation

      END SUBROUTINE ConstructTranslationTransform
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION PerformTranslationTransform(self, x)    RESULT(y)
         IMPLICIT NONE
         TYPE(TranslationTransform) :: self
         REAL(KIND=RP)              :: x(3), y(3)

         y = x + self % translation

      END FUNCTION PerformTranslationTransform
!
!////////////////////////////////////////////////////////////////////////
!
!                 VECTOR OPS
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
      SUBROUTINE Normalize(u)
         IMPLICIT NONE
         REAL(KIND=RP), INTENT(INOUT)  :: u(3)

         REAL(KIND=RP)                 :: norm

         CALL Norm3D(u,norm)
         u = u/norm
      END SUBROUTINE Normalize

      END Module Geometry3DModule

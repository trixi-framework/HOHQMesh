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
!      SMTopographyClass.f90
!      Created: 9/18/20, 8:34 AM
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
      Module SMTopographyClass
      USE SMConstants
      USE FTObjectClass
      USE GaussianCurvatureModule
      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      INTEGER, PARAMETER :: SM_SURFACE_NAME_LENGTH = 32
      CHARACTER(LEN=6), PARAMETER ::TOPOGRAPHY_SIZING_KEY = "sizing"
!
!     ---------------------
!     Base class definition
!     ---------------------
!
      TYPE, EXTENDS(FTObject) :: SMTopography
         INTEGER                              , PRIVATE :: id_
         CHARACTER(LEN=SM_SURFACE_NAME_LENGTH), PRIVATE :: surfaceName_
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE, NON_OVERRIDABLE :: initTopography
         FINAL                      :: destructBaseTopography
         PROCEDURE                  :: printDescription   => printTopographyDescription
         PROCEDURE                  :: heightAt => heightAtTopography
         PROCEDURE                  :: gaussianCurvatureAt => gaussianCurvatureBaseAt
      END TYPE SMTopography
!
!     -------
!     Casting
!     -------
!
      INTERFACE cast
         MODULE PROCEDURE castToSMTopography
      END INTERFACE cast

      REAL(KIND=RP), PARAMETER, PRIVATE  :: gc_dx = 1.0d-4, gc_dx2 = 1.0d-8
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initTopography( self )
         IMPLICIT NONE
         CLASS(SMTopography) :: self

         CALL self % FTObject % init()

      END SUBROUTINE initTopography
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE destructBaseTopography(self)
         IMPLICIT NONE
         TYPE(SMTopography) :: self

      END SUBROUTINE destructBaseTopography
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE releaseBaseTopography(self)
         IMPLICIT NONE
         CLASS(SMTopography), POINTER :: self
         CLASS(FTObject) , POINTER :: obj

         IF(.NOT. ASSOCIATED(self)) RETURN

         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL()
         END IF
      END SUBROUTINE releaseBaseTopography
!
!////////////////////////////////////////////////////////////////////////
!
     FUNCTION heightAtTopography(self,t)  RESULT(x)
        IMPLICIT NONE
        CLASS(SMTopography) :: self
        REAL(KIND=RP)       :: t(2)
        REAL(KIND=RP)       :: x(3)
        x = 0.0_RP
     END FUNCTION heightAtTopography
!
!////////////////////////////////////////////////////////////////////////
!
     REAL(KIND=RP) FUNCTION gaussianCurvatureBaseAt(self, t)
        IMPLICIT NONE
        CLASS(SMTopography) :: self
        REAL(KIND=RP)       :: t(2)
        REAL(KIND=RP)       :: gradF(2), nHat(2), deriv2, radInv, nrmGradF2
        REAL(KIND=RP)       :: xp(3)  , xm(3)  , yp(3)  , ym(3)  , x(3)
        REAL(KIND=RP)       :: xpyp(3), xmyp(3), xpym(3), xmym(3)
        REAL(KIND=RP)       :: hessian(2,2)
!
!       -----------------------------------------------------------------------
!       Compute Grad and Hessian here directly rather than use the routines
!       in GaussianCurvature to cut down on the number of function evaluations.
!       -----------------------------------------------------------------------
!
        x  = self % heightAt(t)
        xp = self % heightAt(t + [gc_dx,0.0_RP])
        xm = self % heightAt(t - [gc_dx,0.0_RP])
        yp = self % heightAt(t + [0.0_RP,gc_dx])
        ym = self % heightAt(t - [0.0_RP,gc_dx])

        gradF(1) = 0.5_RP*(xp(3) - xm(3))/gc_dx
        gradF(2) = 0.5_RP*(yp(3) - ym(3))/gc_dx

        hessian(1,1) = (xp(3) - 2.0_RP*x(3) + xm(3))/gc_dx2
        hessian(2,2) = (yp(3) - 2.0_RP*x(3) + ym(3))/gc_dx2


        xpyp = self % heightAt(t + [gc_dx,gc_dx])
        xmyp = self % heightAt(t + [-gc_dx,gc_dx])
        xmym = self % heightAt(t - [gc_dx,gc_dx])
        xpym = self % heightAt(t + [gc_dx,-gc_dx])
        hessian(1,2) = 0.25_RP*(xpyp(3) - xmyp(3) - xpym(3) + xmym(3))/gc_dx2
        hessian(2,1) = hessian(1,2)

        gaussianCurvatureBaseAt = GaussianCurvatureWithDerivs(gradF, hessian)
        gaussianCurvatureBaseAt = ABS(gaussianCurvatureBaseAt) !Can be negative
!
!       -------------------------------------------------------------------
!       The Guassian curvature will be zero if the curvature is zero in one
!       of the two tangential directions. In that case, use the curvature
!       along the gradient direction.
!       -------------------------------------------------------------------
!
        nrmGradF2 = gradF(1)**2 + gradF(2)**2
        nHat      = gradF/(SQRT(nrMGradF2) + 1.0d-12) ! Regularize the gradient
        deriv2    = nHat(1)*(nHat(1)*hessian(1,1) + nHat(2)*hessian(1,2)) + &
                    nHat(2)*(nHat(2)*hessian(2,2) + nHat(1)*hessian(1,2))
        radInv    = ABS(deriv2/(1.0_RP + nrmGradF2)**1.5_RP)

        gaussianCurvatureBaseAt = MAX(gaussianCurvatureBaseAt, radInv)

     END FUNCTION gaussianCurvatureBaseAt
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE castToSMTopography(obj,cast)
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE
         CLASS(FTObject) , POINTER :: obj
         CLASS(SMTopography), POINTER :: cast

         cast => NULL()
         SELECT TYPE (e => obj)
            CLASS IS(SMTopography)
               cast => e
            CLASS DEFAULT

         END SELECT

      END SUBROUTINE castToSMTopography
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE printTopographyDescription(self,iUnit)
         IMPLICIT NONE
         INTEGER          :: iUnit
         CLASS(SMTopography) :: self
         WRITE(iUnit,*) "Topography "
      END SUBROUTINE printTopographyDescription

     END Module SMTopographyClass

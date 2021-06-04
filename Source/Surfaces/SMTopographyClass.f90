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
      IMPLICIT NONE 
!
!     ---------
!     Constants
!     ---------
!
      INTEGER, PARAMETER :: SM_SURFACE_NAME_LENGTH = 32
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
      END TYPE SMTopography
!
!     -------
!     Casting
!     -------
!
      INTERFACE cast
         MODULE PROCEDURE castToSMTopography
      END INTERFACE cast
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

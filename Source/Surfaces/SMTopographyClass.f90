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

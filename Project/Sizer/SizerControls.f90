!
!////////////////////////////////////////////////////////////////////////
!
!      SizerControls.f90
!      Created: August 15, 2013 1:17 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      MODULE SizerControls
      USE SMConstants
      USE FTObjectClass
      IMPLICIT NONE
!
!     ----------
!     Parameters
!     ----------
!
      INTEGER, PARAMETER, PUBLIC  :: CENTER_SHARP = 0, CENTER_SMOOTH = 1
!
!     -------------
!     Derived types
!     -------------
!
      TYPE, EXTENDS(FTObject) :: SizerCenterControl
         REAL(KIND=RP) :: width, meshSize, center(3)
         INTEGER       :: centerType
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithProperties => initSizerCenter
         PROCEDURE :: printDescription   => printCenterDescription
      END TYPE SizerCenterControl
      
      TYPE, EXTENDS(FTObject) :: SizerLineControl
         REAL(KIND=RP) :: width, meshSize, lStart(3), lEnd(3)
         INTEGER       :: lineControlType
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initwithProperties => initSizerLineControl
      END TYPE SizerLineControl
!
!     ========
      CONTAINS
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initSizerCenter( self, c, width, meshSize, centerType )  
         IMPLICIT NONE  
         CLASS(SizerCenterControl) :: self
         REAL(KIND=RP)             :: width, meshSize, c(3)
         INTEGER                   :: centerType
         
         CALL self % FTObject % init()
         
         self % width      = width
         self % meshSize   = meshSize
         self % center     = c
         self % centerType = centerType
         
      END SUBROUTINE initSizerCenter
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initSizerLineControl( self, lStart, lEnd, width, meshSize, lineControlType )
         IMPLICIT NONE 
         CLASS(SizerLineControl) :: self
         REAL(KIND=RP)          :: width, meshSize, lStart(3), lEnd(3)
         INTEGER                :: lineControlType
         
         CALL self % FTObject % init()
         
         self % width      = width
         self % meshSize   = meshSize
         self % lStart     = lStart
         self % lEnd       = lEnd
         self % lineControlType = lineControlType
         
      END SUBROUTINE initSizerLineControl
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToCenterControl(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)          , POINTER :: obj
         CLASS(SizerCenterControl), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            CLASS IS(SizerCenterControl)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castToCenterControl
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToLineControl(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)        , POINTER :: obj
         CLASS(SizerLineControl), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            CLASS IS(SizerLineControl)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castToLineControl
!      
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION hInvForCenter( self, x )
      
         IMPLICIT NONE
         CLASS(SizerCenterControl) :: self
         REAL(KIND=RP)             :: x(3)
         REAL(KIND=RP)             :: d
         
         d = SQRT((x(1) - self % center(1))**2 + (x(2) - self % center(2))**2)
         
         IF( self % centerType == CENTER_SMOOTH )      THEN
            hInvForCenter = EXP(-LOG(3.0_RP)*d/self % width)/self % meshSize
         ELSE
            IF ( d < self % width )     THEN
               hInvForCenter = 1.0_RP/self % meshSize
            ELSE
               hInvForCenter = 0.0_RP
            END IF
         END IF
         
      END FUNCTION hInvForCenter
!      
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION hInvForLineControl( self, x )
      
         IMPLICIT NONE
         TYPE(SizerLineControl) :: self
         REAL(KIND=RP)          :: x(3)
         REAL(KIND=RP)          :: d, vLine(3), vTarget(3), lLine, lTarget, vEndTarget(3)
         REAL(KIND=RP)          :: targetDotStartLine, targetDotEndLine
         
         vLine      = self % lEnd - self % lStart
         vTarget    = x - self%lStart
         vEndTarget = x - self%lEnd
         
         lLine   = SQRT( vLine(1)**2 + vLine(2)**2)
         lTarget = SQRT( vTarget(1)**2 + vTarget(2)**2 )
         
         targetDotStartLine = vLine(1)*vTarget(1) + vLine(2)*vTarget(2)
         targetDotEndLine   = vLine(1)*vEndTarget(1) + vLine(2)*vEndTarget(2)
         
         IF( targetDotStartLine >= 0.0_RP .AND. targetDotEndLine <= 0.0_RP )     THEN         
            d = SQRT(lTarget**2 - (targetDotStartLine/lLine)**2)
            IF( self % lineControlType == CENTER_SMOOTH )      THEN
               hInvForLineControl = EXP(-LOG(3.0_RP)*d/self % width)/self % meshSize
            ELSE
               IF ( d < self % width )     THEN
                  hInvForLineControl = 1.0_RP/self % meshSize
               ELSE
                  hInvForLineControl = 0.0_RP
               END IF
            END IF
         ELSE 
            hInvForLineControl = 0.0_RP
         END IF
         
      END FUNCTION hInvForLineControl
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printCenterDescription(self,iUnit)  
         IMPLICIT NONE  
         CLASS(SizerCenterControl) :: self
         INTEGER                   :: iUnit
         WRITE(iUnit,*) "SizerCenter object"
      END SUBROUTINE  
      END MODULE SizerControls

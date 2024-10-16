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
!      SizerControls.f90
!      Created: August 15, 2013 1:17 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      MODULE SizerControls
      USE SMConstants
      USE FTObjectClass
      USE FTLinkedListClass
      USE FTLinkedListIteratorClass
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
      SUBROUTINE releaseSizerLineControl(self)  
         IMPLICIT NONE
         CLASS(SizerLineControl), POINTER :: self
         CLASS(FTObject)        , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseSizerLineControl
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseSizerCenterControl(self)  
         IMPLICIT NONE
         CLASS(SizerCenterControl), POINTER :: self
         CLASS(FTObject)          , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseSizerCenterControl
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
         REAL(KIND=RP)             :: d, arg
         
         d = SQRT((x(1) - self % center(1))**2 + (x(2) - self % center(2))**2)
         
         IF( self % centerType == CENTER_SMOOTH )      THEN
            arg = LOG(3.0_RP)*d/self % width
            IF ( arg < 34.5_RP )     THEN
               hInvForCenter = EXP(-arg)/self % meshSize
            ELSE 
               hInvForCenter = 0.0_RP
            END IF 
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
         REAL(KIND=RP)          :: targetDotStartLine, targetDotEndLine, arg
         
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
               arg = LOG(3.0_RP)*d/self % width
               IF ( arg < 34.5_RP )     THEN
                  hInvForLineControl = EXP(-arg)/self % meshSize
               ELSE 
                  hInvForLineControl = 0.0_RP
               END IF 
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
      REAL(KIND=RP) FUNCTION controlsSize( controlsList, x ) 
!
!     -------------------------------------------------------
!     Go through a list of controls and find the minimum size
!     -------------------------------------------------------
!      
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList), POINTER :: controlsList
         REAL(KIND=RP)                :: x(3)
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         CLASS(FTObject)            , POINTER :: obj      => NULL()
         CLASS(SizerCenterControl)  , POINTER :: center   => NULL()
         CLASS(SizerLineControl)    , POINTER :: line     => NULL()
         REAL(KIND=RP)                        :: hFunInv
         
         controlsSize = HUGE(controlsSize)
!
!        --------------------------------
!        Sizes given by centers and lines
!        --------------------------------
!
         IF ( ASSOCIATED(controlsList) )     THEN
         
            ALLOCATE(iterator)
            CALL iterator % initwithFTLinkedList(controlsList)
            CALL iterator % setToStart()
            
            hFunInv = TINY(hFunInv)
            
            DO WHILE (.NOT.iterator % isAtEnd())
               obj => iterator % object()
               SELECT TYPE(obj)
                  TYPE IS (SizerCenterControl)
                     center  => obj
                     hFunInv = MAX(hFunInv , hInvForCenter( center, x ) )
                  TYPE IS (sizerLineControl)
                     line => obj
                     hFunInv = MAX(hFunInv , hInvForLineControl( line, x ) )
                  CLASS DEFAULT
                  
               END SELECT 
               
               CALL iterator % moveToNext()
            END DO
            obj => iterator
            CALL release(obj)
!
!           -------------------------
!           Compute the size function
!           -------------------------
!
            controlsSize = 1.0_RP/hFunInv
           
         END IF 
         
      END FUNCTION controlsSize
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printCenterDescription(self,iUnit)  
         IMPLICIT NONE  
         CLASS(SizerCenterControl) :: self
         INTEGER                   :: iUnit
         WRITE(iUnit,*) "SizerCenter object"
      END SUBROUTINE printCenterDescription
      
      END MODULE SizerControls

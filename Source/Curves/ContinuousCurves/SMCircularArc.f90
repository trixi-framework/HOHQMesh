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
!      SMCircularArc.f90
!      Created: July 30, 2013 4:04 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module SMCircularArcClass
      USE SMCurveClass
      USE SMConstants
      USE SMEllipticArcClass
      USE ProgramGlobals
      IMPLICIT NONE
!
!     ---------------------
!     Class type definition
!     ---------------------
!
      TYPE, EXTENDS(SMEllipticArc) :: SMCircularArc
!
!        ========
         CONTAINS
!        ========
!
         FINAL     :: destructCircularArc
         PROCEDURE :: printDescription        => printCircularArcDescription
         PROCEDURE :: className               => CircularArcClassName
      END TYPE SMCircularArc
      
      CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: CIRCULAR_ARC_CONTROL_KEY      = "CIRCULAR_ARC"
      CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: CIRCULAR_ARC_RADIUS_KEY       = "radius"
      CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: CIRCULAR_ARC_CENTER_KEY       = "center"
      CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: CIRCULAR_ARC_UNITS_KEY        = "units"
      CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: CIRCULAR_ARC_START_ANGLE_KEY  = "start angle"
      CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: CIRCULAR_ARC_END_ANGLE_KEY    = "end angle"
!
!     ========
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructCircularArc(self)  
         IMPLICIT NONE
         TYPE(SMCircularArc) :: self

         self % center = 0.0_RP
         self % xRadius = 0.0_RP
         self % yRadius = 0.0_RP

      END SUBROUTINE destructCircularArc
!
!//////////////////////////////////////////////////////////////////////// 
! 
     SUBROUTINE printCircularArcDescription(self,iUnit)  
        IMPLICIT NONE
        CLASS(SMCircularArc) :: self
        INTEGER       :: iUnit
        WRITE(iUnit,*) "SMCircularArc Object"
        IF(self % refCount() >= 0)     CONTINUE 
     END SUBROUTINE printCircularArcDescription
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> Class name returns a string with the name of the type of the object
!>
!>  ### Usage:
!>
!>        PRINT *,  obj % className()
!>        if( obj % className = "CircularArc")
!>
      FUNCTION CircularArcClassName(self)  RESULT(s)
         IMPLICIT NONE  
         CLASS(SMCircularArc)                       :: self
         CHARACTER(LEN=CLASS_NAME_CHARACTER_LENGTH) :: s
         
         s = "CircularArc"
         IF( self % refCount() >= 0 ) CONTINUE 
 
      END FUNCTION CircularArcClassName
 
      END Module SMCircularArcClass

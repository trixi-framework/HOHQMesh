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
!      SMEllipticArc.f90
!      Created: October 22, 2024 9:42 PM 
!      By: Garrett Byrd
!
!////////////////////////////////////////////////////////////////////////
!
        Module SMEllipticArcClass
        USE SMCurveClass
        USE SMConstants
        USE ProgramGlobals
        IMPLICIT NONE
!
!     ---------------------
!     Class type definition
!     ---------------------
!
        TYPE, EXTENDS(SMCurve) :: SMEllipticArc
            REAL(KIND=RP) :: center(3)
            REAL(KIND=RP) :: xRadius
            REAL(KIND=RP) :: yRadius
            REAL(KIND=RP) :: startAngle
            REAL(KIND=RP) :: endAngle
            REAL(KIND=RP) :: rotation
!
!           ========
            CONTAINS
!           ========
!
            PROCEDURE :: initWithParametersNameAndID => initWithParametersNameAndID_SMEllipticArc
            FINAL     :: destructEllipticArc
            PROCEDURE :: positionAt       => positionOnEllipticArcAt
            PROCEDURE :: tangentAt        => tangentOnEllipticArcAt
            PROCEDURE :: printDescription => printEllipticArcDescription
            PROCEDURE :: className        => EllipticArcClassName
        END TYPE SMEllipticArc

        CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: ELLIPTIC_ARC_CONTROL_KEY     = "ELLIPTIC_ARC"
        CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: ELLIPTIC_ARC_X_RADIUS_KEY    = "xRadius"
        CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: ELLIPTIC_ARC_Y_RADIUS_KEY    = "yRadius"
        CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: ELLIPTIC_ARC_CENTER_KEY      = "center"
        CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: ELLIPTIC_ARC_UNITS_KEY       = "units"
        CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: ELLIPTIC_ARC_START_ANGLE_KEY = "start angle"
        CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: ELLIPTIC_ARC_END_ANGLE_KEY   = "end angle"
        CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: ELLIPTIC_ARC_ROTATION_KEY    = "rotation"
!
!       ========
        CONTAINS
!       ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
        SUBROUTINE initWithParametersNameAndID_SMEllipticArc( self, center, xRadius, yRadius, &
                    startAngle, endAngle, rotation, cName, id )
            IMPLICIT NONE
            CLASS(SMEllipticArc) :: self
            CHARACTER(LEN=*)     :: cName
            INTEGER              :: id
            REAL(KIND=RP)        :: center(3), xRadius, yRadius
            REAL(KIND=RP)        :: startAngle, endAngle
            REAL(KIND=RP)        :: rotation

            CALL self % SMCurve % initWithNameAndID(cName, id)
            
            self % center = center
            self % xRadius = xRadius
            self % yRadius = yRadius
            self % startAngle = startAngle
            self % endAngle = endAngle
            self % rotation = rotation
            
        END SUBROUTINE initWithParametersNameAndID_SMEllipticArc
!
!//////////////////////////////////////////////////////////////////////// 
! 
        SUBROUTINE initDefaultEllipse( self, cName, id )
            IMPLICIT NONE
            CLASS(SMEllipticArc) :: self
            CHARACTER(len=*)     :: cName
            INTEGER              :: id

            CALL self % initWithParametersNameAndID([0.0_RP, 0.0_RP, 0.0_RP], &
                0.0_RP, 0.0_RP, &
                0.0_RP, 2.0_RP*PI, &
                0.0_RP, cName, id )

        END SUBROUTINE initDefaultEllipse
!
!//////////////////////////////////////////////////////////////////////// 
! 
        SUBROUTINE destructEllipticArc(self)
            IMPLICIT NONE
            TYPE(SMEllipticArc) :: self
            
            self % center = 0.0_RP
            self % xRadius = 0.0_RP
            self % yRadius = 0.0_RP
        
        END SUBROUTINE destructEllipticArc
!
!//////////////////////////////////////////////////////////////////////// 
! 
        SUBROUTINE releaseEllipticArc(self)
            IMPLICIT NONE
            TYPE (SMEllipticArc), POINTER :: self
            CLASS(FTObject), POINTER      :: obj

            IF(.NOT. ASSOCIATED(self)) RETURN

            obj => self
            CALL releaseFTObject(self = obj)
            IF ( .NOT. ASSOCIATED(obj))     THEN
                self => NULL()
            END IF
        END SUBROUTINE releaseEllipticArc
!
!//////////////////////////////////////////////////////////////////////// 
!
        FUNCTION positionOnEllipticArcAt(self,t) RESULT(x)
            IMPLICIT NONE
            CLASS(SMEllipticArc) :: self
            REAL(KIND=RP)        :: t
            REAL(KIND=RP)        :: x(3)
            
            REAL(KIND=RP)        :: s
            
            s = self % startAngle*(1.0_RP-t) + self % endAngle*t
            x = self % center
            x(1) = x(1) + self % xRadius*COS(s)*COS(self % rotation) - self % yRadius*SIN(s)*SIN(self % rotation)
            x(2) = x(2) + self % xRadius*COS(s)*SIN(self % rotation) + self % yRadius*SIN(s)*COS(self % rotation)

        END FUNCTION positionOnEllipticArcAt
!
!//////////////////////////////////////////////////////////////////////// 
! 
        FUNCTION tangentOnEllipticArcAt(self,t)     RESULT(x)
            IMPLICIT NONE
            CLASS(SMEllipticArc) :: self
            REAL(KIND=RP) :: t
            REAL(KIND=RP) :: x(3)
            REAL(KIND=RP) :: s, dsdt

            s    = self % startAngle*(1.0_RP-t) + self % endAngle*t
            dsdt = self % endAngle - self % startAngle

            x(1) = (-self % xRadius*SIN(s)*COS(self % rotation) - self % yRadius*COS(s)*SIN(self % rotation))*dsdt
            x(2) = (-self % xRadius*SIN(s)*SIN(self % rotation) + self % yRadius*COS(s)*COS(self % rotation))*dsdt
            x(3) = 0.0_RP
            x    = x/SQRT(x(1)**2 + x(2)**2 + x(3)**2)
        
        END FUNCTION tangentOnEllipticArcAt
!
!//////////////////////////////////////////////////////////////////////// 
! 
        SUBROUTINE printEllipticArcDescription(self,iUnit)
            IMPLICIT NONE
            CLASS(SMEllipticArc) :: self
            INTEGER              :: iUnit
            WRITE(iUnit,*) "SMEllipticArc Object"
            IF(self % refCount() >= 0) CONTINUE
        END SUBROUTINE printEllipticArcDescription
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> Class name returns a string with the name of the type of the object
!>
!>  ### Usage:
!>
!>        PRINT *,  obj % className()
!>        if( obj % className = "EllipticArc")
!>
        FUNCTION EllipticArcClassName(self) RESULT(s)
            IMPLICIT NONE
            CLASS(SMEllipticArc)                       :: self
            CHARACTER(LEN=CLASS_NAME_CHARACTER_LENGTH) :: s

            s = "EllipticArc"
            IF( self % refCount() >= 0 ) CONTINUE
        END FUNCTION EllipticArcClassName

    END MODULE SMEllipticArcClass

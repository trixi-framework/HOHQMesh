!
!////////////////////////////////////////////////////////////////////////
!
!      SMLine.f90
!      Created: July 30, 2013 4:04 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module SMLineClass
      USE SMCurveClass
      USE SMConstants
      IMPLICIT NONE
!
!     ---------------------
!     Class type definition
!     ---------------------
!
      TYPE, EXTENDS(SMCurve) :: SMLine
         REAL(KIND=RP) :: xStart(3), xEnd(3) 
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithStartEndNameAndID
         FINAL     :: destructLine
         PROCEDURE :: positionAt       => positionOnLineAt
         PROCEDURE :: tangentAt        => tangentOnLineAt
         PROCEDURE :: printDescription => printLineDescription
         PROCEDURE :: className        => LineClassName
      END TYPE SMLine
!
!     ========
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initWithStartEndNameAndID( self, xStart, xEnd, cName, id )  
         IMPLICIT NONE
         CLASS(SMLine)    :: self
         CHARACTER(LEN=*) :: cName
         INTEGER          :: id
         REAL(KIND=RP)    :: xStart(3), xEnd(3)
         
         CALL self % SMCurve % initWithNameAndID(cName,id)
         
         self % xStart = xStart
         self % xEnd   = xEnd
                  
      END SUBROUTINE initWithStartEndNameAndID
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructLine(self)  
         IMPLICIT NONE
         TYPE(SMLine) :: self
         
         self % xStart = 0.0_RP
         self % xEnd   = 0.0_RP
         
      END SUBROUTINE destructLine
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseLine(self)  
         IMPLICIT NONE
         TYPE (SMLine)  , POINTER :: self
         CLASS(FTObject), POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseLine
!
!//////////////////////////////////////////////////////////////////////// 
! 
     FUNCTION positionOnLineAt(self,t)  RESULT(x)
        IMPLICIT NONE  
        CLASS(SMLine) :: self
        REAL(KIND=RP) :: t
        REAL(KIND=RP) :: x(3)
        
        x = self%xStart + t*(self%xEnd - self%xStart)
        
     END FUNCTION positionOnLineAt
!
!//////////////////////////////////////////////////////////////////////// 
! 
     FUNCTION tangentOnLineAt(self,t)  RESULT(x)
        IMPLICIT NONE  
        CLASS(SMLine) :: self
        REAL(KIND=RP) :: t
        REAL(KIND=RP) :: x(3)
        
        x = self%xEnd - self%xStart
        x = x/SQRT(x(1)**2 + x(2)**2 + x(3)**2)
        t = t
        
     END FUNCTION tangentOnLineAt
!
!//////////////////////////////////////////////////////////////////////// 
! 
     SUBROUTINE printLineDescription(self,iUnit)  
        IMPLICIT NONE
        CLASS(SMLine) :: self
        INTEGER       :: iUnit
        WRITE(iUnit,*) "SMLine Object"
        IF(self % refCount() >= 0)     CONTINUE 
     END SUBROUTINE printLineDescription
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> Class name returns a string with the name of the type of the object
!>
!>  ### Usage:
!>
!>        PRINT *,  obj % className()
!>        if( obj % className = "Line")
!>
      FUNCTION LineClassName(self)  RESULT(s)
         IMPLICIT NONE  
         CLASS(SMLine)                              :: self
         CHARACTER(LEN=CLASS_NAME_CHARACTER_LENGTH) :: s
         
         s = "Line"
         IF( self % refCount() >= 0 ) CONTINUE 
 
      END FUNCTION LineClassName
 
      END Module SMLineClass

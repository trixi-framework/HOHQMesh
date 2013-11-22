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
         PROCEDURE :: destruct         => destructLine
         PROCEDURE :: positionAt       => positionOnLineAt
         PROCEDURE :: tangentAt        => tangentOnLineAt
         PROCEDURE :: printDescription => printLineDescription
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
         CLASS(SMLine) :: self
         
         self % xStart = 0.0_RP
         self % xEnd   = 0.0_RP
         
         CALL self % SMCurve % destruct()
         
      END SUBROUTINE destructLine

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
        
     END FUNCTION tangentOnLineAt
!
!//////////////////////////////////////////////////////////////////////// 
! 
     SUBROUTINE printLineDescription(self,iUnit)  
        IMPLICIT NONE
        CLASS(SMLine) :: self
        INTEGER       :: iUnit
        WRITE(iUnit,*) "SMLine Object"
     END SUBROUTINE printLineDescription
 
      END Module SMLineClass

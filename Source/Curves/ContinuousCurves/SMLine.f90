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

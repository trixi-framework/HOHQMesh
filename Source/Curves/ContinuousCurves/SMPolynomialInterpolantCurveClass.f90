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
!      SMPolynomialInterpolant.f90
!      Created: April 13, 2026 at 3:10 PM 
!      By: David Kopriva  
!
!      This class is  a wrapper around the older
!      CurveInterpolantClass to make it a subclass of SMCurve. It
!      defines a curve by a Lagrange interpolating polynomial
!      through a set of nodes and values.
!
!////////////////////////////////////////////////////////////////////////
!
      Module SMPolynomialInterpolantClass
      USE SMCurveClass
      USE SMConstants
      USE CurveInterpolantClass
      
      IMPLICIT NONE
!
!     ---------------------
!     Class type definition
!     ---------------------
!
      TYPE, EXTENDS(SMCurve) :: SMPolynomialInterpolant
         TYPE(CurveInterpolant) :: selfInterpolant 
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithNodesValuesAndID
         FINAL     :: destructPoly
         PROCEDURE :: positionAt       => positionOnPolyAt
         PROCEDURE :: tangentAt        => tangentOnPolyAt
         PROCEDURE :: derivativeAt     => derivativeOnPolyAt
         PROCEDURE :: printDescription => printPolyDescription
         PROCEDURE :: className        => PolyClassName
      END TYPE SMPolynomialInterpolant
!
!     ========
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initWithNodesValuesAndID( self, N, nodes, values, cName, id )  
         IMPLICIT NONE
         CLASS(SMPolynomialInterpolant)    :: self
         CHARACTER(LEN=*)                             :: cName
         INTEGER                                      :: id
         INTEGER                        , INTENT(IN)  :: N
         REAL(KIND=RP), DIMENSION(0:N)  , INTENT(IN)  :: nodes
         REAL(KIND=RP), DIMENSION(0:N,3), INTENT(IN)  :: values
         
         CALL self % SMCurve % initWithNameAndID(cName,id)
         CALL ConstructCurveInterpolant( self % selfInterpolant, N, nodes, values)
                  
      END SUBROUTINE initWithNodesValuesAndID
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructPoly(self)  
         IMPLICIT NONE
         TYPE(SMPolynomialInterpolant) :: self
         
         CALL DestructCurveInterpolant(self % selfInterpolant)
         
      END SUBROUTINE destructPoly
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releasePoly(self)  
         IMPLICIT NONE
         TYPE (SMPolynomialInterpolant)  , POINTER :: self
         CLASS(FTObject), POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releasePoly
!
!//////////////////////////////////////////////////////////////////////// 
! 
     FUNCTION positionOnPolyAt(self,t)  RESULT(x)
        IMPLICIT NONE  
        CLASS(SMPolynomialInterpolant) :: self
        REAL(KIND=RP) :: t
        REAL(KIND=RP) :: x(3)
        
        CALL EvaluateAt(self % selfInterpolant,t,x)
        
     END FUNCTION positionOnPolyAt
!
!//////////////////////////////////////////////////////////////////////// 
! 
     FUNCTION tangentOnPolyAt(self,t)  RESULT(x)
        IMPLICIT NONE  
        CLASS(SMPolynomialInterpolant) :: self
        REAL(KIND=RP) :: t
        REAL(KIND=RP) :: x(3)
        
        CALL Derivative_At(self % selfInterpolant,t,x)
        x = x/SQRT(x(1)**2 + x(2)**2 + x(3)**2)
        
     END FUNCTION tangentOnPolyAt
!
!//////////////////////////////////////////////////////////////////////// 
! 
     FUNCTION derivativeOnPolyAt(self,t)  RESULT(x)
        IMPLICIT NONE  
        CLASS(SMPolynomialInterpolant) :: self
        REAL(KIND=RP) :: t
        REAL(KIND=RP) :: x(3)
        
        CALL Derivative_At(self % selfInterpolant,t,x)
        
     END FUNCTION derivativeOnPolyAt
!
!//////////////////////////////////////////////////////////////////////// 
! 
     SUBROUTINE printPolyDescription(self,iUnit)  
        IMPLICIT NONE
        CLASS(SMPolynomialInterpolant) :: self
        INTEGER       :: iUnit
        WRITE(iUnit,'(A)') "SMPolynomialInterpolant Object"
        IF(self % refCount() >= 0)     CONTINUE 
     END SUBROUTINE printPolyDescription
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> Class name returns a string with the name of the type of the object
!>
!>  ### Usage:
!>
!>        PRINT *,  obj % className()
!>        if( obj % className = "Poly")
!>
      FUNCTION PolyClassName(self)  RESULT(s)
         IMPLICIT NONE  
         CLASS(SMPolynomialInterpolant)                              :: self
         CHARACTER(LEN=CLASS_NAME_CHARACTER_LENGTH) :: s
         
         s = "Polynomial Interpolant"
         IF( self % refCount() >= 0 ) CONTINUE 
 
      END FUNCTION PolyClassName
      
      END Module SMPolynomialInterpolantClass

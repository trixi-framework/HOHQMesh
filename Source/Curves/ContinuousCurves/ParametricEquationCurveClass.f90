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
!      ParametricEquationCurveClass.f90
!      Created: July 30, 2013 5:02 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module SMParametricEquationCurveClass
      
      USE SMCurveClass
      USE EquationEvaluatorClass
      USE SMConstants
      USE SharedExceptionManagerModule
      
      IMPLICIT NONE 
!
!     ---------
!     Constants
!     ---------
!
      CHARACTER(LEN=25) :: EQUATION_FORMAT_EXCEPTION = "Equation formatting error"
!
!     ----------------------
!     Class type definition 
!     ----------------------
!
      TYPE, EXTENDS(SMCurve) :: SMParametricEquationCurve
         TYPE(EquationEvaluator) :: xEqn, yEqn, zEqn
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithEquationsNameAndID
         FINAL     :: destructPECurve
         PROCEDURE :: positionAt => positionOnPECurveAt
         PROCEDURE :: printDescription => printPEDescription
         PROCEDURE :: className        => PEClassName
      END TYPE SMParametricEquationCurve
!
!     ========
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
       SUBROUTINE initWithEquationsNameAndID(self, xEqn, yEqn, zEqn, curveName, id)
         IMPLICIT NONE
         CLASS(SMParametricEquationCurve) :: self
         CHARACTER(LEN=*)                 :: xEqn, yEqn, zEqn, curveName
         INTEGER                          :: id
         
         CHARACTER(LEN=ERROR_MESSAGE_LENGTH) :: msg
         
         CALL self % SMCurve % initWithNameAndID(curveName,id)
         
         CALL ConstructEquationEvaluator( self % xEqn, xEqn  )
         IF( .NOT.StatusOK(self%xEqn) )     THEN
            msg = ErrorMessageString( self%xEqn )
            CALL ThrowEquationFormatException(curveName, xEqn,"X-Equation",msg)
         END IF
         
         CALL ConstructEquationEvaluator( self % yEqn, yEqn )
         IF( .NOT.StatusOK(self%yEqn) )     THEN
            msg = ErrorMessageString( self%yEqn )
            CALL ThrowEquationFormatException(curveName, yEqn,"Y-Equation",msg)
         END IF
         
         CALL ConstructEquationEvaluator( self % zEqn, zEqn )
         IF( .NOT.StatusOK(self%zEqn) )     THEN
            msg = ErrorMessageString( self%zEqn )
            CALL ThrowEquationFormatException(curveName, zEqn, "Z-Equation",msg)
         END IF
         
       END SUBROUTINE initWithEquationsNameAndID
!
!//////////////////////////////////////////////////////////////////////// 
! 
       SUBROUTINE destructPECurve(self)  
         IMPLICIT NONE
         TYPE(SMParametricEquationCurve) :: self
         
         CALL DestructEquationEvaluator( self%xEqn )
         CALL DestructEquationEvaluator( self%yEqn )
         CALL DestructEquationEvaluator( self%zEqn )
         
       END SUBROUTINE destructPECurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releasePECurve(self)  
         IMPLICIT NONE
         TYPE (SMParametricEquationCurve), POINTER :: self
         CLASS(FTObject)                 , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releasePECurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION positionOnPECurveAt(self,t)  RESULT(x)
        IMPLICIT NONE  
        CLASS(SMParametricEquationCurve) :: self
        REAL(KIND=RP)                    :: t
        REAL(KIND=RP)                    :: x(3)
        
         x(1) = EvaluateEquation_At_( self%xEqn, t )
         x(2) = EvaluateEquation_At_( self%yEqn, t )
         x(3) = EvaluateEquation_At_( self%zEqn, t )
        
      END FUNCTION positionOnPECurveAt
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printPEDescription(self,iUnit)  
        IMPLICIT NONE  
        CLASS(SMParametricEquationCurve) :: self
        INTEGER                          :: iUnit
        
        WRITE(iUnit,*) self % curveName()
        WRITE(iUnit,*) TRIM(self % xEqn % equation)
        WRITE(iUnit,*) TRIM(self % yEqn % equation)
        WRITE(iUnit,*) TRIM(self % zEqn % equation)
      END SUBROUTINE printPEDescription
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ThrowEquationFormatException(curveName,eqn,objectName,msg)  
         USE FTValueClass
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=*)  :: msg
         CHARACTER(LEN=*)  :: objectName
         CHARACTER(LEN=*)  :: curveName
         CHARACTER(LEN=*)  :: eqn
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (FTException)   , POINTER :: exception => NULL()
         CLASS(FTDictionary)  , POINTER :: userDictionary => NULL()
         CLASS(FTObject)      , POINTER :: obj => NULL()
         CLASS(FTValue)       , POINTER :: v => NULL()
!
!        -----------------------------------------------------
!        The userDictionary for this exception contains the
!        message to be delivered under the name "message"
!        and the object being created by the name "objectName"
!        The curve name will have the key "curveName"
!        -----------------------------------------------------
!
         ALLOCATE(userDictionary)
         CALL userDictionary % initWithSize(4)
          
         ALLOCATE(v)
         CALL v % initWithValue(curveName)
         obj => v
         CALL userDictionary % addObjectForKey(obj,"curveName")
         CALL release(obj)
           
         ALLOCATE(v)
         CALL v % initWithValue(eqn)
         obj => v
         CALL userDictionary % addObjectForKey(obj,"Equation String")
         CALL release(obj)
       
         ALLOCATE(v)
         CALL v % initWithValue(objectName)
         obj => v
         CALL userDictionary % addObjectForKey(obj,"objectName")
         CALL release(obj)
         
         ALLOCATE(v)
         CALL v % initWithValue(msg)
         obj => v
         CALL userDictionary % addObjectForKey(obj,"message")
         CALL release(obj)
!
!        --------------------
!        Create the exception
!        --------------------
!
         ALLOCATE(exception)
         
         CALL exception % initFTException(FT_ERROR_FATAL, &
                              exceptionName   = EQUATION_FORMAT_EXCEPTION, &
                              infoDictionary  = userDictionary)
         obj => userDictionary
         CALL release(obj)
!
!        -------------------
!        Throw the exception
!        -------------------
!
         CALL throw(exception)
         obj => exception
         CALL release(obj)
         
      END SUBROUTINE ThrowEquationFormatException
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> Class name returns a string with the name of the type of the object
!>
!>  ### Usage:
!>
!>        PRINT *,  obj % className()
!>        if( obj % className = "ParametricEquation")
!>
      FUNCTION PEClassName(self)  RESULT(s)
         IMPLICIT NONE  
         CLASS(SMParametricEquationCurve)           :: self
         CHARACTER(LEN=CLASS_NAME_CHARACTER_LENGTH) :: s
         
         s = "ParametricEquation"
         IF( self % refCount() >= 0 ) CONTINUE 
 
      END FUNCTION PEClassName
      
      END Module SMParametricEquationCurveClass

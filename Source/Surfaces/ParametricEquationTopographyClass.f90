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
!      EquationTopographyClass.f90
!      Created: July 30, 2013 5:02 PM 
!      By: David Kopriva  
!
!      Computes z(x,y) using an equation as a string.
!
!////////////////////////////////////////////////////////////////////////
!
      Module SMEquationTopographyClass
      
      USE SMTopographyClass
      USE EquationEvaluatorClass
      USE SMConstants
      USE SharedExceptionManagerModule
      USE ProgramGlobals, ONLY: STRING_CONSTANT_LENGTH
      
      IMPLICIT NONE 
!
!     ---------
!     Constants
!     ---------
!
      CHARACTER(LEN=25) :: EQUATION_FORMAT_EXCEPTION = "Equation formatting error"
      CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: TOPOGRAPHY_EQUATION_KEY = "eqn"
!
!     ----------------------
!     Class type definition 
!     ----------------------
!
      TYPE, EXTENDS(SMTopography) :: SMEquationTopography
         TYPE(EquationEvaluator)  :: zEqn
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithEquation
         FINAL     :: destructPETopography
         PROCEDURE :: heightAt => positionOnPETopographyAt
         PROCEDURE :: printDescription => printPEDescription
      END TYPE SMEquationTopography
      
      PRIVATE:: addStringToDictionary
!
!     ========
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
       SUBROUTINE initWithEquation(self, zEqn )
         IMPLICIT NONE
         CLASS(SMEquationTopography) :: self
         CHARACTER(LEN=*)                      :: zEqn
         
         CHARACTER(LEN=ERROR_MESSAGE_LENGTH)   :: msg
         
         CALL self % SMTopography % initTopography()
         
         CALL ConstructEquationEvaluator( self % zEqn, zEqn )
         IF( .NOT.StatusOK(self % zEqn) )     THEN
            msg = ErrorMessageString( self % zEqn )
            CALL ThrowEquationFormatException("Topography", zEqn, "Topography Equation",msg)
         END IF
         
       END SUBROUTINE initWithEquation
!
!//////////////////////////////////////////////////////////////////////// 
! 
       SUBROUTINE destructPETopography(self)  
         IMPLICIT NONE
         TYPE(SMEquationTopography) :: self
         
         CALL DestructEquationEvaluator( self % zEqn )
         
       END SUBROUTINE destructPETopography
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releasePETopography(self)  
         IMPLICIT NONE
         TYPE (SMEquationTopography), POINTER :: self
         CLASS(FTObject)                 , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releasePETopography
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION positionOnPETopographyAt(self,t)  RESULT(x)
        IMPLICIT NONE  
        CLASS(SMEquationTopography) :: self
        REAL(KIND=RP)                      :: t(2)
        REAL(KIND=RP)                      :: x(3)
        
         x(3) = EvaluateEquation_At_( self % zEqn, t )
         x(1) = t(1)
         x(2) = t(2)
        
      END FUNCTION positionOnPETopographyAt
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printPEDescription(self,iUnit)  
        IMPLICIT NONE  
        CLASS(SMEquationTopography) :: self
        INTEGER                          :: iUnit
        
        WRITE(iUnit,*) TRIM(self % zEqn % equation)
      END SUBROUTINE printPEDescription
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ThrowEquationFormatException(surfaceName, eqn, objectName, msg)  
         USE FTValueClass
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=*)  :: msg
         CHARACTER(LEN=*)  :: objectName
         CHARACTER(LEN=*)  :: surfaceName
         CHARACTER(LEN=*)  :: eqn
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (FTException)   , POINTER :: exception => NULL()
         CLASS(FTDictionary)  , POINTER :: userDictionary => NULL()
         CLASS(FTObject)      , POINTER :: obj => NULL()
!
!        -----------------------------------------------------
!        The userDictionary for this exception contains the
!        message to be delivered under the name "message"
!        and the object being created by the name "objectName"
!        The surface name will have the key "surfaceName"
!        -----------------------------------------------------
!
         ALLOCATE(userDictionary)
         CALL userDictionary % initWithSize(4)
         
         CALL addStringToDictionary(key  = "surfaceName",    &
                                    str  = surfaceName,      &
                                    dict = userDictionary)
         CALL addStringToDictionary(key  = "EquationString", &
                                    str  = eqn, &
                                    dict = userDictionary)
         CALL addStringToDictionary(key  = "objectName",     &
                                    str  = objectName,       &
                                    dict = userDictionary)
         CALL addStringToDictionary(key  = "message",        &
                                    str  = msg,              &
                                    dict = userDictionary)
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
      SUBROUTINE addStringToDictionary(key, str, dict)  
         IMPLICIT NONE  
         CHARACTER(LEN=*)    :: str, key
         CLASS(FTDictionary) :: dict
         
         CLASS(FTObject)      , POINTER :: obj => NULL()
         CLASS(FTValue)       , POINTER :: v => NULL()
         
         ALLOCATE(v)
         CALL v % initWithValue(str)
         obj => v
         CALL dict % addObjectForKey(obj,key)
         CALL release(obj)
         
      END SUBROUTINE addStringToDictionary
      
      END Module SMEquationTopographyClass

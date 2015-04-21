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
         PROCEDURE :: destruct   => destructPECurve
         PROCEDURE :: positionAt => positionOnPECurveAt
         PROCEDURE :: printDescription => printPEDescription
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
         CLASS(SMParametricEquationCurve) :: self
         
         CALL DestructEquationEvaluator( self%xEqn )
         CALL DestructEquationEvaluator( self%yEqn )
         CALL DestructEquationEvaluator( self%zEqn )
         
         CALL self % SMCurve % destruct()
         
       END SUBROUTINE destructPECurve

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
         CLASS(FTException)   , POINTER :: exception => NULL()
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
         CALL v % release()
           
         ALLOCATE(v)
         CALL v % initWithValue(eqn)
         obj => v
         CALL userDictionary % addObjectForKey(obj,"Equation String")
         CALL v % release()
       
         ALLOCATE(v)
         CALL v % initWithValue(objectName)
         obj => v
         CALL userDictionary % addObjectForKey(obj,"objectName")
         CALL v % release()
         
         ALLOCATE(v)
         CALL v % initWithValue(msg)
         obj => v
         CALL userDictionary % addObjectForKey(obj,"message")
         CALL v % release()
!
!        --------------------
!        Create the exception
!        --------------------
!
         ALLOCATE(exception)
         
         CALL exception % initFTException(FT_ERROR_FATAL, &
                              exceptionName   = EQUATION_FORMAT_EXCEPTION, &
                              infoDictionary  = userDictionary)
         CALL userDictionary % release()
!
!        -------------------
!        Throw the exception
!        -------------------
!
         CALL throw(exception)
         CALL exception % release()
         
      END SUBROUTINE ThrowEquationFormatException
      
      END Module SMParametricEquationCurveClass

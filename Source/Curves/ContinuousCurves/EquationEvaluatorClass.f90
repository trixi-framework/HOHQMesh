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
!      EquationEvaluatorClass.f95
!      Created: 2010-08-22 11:44:57 -0400
!      Modified: 9/17/20, 3:24 PM : Enable multiple arguments
!      By: David Kopriva
!
!        *Creation*
!
!           CALL ConstructEquationEvaluator( equationInstance, string )
!
!        *Destruction*
!
!           CALL DestructEquationEvaluator( equationInstance )
!
!
!        *Error Handling*
!
!           IF( .NOT.StatusOK(equationInstance) )     THEN
!              PRINT *, ErrorMessageString( equationInstance )
!              STOP
!           END IF
!
!        *Evaluating*
!
!           x = EvaluateEquation_At_( equationInstance, t )
!
!        *Evaluating multidimensional*
!
!           x = EvaluateEquation_At_( equationInstance, [t,t1,t2] ) [vector of max size EEC_MAX_NUMBER_OF_VARIABLES]
!
!////////////////////////////////////////////////////////////////////////
!
      Module EquationEvaluatorClass
      IMPLICIT NONE
!
!     ---------
!     Precision
!     ---------
!
      INTEGER      , PARAMETER, PRIVATE :: noOfDigits = 15                                ! # of desired digits
      INTEGER      , PARAMETER          :: EP          = SELECTED_REAL_KIND( noOfDigits ) ! Real Kind
      REAL(KIND=EP), PARAMETER, PRIVATE :: PI          = 3.141592653589793238462643_EP
!
!     -------
!     Strings
!     -------
!
      INTEGER, PARAMETER :: EQUATION_STRING_LENGTH = 256
      INTEGER, PARAMETER :: VARIABLE_NAME_LENGTH   = 6
      INTEGER, PARAMETER :: TOKEN_LENGTH           = 32
!
!     -----------
!     Token types
!     -----------
!
      INTEGER, PARAMETER, PRIVATE :: TYPE_NONE      = 0, TYPE_NUMBER        = 1
      INTEGER, PARAMETER, PRIVATE :: TYPE_OPERATOR  = 2, TYPE_FUNCTION      = 3
      INTEGER, PARAMETER, PRIVATE :: TYPE_PAREN     = 4, TYPE_MONO_OPERATOR = 5, TYPE_VARIABLE = 6
      !Additional variables are TYPE_VARIABLE + (i-1)
!
!     -----------------------------
!     Separators, DIGITS, functions
!     -----------------------------
!
      INTEGER         , PARAMETER                , PRIVATE :: separatorCount = 7
      CHARACTER(LEN=1), DIMENSION(separatorCount), PRIVATE :: separators = (/"+","-","*","/","(",")","^"/)
      CHARACTER(LEN=1), DIMENSION(10)            , PRIVATE :: DIGITS = (/"0","1","2","3","4","5","6","7","8","9"/)
      CHARACTER (LEN=5),DIMENSION(13)            , PRIVATE :: functions = &
                                                              (/"cos  ","sin  ","exp  ","tan  ","sqrt ",&
                                                                "abs  ","ln   ","log  ",                &
                                                                "acos ","asin ","atan ", "atanh", "tanh " /)
      CHARACTER (LEN=1),DIMENSION(5)             , PRIVATE :: operators = (/"*","/","^","+","-"/)
!
!     -----
!     Other
!     -----
!
      INTEGER, PARAMETER :: EEC_NONE                    = -1
      INTEGER, PARAMETER :: EEC_MAX_NUMBER_OF_VARIABLES =  4
      INTEGER, PARAMETER :: EEC_TOO_MANY_ARGUMENTS      =  HUGE(1)
!
!     -------------
!     Derived types
!     -------------
!
      TYPE Token
         CHARACTER(LEN=TOKEN_LENGTH) :: token
         INTEGER                     :: tokenType
      END TYPE Token

      TYPE TokenStack
         TYPE(Token), DIMENSION(:), POINTER :: tokens  => NULL()
         INTEGER                            :: top
      END TYPE TokenStack

      PRIVATE :: TokenStack
      PRIVATE :: ConstructTokenStack, DestructTokenStack, TokenStackPush, TokenStackPop, TokenStackPeek

      TYPE NumberStack
         REAL(KIND=EP), DIMENSION(:), POINTER :: values  => NULL()
         INTEGER                              :: top
      END TYPE NumberStack

      PRIVATE :: NumberStack
      PRIVATE :: ConstructNumberStack, DestructNumberStack, NumberStackPush, NumberStackPop !, NumberStackPeek
!
!     ----------
!     Class type
!     ----------
!
      TYPE EquationEvaluator
         CHARACTER(LEN=EQUATION_STRING_LENGTH)                       :: equation
         CHARACTER(LEN=VARIABLE_NAME_LENGTH)                         :: variableNames(EEC_MAX_NUMBER_OF_VARIABLES)
         INTEGER                                                     :: numberOfVariables
         TYPE(Token)                          ,DIMENSION(:), POINTER :: postfix => NULL()
      END TYPE EquationEvaluator
!
!     --------------
!     Error handling
!     --------------
!
      INTEGER, PARAMETER                 , PUBLIC  :: ERROR_MESSAGE_LENGTH = 256
      LOGICAL                            , PRIVATE :: success
      CHARACTER(LEN=ERROR_MESSAGE_LENGTH), PRIVATE :: EQNErrorMessage

      INTERFACE ErrorMessageString
         MODULE PROCEDURE EQNErrorMessageString
      END INTERFACE ErrorMessageString
      PRIVATE :: EQNErrorMessageString

      INTERFACE StatusOK
         MODULE PROCEDURE StatusOfEQN
      END INTERFACE StatusOK
      PRIVATE :: StatusOfEQN

      INTERFACE EvaluateEquation_At_
         MODULE PROCEDURE EvaluateEquationAtS
         MODULE PROCEDURE EvaluateEquation_At_V
      END INTERFACE EvaluateEquation_At_
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
       SUBROUTINE ConstructEquationEvaluator( self, eqn )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(EquationEvaluator) :: self
         CHARACTER(LEN=*)        :: eqn
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=EQUATION_STRING_LENGTH) :: formulaInInfix
         CHARACTER(LEN=VARIABLE_NAME_LENGTH)   :: variableNames(EEC_MAX_NUMBER_OF_VARIABLES)
         INTEGER                               :: tCount, tPFCount, indx
         INTEGER                               :: t, nVars

         CHARACTER(LEN=TOKEN_LENGTH), DIMENSION(EQUATION_STRING_LENGTH/2) :: components
         INTEGER                    , DIMENSION(EQUATION_STRING_LENGTH/2) :: classification
         TYPE(Token)                , DIMENSION(EQUATION_STRING_LENGTH/2) :: postFixArray
         TYPE(Token)                , DIMENSION(:)          , ALLOCATABLE :: tokens


         EQNErrorMessage  = " "
         self % equation = eqn

         IF( FindExpression( eqn, formulaInInfix, variableNames, nVars ) )     THEN

            IF ( nVars == EEC_TOO_MANY_ARGUMENTS )     THEN
               indx = INDEX(STRING = eqn, SUBSTRING = '=')
               IF ( indx > 0 )     THEN
                  EQNErrorMessage = "Syntax Error: Too many independent variables: "// eqn(1:indx-1)
               ELSE
                  EQNErrorMessage = "Syntax Error: Too many independent variables: "
               END IF
               success = .FALSE.
               RETURN
            END IF

            self % variableNames     = variableNames
            self % numberOfVariables = nVars

            CALL GetComponents     ( formulaInInfix, components, tCount )
            CALL ClassifyComponents( components, classification, variableNames, tCount )

            ALLOCATE(tokens(tCount))
            DO t = 1, tCount
               tokens(t) % token     = components(t)
               tokens(t) % tokenType = classification(t)
            END DO

            postFixArray % tokenType = TYPE_NONE
            CALL ConvertToPostfix( tokens, postFixArray, tPFCount )
            ALLOCATE(self % postfix(tPFCount))
            self % postfix = postFixArray(1:tPFCount)

            IF( FinalSyntaxCheckOK( self ) )     THEN
               success = .true.
            ELSE
               success = .false.
            END IF
         ELSE
            success = .false.
         END IF

      END SUBROUTINE ConstructEquationEvaluator
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DestructEquationEvaluator( self )
         IMPLICIT NONE
         TYPE(EquationEvaluator) :: self
         IF( ASSOCIATED(self % postfix) ) DEALLOCATE(self % postfix)
      END SUBROUTINE
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION EQNErrorMessageString( self ) RESULT(s)
         IMPLICIT NONE
         TYPE( EquationEvaluator )           :: self
         CHARACTER(LEN=ERROR_MESSAGE_LENGTH) :: s
         s = EQNErrorMessage
      END FUNCTION EQNErrorMessageString
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION StatusOfEQN( self ) RESULT(s)
         IMPLICIT NONE
         TYPE( EquationEvaluator )  :: self
         LOGICAL                    :: s
         s = success
      END FUNCTION StatusOfEQN
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION EvaluateEquationAtS( self, x ) RESULT(y)
      IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(EquationEvaluator) :: self
         REAL(KIND=EP)           :: x, y
         REAL(KIND=EP)           :: xArray(EEC_MAX_NUMBER_OF_VARIABLES)
         xArray(1) = x
         y = EvaluateEquation_At_V( self, xArray )

      END FUNCTION EvaluateEquationAtS
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION EvaluateEquation_At_V( self, x ) RESULT(y)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(EquationEvaluator) :: self
         REAL(KIND=EP)           :: x(:), y
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER           :: k, N
         TYPE(Token)       :: t
         TYPE(NumberStack) :: stack
         REAL(KIND=EP)     :: v, a, b, c

         N = SIZE(self % postfix)
         CALL ConstructNumberStack( stack, N )

         DO k = 1, N
            t = self % postfix(k)
            SELECT CASE ( t % tokenType )

               CASE( TYPE_NUMBER )
                  IF( t % token == "pi" .OR. t % token == "PI" )     THEN
                     v = PI
                  ELSE
                     READ( t % token, * ) v
                  END IF
                  CALL NumberStackPush( stack, v )
!
               CASE ( TYPE_OPERATOR )
                  CALL NumberStackPop( stack, a )
                  CALL NumberStackPop( stack, b )
                  SELECT CASE ( t % token )
                     CASE ( "+" )
                        c = a + b
                     CASE ( "-" )
                        c = b - a
                     CASE ( "*" )
                        c = a*b
                     CASE ( "/" )
                        c = b/a
                     CASE ( "^" )
                        IF( MOD(a,2.0_EP) == 0.0 )     THEN
                           c = ABS(b)**a
                        ELSE
                           c = b**a
                        END IF
                     CASE DEFAULT
                  END SELECT
                  CALL NumberStackPush( stack, c )

               CASE ( TYPE_FUNCTION )
                  CALL NumberStackPop( stack, a )
                  CALL FunOfx( t % token, a, b )
                  CALL NumberStackPush( stack, b )

               CASE (TYPE_MONO_OPERATOR )
                 IF( t % token == "-" )     THEN
                    CALL NumberStackPop( stack, a )
                    a = -a
                    CALL NumberStackPush( stack, a )
                 END IF

               CASE DEFAULT ! is a variable
                  CALL NumberStackPush( stack, x(t % tokenType - TYPE_VARIABLE + 1) )
            END SELECT
         END DO
!
!        ----
!        Done
!        ----
!
         CALL NumberStackPop( stack, a )
         y = a
!
!        --------
!        Clean up
!        --------
!
         CALL DestructNumberStack( stack )

      END FUNCTION EvaluateEquation_At_V
!
!///////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION FindExpression( equation, formula, variableNames, nVars )
      IMPLICIT NONE
!
!      ------------------------------------------------------------
!      Find the variable and the start and stop of the expression.
!      Returns .true. for successful completion, .false. otherwise.
!      The EQNErrorMessage stores the error.
!      ------------------------------------------------------------
!
!
      CHARACTER(LEN=*)                     , INTENT(IN)  :: equation
      CHARACTER(LEN=VARIABLE_NAME_LENGTH)  , INTENT(OUT) :: variableNames(EEC_MAX_NUMBER_OF_VARIABLES)
      CHARACTER(LEN=EQUATION_STRING_LENGTH), INTENT(OUT) :: formula
      INTEGER                                            :: nVars


      INTEGER           ::  eqPos, nf, nl, nr, n
!
      FindExpression = .false.
      variableNames   = "%none"
!
!      ----------------------------------------------------------------
!      Find the location of the equal sign and the left and right
!      parentheses that should surround the arguments.
!      ----------------------------------------------------------------
!
      nf    = LEN_TRIM(equation)
      eqPos = INDEX(equation,"=")

      IF (eqPos == 0)     THEN
          EQNErrorMessage = "No equal sign in expression"
          RETURN
      END IF
      formula = equation(eqPos+1:)
      formula = ADJUSTL(formula)

      nl  = index(equation(1:eqPos),"(") ! location of left parenthesis (
      IF ( nl == 0 )     THEN   ! find position of left parenthesis
          EQNErrorMessage = "Function name has no left parenthesis"
          RETURN
      END IF

      nr = index(equation(1:eqPos),")") ! location of right parenthesis
      IF (nr==0) THEN
          EQNErrorMessage = "Function name has no right parenthesis"
          RETURN
      END IF
!
      IF ( nr == nl+1 ) THEN
          EQNErrorMessage = "No variable found"
          RETURN
      END IF
!
!     -----------------------------------
!     Extract the independent variable(s)
!     -----------------------------------
!
      CALL getArguments(stringToScan = equation, &
                        arguments    = variableNames, &
                        nArguments   = nVars)
!
!     --------------------------
!     Remove blanks from formula
!     --------------------------
!
      nl = 1
      DO n = 1, LEN_TRIM(formula)
         IF( formula(n:n) /= " " )     THEN
            formula(nl:nl) = formula(n:n)
            nl = nl + 1
         END IF
      END DO
      formula(nl:EQUATION_STRING_LENGTH) = " "

      FindExpression = .true.
!
      END FUNCTION FindExpression
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GetComponents( formula, components, tCount )
         IMPLICIT NONE
         CHARACTER(LEN=EQUATION_STRING_LENGTH)    , INTENT(IN)  :: formula
         CHARACTER(LEN=TOKEN_LENGTH), DIMENSION(:), INTENT(OUT) :: components
         INTEGER                                  , INTENT(OUT) :: tCount

         INTEGER                                     :: startPos
         INTEGER                                     :: l, m


         CHARACTER(LEN=TOKEN_LENGTH)                 :: token
         LOGICAL                                     :: firstCharacterIsDigit
         CHARACTER(LEN=1)                            :: lastChar

         tCount   = 0
         startPos = 1
!
!        ---------------------------------
!        For each CHARACTER IN the formula
!        ---------------------------------
!
         DO l = 1, LEN_TRIM(formula)
!
!           ------------------------------
!           For each separator IN the list
!           ------------------------------
!
            DO m = 1, separatorCount

               IF( formula(l:l) == separators(m) )     THEN ! A separator is found
!
!                 -----------------
!                 Get the component
!                 -----------------
!
                  IF( startPos <= l-1 )     THEN
                     tCount             = tCount + 1
                     components(tCount) = formula(startPos:l-1)
                  END IF
!
!                 -----------------
!                 Get the separator
!                 -----------------
!
                  tCount             = tCount + 1
                  components(tCount) = formula(l:l)
!
!                 --------------
!                 Move the start
!                 --------------
!
                  startPos = l + 1
                  EXIT
               END IF
            END DO

         END DO
!
!        ----------------
!        Get the last one
!        ----------------
!
         IF( startPos <= LEN_TRIM(formula))     THEN
            tCount = tCount + 1
            components( tCount ) = formula(startPos:LEN_TRIM(formula))
         END IF
!
!        -------------------------------------------------
!        Concatenate scientific notation that has possibly
!        been split into components
!        -------------------------------------------------
!
         DO l = 1, tCount-2
            token = components(l)
!
!           ---------------------------------
!           See if first character is a digit
!           ---------------------------------
!
            firstCharacterIsDigit = .false.
            DO m = 1, SIZE(DIGITS)
               IF( token(1:1) == DIGITS(m) )     THEN
                  firstCharacterIsDigit = .true.
                  EXIT
               END IF
            END DO
!
!           ---------------------------------------
!           If so, see if it has an exponent symbol
!           ---------------------------------------
!
            IF( firstCharacterIsDigit )     THEN
               lastChar = token(LEN_TRIM(token):LEN_TRIM(token))
               IF( lastChar == "e" .OR. lastchar == "E" )     THEN
                  components(l) = TRIM(components(l)) // TRIM(components(l+1)) // TRIM(components(l+2))
                  components(l+1) = " "
                  components(l+2) = " "
               END IF
            END IF
         END DO
!
!        -----------------
!        Compact the array
!        -----------------
!
         m = 0
         DO l = 1, tCount
            IF ( components(l) /= " " )     THEN
               m = m + 1
               components(m) = components(l)
            END IF
         END DO
         tCount = m
!
      END SUBROUTINE GetComponents
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ClassifyComponents( components, classification, variableNames, tCount )
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER                                            :: tCount
      CHARACTER(LEN=TOKEN_LENGTH)         , DIMENSION(:) :: components
      INTEGER                             , DIMENSION(:) :: classification
      CHARACTER(LEN=VARIABLE_NAME_LENGTH) ,  INTENT(IN)  :: variableNames(EEC_MAX_NUMBER_OF_VARIABLES)
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER :: l, m, k
      LOGICAL :: isNumber, isFunction, isOperator, isVariable
!
!     ----------------------
!     Examine each component
!     ----------------------
!
      DO l = 1, tCount
         IF ( components(l) == "pi" .OR. components(l) == "PI" )     THEN
            classification(l) = TYPE_NUMBER
         ELSE IF (components(l) == "(" .OR. components(l) == ")" )   THEN
            classification(l) = TYPE_PAREN
         ELSE
!
!           ----------------------------
!           See if it is a variable name
!           ----------------------------
!
            isVariable = .FALSE.
            DO k = 1, EEC_MAX_NUMBER_OF_VARIABLES
               IF ( components(l) == variableNames(k) )     THEN
                  classification(l) = TYPE_VARIABLE + k - 1
                  isVariable = .TRUE.
                  EXIT
               END IF
            END DO
            IF(isVariable) CYCLE
!
!           ---------------------
!           See if it is a number
!           ---------------------
!
            isNumber = .false.
            DO m = 1, SIZE(DIGITS)
               IF( components(l)(1:1) == DIGITS(m) )     THEN
                  classification(l) = TYPE_NUMBER
                  isNumber = .true.
                  EXIT
               END IF
            END DO
            IF( isNumber ) CYCLE
!
!           -----------------------
!           See if it is a function
!           -----------------------
!
            isFunction = .false.
            DO m = 1, SIZE(functions)
               IF( components(l) == functions(m) )     THEN
                  classification(l) = TYPE_FUNCTION
                  isFunction = .true.
                  EXIT
               END IF
            END DO
            IF( isFunction ) CYCLE
!
!           ------------------------
!           See if it is an operator
!           ------------------------
!
            isOperator = .false.
            DO m = 1, SIZE(operators)
               IF( components(l) == operators(m) )     THEN
                  classification(l) = TYPE_OPERATOR
                  isOperator = .true.
                  EXIT
               END IF
            END DO
         END IF
      END DO
!
!     --------------------------
!     Classify monadic operators
!     --------------------------
!
      IF( classification(1) == TYPE_OPERATOR .OR. components(1) == '(' )     THEN
         IF( components(1) == "+" .OR. components(1) == "-" )     THEN
            classification(1) = TYPE_MONO_OPERATOR
         END IF
      END IF

      DO l = 2, tCount
         IF( classification(l) == TYPE_OPERATOR .AND. &
           (classification(l-1) == TYPE_OPERATOR .OR. components(l-1) == '('))     THEN
            classification(l) = TYPE_MONO_OPERATOR
         END IF
      END DO

      END SUBROUTINE ClassifyComponents
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConvertToPostfix( tokens, postFixArray, sizeOfPostfixArray )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(Token), DIMENSION(:), INTENT(IN)   :: tokens
         TYPE(Token), DIMENSION(:), INTENT(OUT)  :: postFixArray
         INTEGER                  , INTENT(OUT)  :: sizeOfPostfixArray
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER           :: t, top, m, N
         TYPE(Token)       :: tok
         TYPE(TokenStack)  :: stack, postfixStack
         CHARACTER(LEN=1)  :: c,s

         N = SIZE(tokens)
         CALL ConstructTokenStack( stack       , N )
         CALL ConstructTokenStack( postfixStack, N )

         DO t = 1, SIZE(tokens)

            IF ( tokens(t) % tokenType == TYPE_NUMBER .OR. &
                 tokens(t) % tokenType >= TYPE_VARIABLE )     THEN

               CALL TokenStackPush( postfixStack, tokens(t) )

            ELSE IF ( tokens(t) % token == ")" )     THEN
!
!              ------------------------------------------------------
!              Right Paren found, unstack until a left paren is found
!              ------------------------------------------------------
!
               top = stack % top
               DO m = top, 1, -1
                  CALL TokenStackPop( stack, tok )
                  IF( tok % token == "(" ) EXIT
                  CALL TokenStackPush( postfixStack, tok )
               END DO
!
!              --------------------------------------------------
!              Left Paren is found - see of it is a function call
!              --------------------------------------------------
!
               top = stack % top
               IF( top /= 0 )     THEN
                  CALL TokenStackPeek( stack, tok )
                  IF ( tok % tokenType == TYPE_FUNCTION )     THEN
                     CALL TokenStackPop ( stack, tok )
                     CALL TokenStackPush( postfixStack, tok )
                  END IF
               END IF

            ELSE
!
!              ---------------------------------------------------
!              Add the token either to the stack or to the postfix
!              ---------------------------------------------------
!
               top = stack % top
               s   = tokens(t) % token(1:1)
               DO m = top, 1, -1
                  CALL TokenStackPeek( stack, tok )
                  c = tok % token(1:1)
                  IF( isp(c) < icp(s) ) EXIT
                  CALL TokenStackPop( stack, tok )
                  CALL TokenStackPush( postfixStack, tok )
               END DO
               CALL TokenStackPush( stack, tokens(t) )
            END IF
         END DO
!
!        ---------------
!        Empty the stack
!        ---------------
!
         top = stack % top
         DO m = 1, top
            CALL TokenStackPop( stack, tok )
            CALL TokenStackPush( postfixStack, tok )
         END DO
!
!        ------------------------
!        Copy to the result array
!        ------------------------
!
         top                = postfixStack % top
         sizeOfPostfixArray = top
         DO m = 1, top
            postFixArray(m) = postfixStack % tokens(m)
         END DO
!
!        -------
!        Cleanup
!        -------
!
         CALL DestructTokenStack( stack )
         CALL DestructTokenStack( postfixStack )

      END SUBROUTINE ConvertToPostfix

!
!///////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION isp(char)
!
!     -----------------------------------------------------------------------
!     determines in-stack priority of operator. looks only at first character
!     -----------------------------------------------------------------------
!
      character char*1
!
      if (char=='(') THEN
          isp = 0

      ELSE IF (char=='+' .or. char=='-') THEN
          isp = 1

      ELSE IF (char=='*' .or. char=='/') THEN
          isp = 2

      ELSE IF (char=='^') THEN
          isp = 3

      ELSE IF (char==';') THEN
          isp = -2

      ELSE IF (char=='@') THEN
          isp = 4

      else
          isp = -1
      end if
!
      END FUNCTION isp
!
!///////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION icp(char)
!
!     ------------------------------------------------------------------------
!     Determines in-stack priority of operator.  Looks only at first character
!     ------------------------------------------------------------------------
!
      character char*1

      if (char=='(') THEN
          icp = 5

      ELSE IF (char=='+' .or. char=='-') THEN
          icp = 1

      ELSE IF (char=='*' .or. char=='/') THEN
          icp = 2

      ELSE IF (char=='^') THEN
          icp = 5

      ELSE IF (char=='@') THEN
          icp = 6

      else
          icp = 7
      end if
!
      END FUNCTION icp
!
!///////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FunOfx(fun,a,result)
!
      REAL(KIND=EP)    :: a,result
      CHARACTER(LEN=*) :: fun
      INTRINSIC        ::  abs,acos,asin,atan,cos,exp,log,log10,sin,sqrt,tan
!---
!     ..
      if ( fun == "cos" .OR. fun == "COS") then
          result = cos(a)

      else if ( fun == "sin".OR. fun == "SIN") then
          result = sin(a)

      else if ( fun == "exp".OR. fun == "EXP") then
          result = exp(a)

      else if ( fun == "sqrt".OR. fun == "SQRT") then
          result = sqrt(a)

      else if ( fun == "ln".OR. fun == "LN") then
          result = log(a)

      else if ( fun == "log".OR. fun == "LOG") then
          result = log10(a)

      else if ( fun == "abs".OR. fun == "ABS") then
          result = abs(a)

      else if ( fun == "acos".OR. fun == "ACOS") then
          result = acos(a)

      else if ( fun == "asin".OR. fun == "ASIN") then
          result = asin(a)

      else if ( fun == "tan".OR. fun == "TAN") then
          result = tan(a)

      else if ( fun == "tanh".OR. fun == "TANH") then
          result = tanh(a)

      else if ( fun == "atan".OR. fun == "ATAN") then
          result = atan(a)

      else if ( fun == "atanh".OR. fun == "ATANH") then
          result = atanh(a)

      else
          write (6,fmt=*) "unknown function"
          result = 0.0d0
      end if
      END SUBROUTINE funofx
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructTokenStack( stack, N )
         IMPLICIT NONE
         INTEGER          :: N
         TYPE(TokenStack) :: stack
         ALLOCATE( stack % tokens(N) )
         stack % top = 0
      END SUBROUTINE ConstructTokenStack
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DestructTokenStack( stack )
         IMPLICIT NONE
         TYPE(TokenStack) :: stack
         DEALLOCATE( stack % tokens )
         stack % top = 0
      END SUBROUTINE DestructTokenStack
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE TokenStackPush( stack, tok )
         IMPLICIT NONE
         TYPE(TokenStack) :: stack
         TYPE(Token)      :: tok
         stack % top               = stack % top + 1
         stack % tokens(stack % top) = tok
      END SUBROUTINE TokenStackPush
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE TokenStackPop( stack, tok )
         IMPLICIT NONE
         TYPE(TokenStack) :: stack
         TYPE(Token)      :: tok

         IF( stack % top <= 0 ) THEN
            PRINT *, "Attempt to pop from empty token stack"
         ELSE
            tok       = stack % tokens(stack % top)
            stack % top = stack % top - 1
         END IF
      END SUBROUTINE TokenStackPop
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE TokenStackPeek( stack, tok )
         IMPLICIT NONE
         TYPE(TokenStack) :: stack
         TYPE(Token)      :: tok

         IF( stack % top <= 0 ) THEN
            PRINT *, "Attempt to peek from empty token stack"
         ELSE
            tok = stack % tokens(stack % top)
         END IF
      END SUBROUTINE TokenStackPeek
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructNumberStack( stack, N )
         IMPLICIT NONE
         INTEGER           :: N
         TYPE(NumberStack) :: stack
         ALLOCATE( stack % values(N) )
         stack % top = 0
      END SUBROUTINE ConstructNumberStack
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DestructNumberStack( stack )
         IMPLICIT NONE
         TYPE(NumberStack) :: stack
         DEALLOCATE( stack % values )
         stack % top = 0
      END SUBROUTINE DestructNumberStack
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE NumberStackPush( stack, v )
         IMPLICIT NONE
         TYPE(NumberStack) :: stack
         REAL(KIND=EP)     :: v
         stack % top               = stack % top + 1
         stack % values(stack % top) = v
      END SUBROUTINE NumberStackPush
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE NumberStackPop( stack, v )
         IMPLICIT NONE
         TYPE(NumberStack) :: stack
         REAL(KIND=EP)     :: v

         IF( stack % top <= 0 ) THEN
            PRINT *, "Attempt to pop from empty number stack"
         ELSE
            v       = stack % values(stack % top)
            stack % top = stack % top - 1
         END IF
      END SUBROUTINE NumberStackPop
!
!////////////////////////////////////////////////////////////////////////
!
!      SUBROUTINE NumberStackPeek( stack, v )
!         IMPLICIT NONE
!         TYPE(NumberStack) :: stack
!         REAL(KIND=EP)     :: v
!
!         IF( stack % top <= 0 ) THEN
!            PRINT *, "Attempt to peek from empty number stack"
!         ELSE
!            v = stack % values(stack % top)
!         END IF
!      END SUBROUTINE NumberStackPeek
!
!////////////////////////////////////////////////////////////////////////
!
       FUNCTION FinalSyntaxCheckOK( self ) RESULT(success)
         IMPLICIT NONE
         TYPE(EquationEvaluator) :: self
         INTEGER                 :: t, p
         LOGICAL                 :: success
         TYPE(Token)             :: tok
         INTEGER                 :: stackTop

         success = .true.
!
!        -----------------
!        Count Parentheses
!        -----------------
!
         p = 0
         DO t = 1, LEN_TRIM(self % equation)
            IF( self % equation(t:t) == "(" )     THEN
               p = p + 1
            END IF
            IF( self % equation(t:t) == ")" )     THEN
               p = p - 1
            END IF
         END DO
         IF( p /= 0 )     THEN
            success         = .false.
            EQNErrorMessage = "Syntax Error: Unbalanced Parentheses"
            RETURN
         END IF
!
!        -------------------------------------
!        Check for unknown functions/variables
!        -------------------------------------
!
         DO t = 1, SIZE(self % postfix)
            IF(self % postfix(t) % tokenType == TYPE_NONE )     THEN
               EQNErrorMessage = "Syntax Error: Unknown Variable or function: " // self % postfix(t) % token
               success = .false.
               RETURN
            END IF
         END DO
!
!        -----
!        Other
!        -----
!
         stackTop = 0
         DO t = 1, SIZE(self % postfix)
            tok = self % postfix(t)
            SELECT CASE ( tok % tokenType )

               CASE( TYPE_NUMBER )
                  stackTop = stackTop + 1

               CASE ( TYPE_OPERATOR )
                  stackTop = stackTop - 1
                  IF( stackTop <= 0 )     THEN
                     EQNErrorMessage = "Syntax Error: Too many or wrong ordering of operators"
                     success = .false.
                     RETURN
                  END IF

               CASE DEFAULT ! Type Variable
                  IF ( tok % tokenType >= TYPE_VARIABLE )     THEN
                     stackTop = stackTop + 1
                  END IF

            END SELECT
         END DO
         IF ( stackTop > 1 )     THEN
            EQNErrorMessage = "Syntax Error: Not enough or wrong ordering of operators"
            success = .false.
         END IF

       END FUNCTION FinalSyntaxCheckOK
!
!////////////////////////////////////////////////////////////////////////
!
       FUNCTION scanUpToString(stringToScan, startPos, stopChars) RESULT(s)
!
!      -------------------------------------------------------------------------------------
!      Returns from stringToScan the string starting at startPos up to but not including the
!      stopChar. If the stopChar is not found, then startPos = EE_FAIL
!      -------------------------------------------------------------------------------------
!
          IMPLICIT NONE
          CHARACTER(LEN=*)                        :: stringToScan
          INTEGER                                 :: startPos
          CHARACTER(LEN=*)                        :: stopChars
          CHARACTER(LEN = EQUATION_STRING_LENGTH) :: s
!
!         ---------------
!         Local variables
!         ---------------
!
          INTEGER                                 :: i, j
          LOGICAL                                 :: charFound

          iLoop: DO i = startPos, LEN_TRIM(stringToScan)
             charFound = .FALSE.
             DO j = 1,LEN_TRIM(stopChars)
                IF(stringToScan(i:i) == stopChars(j:j))     THEN
                   charFound = .TRUE.
                   EXIT iLoop
                END IF
             END DO
          END DO iLoop

          IF ( .NOT.charFound )     THEN
             s        = ''
             startPos = EEC_NONE
          ELSE
             s        = stringToScan(startPos:i-1)
             startPos = i
          END IF

       END FUNCTION scanUpToString
!
!////////////////////////////////////////////////////////////////////////
!
       SUBROUTINE getArguments(stringToScan, arguments, nArguments)
          IMPLICIT NONE
!
!         ---------
!         Arguments
!         ---------
!
          CHARACTER(LEN=*), INTENT(IN)                     :: stringToScan
          CHARACTER(LEN=VARIABLE_NAME_LENGTH), INTENT(OUT) :: arguments(EEC_MAX_NUMBER_OF_VARIABLES)
          INTEGER, INTENT(OUT)                             :: nArguments
!
!         ---------------
!         Local Variables
!         ---------------
!
          INTEGER                               :: n, k, indx
          CHARACTER(LEN=LEN_TRIM(stringToScan)) :: r

          arguments = "%none"
          n = 1
          r = scanUpToString(stringToScan = stringToScan, startPos = n, stopChars = '(')

          n    = n + 1
          indx = 1
          DO k = 1, LEN_TRIM(stringToScan)
               r = scanUpToString(stringToScan = stringToScan,startPos = n,stopChars = ',)')
               IF(n == EEC_NONE)   EXIT

               nArguments      = indx
               arguments(indx) = r
               IF(stringToScan(n:n) == ')') EXIT

               n          = n + 1
               indx       = indx + 1
               IF(indx > EEC_MAX_NUMBER_OF_VARIABLES)     THEN
                  nArguments = EEC_TOO_MANY_ARGUMENTS
                  RETURN
               END IF
            END DO

       END SUBROUTINE getArguments

      END Module EquationEvaluatorClass

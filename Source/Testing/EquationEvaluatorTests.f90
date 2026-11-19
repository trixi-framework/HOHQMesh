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
!      EquationEvaluatorTests.f90
!      Created: 2026-11-19
!      Unit tests for the equation evaluator used by parametric curves
!      and parametric topographies:
!        1. Every intrinsic function, pi, operator precedence, unary
!           minus and several independent variables evaluate correctly
!        2. Malformed equations are rejected with the expected message
!        3. A bad equation in a parametric curve or topography throws the
!           "Equation formatting error" exception without stopping
!////////////////////////////////////////////////////////////////////////
      Module EquationEvaluatorTestsModule
      USE SMConstants
      USE EquationEvaluatorClass
      USE SMParametricEquationCurveClass, ONLY : SMParametricEquationCurve, releasePECurve
      USE SMEquationTopographyClass     , ONLY : SMEquationTopography, releasePETopography
      USE FTValueClass
      USE SharedExceptionManagerModule
      USE FTAssertions
      IMPLICIT NONE

      PRIVATE
      PUBLIC :: EquationEvaluatorTests, EquationFormatExceptionTests
!     ========
      CONTAINS
!     ========
!////////////////////////////////////////////////////////////////////////
      SUBROUTINE EquationEvaluatorTests(optData)
         IMPLICIT NONE
!        Arguments
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused
!        Local variables
         INTEGER, PARAMETER :: nFunctions = 13
         CHARACTER(LEN=5)   :: functionNames(nFunctions)
         REAL(KIND=EP)      :: expected(nFunctions)
         REAL(KIND=EP)      :: x, xVec(EEC_MAX_NUMBER_OF_VARIABLES)
         REAL(KIND=EP)      :: pi = 3.141592653589793238462643_EP
         REAL(KIND=EP)      :: tol = 1.0E-13_EP
         INTEGER            :: k
!
!        -------------------------------------------------------------
!        Each function at x = 0.5, where all of them are defined. The
!        argument is written as 2*x - 1/2 so the function is applied to
!        an expression rather than a bare variable.
!        -------------------------------------------------------------
!
         x = 0.5_EP
         functionNames = ["cos  ","sin  ","exp  ","tan  ","sqrt ","abs  ","ln   ","log  ", &
                          "acos ","asin ","atan ","atanh","tanh "]
         expected      = [COS(x), SIN(x), EXP(x), TAN(x), SQRT(x), ABS(x), LOG(x), LOG10(x), &
                          ACOS(x), ASIN(x), ATAN(x), ATANH(x), TANH(x)]
         DO k = 1, nFunctions
            CALL CheckEquation("f(x) = " // TRIM(functionNames(k)) // "(2*x - 1/2)", [x], expected(k), tol, &
                               "Function " // TRIM(functionNames(k)))
         END DO
         CALL CheckEquation("f(x) = abs(1 - 3*x)", [x], 0.5_EP, tol, "abs of a negative argument")
!
!        ------------------------------------------
!        pi, precedence, powers and unary minus
!        ------------------------------------------
!
         CALL CheckEquation("x(t) = 4.0*cos(2*pi*t)", [0.125_EP], 4.0_EP*COS(0.25_EP*pi), tol, "pi constant")
         CALL CheckEquation("f(x) = 2 + 3*x^2 - (x - 1)/4", [x], 2.0_EP + 0.75_EP + 0.125_EP, tol, &
                            "Operator precedence")
         CALL CheckEquation("f(x) = -x + 2", [x], 1.5_EP, tol, "Unary minus")
         CALL CheckEquation("f(x) = 2^3^2", [x], 512.0_EP, tol, "Right associative power")
!
!        -----------------------------
!        Several independent variables
!        -----------------------------
!
         xVec = 0.0_EP
         xVec(1:3) = [1.0_EP, 2.0_EP, 3.0_EP]
         CALL CheckEquation("g(x,y,z) = x*y + z/x - y^2", xVec, 2.0_EP + 3.0_EP - 4.0_EP, tol, &
                            "Three independent variables")
!
!        -------------------------------------------------
!        Malformed equations: rejected with a clear message
!        -------------------------------------------------
!
         CALL CheckSyntaxError("f(x) = (x + 1"        , "Unbalanced Parentheses")
         CALL CheckSyntaxError("f(x) = x + y"         , "Unknown Variable or function")
         CALL CheckSyntaxError("f(x) = foo(x)"        , "Unknown Variable or function")
         CALL CheckSyntaxError("f(x) = x * / 2"       , "Too many or wrong ordering of operators")
         CALL CheckSyntaxError("f(x) = (x)(2)"        , "Not enough or wrong ordering of operators")
!        Blanks are removed before parsing, so "x 2" is the unknown name "x2"
         CALL CheckSyntaxError("f(x) = x 2"           , "Unknown Variable or function: x2")
         CALL CheckSyntaxError("x + 1"                , "No equal sign")
         CALL CheckSyntaxError("f = x + 1"            , "no left parenthesis")
         CALL CheckSyntaxError("f(x = x + 1"          , "no right parenthesis")
         CALL CheckSyntaxError("f() = 1"              , "No variable found")
         CALL CheckSyntaxError("f(a,b,c,d,e) = a"     , "Too many independent variables")

      END SUBROUTINE EquationEvaluatorTests
!////////////////////////////////////////////////////////////////////////
      SUBROUTINE CheckEquation(eqnString, x, expected, tol, msg)
         IMPLICIT NONE
!        Arguments
         CHARACTER(LEN=*) :: eqnString, msg
         REAL(KIND=EP)    :: x(:), expected, tol
!        Local variables
         TYPE(EquationEvaluator) :: eqn
         REAL(KIND=EP)           :: xVec(EEC_MAX_NUMBER_OF_VARIABLES)

         xVec = 0.0_EP
         xVec(1:SIZE(x)) = x

         CALL ConstructEquationEvaluator(eqn, eqnString)
         CALL FTAssert(test = StatusOK(eqn), msg = msg // ": '" // eqnString // "' should parse")
         IF ( StatusOK(eqn) )     THEN
            CALL FTAssertEqual(expectedValue = expected,                       &
                               actualValue   = EvaluateEquation_At_(eqn, xVec), &
                               relTol        = tol, absTol = tol,             &
                               msg           = msg // ": " // eqnString)
         END IF
         CALL DestructEquationEvaluator(eqn)

      END SUBROUTINE CheckEquation
!////////////////////////////////////////////////////////////////////////
      SUBROUTINE CheckSyntaxError(eqnString, expectedMessage)
         IMPLICIT NONE
!        Arguments
         CHARACTER(LEN=*) :: eqnString, expectedMessage
!        Local variables
         TYPE(EquationEvaluator)             :: eqn
         CHARACTER(LEN=ERROR_MESSAGE_LENGTH) :: errorMessage

         CALL ConstructEquationEvaluator(eqn, eqnString)
         CALL FTAssert(test = .NOT.StatusOK(eqn), msg = "'" // eqnString // "' should be rejected")
         errorMessage = ErrorMessageString(eqn)
         CALL FTAssert(test = INDEX(errorMessage, expectedMessage) > 0,                         &
                       msg  = "'" // eqnString // "' should report '" // expectedMessage //    &
                              "' but reported '" // TRIM(errorMessage) // "'")
         CALL DestructEquationEvaluator(eqn)

      END SUBROUTINE CheckSyntaxError
!////////////////////////////////////////////////////////////////////////
      SUBROUTINE EquationFormatExceptionTests(optData)
         IMPLICIT NONE
!        Arguments
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused
!        Local variables
         TYPE(SMParametricEquationCurve), POINTER :: curve
         TYPE(SMEquationTopography)     , POINTER :: topography
!
!        ---------------------------------------------------------
!        A good curve throws nothing. A bad y-equation in a curve
!        throws, and the exception names the equation that failed.
!        ---------------------------------------------------------
!
         ALLOCATE(curve)
         CALL curve % initWithEquationsNameAndID(xEqn = "x(t) = cos(2*pi*t)", yEqn = "y(t) = sin(2*pi*t)", &
                                                 zEqn = "z(t) = 0.0", curveName = "goodCurve", id = 1)
         CALL FTAssert(test = .NOT.catch(), msg = "Good parametric curve should not throw")
         CALL releasePECurve(curve)

         ALLOCATE(curve)
         CALL curve % initWithEquationsNameAndID(xEqn = "x(t) = cos(2*pi*t)", yEqn = "y(t) = sin(2*pi*t", &
                                                 zEqn = "z(t) = 0.0", curveName = "badCurve", id = 2)
         CALL CheckFormatException(expectedObject = "Y-Equation", context = "Parametric curve")
         CALL releasePECurve(curve)
!
!        --------------------------------------
!        The same for a parametric topography
!        --------------------------------------
!
         ALLOCATE(topography)
         CALL topography % initWithEquation(zEqn = "f(x,y) = x*y")
         CALL FTAssert(test = .NOT.catch(), msg = "Good topography should not throw")
         CALL releasePETopography(topography)

         ALLOCATE(topography)
         CALL topography % initWithEquation(zEqn = "f(x,y) = x * y)")
         CALL CheckFormatException(expectedObject = "Topography Equation", context = "Topography")
         CALL releasePETopography(topography)
!
!        -----------------------------------------------------
!        Reset the shared exception manager so the fatal
!        severities recorded above do not leak into later tests
!        -----------------------------------------------------
!
         CALL FTAssert(test = .NOT.catch(), msg = "No exceptions left over")
         CALL initializeFTExceptions

      END SUBROUTINE EquationFormatExceptionTests
!////////////////////////////////////////////////////////////////////////
      SUBROUTINE CheckFormatException(expectedObject, context)
!        Pop the "Equation formatting error" exception and check that it
!        is fatal and names the failing equation
         IMPLICIT NONE
!        Arguments
         CHARACTER(LEN=*) :: expectedObject, context
!        Local variables
         CLASS(FTException) , POINTER :: exception
         CLASS(FTDictionary), POINTER :: info
         CLASS(FTValue)     , POINTER :: v

         CALL FTAssert(test = catch("Equation formatting error"), &
                       msg  = context // " with a bad equation should throw 'Equation formatting error'")
         exception => errorObject()
         IF ( .NOT.ASSOCIATED(exception) )     RETURN

         CALL FTAssertEqual(expectedValue = FT_ERROR_FATAL, actualValue = exception % severity(), &
                            msg = context // " equation exception is fatal")
         info => exception % infoDictionary()
         v    => valueFromObject(info % objectForKey("objectName"))
         CALL FTAssert(test = ASSOCIATED(v), msg = context // " exception has an objectName")
         IF ( ASSOCIATED(v) )     THEN
            CALL FTAssertEqual(expectedValue = expectedObject, &
                               actualValue   = v % stringValue(requestedLength = 32), &
                               msg           = context // " exception names the failing equation")
         END IF

      END SUBROUTINE CheckFormatException
!////////////////////////////////////////////////////////////////////////
      END Module EquationEvaluatorTestsModule

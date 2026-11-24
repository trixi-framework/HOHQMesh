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
!      ControlInputTests.f90
!      Created: 2026-11-19
!      Tests of bad control file input. Each case is written to a
!      scratch file, read with the ControlFileReader and, for the model
!      cases, built into an SMModel. The test checks that the expected
!      warning or fatal exception is raised with the expected message,
!      and for warnings that the documented default is used.
!////////////////////////////////////////////////////////////////////////
      Module ControlInputTestsModule
      USE SMConstants
      USE ProgramGlobals          , ONLY : L2_NORM, H1_NORM
      USE FTAssertions
      USE FTValueClass
      USE FTValueDictionaryClass
      USE FTObjectClass
      USE FTDictionaryClass
      USE FTExceptionClass
      USE SharedExceptionManagerModule
      USE ErrorTypesModule
      USE ControlFileReaderClass
      USE SMChainedCurveClass
      USE SMModelClass
      IMPLICIT NONE

      INTEGER, PARAMETER, PRIVATE :: LL = 80 ! Length of a control file line in these tests
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ImportControlText( text, controlDict )
!
!     -----------------------------------------------------------
!     Read the lines of text as a control file. The dictionary is
!     returned retained; the exceptions raised are left in place.
!     -----------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=*)                  :: text(:)
         TYPE (FTValueDictionary), POINTER :: controlDict

         TYPE(ControlFileReader) :: reader
         INTEGER                 :: fUnit, k

         OPEN(NEWUNIT = fUnit, STATUS = "SCRATCH", ACTION = "READWRITE", FORM = "FORMATTED", RECL = 4096)
         DO k = 1, SIZE(text)
            WRITE(fUnit,'(A)') TRIM(text(k))
         END DO
         REWIND(fUnit)

         CALL reader % init()
         CALL reader % importFromControlFile(fileUnit = fUnit)
         CLOSE(fUnit)

         controlDict => reader % controlDict
         CALL controlDict % retain()
         CALL destructControlFileReader(reader)

      END SUBROUTINE ImportControlText
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION raised( exceptionName, text )
         IMPLICIT NONE
         CHARACTER(LEN=*) :: exceptionName, text
         raised = raisedAll(exceptionName, [text])
      END FUNCTION raised
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION raisedAll( exceptionName, texts )
!
!     ------------------------------------------------------------
!     Remove all the exceptions with the given name and report if
!     each of the texts is contained in one of their messages
!     ------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=*) :: exceptionName, texts(:)

         CLASS(FTException) , POINTER :: exception
         CLASS(FTDictionary), POINTER :: info
         CLASS(FTValue)     , POINTER :: v
         CHARACTER(LEN=256)           :: msg
         LOGICAL                      :: found(SIZE(texts))
         INTEGER                      :: k

         found = .FALSE.
         DO WHILE ( catch(exceptionName) )
            exception => errorObject()
            info      => exception % infoDictionary()
            v         => valueFromObject(info % objectForKey("message"))
            IF ( ASSOCIATED(v) )     THEN
               msg = v % stringValue(requestedLength = 256)
               DO k = 1, SIZE(texts)
                  IF ( INDEX(msg, TRIM(texts(k))) > 0 )     found(k) = .TRUE.
               END DO
            END IF
         END DO
         raisedAll = ALL(found)

      END FUNCTION raisedAll
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ClearExceptions
!
!     ------------------------------------------------------
!     Remove what is left so that the next case, and later
!     tests that ask for maximumErrorSeverity, start clean
!     ------------------------------------------------------
!
         IMPLICIT NONE
         LOGICAL :: found
         found = raised(FATAL_ERROR_EXCEPTION   , "")
         found = raised(WARNING_ERROR_EXCEPTION , "")
         found = raised(FTFatalErrorException   , "")
         found = raised(FTWarningErrorException , "")
         CALL initializeFTExceptions
      END SUBROUTINE ClearExceptions
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckReaderError( label, text, expectedMessage )
         IMPLICIT NONE
         CHARACTER(LEN=*) :: label, expectedMessage
         CHARACTER(LEN=*) :: text(:)

         TYPE (FTValueDictionary), POINTER :: controlDict

         CALL ClearExceptions
         CALL ImportControlText(text, controlDict)
         CALL FTAssertEqual(expectedValue = 1, actualValue = errorCount(), &
                            msg = label // ": reading stops at the first error")
         CALL FTAssert(test = raised(FTFatalErrorException, expectedMessage), &
                       msg  = label // ": fatal '" // expectedMessage // "'")
         CALL releaseFTValueDictionary(controlDict)
         CALL ClearExceptions

      END SUBROUTINE CheckReaderError
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ControlFileReaderErrorTests(optData)
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused

         CALL CheckReaderError("Wrong block end",                                   &
                 [CHARACTER(LEN=LL) :: "\begin{CONTROL_INPUT}",                    &
                                       "   \begin{RUN_PARAMETERS}",                &
                                       "      polynomial order = 4",               &
                                       "   \end{SPRING_SMOOTHER}",                 &
                                       "\end{CONTROL_INPUT}",                      &
                                       "\end{FILE}"],                              &
                 "Improper block termination. Expected RUN_PARAMETERS; found SPRING_SMOOTHER")

         CALL CheckReaderError("Upper case command",                                &
                 [CHARACTER(LEN=LL) :: "\Begin{CONTROL_INPUT}",                    &
                                       "\end{FILE}"],                              &
                 "Syntax error in control file line: \Begin{CONTROL_INPUT}")

         CALL CheckReaderError("Upper case command in a block",                     &
                 [CHARACTER(LEN=LL) :: "\begin{CONTROL_INPUT}",                    &
                                       "   \begin{RUN_PARAMETERS}",                &
                                       "      polynomial order = 4",               &
                                       "   \end{RUN_PARAMETERS}",                  &
                                       "   \Begin{BACKGROUND_GRID}",               &
                                       "      background grid size = [1.0,1.0,0.0]", &
                                       "   \end{BACKGROUND_GRID}",                 &
                                       "\end{CONTROL_INPUT}",                      &
                                       "\begin{MODEL}",                            &
                                       "\end{MODEL}",                              &
                                       "\end{FILE}"],                              &
                 "Syntax error in control file line: \Begin{BACKGROUND_GRID}")

         CALL CheckReaderError("Wrong collection end",                              &
                 [CHARACTER(LEN=LL) :: "\begin{MODEL}",                            &
                                       "   \begin{OUTER_BOUNDARY}",                &
                                       "      \begin{PARAMETRIC_EQUATION_CURVE}",  &
                                       "         name = circle",                   &
                                       "         xEqn = x(t) = cos(2*pi*t)",       &
                                       "         yEqn = y(t) = sin(2*pi*t)",       &
                                       "         zEqn = z(t) = 0.0",               &
                                       "      \end{PARAMETRIC_EQUATION_CURVE}",    &
                                       "   \end{INNER_BOUNDARIES}",                &
                                       "\end{MODEL}",                              &
                                       "\end{FILE}"],                              &
                 "Block termination error. Expected: OUTER_BOUNDARY; Found: INNER_BOUNDARIES")

         CALL CheckReaderError("Key after the first chain",                         &
                 [CHARACTER(LEN=LL) :: "\begin{MODEL}",                            &
                                       "   \begin{INTERFACE_BOUNDARIES}",          &
                                       "      \begin{CHAIN}",                      &
                                       "         name = stone",                    &
                                       "         \begin{PARAMETRIC_EQUATION_CURVE}", &
                                       "            name = stone",                 &
                                       "            xEqn = x(t) = cos(2*pi*t)",    &
                                       "            yEqn = y(t) = sin(2*pi*t)",    &
                                       "            zEqn = z(t) = 0.0",            &
                                       "         \end{PARAMETRIC_EQUATION_CURVE}", &
                                       "      \end{CHAIN}",                        &
                                       "      paving = OFF",                       &
                                       "   \end{INTERFACE_BOUNDARIES}",            &
                                       "\end{MODEL}",                              &
                                       "\end{FILE}"],                              &
                 "Unexpected line in the INTERFACE_BOUNDARIES block: paving = OFF")

         CALL CheckReaderError("Spline without nKnots", splineText(nKnots = "", endMarker = .TRUE.), &
                               "Malformed Spline data. No nKnots")
         CALL CheckReaderError("Spline without end marker", splineText(nKnots = "nKnots = 3", endMarker = .FALSE.), &
                               "No \end{SPLINE_DATA} marker for spline data")
         CALL CheckReaderError("Spline with too few rows", splineText(nKnots = "nKnots = 4", endMarker = .TRUE.), &
                               "Spline data has fewer than nKnots rows")

      END SUBROUTINE ControlFileReaderErrorTests
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION splineText( nKnots, endMarker ) RESULT(text)
!
!     ----------------------------------------------------------
!     A spline curve with three rows of data, with the given
!     nKnots line and with or without the \end{SPLINE_DATA} line
!     ----------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=*)               :: nKnots
         LOGICAL                        :: endMarker
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)
         CHARACTER(LEN=LL)              :: endLine

         endLine = "      \end{SPLINE_DATA}"
         IF ( .NOT.endMarker )     endLine = "%"
         text = [CHARACTER(LEN=LL) :: "\begin{MODEL}",                    &
                                      "   \begin{OUTER_BOUNDARY}",        &
                                      "      \begin{SPLINE_CURVE}",       &
                                      "         name = spline",           &
                                      "         " // nKnots,              &
                                      "      \begin{SPLINE_DATA}",        &
                                      "         0.0  1.0  0.0  0.0",      &
                                      "         0.5 -1.0  0.0  0.0",      &
                                      "         1.0  1.0  0.0  0.0",      &
                                      endLine,                            &
                                      "      \end{SPLINE_CURVE}",         &
                                      "   \end{OUTER_BOUNDARY}",          &
                                      "\end{MODEL}",                      &
                                      "\end{FILE}"]
      END FUNCTION splineText
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE BuildModelFromText( text, model )
!
!     -------------------------------------------------------
!     Import the text and build the model from its MODEL block
!     -------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=*)                  :: text(:)
         TYPE (SMModel)          , POINTER :: model

         TYPE (FTValueDictionary), POINTER :: controlDict
         CLASS(FTValueDictionary), POINTER :: modelDict ! valueDictionaryFromObject returns CLASS

         CALL ImportControlText(text, controlDict)
         modelDict => valueDictionaryFromObject(controlDict % objectForKey("MODEL"))
         ALLOCATE(model)
         CALL model % initWithContentsOfDictionary(modelDict)
         CALL releaseFTValueDictionary(controlDict)

      END SUBROUTINE BuildModelFromText
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION outerBoundaryText( chainKeys, curves ) RESULT(text)
         IMPLICIT NONE
         CHARACTER(LEN=LL)              :: chainKeys(:), curves(:)
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)
         text = [CHARACTER(LEN=LL) :: "\begin{MODEL}", "\begin{OUTER_BOUNDARY}", chainKeys, curves, &
                                      "\end{OUTER_BOUNDARY}", "\end{MODEL}", "\end{FILE}"]
      END FUNCTION outerBoundaryText
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION innerChainText( chainKeys, curves ) RESULT(text)
         IMPLICIT NONE
         CHARACTER(LEN=LL)              :: chainKeys(:), curves(:)
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)
         text = [CHARACTER(LEN=LL) :: "\begin{MODEL}", "\begin{INNER_BOUNDARIES}", "\begin{CHAIN}", &
                                      "name = triangle", chainKeys, curves,                          &
                                      "\end{CHAIN}", "\end{INNER_BOUNDARIES}", "\end{MODEL}", "\end{FILE}"]
      END FUNCTION innerChainText
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION circleText( ) RESULT(text)
         IMPLICIT NONE
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)
         text = [CHARACTER(LEN=LL) :: "\begin{PARAMETRIC_EQUATION_CURVE}", "name = circle", &
                                      "xEqn = x(t) = cos(2*pi*t)", "yEqn = y(t) = sin(2*pi*t)", &
                                      "zEqn = z(t) = 0.0", "\end{PARAMETRIC_EQUATION_CURVE}"]
      END FUNCTION circleText
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION lineText( name, xStart, xEnd ) RESULT(text)
         IMPLICIT NONE
         CHARACTER(LEN=*)               :: name, xStart, xEnd
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)
         text = [CHARACTER(LEN=LL) :: "\begin{END_POINTS_LINE}", "name = " // name, &
                                      "xStart = " // xStart, "xEnd = " // xEnd, "\end{END_POINTS_LINE}"]
      END FUNCTION lineText
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION triangleText( ) RESULT(text)
         IMPLICIT NONE
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)
         text = [lineText("a", "[0.0,0.0,0.0]", "[1.0,0.0,0.0]"), &
                 lineText("b", "[1.0,0.0,0.0]", "[0.0,1.0,0.0]"), &
                 lineText("c", "[0.0,1.0,0.0]", "[0.0,0.0,0.0]")]
      END FUNCTION triangleText
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckModelInput( label, text, exceptionName, expectedMessage )
!
!     ------------------------------------------------------------
!     Build the model and check that the exception is raised.
!     Nothing else may be raised.
!     ------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=*) :: label, exceptionName, expectedMessage
         CHARACTER(LEN=*) :: text(:)
         TYPE (SMModel), POINTER :: model

         CALL ClearExceptions
         CALL BuildModelFromText(text, model)
         CALL FTAssert(test = raised(exceptionName, expectedMessage), &
                       msg  = label // ": '" // expectedMessage // "'")
         CALL FTAssert(test = .NOT.catch(), msg = label // ": no other exception is raised")
         CALL releaseModel(model)
         CALL ClearExceptions

      END SUBROUTINE CheckModelInput
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ChainOptimizationInputTests(optData)
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused

         TYPE (SMModel)       , POINTER :: model
         CLASS(SMChainedCurve), POINTER :: chain
         LOGICAL                        :: found
!
!        -----------------------------------------------------------
!        Without continuity and tolerance the defaults 0 and 1.0E-3
!        are used, each with a warning
!        -----------------------------------------------------------
!
         CALL ClearExceptions
         CALL BuildModelFromText(outerBoundaryText([CHARACTER(LEN=LL) :: "optimize = L2Norm"], circleText()), model)
         CALL FTAssert(test = raisedAll(WARNING_ERROR_EXCEPTION,                                       &
                                        [CHARACTER(LEN=LL) :: "Optimization continuity key not found", &
                                                              "Optimization tolerance key not found"]), &
                       msg  = "Missing continuity and tolerance warn")
         CALL FTAssertEqual(expectedValue = L2_NORM, actualValue = model % outerBoundary % optimization, &
                            msg = "optimize = L2Norm")
         CALL FTAssertEqual(expectedValue = 0, actualValue = model % outerBoundary % continuity, &
                            msg = "Default continuity")
         CALL FTAssert(test = model % outerBoundary % tolerance == 1.0E-3_RP, msg = "Default tolerance")
         CALL FTAssert(test = SIZE(model % outerBoundary % breaks) == 2, msg = "One curve has breaks [0,1]")
         CALL releaseModel(model)
!
!        ------------------------------------------------------------
!        connect: a valid statement removes the break between curves
!        1 and 2; an ill-formed one is ignored with a warning and all
!        joints are breaks
!        ------------------------------------------------------------
!
         CALL ClearExceptions
         CALL BuildModelFromText(innerChainText([CHARACTER(LEN=LL) :: "optimize = H1Norm", "continuity = 1", &
                                                 "tolerance = 1.0e-3", "connect = 1-2"], triangleText()), model)
         CALL FTAssert(test = .NOT.catch(), msg = "A valid connect statement raises nothing")
         chain => firstInnerChain(model)
         CALL FTAssertEqual(expectedValue = H1_NORM, actualValue = chain % optimization, msg = "optimize = H1Norm")
         CALL CheckBreaks(chain, [0.0_RP, 2.0_RP/3.0_RP, 1.0_RP], "connect = 1-2")
         CALL releaseModel(model)

         CALL CheckModelInput("Ill-formed connect",                                                       &
                              innerChainText([CHARACTER(LEN=LL) :: "optimize = H1Norm", "continuity = 1", &
                                              "tolerance = 1.0e-3", "connect = 1-"], triangleText()),     &
                              FTWarningErrorException, "Format Error in connect statement:1-. Ignoring.")
         CALL ClearExceptions
         CALL BuildModelFromText(innerChainText([CHARACTER(LEN=LL) :: "optimize = H1Norm", "continuity = 1", &
                                                 "tolerance = 1.0e-3", "connect = 1-"], triangleText()), model)
         found = raised(FTWarningErrorException, "")
         chain => firstInnerChain(model)
         CALL CheckBreaks(chain, [0.0_RP, 1.0_RP/3.0_RP, 2.0_RP/3.0_RP, 1.0_RP], "Ill-formed connect")
         CALL releaseModel(model)
         CALL ClearExceptions

      END SUBROUTINE ChainOptimizationInputTests
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION firstInnerChain( model ) RESULT(chain)
         IMPLICIT NONE
         TYPE (SMModel)       , POINTER :: model
         CLASS(SMChainedCurve), POINTER :: chain
         CLASS(FTObject)      , POINTER :: obj
         CALL model % innerBoundariesIterator % setToStart()
         obj => model % innerBoundariesIterator % object()
         CALL castToSMChainedCurve(obj, chain)
      END FUNCTION firstInnerChain
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckBreaks( chain, expected, label )
         IMPLICIT NONE
         CLASS(SMChainedCurve), POINTER :: chain
         REAL(KIND=RP)                  :: expected(:)
         CHARACTER(LEN=*)               :: label
         LOGICAL                        :: same
         same = SIZE(chain % breaks) == SIZE(expected)
         IF ( same )     same = MAXVAL(ABS(chain % breaks - expected)) <= 4.0_RP*EPSILON(1.0_RP)
         CALL FTAssert(test = same, msg = label // ": breaks")
      END SUBROUTINE CheckBreaks
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CurveInputErrorTests(optData)
!
!     -----------------------------------------------------------
!     A missing name gives a warning and a default name. Missing
!     geometry is fatal.
!     -----------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused
         CHARACTER(LEN=LL), PARAMETER :: none(0) = [CHARACTER(LEN=LL) ::]
!
!        --------------------
!        Parametric equations
!        --------------------
!
         CALL CheckModelInput("Equation curve without name", outerBoundaryText(none,                      &
                 [CHARACTER(LEN=LL) :: "\begin{PARAMETRIC_EQUATION_CURVE}", "xEqn = x(t) = cos(2*pi*t)",   &
                                       "yEqn = y(t) = sin(2*pi*t)", "zEqn = z(t) = 0.0",                   &
                                       "\end{PARAMETRIC_EQUATION_CURVE}"]),                                &
                 WARNING_ERROR_EXCEPTION, "PARAMETRIC_EQUATION_CURVE has no name")
         CALL CheckModelInput("Equation curve without zEqn", outerBoundaryText(none,                      &
                 [CHARACTER(LEN=LL) :: "\begin{PARAMETRIC_EQUATION_CURVE}", "name = c",                    &
                                       "xEqn = x(t) = cos(2*pi*t)", "yEqn = y(t) = sin(2*pi*t)",           &
                                       "\end{PARAMETRIC_EQUATION_CURVE}"]),                                &
                 WARNING_ERROR_EXCEPTION, "PARAMETRIC_EQUATION_CURVE has no zEqn")
         CALL CheckModelInput("Equation curve without xEqn", outerBoundaryText(none,                      &
                 [CHARACTER(LEN=LL) :: "\begin{PARAMETRIC_EQUATION_CURVE}", "name = c",                    &
                                       "yEqn = y(t) = sin(2*pi*t)", "zEqn = z(t) = 0.0",                   &
                                       "\end{PARAMETRIC_EQUATION_CURVE}"]),                                &
                 FATAL_ERROR_EXCEPTION, "PARAMETRIC_EQUATION_CURVE has no xEqn")
         CALL CheckModelInput("Equation curve without yEqn", outerBoundaryText(none,                      &
                 [CHARACTER(LEN=LL) :: "\begin{PARAMETRIC_EQUATION_CURVE}", "name = c",                    &
                                       "xEqn = x(t) = cos(2*pi*t)", "zEqn = z(t) = 0.0",                   &
                                       "\end{PARAMETRIC_EQUATION_CURVE}"]),                                &
                 FATAL_ERROR_EXCEPTION, "PARAMETRIC_EQUATION_CURVE has no yEqn")
!
!        -----
!        Lines
!        -----
!
         CALL CheckModelInput("Line without name", outerBoundaryText(none,                                &
                 [CHARACTER(LEN=LL) :: "\begin{END_POINTS_LINE}", "xStart = [0.0,0.0,0.0]",                &
                                       "xEnd = [0.0,0.0,0.0]", "\end{END_POINTS_LINE}"]),                  &
                 WARNING_ERROR_EXCEPTION, "No name found in line curve definition")
         CALL CheckModelInput("Line without xStart", outerBoundaryText(none,                              &
                 [CHARACTER(LEN=LL) :: "\begin{END_POINTS_LINE}", "name = a", "xEnd = [0.0,0.0,0.0]",      &
                                       "\end{END_POINTS_LINE}"]),                                          &
                 FATAL_ERROR_EXCEPTION, "No xStart in line curve definition")
         CALL CheckModelInput("Line without xEnd", outerBoundaryText(none,                                &
                 [CHARACTER(LEN=LL) :: "\begin{END_POINTS_LINE}", "name = a", "xStart = [0.0,0.0,0.0]",    &
                                       "\end{END_POINTS_LINE}"]),                                          &
                 FATAL_ERROR_EXCEPTION, "No xEnd in line curve definition")
!
!        -------------
!        Circular arcs
!        -------------
!
         CALL CheckModelInput("Circular arc without name", outerBoundaryText(none, &
                 circularArcText([CHARACTER(LEN=LL) :: "", "units = degrees", "center = [0.0,0.0,0.0]", "radius = 1.0", &
                                  "start angle = 0.0", "end angle = 360.0"])),                       &
                 WARNING_ERROR_EXCEPTION, "No name found in circular arc curve definition")
         CALL CheckModelInput("Circular arc without start angle", outerBoundaryText(none, &
                 circularArcText([CHARACTER(LEN=LL) :: "name = arc", "units = degrees", "center = [0.0,0.0,0.0]", "radius = 1.0", &
                                  "", "end angle = 360.0"])),                                                  &
                 FATAL_ERROR_EXCEPTION, "No start angle in circular arc curve definition")
         CALL CheckModelInput("Circular arc without end angle", outerBoundaryText(none, &
                 circularArcText([CHARACTER(LEN=LL) :: "name = arc", "units = degrees", "center = [0.0,0.0,0.0]", "radius = 1.0", &
                                  "start angle = 0.0", ""])),                                                  &
                 FATAL_ERROR_EXCEPTION, "No end angle in circular arc curve definition")
         CALL CheckModelInput("Circular arc without radius", outerBoundaryText(none, &
                 circularArcText([CHARACTER(LEN=LL) :: "name = arc", "units = degrees", "center = [0.0,0.0,0.0]", "", &
                                  "start angle = 0.0", "end angle = 360.0"])),                     &
                 FATAL_ERROR_EXCEPTION, "No radius in circular arc curve definition")
         CALL CheckModelInput("Circular arc without center", outerBoundaryText(none, &
                 circularArcText([CHARACTER(LEN=LL) :: "name = arc", "units = degrees", "", "radius = 1.0", &
                                  "start angle = 0.0", "end angle = 360.0"])),           &
                 FATAL_ERROR_EXCEPTION, "No center in circular arc curve definition")
!
!        -------------
!        Elliptic arcs
!        -------------
!
         CALL CheckModelInput("Elliptic arc without name", outerBoundaryText(none, &
                 ellipticArcText([CHARACTER(LEN=LL) :: "", "units = degrees", "center = [0.0,0.0,0.0]", "xRadius = 2.0", &
                                  "yRadius = 1.0", "start angle = 0.0", "end angle = 360.0",        &
                                  "rotation = 0.0"])),                                              &
                 WARNING_ERROR_EXCEPTION, "No name found in elliptic arc curve definition")
         CALL CheckModelInput("Elliptic arc without rotation", outerBoundaryText(none, &
                 ellipticArcText([CHARACTER(LEN=LL) :: "name = arc", "units = degrees", "center = [0.0,0.0,0.0]", "xRadius = 2.0", &
                                  "yRadius = 1.0", "start angle = 0.0", "end angle = 360.0", ""])),           &
                 WARNING_ERROR_EXCEPTION, "No rotation found in elliptic arc curve definition")
         CALL CheckModelInput("Elliptic arc without start angle", outerBoundaryText(none, &
                 ellipticArcText([CHARACTER(LEN=LL) :: "name = arc", "units = degrees", "center = [0.0,0.0,0.0]", "xRadius = 2.0", &
                                  "yRadius = 1.0", "", "end angle = 360.0", "rotation = 0.0"])),              &
                 FATAL_ERROR_EXCEPTION, "No start angle in elliptic arc curve definition")
         CALL CheckModelInput("Elliptic arc without end angle", outerBoundaryText(none, &
                 ellipticArcText([CHARACTER(LEN=LL) :: "name = arc", "units = degrees", "center = [0.0,0.0,0.0]", "xRadius = 2.0", &
                                  "yRadius = 1.0", "start angle = 0.0", "", "rotation = 0.0"])),              &
                 FATAL_ERROR_EXCEPTION, "No end angle in elliptic arc curve definition")
         CALL CheckModelInput("Elliptic arc without xRadius", outerBoundaryText(none, &
                 ellipticArcText([CHARACTER(LEN=LL) :: "name = arc", "units = degrees", "center = [0.0,0.0,0.0]", "", &
                                  "yRadius = 1.0", "start angle = 0.0", "end angle = 360.0",     &
                                  "rotation = 0.0"])),                                           &
                 FATAL_ERROR_EXCEPTION, "No xRadius in elliptic arc curve definition")
         CALL CheckModelInput("Elliptic arc without yRadius", outerBoundaryText(none, &
                 ellipticArcText([CHARACTER(LEN=LL) :: "name = arc", "units = degrees", "center = [0.0,0.0,0.0]", "xRadius = 2.0", &
                                  "", "start angle = 0.0", "end angle = 360.0", "rotation = 0.0"])),          &
                 FATAL_ERROR_EXCEPTION, "No yRadius in elliptic arc curve definition")
         CALL CheckModelInput("Elliptic arc without center", outerBoundaryText(none, &
                 ellipticArcText([CHARACTER(LEN=LL) :: "name = arc", "units = degrees", "", "xRadius = 2.0", "yRadius = 1.0", &
                                  "start angle = 0.0", "end angle = 360.0", "rotation = 0.0"])),         &
                 FATAL_ERROR_EXCEPTION, "No center in elliptic arc curve definition")

      END SUBROUTINE CurveInputErrorTests
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION circularArcText( keys ) RESULT(text)
!
!     -------------------------------------------------------------
!     Empty entries of keys are dropped, to leave a key out of the
!     block. (An empty line in a block is not allowed.)
!     -------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=*)               :: keys(:)
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)
         text = [CHARACTER(LEN=LL) :: "\begin{CIRCULAR_ARC}", nonEmpty(keys), "\end{CIRCULAR_ARC}"]
      END FUNCTION circularArcText
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION ellipticArcText( keys ) RESULT(text)
         IMPLICIT NONE
         CHARACTER(LEN=*)               :: keys(:)
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)
         text = [CHARACTER(LEN=LL) :: "\begin{ELLIPTIC_ARC}", nonEmpty(keys), "\end{ELLIPTIC_ARC}"]
      END FUNCTION ellipticArcText
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION nonEmpty( keys ) RESULT(text)
         IMPLICIT NONE
         CHARACTER(LEN=*)               :: keys(:)
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)
         INTEGER                        :: k
         text = [CHARACTER(LEN=LL) ::]
         DO k = 1, SIZE(keys)
            IF ( LEN_TRIM(keys(k)) > 0 )     text = [CHARACTER(LEN=LL) :: text, keys(k)]
         END DO
      END FUNCTION nonEmpty
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SymmetryInputTests(optData)
!
!     ---------------------------------------------------------------
!     Symmetry curves (named ":symmetry") must be straight and lie on
!     one line. Each violation is a warning and the check fails.
!     ---------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused
         CHARACTER(LEN=LL), PARAMETER :: none(0) = [CHARACTER(LEN=LL) ::]
         TYPE (SMModel), POINTER :: model
!
!        -----------------------------------------------------
!        Square [-1,1]x[0,2] with the bottom side split in two:
!        both halves are on y = 0
!        -----------------------------------------------------
!
         CALL ClearExceptions
         CALL BuildModelFromText(outerBoundaryText(none,                                                  &
                 [lineText(":symmetry", "[-1.0,0.0,0.0]", "[0.0,0.0,0.0]"),                               &
                  lineText(":symmetry", "[0.0,0.0,0.0]" , "[1.0,0.0,0.0]"),                               &
                  lineText("right"    , "[1.0,0.0,0.0]" , "[1.0,2.0,0.0]"),                               &
                  lineText("top"      , "[1.0,2.0,0.0]" , "[-1.0,2.0,0.0]"),                              &
                  lineText("left"     , "[-1.0,2.0,0.0]", "[-1.0,0.0,0.0]")]), model)
         CALL FTAssert(test = allSymmetryCurvesAreColinear(model), msg = "Colinear symmetry curves")
         CALL FTAssert(test = .NOT.catch(), msg = "Colinear symmetry curves raise nothing")
         CALL releaseModel(model)
!
!        ------------------------------------------
!        The bottom and the left side are symmetry
!        curves, but are not on one line
!        ------------------------------------------
!
         CALL BuildModelFromText(outerBoundaryText(none,                                                  &
                 [lineText(":symmetry", "[-1.0,0.0,0.0]", "[1.0,0.0,0.0]"),                               &
                  lineText("right"    , "[1.0,0.0,0.0]" , "[1.0,2.0,0.0]"),                               &
                  lineText("top"      , "[1.0,2.0,0.0]" , "[-1.0,2.0,0.0]"),                              &
                  lineText(":symmetry", "[-1.0,2.0,0.0]", "[-1.0,0.0,0.0]")]), model)
         CALL FTAssert(test = .NOT.allSymmetryCurvesAreColinear(model), msg = "Symmetry curves not colinear")
         CALL FTAssert(test = raised(WARNING_ERROR_EXCEPTION, "are not colinear"), &
                       msg  = "Not colinear warning")
         CALL releaseModel(model)
         CALL ClearExceptions
!
!        -------------------------------------
!        A curved symmetry curve: a half circle
!        closed by a straight line
!        -------------------------------------
!
         CALL BuildModelFromText(outerBoundaryText(none,                                                     &
                 [lineText("bottom", "[-1.0,0.0,0.0]", "[1.0,0.0,0.0]"),                                     &
                  [CHARACTER(LEN=LL) :: "\begin{PARAMETRIC_EQUATION_CURVE}", "name = :symmetry",             &
                                        "xEqn = x(t) = cos(pi*t)", "yEqn = y(t) = sin(pi*t)",                &
                                        "zEqn = z(t) = 0.0", "\end{PARAMETRIC_EQUATION_CURVE}"]]), model)
         CALL FTAssert(test = .NOT.allSymmetryCurvesAreColinear(model), msg = "Curved symmetry curve")
         CALL FTAssert(test = raised(WARNING_ERROR_EXCEPTION, "Symmetry curve   2 is not straight"), &
                       msg  = "Not straight warning")
         CALL releaseModel(model)
         CALL ClearExceptions

      END SUBROUTINE SymmetryInputTests

      END Module ControlInputTestsModule

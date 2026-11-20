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
!      ProjectInputTests.f90
!      Created: 2026-11-20
!      Tests of control files that are read correctly but describe a
!      project that cannot be built: missing blocks, overlapping
!      boundary curves, sizes the quadtree cannot reach, too short
!      splines and missing data files. Each case checks that the
!      expected exception is raised with the expected message.
!      Also checks how interface curves are assigned material IDs.
!////////////////////////////////////////////////////////////////////////
      Module ProjectInputTestsModule
      USE SMConstants
      USE ProgramGlobals           , ONLY : A_OK_ERROR_CODE
      USE FTAssertions
      USE FTObjectClass
      USE FTValueDictionaryClass
      USE FTLinkedListIteratorClass
      USE SharedExceptionManagerModule
      USE ErrorTypesModule
      USE ControlInputTestsModule  , ONLY : ImportControlText, raised, ClearExceptions, CheckModelInput
      USE SMChainedCurveClass
      USE SMMeshObjectsModule
      USE MeshProjectClass
      USE MeshGenerationMethods    , ONLY : GenerateQuadMesh
      IMPLICIT NONE

      INTEGER, PARAMETER, PRIVATE :: LL = 80 ! Length of a control file line in these tests
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION runParametersText( meshFileFormat ) RESULT(text)
         IMPLICIT NONE
         CHARACTER(LEN=*)               :: meshFileFormat
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)
         text = [CHARACTER(LEN=LL) :: "\begin{RUN_PARAMETERS}",                     &
                                      "mesh file name   = ProjectInputTest.mesh",   &
                                      "plot file name   = ProjectInputTest.tec",    &
                                      "stats file name  = none",                    &
                                      "mesh file format = " // meshFileFormat,      &
                                      "polynomial order = 4",                       &
                                      "plot file format = skeleton",                &
                                      "\end{RUN_PARAMETERS}"]
      END FUNCTION runParametersText
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION backgroundGridText( ) RESULT(text)
         IMPLICIT NONE
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)
         text = [CHARACTER(LEN=LL) :: "\begin{BACKGROUND_GRID}", "background grid size = [0.75,0.75,0.0]", &
                                      "\end{BACKGROUND_GRID}"]
      END FUNCTION backgroundGridText
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION controlInputText( blocks ) RESULT(text)
         IMPLICIT NONE
         CHARACTER(LEN=*)               :: blocks(:)
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)
         text = [CHARACTER(LEN=LL) :: "\begin{CONTROL_INPUT}", blocks,                               &
                                      "\begin{SPRING_SMOOTHER}", "smoothing = ON",                   &
                                      "smoothing type = LinearAndCrossBarSpring",                    &
                                      "number of iterations = 25", "\end{SPRING_SMOOTHER}",          &
                                      "\end{CONTROL_INPUT}"]
      END FUNCTION controlInputText
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION circleText( name, center, radius ) RESULT(text)
         IMPLICIT NONE
         CHARACTER(LEN=*)               :: name, center, radius
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)
         text = [CHARACTER(LEN=LL) :: "\begin{CIRCULAR_ARC}", "name = " // name, "units = degrees", &
                                      "center = " // center, "radius = " // radius,                 &
                                      "start angle = 0.0", "end angle = 360.0", "\end{CIRCULAR_ARC}"]
      END FUNCTION circleText
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION chainText( name, curve, materialName ) RESULT(text)
         IMPLICIT NONE
         CHARACTER(LEN=*)               :: name, curve(:)
         CHARACTER(LEN=*), OPTIONAL     :: materialName
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)
         text = [CHARACTER(LEN=LL) :: "\begin{CHAIN}", "name = " // name]
         IF ( PRESENT(materialName) )     text = [CHARACTER(LEN=LL) :: text, "material name = " // materialName]
         text = [CHARACTER(LEN=LL) :: text, curve, "\end{CHAIN}"]
      END FUNCTION chainText
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION modelText( innerCurves, interfaceCurves, otherBlocks ) RESULT(text)
!
!     ---------------------------------------------------------------
!     A model with the circle of radius 4 at the origin as the outer
!     boundary, and the given inner and interface boundary chains.
!     Empty lists leave the block out.
!     ---------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=*)               :: innerCurves(:), interfaceCurves(:), otherBlocks(:)
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)

         text = [CHARACTER(LEN=LL) :: "\begin{MODEL}", otherBlocks, "\begin{OUTER_BOUNDARY}", &
                                      circleText("outer", "[0.0,0.0,0.0]", "4.0"), "\end{OUTER_BOUNDARY}"]
         IF ( SIZE(innerCurves) > 0 )     &
            text = [CHARACTER(LEN=LL) :: text, "\begin{INNER_BOUNDARIES}", innerCurves, "\end{INNER_BOUNDARIES}"]
         IF ( SIZE(interfaceCurves) > 0 ) &
            text = [CHARACTER(LEN=LL) :: text, "\begin{INTERFACE_BOUNDARIES}", interfaceCurves, &
                                           "\end{INTERFACE_BOUNDARIES}"]
         text = [CHARACTER(LEN=LL) :: text, "\end{MODEL}"]
      END FUNCTION modelText
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE BuildProjectFromText( text, controlDict, project )
         IMPLICIT NONE
         CHARACTER(LEN=*)                  :: text(:)
         TYPE (FTValueDictionary), POINTER :: controlDict
         CLASS(MeshProject)      , POINTER :: project

         CALL ImportControlText([CHARACTER(LEN=LL) :: text, "\end{FILE}"], controlDict)
         ALLOCATE(project)
         CALL project % initWithDictionary(controlDict)

      END SUBROUTINE BuildProjectFromText
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckProjectInput( label, text, exceptionName, expectedMessage )
!
!     -------------------------------------------------------------
!     Build the project and check that the exception is raised. The
!     warnings that come with an incomplete control file are ignored.
!     -------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=*) :: label, exceptionName, expectedMessage
         CHARACTER(LEN=*) :: text(:)

         TYPE (FTValueDictionary), POINTER :: controlDict
         CLASS(MeshProject)      , POINTER :: project

         CALL ClearExceptions
         CALL BuildProjectFromText(text, controlDict, project)
         CALL FTAssert(test = raised(exceptionName, expectedMessage), &
                       msg  = label // ": '" // expectedMessage // "'")
         CALL releaseMeshProject(project)
         CALL releaseFTValueDictionary(controlDict)
         CALL ClearExceptions

      END SUBROUTINE CheckProjectInput
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ProjectInputErrorTests(optData)
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused
         CHARACTER(LEN=LL), PARAMETER :: none(0) = [CHARACTER(LEN=LL) ::]
         CHARACTER(LEN=LL), ALLOCATABLE :: model(:)
!
!        --------------
!        Missing blocks
!        --------------
!
         model = modelText(none, none, none)
         CALL CheckProjectInput("No CONTROL_INPUT block", model, FATAL_ERROR_EXCEPTION, &
                                "CONTROL_INPUT block is missing from control file")
         CALL CheckProjectInput("No BACKGROUND_GRID block",                                         &
                                [controlInputText(runParametersText("ISM")), model],                &
                                FATAL_ERROR_EXCEPTION, "Control file needs a BACKGROUND_GRID block")
         CALL CheckProjectInput("No RUN_PARAMETERS block",                                          &
                                [controlInputText(backgroundGridText()), model],                    &
                                FATAL_ERROR_EXCEPTION, "Control file is missing the block: RUN_PARAMETERS")
         CALL CheckProjectInput("ISM-MM without a model",                                           &
                                controlInputText([runParametersText("ISM-MM"), backgroundGridText()]), &
                                FATAL_ERROR_EXCEPTION, "A model is required to define materials")
!
!        --------------------------------------------------------
!        Overlapping boundary curves stop the project being built
!        --------------------------------------------------------
!
         CALL CheckProjectInput("Inner curves overlap",                                             &
                 [controlInputText([runParametersText("ISM"), backgroundGridText()]),               &
                  modelText([chainText("left" , circleText("left" , "[-0.5,0.0,0.0]", "1.0")),      &
                             chainText("right", circleText("right", "[0.5,0.0,0.0]" , "1.0"))],     &
                            none, none)],                                                           &
                 FATAL_ERROR_EXCEPTION, "Interior curves")
         CALL CheckProjectInput("Inner curves cross between their points",                         &
                 [controlInputText([runParametersText("ISM"), backgroundGridText()]),               &
                  modelText([chainText("big"  , circleText("big"  , "[0.0,0.0,0.0]", "1.0")),       &
                             chainText("small", circleText("small", "[0.9,0.9,0.0]", "0.5"))],      &
                            none, none)],                                                           &
                 FATAL_ERROR_EXCEPTION, "Interior curves")
         CALL CheckProjectInput("Inner curve inside another",                                       &
                 [controlInputText([runParametersText("ISM"), backgroundGridText()]),               &
                  modelText([chainText("big"  , circleText("big"  , "[0.0,0.0,0.0]", "1.5")),       &
                             chainText("small", circleText("small", "[0.2,0.0,0.0]", "0.5"))],      &
                            none, none)],                                                           &
                 FATAL_ERROR_EXCEPTION, "is inside of interior curve")
         CALL CheckProjectInput("Inner curve crosses the outer boundary inside its box",            &
                 [controlInputText([runParametersText("ISM"), backgroundGridText()]),               &
                  modelText([chainText("corner", circleText("corner", "[2.9,2.9,0.0]", "0.5"))],     &
                            none, none)],                                                           &
                 FATAL_ERROR_EXCEPTION, "overlaps with exterior curve")
         CALL CheckProjectInput("Inner curve outside the outer boundary",                           &
                 [controlInputText([runParametersText("ISM"), backgroundGridText()]),               &
                  modelText([chainText("away", circleText("away", "[3.6,3.6,0.0]", "0.5"))],         &
                            none, none)],                                                           &
                 FATAL_ERROR_EXCEPTION, "is outside of the exterior curve")
         CALL CheckProjectInput("Inner curve overlaps the outer boundary",                          &
                 [controlInputText([runParametersText("ISM"), backgroundGridText()]),               &
                  modelText([chainText("crossing", circleText("crossing", "[3.5,0.0,0.0]", "1.0"))], &
                            none, none)],                                                           &
                 FATAL_ERROR_EXCEPTION, "overlaps with exterior curve")
!
!        -------------------------------------------------------
!        A curve that needs more subdivision levels than allowed
!        -------------------------------------------------------
!
         CALL CheckProjectInput("Too many subdivision levels",                                      &
                 [controlInputText([runParametersText("ISM"), backgroundGridText()]),               &
                  modelText([chainText("tiny", circleText("tiny", "[0.0,0.0,0.0]", "1.0e-3"))],      &
                            none, none)],                                                           &
                 FATAL_ERROR_EXCEPTION, "To override, rerun with the command line flag '-sLimit")
!
!        ---------------------------
!        Model data that is not usable
!        ---------------------------
!
         CALL CheckSplineTooShort()
         CALL CheckModelInput("Missing topography file",                                            &
                 [CHARACTER(LEN=LL) :: modelText(none, none, [CHARACTER(LEN=LL) :: "\begin{TOPOGRAPHY}",                  &
                                        "data file = ProjectInputTestNoSuchFile.txt",              &
                                        "\end{TOPOGRAPHY}"]), "\end{FILE}"], &
                 FATAL_ERROR_EXCEPTION, "Unable to open the topography data file")

      END SUBROUTINE ProjectInputErrorTests
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckSplineTooShort()
!
!     ------------------------------------------------------------
!     A spline needs at least four points, whether the points are
!     in the control file or in a separate data file
!     ------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=*), PARAMETER    :: fileName = "ProjectInputTestSpline.txt"
         CHARACTER(LEN=LL), PARAMETER   :: none(0) = [CHARACTER(LEN=LL) ::]
         CHARACTER(LEN=LL), ALLOCATABLE :: spline(:)
         INTEGER                        :: fUnit
         LOGICAL                        :: isOpen

         spline = [CHARACTER(LEN=LL) :: "\begin{SPLINE_CURVE}", "name = spline", "nKnots = 3", &
                                        "\begin{SPLINE_DATA}", "0.0  1.0  0.0  0.0",            &
                                        "0.5 -1.0  0.0  0.0", "1.0  1.0  0.0  0.0",             &
                                        "\end{SPLINE_DATA}", "\end{SPLINE_CURVE}"]
         CALL CheckModelInput("Spline with three knots", &
                              [CHARACTER(LEN=LL) :: modelText([chainText("spline", spline)], none, none), "\end{FILE}"], &
                              FTFatalErrorException, "A spline curve must have at least 4 points")

         OPEN(NEWUNIT = fUnit, FILE = fileName, STATUS = "REPLACE", ACTION = "WRITE")
         WRITE(fUnit,'(A)') "3", "0.0  1.0  0.0  0.0", "0.5 -1.0  0.0  0.0", "1.0  1.0  0.0  0.0"
         CLOSE(fUnit)
         spline = [CHARACTER(LEN=LL) :: "\begin{SPLINE_CURVE}", "name = spline", "file = " // fileName, &
                                        "\end{SPLINE_CURVE}"]
         CALL CheckModelInput("Spline file with three knots", &
                              [CHARACTER(LEN=LL) :: modelText([chainText("spline", spline)], none, none), "\end{FILE}"], &
                              FTFatalErrorException, "A spline curve must have at least 4 points")
         INQUIRE(FILE = fileName, OPENED = isOpen)
         CALL FTAssert(test = .NOT.isOpen, msg = "The spline file is closed after the error")
         OPEN(NEWUNIT = fUnit, FILE = fileName)
         CLOSE(fUnit, STATUS = "DELETE")

      END SUBROUTINE CheckSplineTooShort
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MaterialNameTests(optData)
!
!     --------------------------------------------------------------
!     Interfaces with the same material name share a material ID,
!     and an interface of the background material gets the
!     background ID, 0. Without a background material name the
!     default, "base", is used with a warning.
!     --------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused
         CHARACTER(LEN=LL), PARAMETER :: none(0) = [CHARACTER(LEN=LL) ::]

         TYPE (FTValueDictionary)   , POINTER :: controlDict
         CLASS(MeshProject)         , POINTER :: project
         CLASS(SMChainedCurve)      , POINTER :: chain
         TYPE (SMElement)           , POINTER :: e
         CLASS(FTObject)            , POINTER :: obj
         TYPE (FTLinkedListIterator)          :: iterator
         INTEGER                              :: errorCode, k
         INTEGER                              :: materialIDs(3)
         INTEGER                              :: elementCount(0:1)
         LOGICAL                              :: otherIDs

         CALL ClearExceptions
         CALL BuildProjectFromText(                                                                &
                 [controlInputText([runParametersText("ISM-MM"), backgroundGridText()]),           &
                  modelText(none,                                                                   &
                            [chainText("left"  , circleText("left"  , "[-2.0,0.0,0.0]", "0.75"), "steel"), &
                             chainText("right" , circleText("right" , "[2.0,0.0,0.0]" , "0.75"), "steel"), &
                             chainText("center", circleText("center", "[0.0,0.0,0.0]" , "0.75"), "water")], &
                            [CHARACTER(LEN=LL) :: "\begin{MATERIALS}", "background material name = water", &
                                                  "\end{MATERIALS}"])],                             &
                 controlDict, project)
         CALL FTAssert(test = .NOT.catch(), msg = "Material model raises nothing")

         errorCode = A_OK_ERROR_CODE
         CALL GenerateQuadMesh(project, errorCode)
         CALL FTAssertEqual(expectedValue = A_OK_ERROR_CODE, actualValue = errorCode, &
                            msg = "Material model mesh generation")

         k = 0
         CALL project % model % interfaceBoundariesIterator % setToStart()
         DO WHILE ( .NOT.project % model % interfaceBoundariesIterator % isAtEnd() )
            obj => project % model % interfaceBoundariesIterator % object()
            CALL castToSMChainedCurve(obj, chain)
            k = k + 1
            materialIDs(k) = chain % materialID
            CALL project % model % interfaceBoundariesIterator % moveToNext()
         END DO
         CALL FTAssertEqual(expectedValue = 3, actualValue = k, msg = "Three interfaces")
         CALL FTAssertEqual(expectedValue = 1, actualValue = materialIDs(1), msg = "First steel interface")
         CALL FTAssertEqual(expectedValue = 1, actualValue = materialIDs(2), msg = "Second steel interface shares the ID")
         CALL FTAssertEqual(expectedValue = 0, actualValue = materialIDs(3), msg = "Background material interface")
         CALL FTAssertEqual(expectedValue = 1, actualValue = UBOUND(project % mesh % materialNameForID,1), &
                            msg = "Two materials")
         CALL FTAssert(test = project % mesh % materialNameForID(0) == "water", msg = "Material 0 is water")
         CALL FTAssert(test = project % mesh % materialNameForID(1) == "steel", msg = "Material 1 is steel")

         elementCount = 0
         otherIDs     = .FALSE.
         CALL iterator % initWithFTLinkedList(project % mesh % elements)
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMElement(obj, e)
            IF ( e % materialID == 0 .OR. e % materialID == 1 )     THEN
               elementCount(e % materialID) = elementCount(e % materialID) + 1
            ELSE
               otherIDs = .TRUE.
            END IF
            CALL iterator % moveToNext()
         END DO
         CALL FTAssert(test = .NOT.otherIDs, msg = "Elements are steel or water")
         CALL FTAssert(test = elementCount(0) > 0 .AND. elementCount(1) > 0, msg = "Both materials have elements")

         CALL releaseMeshProject(project)
         CALL releaseFTValueDictionary(controlDict)
!
!        ---------------------------------------------------------------
!        Interfaces without ISM-MM output: the background material is
!        still named, "base", since interfaces are matched against it
!        ---------------------------------------------------------------
!
         CALL ClearExceptions
         CALL BuildProjectFromText(                                                                &
                 [controlInputText([runParametersText("ISM"), backgroundGridText()]),              &
                  modelText(none, [chainText("left", circleText("left", "[-2.0,0.0,0.0]", "0.75"))], none)], &
                 controlDict, project)
         CALL FTAssert(test = project % backgroundMaterialName == "base", &
                       msg  = "Background material without ISM-MM")
         CALL releaseMeshProject(project)
         CALL releaseFTValueDictionary(controlDict)
!
!        ----------------------------------------
!        No background material name: use "base"
!        ----------------------------------------
!
         CALL ClearExceptions
         CALL BuildProjectFromText(                                                                &
                 [controlInputText([runParametersText("ISM-MM"), backgroundGridText()]),           &
                  modelText(none, none, [CHARACTER(LEN=LL) :: "\begin{MATERIALS}", "\end{MATERIALS}"])], &
                 controlDict, project)
         CALL FTAssert(test = raised(WARNING_ERROR_EXCEPTION, "Background material name not found"), &
                       msg  = "No background material name warns")
         CALL FTAssert(test = project % backgroundMaterialName == "base", msg = "Default background material")
         CALL releaseMeshProject(project)
         CALL releaseFTValueDictionary(controlDict)
         CALL ClearExceptions

      END SUBROUTINE MaterialNameTests

      END Module ProjectInputTestsModule

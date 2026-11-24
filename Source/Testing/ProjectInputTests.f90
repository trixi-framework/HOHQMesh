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
      USE ProgramGlobals           , ONLY : A_OK_ERROR_CODE, BASIC_MESH_FORMAT, BASIC_PLUS_EDGES_FORMAT, ISM
      USE FTAssertions
      USE FTObjectClass
      USE FTValueDictionaryClass
      USE FTLinkedListIteratorClass
      USE SharedExceptionManagerModule
      USE ErrorTypesModule
      USE ControlInputTestsModule  , ONLY : ImportControlText, raised, raisedAll, ClearExceptions, CheckModelInput
      USE SMChainedCurveClass
      USE SMMeshObjectsModule
      USE MeshProjectClass
      USE MeshGenerationMethods    , ONLY : GenerateQuadMesh
      USE MeshController3D         , ONLY : Check3DMeshParametersIntegrity
      USE SMTopographyFromFileClass, ONLY : SMTopographyFromFile, releaseDFTopography
      USE ProgramGlobals           , ONLY : BBOX_TOP, BBOX_LEFT, BBOX_BOTTOM, BBOX_RIGHT
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
         CALL CheckModelInput("Unknown curve type",                                                 &
                 [CHARACTER(LEN=LL) :: modelText([chainText("odd", [CHARACTER(LEN=LL) ::            &
                                        "\begin{NO_SUCH_CURVE}", "name = odd",                     &
                                        "\end{NO_SUCH_CURVE}"])], none, none), "\end{FILE}"],     &
                 FATAL_ERROR_EXCEPTION, "Unimplemented curve type NO_SUCH_CURVE")
         CALL CheckModelInput("Parametric equation without an equation",                            &
                 [CHARACTER(LEN=LL) :: modelText([chainText("p", [CHARACTER(LEN=LL) ::              &
                                        "\begin{PARAMETRIC_EQUATION}", "name = p",                 &
                                        "\end{PARAMETRIC_EQUATION}"])], none, none), "\end{FILE}"], &
                 FATAL_ERROR_EXCEPTION, "PARAMETRIC_EQUATION has no eqn key")
         CALL CheckModelInput("Missing spline file",                                                &
                 [CHARACTER(LEN=LL) :: modelText([chainText("s", [CHARACTER(LEN=LL) ::              &
                                        "\begin{SPLINE_CURVE}", "name = s",                        &
                                        "file = ProjectInputTestNoSuchSpline.txt",                 &
                                        "\end{SPLINE_CURVE}"])], none, none), "\end{FILE}"],      &
                 FATAL_ERROR_EXCEPTION, "Spline curve file not found")
         CALL CheckModelInput("Spline without data",                                                &
                 [CHARACTER(LEN=LL) :: modelText([chainText("s", [CHARACTER(LEN=LL) ::              &
                                        "\begin{SPLINE_CURVE}", "name = s", "nKnots = 4",          &
                                        "\end{SPLINE_CURVE}"])], none, none), "\end{FILE}"],      &
                 FATAL_ERROR_EXCEPTION, "Spline does not appear to contain any data")
!
!        ----------------------------------------------------------------
!        The background grid can only be made from an outer boundary, and
!        a rotation needs a direction
!        ----------------------------------------------------------------
!
         CALL CheckProjectInput("Background grid size without an outer boundary",                   &
                 [controlInputText([runParametersText("ISM"), backgroundGridText()]),               &
                  [CHARACTER(LEN=LL) :: "\begin{MODEL}", "\begin{INNER_BOUNDARIES}",              &
                                        chainText("hole", circleText("hole", "[0.0,0.0,0.0]", "1.0")), &
                                        "\end{INNER_BOUNDARIES}", "\end{MODEL}"]],                &
                 FATAL_ERROR_EXCEPTION, "To automatically create background grid, the model needs an outer boundary")
         CALL CheckProjectInput("Rotation with a zero direction",                                   &
                 [controlInputText([runParametersText("ISM"), backgroundGridText(),                 &
                                    [CHARACTER(LEN=LL) :: "\begin{ROTATION_TRANSFORM}",           &
                                                          "translation = [0.0,0.0,0.0]",           &
                                                          "direction   = [0.0,0.0,0.0]",           &
                                                          "\end{ROTATION_TRANSFORM}"]]),           &
                  modelText(none, none, none)],                                                     &
                 FATAL_ERROR_EXCEPTION, "must not be the zero vector")
!
!        ---------------------------------------------------------------
!        An optimized chain needs fewer continuity constraints than the
!        polynomial order (runParametersText sets polynomial order 4)
!        ---------------------------------------------------------------
!
         CALL CheckProjectInput("Continuity not less than the polynomial order",                    &
                 [controlInputText([runParametersText("ISM"), backgroundGridText()]),               &
                  modelText([chainText("smooth", [CHARACTER(LEN=LL) :: "optimize = H1Norm",        &
                                       "continuity = 4", "tolerance = 1.0e-3",                     &
                                       circleText("smooth", "[0.0,0.0,0.0]", "1.0")])], none, none)], &
                 FATAL_ERROR_EXCEPTION, "Continuity = 4 of chain smooth must be from 0 to 3")

      END SUBROUTINE ProjectInputErrorTests
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ProjectOptionTests(optData)
!
!     ---------------------------------------------------------------
!     Options that are read without an error: the less common mesh
!     and plot file formats, unknown formats (they fall back to the
!     defaults ISM and sem), and sharp refinement regions
!     ---------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused
         CHARACTER(LEN=LL), PARAMETER :: none(0) = [CHARACTER(LEN=LL) ::]

         TYPE (FTValueDictionary), POINTER :: controlDict
         CLASS(MeshProject)      , POINTER :: project

         CALL CheckFileFormats("Basic"         , "skeleton"    , BASIC_MESH_FORMAT      , SKELETON_FORMAT)
         CALL CheckFileFormats("BasicWithEdges", "sem"         , BASIC_PLUS_EDGES_FORMAT, SEM_FORMAT)
         CALL CheckFileFormats("NoSuchFormat"  , "NoSuchFormat", ISM                    , SEM_FORMAT)

         CALL ClearExceptions
         CALL BuildProjectFromText([controlInputText([runParametersText("ISM"), backgroundGridText(), &
                                    [CHARACTER(LEN=LL) :: "\begin{REFINEMENT_REGIONS}",           &
                                       "\begin{REFINEMENT_CENTER}", "type = sharp",                &
                                       "x0 = [1.0,1.0,0.0]", "h = 0.2", "w = 0.5",                  &
                                       "\end{REFINEMENT_CENTER}",                                  &
                                       "\begin{REFINEMENT_LINE}", "type = sharp",                  &
                                       "x0 = [-2.0,-1.0,0.0]", "x1 = [2.0,-1.0,0.0]",               &
                                       "h = 0.2", "w = 0.3", "\end{REFINEMENT_LINE}",              &
                                       "\end{REFINEMENT_REGIONS}"]]),                              &
                                    modelText(none, none, none)], controlDict, project)
         CALL FTAssert(test = .NOT.catch(FATAL_ERROR_EXCEPTION), msg = "Sharp refinement regions are accepted")
         CALL releaseMeshProject(project)
         CALL releaseFTValueDictionary(controlDict)
         CALL ClearExceptions

      END SUBROUTINE ProjectOptionTests
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckFileFormats( meshFormat, plotFormat, expectedMeshFormat, expectedPlotFormat )
         IMPLICIT NONE
         CHARACTER(LEN=*) :: meshFormat, plotFormat
         INTEGER          :: expectedMeshFormat, expectedPlotFormat

         CHARACTER(LEN=LL), PARAMETER      :: none(0) = [CHARACTER(LEN=LL) ::]
         CHARACTER(LEN=LL), ALLOCATABLE    :: runParameters(:)
         TYPE (FTValueDictionary), POINTER :: controlDict
         CLASS(MeshProject)      , POINTER :: project

         runParameters    = runParametersText(meshFormat)
         runParameters(7) = "plot file format = " // plotFormat
         CALL ClearExceptions
         CALL BuildProjectFromText([controlInputText([runParameters, backgroundGridText()]), &
                                    modelText(none, none, none)], controlDict, project)
         CALL FTAssertEqual(expectedValue = expectedMeshFormat, actualValue = project % runParams % meshFileFormat, &
                            msg = "mesh file format = " // meshFormat)
         CALL FTAssertEqual(expectedValue = expectedPlotFormat, actualValue = project % runParams % plotFileFormat, &
                            msg = "plot file format = " // plotFormat)
         CALL releaseMeshProject(project)
         CALL releaseFTValueDictionary(controlDict)
         CALL ClearExceptions

      END SUBROUTINE CheckFileFormats
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE Generator3DInputTests(optData)
!
!     -------------------------------------------------------------
!     The blocks that generate a hex mesh: missing keys, more than
!     one generator, and a sweep without a model
!     -------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused

         CALL Check3DInput("Empty SIMPLE_EXTRUSION block",                                         &
                           [CHARACTER(LEN=LL) :: "\begin{SIMPLE_EXTRUSION}", "\end{SIMPLE_EXTRUSION}"], &
                           [CHARACTER(LEN=LL) :: "key direction not found in extrusion block",     &
                                                 "key height not found in extrusion block",        &
                                                 "key subdivisions not found in extrusion block",  &
                                                 "key start surface name not found in extrusion block", &
                                                 "key end surface name not found in extrusion block"])
         CALL Check3DInput("Empty SIMPLE_ROTATION block",                                          &
                           [CHARACTER(LEN=LL) :: "\begin{SIMPLE_ROTATION}", "\end{SIMPLE_ROTATION}"], &
                           [CHARACTER(LEN=LL) :: "key direction not found in rotation block",      &
                                                 "key rotation angle factor not found in rotation block", &
                                                 "key subdivisions not found in rotation block",   &
                                                 "key start surface name not found in rotation block", &
                                                 "key end surface name not found in rotation block"])
         CALL Check3DInput("Two 3D generators",                                                    &
                           [CHARACTER(LEN=LL) :: "\begin{SIMPLE_EXTRUSION}", "\end{SIMPLE_EXTRUSION}", &
                                                 "\begin{SIMPLE_ROTATION}" , "\end{SIMPLE_ROTATION}"],  &
                           [CHARACTER(LEN=LL) :: "Too many 3D mesh generators specified in control file"])
         CALL Check3DInput("Sweep without a model",                                                &
                           [CHARACTER(LEN=LL) :: "\begin{SWEEP_ALONG_CURVE}", "\end{SWEEP_ALONG_CURVE}"], &
                           [CHARACTER(LEN=LL) :: "A model block and sweep curve is required for sweeping"])

      END SUBROUTINE Generator3DInputTests
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE Check3DInput( label, blocks, expectedMessages )
         IMPLICIT NONE
         CHARACTER(LEN=*) :: label
         CHARACTER(LEN=*) :: blocks(:), expectedMessages(:)

         TYPE (FTValueDictionary), POINTER :: projectDict, controlDict, modelDict
         CLASS(FTObject)         , POINTER :: obj

         CALL ClearExceptions
         CALL ImportControlText([CHARACTER(LEN=LL) :: controlInputText(blocks), "\end{FILE}"], projectDict)
         obj         => projectDict % objectForKey(key = "CONTROL_INPUT")
         controlDict => valueDictionaryFromObject(obj)
         modelDict   => NULL()
         CALL Check3DMeshParametersIntegrity(controlDict, modelDict)
         CALL FTAssert(test = raisedAll(FATAL_ERROR_EXCEPTION, expectedMessages), msg = label)
         CALL releaseFTValueDictionary(projectDict)
         CALL ClearExceptions

      END SUBROUTINE Check3DInput
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE TopographyFromFileTests(optData)
!
!     ----------------------------------------------------------------
!     A topography from a data file of the plane z = x + y. Its box is
!     widened a little so rounding does not put points outside of it,
!     also when a side of the data is at zero. Points outside of the
!     box give a warning.
!     ----------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused

         TYPE(SMTopographyFromFile), POINTER :: topography
         REAL(KIND=RP)                       :: x(3), kappa

         CALL ClearExceptions
         CALL MakeTopography(x0 = 0.0_RP, y0 = -1.0_RP, topography = topography)
         CALL FTAssert(test = topography % box(BBOX_LEFT) < 0.0_RP .AND. topography % box(BBOX_TOP) > 0.0_RP, &
                       msg  = "Data box sides at zero are widened")
         x = topography % heightAt([0.5_RP, -0.25_RP])
         CALL FTAssert(test = ABS(x(3) - 0.25_RP) <= 1.0E-10_RP, msg = "Height of the plane inside the data")
         CALL FTAssert(test = .NOT.catch(), msg = "No warning inside the data")

!
!        The data are on [0,1]x[-1,0] and the box is [-0.01,1.01]x[-1.01,0.01]. Outside of
!        the box the height is the one at the nearest point of the box, here (1.01,-0.5)
!
         x = topography % heightAt([2.0_RP, -0.5_RP])
         CALL FTAssert(test = raised(WARNING_ERROR_EXCEPTION, "outside of"), msg = "Height outside of the data warns")
         CALL FTAssert(test = MAXVAL(ABS(x - [2.0_RP, -0.5_RP, 0.51_RP])) <= 1.0E-10_RP, &
                       msg  = "Height outside of the data is the height at the nearest point of the box")
         CALL ClearExceptions
         x = topography % heightAt([-1.0_RP, 3.0_RP])
         CALL FTAssert(test = MAXVAL(ABS(x - [-1.0_RP, 3.0_RP, 0.0_RP])) <= 1.0E-10_RP, &
                       msg  = "Height outside of the data, nearest point is a corner of the box")
         CALL ClearExceptions
         kappa = topography % principalCurvatureAt([2.0_RP, -0.5_RP])
         CALL FTAssert(test = kappa == 0.0_RP .AND. raised(WARNING_ERROR_EXCEPTION, "outside of data bounding box"), &
                       msg  = "Curvature outside of the data is zero and warns")
         CALL releaseDFTopography(topography)
         CALL ClearExceptions

         CALL MakeTopography(x0 = -1.0_RP, y0 = 0.0_RP, topography = topography)
         CALL FTAssert(test = topography % box(BBOX_RIGHT) > 0.0_RP .AND. topography % box(BBOX_BOTTOM) < 0.0_RP, &
                       msg  = "Data box sides at zero are widened, other sides")
         CALL releaseDFTopography(topography)
         CALL ClearExceptions

      END SUBROUTINE TopographyFromFileTests
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MakeTopography( x0, y0, topography )
!
!     --------------------------------------------------------------
!     Write the plane z = x + y on [x0,x0+1]x[y0,y0+1] with 4x4 nodes
!     to a data file and read it, with curvature sizing on
!     --------------------------------------------------------------
!
         IMPLICIT NONE
         REAL(KIND=RP)                       :: x0, y0
         TYPE(SMTopographyFromFile), POINTER :: topography

         CHARACTER(LEN=*), PARAMETER :: fileName = "ProjectInputTestTopography.txt"
         INTEGER                     :: fUnit, j, k

         OPEN(NEWUNIT = fUnit, FILE = fileName, STATUS = "REPLACE", ACTION = "WRITE")
         WRITE(fUnit,'(A)') "! Number of nodes in each direction"
         WRITE(fUnit,'(A)') "4 4"
         WRITE(fUnit,'(A)') "! x node values"
         WRITE(fUnit,'(ES24.16)') (x0 + j/3.0_RP, j = 0, 3)
         WRITE(fUnit,'(A)') "! y node values"
         WRITE(fUnit,'(ES24.16)') (y0 + k/3.0_RP, k = 0, 3)
         WRITE(fUnit,'(A)') "! z values"
         WRITE(fUnit,'(ES24.16)') ((x0 + j/3.0_RP + y0 + k/3.0_RP, j = 0, 3), k = 0, 3)
         CLOSE(fUnit)

         ALLOCATE(topography)
         CALL topography % initWithDataFile(fileName, .TRUE.)

         OPEN(NEWUNIT = fUnit, FILE = fileName)
         CLOSE(fUnit, STATUS = "DELETE")

      END SUBROUTINE MakeTopography
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

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
!      DebugOutputTests.f90
!      Created: 2026-11-19
!      Tests of the debugging output routines (printDescription and
!      friends). Each routine writes to a scratch file, which is read
!      back and compared with the object that was printed:
!        1. Sizer, refinement center, segmented boundary chains, a
!           segmented curve node and the segmented curve array
!        2. Model curves, a polynomial interpolant curve and
!           printChainForPlotting
!        3. QuadTreeGrid, PrintGridDiagnostics and a grid quad
!        4. The mesh: nodes, edges and elements
!        5. Equation and data file topographies
!      The scratch files are deleted when they are closed.
!////////////////////////////////////////////////////////////////////////
      Module DebugOutputTestsModule
      USE SMConstants
      USE FTAssertions
      USE FTObjectClass
      USE FTLinkedListClass
      USE FTValueDictionaryClass
      USE EncoderModule
      USE ProgramGlobals             , ONLY : A_OK_ERROR_CODE
      USE HOHQMeshModule             , ONLY : ReadControlFile
      USE MeshProjectClass
      USE MeshGenerationMethods      , ONLY : GenerateQuadMesh
      USE QuadTreeGridClass
      USE QuadTreeGridGeneratorModule, ONLY : GenerateConformingGridWithSizer
      USE SMMeshObjectsModule
      USE SMCurveClass
      USE SMChainedCurveClass
      USE SMSplineCurveClass         , ONLY : SMSplineCurve
      USE SMParametricEquationCurveClass, ONLY : SMParametricEquationCurve
      USE SMPolynomialInterpolantClass  , ONLY : SMPolynomialInterpolant
      USE ChainedSegmentedCurveClass
      USE FRSegmentedCurveClass
      USE SegmentedCurveArrayClass
      USE CurveConversionsModule     , ONLY : allocateAndInitSegmentedCurveFromSegmentedChain
      USE SizerControls              , ONLY : SizerCenterControl
      USE SMTopographyClass
      USE SMEquationTopographyClass  , ONLY : SMEquationTopography
      USE SMTopographyFromFileClass  , ONLY : SMTopographyFromFile
      IMPLICIT NONE

      INTEGER      , PARAMETER, PRIVATE :: DEBUG_LINE_LENGTH = 1024
      REAL(KIND=RP), PARAMETER, PRIVATE :: POSITION_TOLERANCE = 1.0E-9_RP
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DebugOutputTests(optData)
!
!     -------------------------------------------------------------
!     optData holds the directory of the benchmark control files,
!     ending with a "/"
!     -------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)

         CHARACTER(LEN=DEFAULT_FILE_PATH_LENGTH) :: controlDirectory
         TYPE (FTValueDictionary), POINTER       :: projectDict => NULL()
         CLASS(MeshProject)      , POINTER       :: project     => NULL()
         INTEGER                                 :: errorCode

         controlDirectory = ""
         CALL DECODE(enc = optData, strOut = controlDirectory)
!
!        ---------------------------------------------------------
!        AllFeatures has every curve type, inner boundaries and a
!        refinement center. Print the objects of the project before
!        and after the mesh is generated.
!        ---------------------------------------------------------
!
         CALL ReadProject(TRIM(controlDirectory) // "AllFeatures.control", projectDict, project)
         CALL CheckSizerOutput( project )
         CALL CheckSegmentedChainOutput( project )
         CALL CheckModelCurveOutput( project )
         CALL CheckPolynomialCurveOutput()

         errorCode = A_OK_ERROR_CODE
         CALL GenerateQuadMesh(project, errorCode)
         CALL FTAssertEqual(expectedValue = A_OK_ERROR_CODE, actualValue = errorCode, &
                            msg = "AllFeatures mesh generation for the debug output test")
         CALL CheckMeshOutput( project % mesh )
         CALL releaseMeshProject(project)
         CALL releaseFTValueDictionary(projectDict)
!
!        --------------------------------------------------------
!        The quadtree grid is released by the mesh generator, so
!        refine the grid of a second, unmeshed project
!        --------------------------------------------------------
!
         CALL ReadProject(TRIM(controlDirectory) // "AllFeatures.control", projectDict, project)
         CALL GenerateConformingGridWithSizer( project % grid, project % sizer )
         CALL CheckGridOutput( project % grid )
         CALL releaseMeshProject(project)
         CALL releaseFTValueDictionary(projectDict)
!
!        ------------
!        Topographies
!        ------------
!
         CALL ReadProject(TRIM(controlDirectory) // "SeaMount.control", projectDict, project)
         CALL CheckTopographyOutput( project, projectDict )
         CALL releaseMeshProject(project)
         CALL releaseFTValueDictionary(projectDict)

         CALL ReadProject(TRIM(controlDirectory) // "BottomFromFile.control", projectDict, project)
         CALL CheckTopographyOutput( project, projectDict )
         CALL releaseMeshProject(project)
         CALL releaseFTValueDictionary(projectDict)

      END SUBROUTINE DebugOutputTests
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReadProject( controlFileName, projectDict, project )
         IMPLICIT NONE
         CHARACTER(LEN=*)                  :: controlFileName
         TYPE (FTValueDictionary), POINTER :: projectDict
         CLASS(MeshProject)      , POINTER :: project

         CALL ReadControlFile(controlFileName, projectDict)
         ALLOCATE(project)
         CALL project % initWithDictionary( projectDict )

      END SUBROUTINE ReadProject
!
!////////////////////////////////////////////////////////////////////////
!
!     ------------------------------------------------------------
!     Scratch file handling: open a new scratch file, then read all
!     of its lines back and close (and so delete) it. The long record
!     length keeps compilers from wrapping list-directed output.
!     ------------------------------------------------------------
!
      SUBROUTINE OpenScratchFile( iUnit )
         IMPLICIT NONE
         INTEGER :: iUnit
         OPEN(NEWUNIT = iUnit, STATUS = "SCRATCH", FORM = "FORMATTED", &
              ACTION = "READWRITE", RECL = 4*DEBUG_LINE_LENGTH)
      END SUBROUTINE OpenScratchFile
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReadAndCloseScratchFile( iUnit, lines )
         IMPLICIT NONE
         INTEGER                                                    :: iUnit
         CHARACTER(LEN=DEBUG_LINE_LENGTH), DIMENSION(:), ALLOCATABLE :: lines
         CHARACTER(LEN=DEBUG_LINE_LENGTH) :: line
         INTEGER                          :: nLines, ios, k

         REWIND(iUnit)
         nLines = 0
         DO
            READ(iUnit, '(A)', IOSTAT = ios) line
            IF ( ios /= 0 )     EXIT
            nLines = nLines + 1
         END DO

         IF ( ALLOCATED(lines) )     DEALLOCATE(lines)
         ALLOCATE(lines(nLines))
         REWIND(iUnit)
         DO k = 1, nLines
            READ(iUnit, '(A)') lines(k)
         END DO
         CLOSE(iUnit)

      END SUBROUTINE ReadAndCloseScratchFile
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION startsWith( line, prefix )
         IMPLICIT NONE
         CHARACTER(LEN=*) :: line, prefix
         startsWith = INDEX(ADJUSTL(line), prefix) == 1
      END FUNCTION startsWith
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION numberOfLinesStartingWith( lines, prefix )
         IMPLICIT NONE
         CHARACTER(LEN=*), DIMENSION(:) :: lines
         CHARACTER(LEN=*)               :: prefix
         INTEGER                        :: k
         numberOfLinesStartingWith = 0
         DO k = 1, SIZE(lines)
            IF ( startsWith(lines(k), prefix) )     numberOfLinesStartingWith = numberOfLinesStartingWith + 1
         END DO
      END FUNCTION numberOfLinesStartingWith
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION textAfterEquals( line )  RESULT(str)
         IMPLICIT NONE
         CHARACTER(LEN=*)                 :: line
         CHARACTER(LEN=DEBUG_LINE_LENGTH) :: str
         str = line(INDEX(line, "=") + 1:)
      END FUNCTION textAfterEquals
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckSizerOutput( project )
!
!     ---------------------------------------------------------
!     The sizer description and the refinement center controls
!     ---------------------------------------------------------
!
         IMPLICIT NONE
         CLASS(MeshProject), POINTER :: project

         CHARACTER(LEN=DEBUG_LINE_LENGTH), DIMENSION(:), ALLOCATABLE :: lines
         CHARACTER(LEN=DEBUG_LINE_LENGTH)                            :: rest
         TYPE (FTLinkedListIterator) :: iterator
         CLASS(FTObject), POINTER    :: obj
         INTEGER                     :: iUnit, n, nCenters
         REAL(KIND=RP)               :: baseSize, xMin(3), xMax(3)

         CALL OpenScratchFile(iUnit)
         CALL project % sizer % printDescription(iUnit)
         CALL ReadAndCloseScratchFile(iUnit, lines)

         CALL FTAssertEqual(expectedValue = 5, actualValue = SIZE(lines), msg = "Sizer description line count")
         IF ( SIZE(lines) == 5 )     THEN
            rest = textAfterEquals(lines(1))
            READ(rest, *) n
            CALL FTAssertEqual(expectedValue = project % model % numberOfInnerCurves, actualValue = n, &
                               msg = "Sizer description: number of inner boundaries")
            rest = textAfterEquals(lines(2))
            READ(rest, *) n
            CALL FTAssertEqual(expectedValue = project % model % numberOfInterfaceCurves, actualValue = n, &
                               msg = "Sizer description: number of interface boundaries")
            rest = textAfterEquals(lines(3))
            READ(rest, *) baseSize
            rest = textAfterEquals(lines(4))
            READ(rest, *) xMin
            rest = textAfterEquals(lines(5))
            READ(rest, *) xMax
            CALL FTAssert(test = ABS(baseSize - project % sizer % baseSize) <= POSITION_TOLERANCE, &
                          msg  = "Sizer description: base size")
            CALL FTAssert(test = MAXVAL(ABS(xMin - project % sizer % xMin)) <= POSITION_TOLERANCE .AND. &
                                 MAXVAL(ABS(xMax - project % sizer % xMax)) <= POSITION_TOLERANCE,     &
                          msg  = "Sizer description: bounding box")
         END IF
!
!        -----------------------------------------------
!        AllFeatures has one refinement center (and one
!        refinement line, which has no description)
!        -----------------------------------------------
!
         CALL OpenScratchFile(iUnit)
         nCenters = 0
         CALL iterator % initWithFTLinkedListClass(project % sizer % controlsList)
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            SELECT TYPE ( obj )
               CLASS IS ( SizerCenterControl )
                  nCenters = nCenters + 1
                  CALL obj % printDescription(iUnit)
            END SELECT
            CALL iterator % moveToNext()
         END DO
         CALL ReadAndCloseScratchFile(iUnit, lines)

         CALL FTAssertEqual(expectedValue = 1, actualValue = nCenters, msg = "Number of refinement centers")
         CALL FTAssertEqual(expectedValue = nCenters, actualValue = numberOfLinesStartingWith(lines, "SizerCenter object"), &
                            msg = "Refinement center descriptions")

      END SUBROUTINE CheckSizerOutput
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckSegmentedChainOutput( project )
!
!     ---------------------------------------------------------------
!     The segmented (discrete) boundary chains held by the sizer.
!     The points of the outer boundary are compared with the
!     continuous model curves they were sampled from.
!     ---------------------------------------------------------------
!
         IMPLICIT NONE
         CLASS(MeshProject), POINTER :: project

         CHARACTER(LEN=DEBUG_LINE_LENGTH), DIMENSION(:), ALLOCATABLE :: lines
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedChain
         CLASS(FRSegmentedCurve)     , POINTER :: frsCurve
         CLASS(SMSegmentedCurveNode) , POINTER :: segmentNode
         CLASS(SegmentedCurveArray)  , POINTER :: curveArray
         CLASS(SMCurve)              , POINTER :: modelCurve
         CLASS(FTObject)             , POINTER :: obj
         TYPE (FTLinkedListIterator)           :: iterator
         INTEGER                               :: iUnit, j, m, k, nExpected, nChains
         REAL(KIND=RP)                         :: t, x(3), nHat(3), invScale, maxError
         LOGICAL                               :: structureOK
!
!        --------------
!        Outer boundary
!        --------------
!
         segmentedChain => project % sizer % outerBoundary
         CALL OpenScratchFile(iUnit)
         CALL segmentedChain % printDescription(iUnit)
         CALL ReadAndCloseScratchFile(iUnit, lines)

         nExpected = 1
         DO j = 1, segmentedChain % curveCount()
            frsCurve  => segmentedChain % segmentedCurveAtIndex(j)
            nExpected = nExpected + frsCurve % COUNT() + 2
         END DO
         CALL FTAssertEqual(expectedValue = nExpected, actualValue = SIZE(lines), &
                            msg = "Segmented outer boundary description line count")

         IF ( SIZE(lines) == nExpected )     THEN
            structureOK = startsWith(lines(1), "Chain") .AND. &
                          INDEX(lines(1), TRIM(segmentedChain % curveName)) > 0
            maxError    = 0.0_RP
            k           = 1
            DO j = 1, segmentedChain % curveCount()
               frsCurve   => segmentedChain % segmentedCurveAtIndex(j)
               modelCurve => project % model % outerBoundary % curveAtIndex(j)
               k = k + 1
               structureOK = structureOK .AND. startsWith(lines(k), "Curve") &
                                         .AND. INDEX(lines(k), TRIM(frsCurve % curveName)) > 0
               DO m = 1, frsCurve % COUNT()
                  k = k + 1
                  READ(lines(k), *) t, x, nHat, invScale
                  maxError = MAX(maxError, MAXVAL(ABS(x - frsCurve % positionAtIndex(m))),    &
                                           MAXVAL(ABS(x - modelCurve % positionAt(t))),        &
                                           ABS(t - frsCurve % argumentAtIndex(m)),             &
                                           ABS(invScale - frsCurve % invScaleAtIndex(m)))
               END DO
               k = k + 1
               structureOK = structureOK .AND. startsWith(lines(k), "-----")
            END DO
            CALL FTAssert(test = structureOK, msg = "Segmented outer boundary description headers")
            CALL FTAssert(test = maxError <= POSITION_TOLERANCE, &
                          msg  = "Segmented outer boundary points lie on the model curves")
         END IF
!
!        ------------------------------------------------------
!        Inner boundaries: one header line per chain, and two
!        per curve plus one per point
!        ------------------------------------------------------
!
         CALL OpenScratchFile(iUnit)
         nExpected = 0
         nChains   = 0
         CALL iterator % initWithFTLinkedListClass(project % sizer % innerBoundariesList)
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToChainedSegmentedCurve(obj, segmentedChain)
            CALL segmentedChain % printDescription(iUnit)
            nChains   = nChains + 1
            nExpected = nExpected + 1
            DO j = 1, segmentedChain % curveCount()
               frsCurve  => segmentedChain % segmentedCurveAtIndex(j)
               nExpected = nExpected + frsCurve % COUNT() + 2
            END DO
            CALL iterator % moveToNext()
         END DO
         CALL ReadAndCloseScratchFile(iUnit, lines)

         CALL FTAssertEqual(expectedValue = project % model % numberOfInnerCurves, actualValue = nChains, &
                            msg = "Number of segmented inner boundaries")
         CALL FTAssertEqual(expectedValue = nExpected, actualValue = SIZE(lines), &
                            msg = "Segmented inner boundary description line count")
         CALL FTAssertEqual(expectedValue = nChains, actualValue = numberOfLinesStartingWith(lines, "Chain"), &
                            msg = "Segmented inner boundary chain headers")
!
!        ----------------------------
!        One node of a segmented curve
!        ----------------------------
!
         segmentedChain => project % sizer % outerBoundary
         frsCurve       => segmentedChain % segmentedCurveAtIndex(1)
         obj            => frsCurve % nodeArray % objectAtIndex(2)
         CALL castToSMSegmentedCurveNode(obj, segmentNode)
         CALL OpenScratchFile(iUnit)
         CALL segmentNode % printDescription(iUnit)
         CALL ReadAndCloseScratchFile(iUnit, lines)

         CALL FTAssertEqual(expectedValue = 1, actualValue = SIZE(lines), msg = "Segmented curve node line count")
         IF ( SIZE(lines) == 1 )     THEN
            READ(lines(1), *) t, x
            CALL FTAssert(test = ABS(t - frsCurve % argumentAtIndex(2)) <= POSITION_TOLERANCE .AND. &
                                 MAXVAL(ABS(x - frsCurve % positionAtIndex(2))) <= POSITION_TOLERANCE, &
                          msg  = "Segmented curve node description")
         END IF
!
!        --------------------------------------------------
!        The segmented curve array made from the outer chain
!        --------------------------------------------------
!
         curveArray => allocateAndInitSegmentedCurveFromSegmentedChain(segmentedChain)
         CALL OpenScratchFile(iUnit)
         CALL curveArray % printDescription(iUnit)
         CALL ReadAndCloseScratchFile(iUnit, lines)

         CALL FTAssertEqual(expectedValue = curveArray % nSegments + 1, actualValue = SIZE(lines), &
                            msg = "Segmented curve array line count")
         IF ( SIZE(lines) == curveArray % nSegments + 1 )     THEN
            maxError = 0.0_RP
            DO j = 0, curveArray % nSegments
               READ(lines(j+1), *) t, x
               maxError = MAX(maxError, ABS(t - curveArray % t(j)), MAXVAL(ABS(x - curveArray % x(:,j))))
            END DO
            CALL FTAssert(test = maxError <= POSITION_TOLERANCE, msg = "Segmented curve array points")
         END IF
         CALL releaseCurveArray(curveArray)

      END SUBROUTINE CheckSegmentedChainOutput
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckModelCurveOutput( project )
!
!     -----------------------------------------------------------
!     printChainForPlotting of the outer boundary, and the
!     descriptions of the equation and spline curves of the model
!     -----------------------------------------------------------
!
         IMPLICIT NONE
         CLASS(MeshProject), POINTER :: project

         CHARACTER(LEN=DEBUG_LINE_LENGTH), DIMENSION(:), ALLOCATABLE :: lines
         CLASS(SMChainedCurve), POINTER :: chain
         CLASS(SMCurve)       , POINTER :: curve
         CLASS(FTObject)      , POINTER :: obj
         INTEGER                        :: iUnit, j, k, nEquationCurves, nSplineCurves
         REAL(KIND=RP)                  :: t, x(3), maxError
         LOGICAL                        :: descriptionOK
         INTEGER, PARAMETER             :: N_PLOT = 200 ! As in printChainForPlotting

         chain => project % model % outerBoundary
         CALL OpenScratchFile(iUnit)
         CALL printChainForPlotting(chain, iUnit)
         CALL ReadAndCloseScratchFile(iUnit, lines)

         CALL FTAssertEqual(expectedValue = N_PLOT + 1, actualValue = SIZE(lines), &
                            msg = "printChainForPlotting line count")
         IF ( SIZE(lines) == N_PLOT + 1 )     THEN
            maxError = 0.0_RP
            DO j = 0, N_PLOT
               READ(lines(j+1), *) t, x
               maxError = MAX(maxError, ABS(t - REAL(j,RP)/N_PLOT), MAXVAL(ABS(x - chain % positionAt(t))))
            END DO
            CALL FTAssert(test = maxError <= POSITION_TOLERANCE, msg = "printChainForPlotting points")
         END IF
!
!        ---------------------------------------------------------
!        Equation curves print their name and their x, y and z
!        equations, and spline curves use the SMCurve description
!        with the name
!        ---------------------------------------------------------
!
         nEquationCurves = 0
         nSplineCurves   = 0
         descriptionOK   = .TRUE.
         DO k = 1, project % model % numberOfChains()
            obj => project % model % allChains(k) % object
            CALL castToSMChainedCurve(obj, chain)
            DO j = 1, chain % COUNT()
               curve => chain % curveAtIndex(j)
               SELECT TYPE ( curve )
                  TYPE IS ( SMParametricEquationCurve )
                     nEquationCurves = nEquationCurves + 1
                     CALL OpenScratchFile(iUnit)
                     CALL curve % printDescription(iUnit)
                     CALL ReadAndCloseScratchFile(iUnit, lines)
                     IF ( SIZE(lines) == 4 )     THEN
                        descriptionOK = descriptionOK                                                 &
                                  .AND. TRIM(ADJUSTL(lines(1))) == TRIM(curve % curveName())          &
                                  .AND. TRIM(ADJUSTL(lines(2))) == TRIM(ADJUSTL(curve % xEqn % equation)) &
                                  .AND. TRIM(ADJUSTL(lines(3))) == TRIM(ADJUSTL(curve % yEqn % equation)) &
                                  .AND. TRIM(ADJUSTL(lines(4))) == TRIM(ADJUSTL(curve % zEqn % equation))
                     ELSE
                        descriptionOK = .FALSE.
                     END IF
                  TYPE IS ( SMSplineCurve )
                     nSplineCurves = nSplineCurves + 1
                     CALL OpenScratchFile(iUnit)
                     CALL curve % printDescription(iUnit)
                     CALL ReadAndCloseScratchFile(iUnit, lines)
                     IF ( SIZE(lines) == 1 )     THEN
                        descriptionOK = descriptionOK .AND. startsWith(lines(1), "Curve with id") &
                                                      .AND. INDEX(lines(1), TRIM(curve % curveName())) > 0
                     ELSE
                        descriptionOK = .FALSE.
                     END IF
               END SELECT
            END DO
         END DO

         CALL FTAssert(test = nEquationCurves > 0 .AND. nSplineCurves > 0, &
                       msg  = "AllFeatures has equation and spline curves")
         CALL FTAssert(test = descriptionOK, msg = "Equation and spline curve descriptions")

      END SUBROUTINE CheckModelCurveOutput
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckPolynomialCurveOutput
!
!     ------------------------------------------------------------
!     A polynomial interpolant curve (not available in a control
!     file) and the SMCurve description of its parent, which names
!     the curve and gives its id
!     ------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=DEBUG_LINE_LENGTH), DIMENSION(:), ALLOCATABLE :: lines
         TYPE(SMPolynomialInterpolant) :: polynomialCurve
         INTEGER      , PARAMETER      :: N = 2
         REAL(KIND=RP)                 :: nodes(0:N), values(0:N,3)
         INTEGER                       :: iUnit, j

         DO j = 0, N
            nodes(j)    = REAL(j,RP)/N
            values(j,:) = [nodes(j), nodes(j)**2, 0.0_RP]
         END DO
         CALL polynomialCurve % initWithNodesValuesAndID(N, nodes, values, "parabola", id = 7)

         CALL OpenScratchFile(iUnit)
         CALL polynomialCurve % printDescription(iUnit)
         CALL polynomialCurve % SMCurve % printDescription(iUnit)
         CALL ReadAndCloseScratchFile(iUnit, lines)

         CALL FTAssertEqual(expectedValue = 2, actualValue = SIZE(lines), &
                            msg = "Polynomial curve description line count")
         IF ( SIZE(lines) == 2 )     THEN
            CALL FTAssert(test = TRIM(lines(1)) == "SMPolynomialInterpolant Object", &
                          msg  = "Polynomial curve description")
            CALL FTAssert(test = startsWith(lines(2), "Curve with id") .AND. INDEX(lines(2), " 7 ") > 0 &
                                 .AND. INDEX(lines(2), "parabola") > 0,                                &
                          msg  = "SMCurve description of the polynomial curve")
         END IF

      END SUBROUTINE CheckPolynomialCurveOutput
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckGridOutput( grid )
!
!     ------------------------------------------------------------
!     The grid description, PrintGridDiagnostics for the refined
!     quadtree, and the description of a quad
!     ------------------------------------------------------------
!
         IMPLICIT NONE
         TYPE (QuadTreeGrid), POINTER :: grid

         CHARACTER(LEN=DEBUG_LINE_LENGTH), DIMENSION(:), ALLOCATABLE :: lines
         CHARACTER(LEN=DEBUG_LINE_LENGTH)                            :: rest
         CLASS(SMQuad), POINTER :: quad
         INTEGER                :: iUnit, level, nGrids, nLines, i, j

         CALL OpenScratchFile(iUnit)
         CALL grid % printDescription(iUnit)
         CALL ReadAndCloseScratchFile(iUnit, lines)

         CALL FTAssertEqual(expectedValue = 2, actualValue = SIZE(lines), msg = "Grid description line count")
         IF ( SIZE(lines) == 2 )     THEN
            rest = textAfterEquals(lines(2))
            READ(rest, *) level
            CALL FTAssert(test = startsWith(lines(1), "QuadTreeGrid object") .AND. level == grid % level, &
                          msg  = "Grid description")
         END IF
!
!        ------------------------------------------------------
!        The diagnostics of the whole tree, compared with counts
!        made by walking the tree the same way
!        ------------------------------------------------------
!
         CALL OpenScratchFile(iUnit)
         CALL PrintGridDiagnostics(grid, iUnit)
         CALL ReadAndCloseScratchFile(iUnit, lines)

         nGrids = 0
         nLines = 0
         CALL CountGridDiagnostics(grid, nGrids, nLines)
         CALL FTAssert(test = nGrids > 1, msg = "The AllFeatures grid is refined")
         CALL FTAssertEqual(expectedValue = nLines, actualValue = SIZE(lines), &
                            msg = "PrintGridDiagnostics line count")
         CALL FTAssertEqual(expectedValue = nGrids, actualValue = numberOfLinesStartingWith(lines, "Nodes at level"), &
                            msg = "PrintGridDiagnostics grids")
         CALL FTAssertEqual(expectedValue = nGrids, actualValue = numberOfLinesStartingWith(lines, "Highest level"), &
                            msg = "PrintGridDiagnostics headers")
         IF ( SIZE(lines) > 0 )     THEN
            rest = textAfterEquals(lines(1))
            READ(rest, *) level
            CALL FTAssertEqual(expectedValue = highestLevel, actualValue = level, &
                               msg = "PrintGridDiagnostics highest level")
         END IF
!
!        ---------------------------
!        The first quad of the grid
!        ---------------------------
!
         quad => NULL()
         DO j = 1, grid % N(2)
            DO i = 1, grid % N(1)
               IF ( ASSOCIATED(grid % quads(i,j) % quad) .AND. .NOT.ASSOCIATED(quad) )     THEN
                  quad => grid % quads(i,j) % quad
               END IF
            END DO
         END DO
         CALL FTAssert(test = ASSOCIATED(quad), msg = "The grid has a quad")
         IF ( ASSOCIATED(quad) )     THEN
            CALL OpenScratchFile(iUnit)
            CALL quad % printDescription(iUnit)
            CALL ReadAndCloseScratchFile(iUnit, lines)
            CALL FTAssertEqual(expectedValue = 1, actualValue = numberOfLinesStartingWith(lines, "SMQuad object"), &
                               msg = "Quad description")
         END IF

      END SUBROUTINE CheckGridOutput
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE CountGridDiagnostics( grid, nGrids, nLines )
!
!     -----------------------------------------------------------
!     Number of grids in the tree and of lines PrintGridDiagnostics
!     writes for it: per grid the "Highest level", "Nodes at level",
!     "Quads at level" and "Neighbors" lines, one line per node,
!     five per quad and one per neighbor
!     -----------------------------------------------------------
!
         IMPLICIT NONE
         TYPE (QuadTreeGrid), POINTER :: grid
         INTEGER                      :: nGrids, nLines
         INTEGER                      :: i, j

         nGrids = nGrids + 1
         nLines = nLines + 4
         DO j = 0, grid % N(2)
            DO i = 0, grid % N(1)
               IF ( ASSOCIATED(grid % nodes(i,j) % node) )     nLines = nLines + 1
            END DO
         END DO
         DO j = 1, grid % N(2)
            DO i = 1, grid % N(1)
               IF ( ASSOCIATED(grid % quads(i,j) % quad) )     nLines = nLines + 5
            END DO
         END DO
         IF ( ASSOCIATED(grid % neighborL) )     nLines = nLines + 1
         IF ( ASSOCIATED(grid % neighborR) )     nLines = nLines + 1
         IF ( ASSOCIATED(grid % neighborT) )     nLines = nLines + 1
         IF ( ASSOCIATED(grid % neighborB) )     nLines = nLines + 1

         IF ( ASSOCIATED(grid % children) )     THEN
            DO j = 1, grid % N(2)
               DO i = 1, grid % N(1)
                  IF ( ASSOCIATED(grid % children(i,j) % grid) )     THEN
                     CALL CountGridDiagnostics(grid % children(i,j) % grid, nGrids, nLines)
                  END IF
               END DO
            END DO
         END IF

      END SUBROUTINE CountGridDiagnostics
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckMeshOutput( mesh )
!
!     ---------------------------------------------------------------
!     The mesh description: a header and blank line for each of the
!     nodes, edges and elements, one line per node, three per edge
!     (the edge and its two nodes) and five per quad element (the
!     element and its four nodes). The node lines are compared with
!     the nodes, and the element node lines with the element nodes.
!     ---------------------------------------------------------------
!
         IMPLICIT NONE
         TYPE (SMMesh), POINTER :: mesh

         CHARACTER(LEN=DEBUG_LINE_LENGTH), DIMENSION(:), ALLOCATABLE :: lines
         CHARACTER(LEN=DEBUG_LINE_LENGTH)                            :: rest
         TYPE (FTLinkedListIterator) :: nodeIterator, elementIterator
         CLASS(FTObject), POINTER    :: obj
         TYPE (SMNode)   , POINTER   :: node ! castToSMNode and castToSMElement
         TYPE (SMElement), POINTER   :: e    ! return non-polymorphic pointers
         INTEGER                     :: iUnit, nNodes, nEdges, nElements, k, m
         INTEGER                     :: id, refCount, chainID, activeStatus, nMismatches
         INTEGER                     :: nodesHeader, edgesHeader, elementsHeader
         REAL(KIND=RP)               :: x(3)

         nNodes    = mesh % nodes % COUNT()
         nEdges    = mesh % edges % COUNT()
         nElements = mesh % elements % COUNT()

         CALL OpenScratchFile(iUnit)
         CALL mesh % printDescription(iUnit)
         CALL ReadAndCloseScratchFile(iUnit, lines)

         CALL FTAssertEqual(expectedValue = 6 + nNodes + 3*nEdges + 5*nElements, actualValue = SIZE(lines), &
                            msg = "Mesh description line count")
         IF ( SIZE(lines) /= 6 + nNodes + 3*nEdges + 5*nElements )     RETURN

         nodesHeader    = 1
         edgesHeader    = nodesHeader + 2 + nNodes
         elementsHeader = edgesHeader + 2 + 3*nEdges
         CALL FTAssert(test = startsWith(lines(nodesHeader)   , "Mesh Nodes...") .AND. &
                              startsWith(lines(edgesHeader)   , "Mesh Edges...") .AND. &
                              startsWith(lines(elementsHeader), "Mesh Elements..."),   &
                       msg  = "Mesh description section headers")
         CALL FTAssertEqual(expectedValue = nEdges, actualValue = numberOfLinesStartingWith(lines, "Edge with ID"), &
                            msg = "Mesh description edges")
         CALL FTAssertEqual(expectedValue = nElements, actualValue = numberOfLinesStartingWith(lines, "Element with ID"), &
                            msg = "Mesh description elements")
!
!        -----------------------------------
!        Node lines, in the order of the list
!        -----------------------------------
!
         nMismatches = 0
         k           = nodesHeader + 1
         CALL nodeIterator % initWithFTLinkedList(mesh % nodes)
         DO WHILE ( .NOT.nodeIterator % isAtEnd() )
            obj => nodeIterator % object()
            CALL castToSMNode(obj, node)
            k = k + 1
            READ(lines(k), *) id, refCount, x, chainID, activeStatus
            IF ( id /= node % id .OR. chainID /= node % bCurveChainID .OR.     &
                 activeStatus /= node % activeStatus .OR.                      &
                 MAXVAL(ABS(x - node % x)) > POSITION_TOLERANCE )     nMismatches = nMismatches + 1
            CALL nodeIterator % moveToNext()
         END DO
         CALL FTAssertEqual(expectedValue = 0, actualValue = nMismatches, msg = "Mesh description node lines")
!
!        ------------------------------------------------
!        Each element line is followed by its four nodes
!        ------------------------------------------------
!
         nMismatches = 0
         k           = elementsHeader + 1
         CALL elementIterator % initWithFTLinkedList(mesh % elements)
         DO WHILE ( .NOT.elementIterator % isAtEnd() )
            obj => elementIterator % object()
            CALL castToSMElement(obj, e)
            k = k + 1
            rest = textAfterEquals(lines(k))
            READ(rest, *) id
            IF ( id /= e % id )     nMismatches = nMismatches + 1
            DO m = 1, 4
               k = k + 1
               READ(lines(k), *) id
               IF ( id /= e % nodes(m) % node % id )     nMismatches = nMismatches + 1
            END DO
            CALL elementIterator % moveToNext()
         END DO
         CALL FTAssertEqual(expectedValue = 0, actualValue = nMismatches, msg = "Mesh description element lines")

      END SUBROUTINE CheckMeshOutput
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckTopographyOutput( project, projectDict )
!
!     ------------------------------------------------------------
!     An equation topography prints its equation, a data file
!     topography its file name, both as given in the control file.
!     The SMTopography parent prints a one line description.
!     ------------------------------------------------------------
!
         IMPLICIT NONE
         CLASS(MeshProject)      , POINTER :: project
         TYPE (FTValueDictionary), POINTER :: projectDict

         CHARACTER(LEN=DEBUG_LINE_LENGTH), DIMENSION(:), ALLOCATABLE :: lines
         CLASS(SMTopography)     , POINTER :: topography
         CLASS(FTValueDictionary), POINTER :: modelDict, topographyDict
         CLASS(FTObject)         , POINTER :: obj
         CHARACTER(LEN=DEBUG_LINE_LENGTH)  :: expected
         INTEGER                           :: iUnit

         obj            => projectDict % objectForKey(key = "MODEL")
         modelDict      => valueDictionaryFromObject(obj)
         obj            => modelDict % objectForKey(key = "TOPOGRAPHY")
         topographyDict => valueDictionaryFromObject(obj)

         topography => project % model % topography
         CALL FTAssert(test = ASSOCIATED(topography), msg = "The project has a topography")
         IF ( .NOT.ASSOCIATED(topography) )     RETURN

         SELECT TYPE ( topography )
            TYPE IS ( SMEquationTopography )
               expected = topographyDict % stringValueForKey(key = "eqn", requestedLength = DEBUG_LINE_LENGTH)
               CALL OpenScratchFile(iUnit)
               CALL topography % printDescription(iUnit)
               CALL topography % SMTopography % printDescription(iUnit)
               CALL ReadAndCloseScratchFile(iUnit, lines)
            TYPE IS ( SMTopographyFromFile )
               expected = topographyDict % stringValueForKey(key = "data file", requestedLength = DEBUG_LINE_LENGTH)
               CALL OpenScratchFile(iUnit)
               CALL topography % printDescription(iUnit)
               CALL topography % SMTopography % printDescription(iUnit)
               CALL ReadAndCloseScratchFile(iUnit, lines)
            CLASS DEFAULT
               CALL FTAssert(test = .FALSE., msg = "Unexpected topography type")
               RETURN
         END SELECT

         CALL FTAssertEqual(expectedValue = 2, actualValue = SIZE(lines), msg = "Topography description line count")
         IF ( SIZE(lines) == 2 )     THEN
            CALL FTAssert(test = TRIM(ADJUSTL(lines(1))) == TRIM(ADJUSTL(expected)), &
                          msg  = "Topography description: " // TRIM(ADJUSTL(lines(1))))
            CALL FTAssert(test = startsWith(lines(2), "Topography"), msg = "SMTopography description")
         END IF

      END SUBROUTINE CheckTopographyOutput

      END Module DebugOutputTestsModule

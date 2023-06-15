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
!      MeshingTests.f90
!      Created: May 13, 2021 at 10:33 AM
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
    Module MeshingTests
      USE MeshProjectClass
      USE MeshQualityAnalysisClass
      USE HOHQMeshModule
      USE TestDataClass
      USE TestSuiteManagerClass
      USE EncoderModule
      USE FTAssertions
      IMPLICIT NONE
!
!---------------------------------------------------------------------
! Runs through a sequence of meshes and tests against standard results
!---------------------------------------------------------------------
!
      CHARACTER(LEN=64), DIMENSION(:), ALLOCATABLE :: controlFiles
!
!  ========
   CONTAINS
!  ========

!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE RunTests(pathToTestFiles, numberOfFailedTests)
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CHARACTER(LEN=*)     :: pathToTestFiles
      INTEGER, INTENT(OUT) :: numberOfFailedTests
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE(TestSuiteManager)                  :: testSuite
      CHARACTER(LEN=1), POINTER               :: optData(:)
      CHARACTER(LEN=1), ALLOCATABLE           :: optDataA(:)
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: fullPath, path, str
      INTEGER                                 :: k, fUnit, nControlFiles
      EXTERNAL                                :: TestCurves, TestGaussianCurvature
      EXTERNAL                                :: TestBiCubicInterpolation
      LOGICAL                                 :: exst
      CHARACTER(LEN=ERROR_MESSAGE_LENGTH)     :: msg
!
!     ------------------------------------------------------------------------------------
!     The control files are located in a Benchmarks directory at the end of (if not empty)
!     the directory given by the "path" to the HOHQMesh directory. That path will be added
!     to the control file names.
!     ------------------------------------------------------------------------------------
!
      CALL testSuite % init()
!
!     -------------------
!     Add hardwired tests
!     -------------------
!
      CALL testSuite % addTestSubroutineWithName(TestCurves,"Curve evaluation tests")
      CALL testSuite % addTestSubroutineWithName(TestGaussianCurvature,"Gaussian Curvature evaluation tests")
      CALL testSuite % addTestSubroutineWithName(TestBiCubicInterpolation,"BiCubic Interpolation tests")
!
!     --------------------------------
!     Get the list of meshing tests...
!     --------------------------------
!
      path = pathToTestFiles

      IF ( TRIM(path) == "" )     THEN
         fullPath =  "Benchmarks/BenchmarkFiles.txt"
      ELSE
         CALL ConvertToPath(path)
         fullPath = TRIM(path) // "Benchmarks/BenchmarkFiles.txt"
      END IF

      INQUIRE( FILE = fullPath, EXIST = exst )
      IF (.NOT.exst) THEN
         WRITE(msg, *) "Unable to open the list of test files: " // TRIM(ADJUSTL(fullPath))
         CALL ThrowErrorExceptionOfType(poster = "RunTests",    &
                                        msg    = msg,           &
                                        typ    = FT_ERROR_FATAL)
         RETURN
      END IF

      OPEN(NEWUNIT = fUnit, FILE = fullPath)
!
!     -------------------------
!     Count the number of lines
!     -------------------------
!
      nControlFiles = 0

      DO
         READ(UNIT = fUnit, FMT = '(A)', END = 1000) str
         nControlFiles = nControlFiles + 1
      END DO
1000  CONTINUE

      ALLOCATE(controlFiles(nControlFiles))
      REWIND(fUnit)
!
!     ------------------------------
!     Read in the control file names
!     ------------------------------
!
      DO k = 1, nControlFiles
         READ(UNIT = fUnit, FMT = '(A)') str
         controlFiles(k) = TRIM(str)
      END DO
!
!     -------------------------------------
!     Add the tests of meshes to test suite
!     -------------------------------------
!
      DO k = 1, SIZE(controlFiles)

         IF ( TRIM(pathToTestFiles) == "" )     THEN
            fullPath =  controlFiles(k)
         ELSE
            CALL ConvertToPath(pathToTestFiles)
            fullPath = TRIM(pathToTestFiles) // controlFiles(k)
         END IF

         optData => NULL()
         CALL encode(str = TRIM(fullPath),enc = optDataA)
         ALLOCATE(optData(SIZE(optDataA)))
         optData = optDataA

         CALL testSuite % addTestSubroutineWithName(testWithControlfile,"ControlFile: " // TRIM(controlFiles(k)), optData)
         DEALLOCATE(optDataA)
      END DO
!
!     -------------
!     Run the tests
!     -------------
!
      CALL testSuite % performTests(numberOfFailedTests)
      CALL finalizeSharedAssertionsManager
      DEALLOCATE(controlFiles)

   END SUBROUTINE RunTests
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE testWithControlfile(optData)
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)
!
!     ---------------
!     Local variables
!     ---------------
!
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: controlFileName
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: testResultsLocation
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: path
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: str
      CLASS( MeshProject )    , POINTER       :: project     => NULL()
      TYPE (FTValueDictionary), POINTER       :: controlDict => NULL()
      TYPE (FTValueDictionary), POINTER       :: projectDict => NULL()
      CLASS(FTValueDictionary), POINTER       :: paramsDict  => NULL()
      CLASS(FTObject)         , POINTER       :: obj
      TYPE( MeshStatistics )                  :: stats
      TYPE(testData)                          :: benchmarkResults, currentResults
      INTEGER                                 :: fUnit, ios, j, lc
      LOGICAL                                 :: didGenerate3DMesh
      INTEGER, EXTERNAL                       :: UnusedUnit

      didGenerate3DMesh = .FALSE.

      controlFileName = ""
      CALL DECODE(enc = optData, strOut = controlFileName)
!
!     ---------------------
!     Read the control file
!     ---------------------
!
      CALL ReadControlFile(controlFileName, projectDict)
!
!     --------------------------
!     Get the benchmark versions
!     --------------------------
!
      obj                 => projectDict % objectForKey(key = "CONTROL_INPUT")
      controlDict         => valueDictionaryFromObject(obj)
      obj                 => controlDict % objectForKey(key = RUN_PARAMETERS_KEY)
      paramsDict          => valueDictionaryFromObject(obj)
      testResultsLocation =  paramsDict % stringValueForKey(key = "test file name", &
                                                            requestedLength = DEFAULT_CHARACTER_LENGTH)
!
!     ------------------
!     Generate the Mesh
!     ------------------
!
      ALLOCATE(project)
      CALL HOHQMesh(projectDict, project, stats, didGenerate3DMesh, .TRUE.)
!
!     -----------------------------------------------------------
!     Find the path and modify output file locations if necessary
!     -----------------------------------------------------------
!
      path = ""
      lc = INDEX(STRING = controlFileName, SUBSTRING = "Benchmarks")
      IF ( lc > 1 )     THEN
         path = controlFileName(1:lc-1)
         CALL ConvertToPath(path)
      END IF
      CALL AddPathToProjectFiles(self = project,path = path)
!
!     --------------
!     Output results
!     --------------
!
      CALL WritePlotFile(project, didGenerate3DMesh)
      CALL WriteMeshFile(project, didGenerate3DMesh)
      str = project % runParams % statsFileName
      CALL toLower(str)
      IF ( str /= "none" )     THEN
         CALL Write2DMeshStatistics(mesh          = project % mesh, &
                                    statsFileName = project % runParams % statsFileName)
      END IF
!
!     ------------------------------------------------------------------------------------
!     The control files are located in a Benchmarks directory at the end of (if not empty)
!     the directory given by the "path" to the HOHQMesh directory. That path can be found
!     in the control file path. So strip out the path here and re-route to the location of
!     the test data files.
!     ------------------------------------------------------------------------------------
!
      IF ( testResultsLocation /= "" )     THEN
         testResultsLocation = TRIM(path) // testResultsLocation
!
!        ---------------------------------------------------
!        Test current results against saved benchmark values
!        ---------------------------------------------------
!
         fUnit = UnusedUnit()
         ios   = 0
         OPEN(FILE = testResultsLocation, UNIT = fUnit, STATUS = "OLD", IOSTAT = ios)
         CALL FTAssertEqual(expectedValue = 0,actualValue = ios,msg = "Cannot open file: "//TRIM(testResultsLocation))
         IF ( ios /= 0 )     RETURN

         CALL readTestValues    (self = benchmarkResults, fUnit = fUnit)
         CALL GatherTestFileData(tData = currentResults, project = project, stats = stats)
!
!        ------------------------
!        Perform the actual tests
!        ------------------------
!
         IF ( benchmarkResults % testDataType == QUAD_MESH_TEST )     THEN
            DO j = 1, SIZE(benchmarkResults % intValues)
               CALL FTAssertEqual(expectedValue = benchmarkResults % intValues(j), &
                                  actualValue   = currentResults % intValues(j),   &
                                  msg = TRIM(integerNames2D(j)))
            END DO

            DO j = 1, SIZE(benchmarkResults % realValues)
               CALL FTAssertEqual(expectedValue = benchmarkResults % realValues(j), &
                                  actualValue   = currentResults % realValues(j),   &
                                  tol           = 1.0d-4,                           &
                                  msg = TRIM(realNames2D(j)))
            END DO
         ELSE
            DO j = 1, SIZE(benchmarkResults % intValues)
               CALL FTAssertEqual(expectedValue = benchmarkResults % intValues(j), &
                                  actualValue   = currentResults % intValues(j),   &
                                  msg = TRIM(integerNames3D(j)))
            END DO

            DO j = 1, SIZE(benchmarkResults % realValues)
               CALL FTAssertEqual(expectedValue = benchmarkResults % realValues(j), &
                                  actualValue   = currentResults % realValues(j),   &
                                  tol           = 1.0d-4,                           &
                                  msg = TRIM(realNames3D(j)))
            END DO
         END IF

      ELSE
         PRINT *, "No benchmark results specified for problem:",  TRIM(controlFileName)
      END IF

      CALL releaseMeshProject(project)
      CALL releaseFTValueDictionary(controlDict)
      CLOSE(fUnit)


   END SUBROUTINE testWithControlfile
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GatherTestFileData(tData, project, stats)
         USE MeshProjectClass
         USE MeshQualityAnalysisClass
         USE FTMutableObjectArrayClass
         USE TestDataClass
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS( MeshProject )  , POINTER :: project
         TYPE( MeshStatistics )          :: stats
         TYPE(testData)                  :: tData
!
!        ---------------
!        Local Variables
!        ---------------
!
         TYPE (FTMutableObjectArray) , POINTER :: badElements => NULL()
         INTEGER                               :: numBadElements = 0
         INTEGER                               :: numNodes, numElements

         IF ( STATs % statsType == QUAD_STATISTICS )     THEN
            numBadElements = 0
            badElements => BadElementsInMesh( project % mesh )
            IF(ASSOCIATED(badElements))     THEN
                numBadElements = badElements % COUNT()
                CALL releaseFTMutableObjectArray(badElements)
            END IF

            CALL ConstructTestData(tData,testDataType = QUAD_MESH_TEST)

            CALL tData % addIntValue(whichValue = TR_NUM_ELEMENTS, &
                                    intValue    = project % mesh % elements % COUNT())
            CALL tData % addIntValue(whichValue = TR_NUM_NODES, &
                                    intValue    = project % mesh % nodes % COUNT())
            CALL tData % addIntValue(whichValue = TR_NUM_EDGES, &
                                    intValue    = project % mesh % edges % COUNT())
            CALL tData % addIntValue(whichValue = TR_NUM_BAD_ELEMENTS, &
                                    intValue    = numBadElements)

            CALL tData % addRealValue(whichValue = TR_SIGNED_AREA  , realValue = STATs % avgValues(SIGNED_AREA_INDEX))
            CALL tData % addRealValue(whichValue = TR_ASPECT_RATIO , realValue = STATs % avgValues(ASPECT_RATIO_INDEX))
            CALL tData % addRealValue(whichValue = TR_CONDITION    , realValue = STATs % avgValues(CONDITION_INDEX))
            CALL tData % addRealValue(whichValue = TR_EDGE_RATIO   , realValue = STATs % avgValues(EDGE_RATIO_INDEX))
            CALL tData % addRealValue(whichValue = JACOBIAN        , realValue = STATs % avgValues(JACOBIAN_INDEX))
            CALL tData % addRealValue(whichValue = MIN_ANGLE       , realValue = STATs % avgValues(MIN_ANGLE_INDEX))
            CALL tData % addRealValue(whichValue = MAX_ANGLE       , realValue = STATs % avgValues(MAX_ANGLE_INDEX))
         ELSE
            CALL ConstructTestData(tData, testDataType = HEX_MESH_TEST)

            numElements = SIZE(project % hexMesh % elements)
            numNodes    = SIZE(project % hexMesh % nodes)
!
            CALL tData % addIntValue(whichValue = TR_NUM_ELEMENTS, intValue = numElements)
            CALL tData % addIntValue(whichValue = TR_NUM_NODES   , intValue = numNodes)

            CALL tData % addRealValue(whichValue = TR_DIAGONAL    , realValue = STATs % avgValues(DIAGONAL3D_INDEX))
            CALL tData % addRealValue(whichValue = TR_EDGE_RATIO3D, realValue = STATs % avgValues(EDGE_RATIO3D_INDEX))
            CALL tData % addRealValue(whichValue = TR_JACOBIAN3D  , realValue = STATs % avgValues(JACOBIAN3D_INDEX))
            CALL tData % addRealValue(whichValue = TR_SHAPE       , realValue = STATs % avgValues(SHAPE3D_INDEX))
            CALL tData % addRealValue(whichValue = SKEW3D_INDEX   , realValue = STATs % avgValues(SKEW3D_INDEX))
            CALL tData % addRealValue(whichValue = TR_VOLUME      , realValue = STATs % avgValues(VOLUME3D_INDEX))
         END IF

      END SUBROUTINE GatherTestFileData

   END Module MeshingTests

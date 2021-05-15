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
      CHARACTER(LEN=64), DIMENSION(5) :: controlFiles = [                                                          &
                                                         "Benchmarks/ControlFiles/PillC.control                 ", &
                                                         "Benchmarks/ControlFiles/SplineGeometry.control        ", &
                                                         "Benchmarks/ControlFiles/NACA0012.control              ", &
                                                         "Benchmarks/ControlFiles/Circles3.control              ", &
                                                         "Benchmarks/ControlFiles/GingerbreadManGeometry.control"  &
                                                        ]
!
!  ======== 
   CONTAINS
!  ========

!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE RunTests(outputFileName)  
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CHARACTER(LEN=*) :: outputFileName
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE(TestSuiteManager)         :: testSuite
      INTEGER                        :: numberOfFailedTests
      CHARACTER(LEN=1), POINTER      :: optData(:) 
      CHARACTER(LEN=1), ALLOCATABLE  :: optDataA(:) 
      INTEGER                        :: k
      INTEGER                        :: fUnit
      INTEGER, EXTERNAL              :: UnusedUnit

      CALL testSuite % init()
!
!     -------------------------------
!     Add test runs to the test suite
!     -------------------------------
!
      DO k = 1, SIZE(controlFiles)

         optData => NULL()
         CALL encode(str = controlFiles(k),enc = optDataA)
         ALLOCATE(optData(SIZE(optDataA)))
         optData = optDataA

         CALL testSuite % addTestSubroutineWithName(testWithControlfile,"ControlFile: " // TRIM(controlFiles(k)), optData)
         DEALLOCATE(optDataA)
      END DO 

      CALL testSuite % performTests()
      
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
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: controlFileName, testResultsLocation
      CLASS( MeshProject )    , POINTER       :: project     => NULL()
      TYPE( MeshStatistics )                  :: stats
      TYPE (FTValueDictionary), POINTER       :: controlDict
      CLASS(FTValueDictionary), POINTER       :: paramsDict
      CLASS(FTObject)         , POINTER       :: obj
      TYPE(testData)                          :: benchmarkResults, currentResults
      INTEGER                                 :: fUnit, ios, j
      INTEGER, EXTERNAL                       :: UnusedUnit
      
      controlFileName = ""
      CALL DECODE(enc = optData, strOut = controlFileName)
      
      ALLOCATE(project)
!
!     ------------------
!     Generate the Mesh 
!     ------------------
!
      CALL HOHQMesh(controlFileName, controlDict, project, stats, .TRUE.)
!
!     --------------------------
!     Get the benchmark versions
!     --------------------------
!
      obj                 => controlDict % objectForKey(key = RUN_PARAMETERS_KEY)
      paramsDict          => valueDictionaryFromObject(obj)
      testResultsLocation =  paramsDict % stringValueForKey(key = "test file name", &
                                                            requestedLength = DEFAULT_CHARACTER_LENGTH) 
      IF ( testResultsLocation /= "" )     THEN
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
         DO j = 1, NUMBER_OF_INT_TEST_VALUES 
            CALL FTAssertEqual(expectedValue = benchmarkResults % intValues(j), &
                               actualValue   = currentResults % intValues(j),   &
                               msg = TRIM(integerNames(j))) 
         END DO 
         
         DO j = 1, NUMBER_OF_REAL_TEST_VALUES 
            CALL FTAssertEqual(expectedValue = benchmarkResults % realValues(j), &
                               actualValue   = currentResults % realValues(j),   &
                               tol           = 1.0d-4,                           &
                               msg = TRIM(realNames(j))) 
         END DO 
         
         
      ELSE
         PRINT *, "No benchmark results specified for problem:",  TRIM(controlFileName)
      END IF 
       
      CALL releaseMeshProject(project)
      CALL releaseFTValueDictionary(controlDict)

    
   END SUBROUTINE testWithControlfile
   
   END Module MeshingTests

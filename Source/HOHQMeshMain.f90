!
!////////////////////////////////////////////////////////////////////////
!
!      HOMeshMain.f90
!      Created: August 19, 2013 11:18 AM 
!      By: David Kopriva  
!
!      Main program for 2/3D meshing
!
!////////////////////////////////////////////////////////////////////////
!
      PROGRAM HOQMeshMain 
         USE MeshProjectClass
         USE MeshQualityAnalysisClass
         USE HOHQMeshModule
         USE MeshingTests
         IMPLICIT NONE
!
!        --------------------
!        Mesh project storage
!        --------------------
!
         CLASS( MeshProject )  , POINTER   :: project     => NULL()
         TYPE( MeshStatistics )            :: stats
         TYPE (FTValueDictionary), POINTER :: controlDict => NULL()
         CLASS(FTValueDictionary), POINTER :: paramsDict
         CLASS(FTObject)         , POINTER :: obj
!
!        ----
!        File
!        ----
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: controlFileName, testResultsName
         INTEGER                                 :: fUnit
         INTEGER, EXTERNAL                       :: UnusedUnit 
!
!        -----
!        Other
!        -----
!         
         CHARACTER(LEN=8) :: version          = "05.03.21"
         LOGICAL          :: test             = .FALSE.
         LOGICAL          :: generateTest     = .FALSE.
         TYPE(testData)   :: tData
!
!        ***********************************************
!                             Start
!        ***********************************************
!
         CALL initializeFTExceptions
         CALL ReadCommandLineArguments(version, test, generateTest, controlFileName)
         
         IF ( test )     THEN
         
            printMessage    = .FALSE.
            testResultsName = "TestResults"
            CALL RunTests(outputFileName = testResultsName)
            
         ELSE 
         
            ALLOCATE(project)
            CALL HOHQMesh(controlFileName, controlDict, project, stats, .FALSE.)
         
            IF ( generateTest )     THEN
               obj             => controlDict % objectForKey(key = RUN_PARAMETERS_KEY)
               paramsDict      => valueDictionaryFromObject(obj)
               testResultsName = paramsDict % stringValueForKey(key = "test file name", &
                                               requestedLength = DEFAULT_CHARACTER_LENGTH) 

               CALL GatherTestFileData( tData, project, stats) 
               
               fUnit = UnusedUnit()
               OPEN(FILE = testResultsName, UNIT = fUnit)
                  CALL tData % writeTestValues(fUnit = fUnit)
               CLOSE(fUnit)
            END IF 
            
            CALL releaseMeshProject(project)
            
         END IF 
!
!        --------
!        Clean up
!        --------
!
         IF(ASSOCIATED(controlDict)) CALL releaseFTValueDictionary(controlDict)
         CALL destructFTExceptions
         IF( PrintMessage ) PRINT *, "Execution complete. Exit."
         
      END PROGRAM HOQMeshMain
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ReadCommandLineArguments(version, test, generateTest, controlFileName)  
         USE CommandLineReader
         USE ProgramGlobals
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: controlFileName
         CHARACTER(LEN=8)                        :: version
         LOGICAL                                 :: test, generateTest
         
         
         IF ( CommandLineArgumentIsPresent("-version") )     THEN
            PRINT *, "HOMesh Version ", version
         END IF
         
         IF ( CommandLineArgumentIsPresent("-help") )     THEN
            PRINT *, "No help avalable yet. Sorry!"
            STOP
         END IF
         
         test = .false.
         IF ( CommandLineArgumentIsPresent("-test") )     THEN
            test = .true.
         END IF
         
         generateTest = .false.
         IF ( CommandLineArgumentIsPresent("-generateTest") )     THEN
            generateTest = .true.
         END IF
   
         printMessage = .false.
         IF ( CommandLineArgumentIsPresent("-verbose") )     THEN
            printMessage = .true.
         END IF
         
         controlFileName = "none"
         IF ( CommandLineArgumentIsPresent(argument = "-f") )     THEN
            controlFileName = StringValueForArgument(argument = "-f")
         END IF 
         
      END SUBROUTINE ReadCommandLineArguments

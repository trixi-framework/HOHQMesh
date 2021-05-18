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
         CLASS( MeshProject )    , POINTER :: project     => NULL()
         TYPE (FTValueDictionary), POINTER :: projectDict => NULL()
         TYPE (FTValueDictionary), POINTER :: controlDict => NULL()
         CLASS(FTValueDictionary), POINTER :: paramsDict  => NULL()
         CLASS(FTObject)         , POINTER :: obj
         TYPE( MeshStatistics )            :: stats
!
!        ----
!        File
!        ----
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: controlFileName, path, testResultsName
         INTEGER                                 :: fUnit
         INTEGER, EXTERNAL                       :: UnusedUnit 
!
!        -----
!        Other
!        -----
!         
         CHARACTER(LEN=8) :: version           = "05.03.21"
         LOGICAL          :: test              = .FALSE.
         LOGICAL          :: generateTest      = .FALSE.
         LOGICAL          :: didGenerate3DMesh = .FALSE.

         TYPE(testData)   :: tData
!
!        ***********************************************
!                             Start
!        ***********************************************
!
         CALL initializeFTExceptions
         CALL ReadCommandLineArguments(version, test, generateTest, controlFileName, path)
         
         IF ( test )     THEN
         
            printMessage    = .FALSE.
            CALL RunTests(pathToTestFiles = path)
            
         ELSE 
         
            ALLOCATE(project)
            
            CALL ReadControlFile(controlFileName, projectDict)
            CALL HOHQMesh(projectDict, project, stats, didGenerate3DMesh, .FALSE.)
            
            IF(path /= "")     THEN
               CALL ConvertToPath(path)
               CALL AddPathToProjectFiles(self = project,path = path)
            END IF 

            CALL WritePlotFile(project, didGenerate3DMesh)
            CALL WriteMeshFile(project, didGenerate3DMesh)
         
            IF ( generateTest )     THEN
               obj             => projectDict % objectForKey(key = "CONTROL_INPUT")
               controlDict     => valueDictionaryFromObject(obj)
               obj             => controlDict % objectForKey(key = RUN_PARAMETERS_KEY)
               paramsDict      => valueDictionaryFromObject(obj)
               testResultsName = paramsDict % stringValueForKey(key = "test file name", &
                                               requestedLength = DEFAULT_CHARACTER_LENGTH) 

               IF(path /= "")  testResultsName = TRIM(path) // testResultsName
               
               CALL GatherTestFileData( tData, project, stats) 
               
               fUnit = UnusedUnit()
               OPEN(FILE = testResultsName, UNIT = fUnit)
                  CALL tData % writeTestValues(fUnit = fUnit)
               CLOSE(fUnit)
            END IF 
            
            CALL releaseMeshProject(project)
            IF(ASSOCIATED(controlDict)) CALL releaseFTValueDictionary(projectDict)
            
         END IF 
!
!        --------
!        Clean up
!        --------
!
         CALL destructFTExceptions
         IF( PrintMessage ) PRINT *, "Execution complete. Exit."
         
      END PROGRAM HOQMeshMain
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ReadCommandLineArguments(version, test, generateTest, controlFileName, path)  
         USE CommandLineReader
         USE ProgramGlobals
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: controlFileName, path
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
          
         path = ""
         IF ( CommandLineArgumentIsPresent(argument = "-path") )     THEN
            path = StringValueForArgument(argument = "-path")
         END IF 
        
      END SUBROUTINE ReadCommandLineArguments

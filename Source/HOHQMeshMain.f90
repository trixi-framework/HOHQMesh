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
         CHARACTER(LEN=*), PARAMETER :: version           = "1.5.5-pre"
         LOGICAL                     :: test              = .FALSE.
         LOGICAL                     :: generateTest      = .FALSE.
         LOGICAL                     :: didGenerate3DMesh = .FALSE.

         TYPE(testData)              :: tData
         INTEGER                     :: numberOfFailedTests = 0

         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: str
!
!        ***********************************************
!                             Start
!        ***********************************************
!
         CALL initializeFTExceptions
         CALL ReadCommandLineArguments(version, test, generateTest, controlFileName, path)

         IF ( test )     THEN

            printMessage    = .FALSE.
            CALL RunTests(pathToTestFiles = path, numberOfFailedTests = numberOfFailedTests)

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

            str = project % runParams % statsFileName
            CALL toLower(str)
            IF ( str /= "none" )     THEN
               CALL Write2DMeshStatistics(mesh          = project % mesh, &
                                          statsFileName = project % runParams % statsFileName)
            END IF

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

         IF ( numberOfFailedTests .gt. 0 )     THEN

           ERROR STOP 'At least one test has failed'

         END IF

      END PROGRAM HOQMeshMain
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReadCommandLineArguments(version, test, generateTest, controlFileName, path)
         USE, INTRINSIC :: iso_fortran_env, only : OUTPUT_UNIT
         USE CommandLineReader
         USE ProgramGlobals
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH)     :: controlFileName, path
         CHARACTER(LEN=*)                            :: version
         LOGICAL                                     :: test, generateTest
         INTEGER                                     :: argCount

         argCount = COMMAND_ARGUMENT_COUNT()
!
!        ------------------------------------------
!        Ensure that there is at least one argument
!        ------------------------------------------
!
         IF(argCount .eq. 0)   THEN
            WRITE(OUTPUT_UNIT,*) "Command line argument error"
            CALL PrintHelpMessage(OUTPUT_UNIT)
            ERROR STOP "Error in command line argument (see `-help` for a list of available options)"
         END IF

         IF ( CommandLineArgumentIsPresent("-version") )     THEN
            PRINT *, "HOHQMesh Version ", version
            STOP
         END IF

         IF ( CommandLineArgumentIsPresent("-help") )     THEN
            CALL PrintHelpMessage(OUTPUT_UNIT)
            STOP
         END IF

         IF ( CommandLineArgumentIsPresent("--help") )     THEN
            CALL PrintHelpMessage(OUTPUT_UNIT)
            STOP
         END IF

         test = .false.
         IF ( CommandLineArgumentIsPresent("-test") )     THEN
            test = .true.
            argCount = argCount - 1
         END IF

         generateTest = .false.
         IF ( CommandLineArgumentIsPresent("-generateTest") )     THEN
            generateTest = .true.
            argCount = argCount - 1
         END IF

         printMessage = .false.
         IF ( CommandLineArgumentIsPresent("-verbose") )     THEN
            printMessage = .true.
            argCount = argCount - 1
         END IF

         controlFileName = "none"
         IF ( CommandLineArgumentIsPresent(argument = "-f") )     THEN
            controlFileName = StringValueForArgument(argument = "-f")
            argCount = argCount - 2
         END IF

         path = ""
         IF ( CommandLineArgumentIsPresent(argument = "-path") )     THEN
            path = StringValueForArgument(argument = "-path")
            argCount = argCount - 2
         END IF

         IF ( CommandLineArgumentIsPresent(argument = "-sLimit") )     THEN
            maxLevelLimit = IntegerValueForArgument(argument = "-sLimit")
            argCount = argCount - 2
         END IF
!
!        ---------------------------------------
!        See if there are any uncaught arguments
!        ---------------------------------------
!
         IF(argCount .ne. 0)   THEN
            WRITE(OUTPUT_UNIT,*) "Command line argument error"
            CALL PrintHelpMessage(OUTPUT_UNIT)
            ERROR STOP "Error in command line argument (see `-help` for a list of available options)"
         END IF

      END SUBROUTINE ReadCommandLineArguments
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE PrintHelpMessage(iUnit)
         IMPLICIT NONE
         INTEGER :: iUnit
         
         WRITE(iUnit,*) "HOHQMesh Help..."
         WRITE(iUnit,*) "Invocation:"
         WRITE(iUnit,*) "   ./HOHQMesh [options]"
         WRITE(iUnit,*) "Options:"
         WRITE(iUnit,*) "-help or --help"
         WRITE(iUnit,*) "   Prints this message. For the user manual, "
         WRITE(iUnit,*) "   see https://trixi-framework.github.io/HOHQMesh/."
         WRITE(iUnit,*) "-version"
         WRITE(iUnit,*) "   Prints the current version number of the software."
         WRITE(iUnit,*) "-f <ControlFileName>"
         WRITE(iUnit,*) "   Generates a mesh using the control file <ControlFileName>."
         WRITE(iUnit,*) "-verbose"
         WRITE(iUnit,*) "   Log the meshing progress to the screen."
         WRITE(iUnit,*) "-test"
         WRITE(iUnit,*) "   Runs the test sequence"
         WRITE(iUnit,*) "-path <path>"
         WRITE(iUnit,*) "   Use to specify location of the HOHQMesh directory for the test sequence."
         WRITE(iUnit,*) "-sLimit n"
         WRITE(iUnit,*) "   Increase the number of subdivisions allowed to 2^n. Default is n = 8."
         WRITE(iUnit,*) "-generateTest"
         WRITE(iUnit,*) "   Generates a test mesh using the control file. "
         WRITE(iUnit,*) "   See https://trixi-framework.github.io/HOHQMesh/adding-a-new-test/"

      END SUBROUTINE PrintHelpMessage

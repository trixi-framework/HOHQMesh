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
         CHARACTER(LEN=*), PARAMETER :: version           = "1.5.4-pre"
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
         USE CommandLineReader
         USE ProgramGlobals
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: controlFileName, path
         CHARACTER(LEN=*)                        :: version
         LOGICAL                                 :: test, generateTest


         IF ( CommandLineArgumentIsPresent("-version") )     THEN
            PRINT *, "HOHQMesh Version ", version
            STOP
         END IF

         IF ( CommandLineArgumentIsPresent("-help") )     THEN
            CALL PrintHelpMessage()
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

         IF ( CommandLineArgumentIsPresent(argument = "-sLimit") )     THEN
            maxLevelLimit = IntegerValueForArgument(argument = "-sLimit")
         END IF

      END SUBROUTINE ReadCommandLineArguments
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE PrintHelpMessage()
         USE, INTRINSIC :: iso_fortran_env, only : OUTPUT_UNIT
         IMPLICIT NONE
         WRITE(OUTPUT_UNIT,*) "HOHQMesh Help..."
         WRITE(OUTPUT_UNIT,*) "Invocation:"
         WRITE(OUTPUT_UNIT,*) "	./HOHQMesh [options]"
         WRITE(OUTPUT_UNIT,*) "Options:"
         WRITE(OUTPUT_UNIT,*) "-help"
         WRITE(OUTPUT_UNIT,*) "	Prints this message. For the user manual, "
         WRITE(OUTPUT_UNIT,*) "	see https://trixi-framework.github.io/HOHQMesh/."
         WRITE(OUTPUT_UNIT,*) "-version"
         WRITE(OUTPUT_UNIT,*) "	Prints the current version number of the software."
         WRITE(OUTPUT_UNIT,*) "-f <ControlFileName>"
         WRITE(OUTPUT_UNIT,*) "	Generates a mesh using the control file <ControlFileName>."
         WRITE(OUTPUT_UNIT,*) "-verbose"
         WRITE(OUTPUT_UNIT,*) "	Log the meshing progress to the screen."
         WRITE(OUTPUT_UNIT,*) "-test"
         WRITE(OUTPUT_UNIT,*) "	Runs the test sequence"
         WRITE(OUTPUT_UNIT,*) "-path <path>"
         WRITE(OUTPUT_UNIT,*) "	Use to specify location of the HOHQMesh directory for the test sequence."
         WRITE(OUTPUT_UNIT,*) "-sLimit n"
         WRITE(OUTPUT_UNIT,*) "	Increase the number of subdivisions allowed to 2^n. Default is n = 8."
         WRITE(OUTPUT_UNIT,*) "-generateTest"
         WRITE(OUTPUT_UNIT,*) "	Generates a test mesh using the control file. "
         WRITE(OUTPUT_UNIT,*) "	See https://trixi-framework.github.io/HOHQMesh/adding-a-new-test/"

      END SUBROUTINE PrintHelpMessage

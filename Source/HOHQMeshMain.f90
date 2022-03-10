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
! HOHQMesh contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend, 
!    https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
! * `fmin`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
! * `spline`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
! * `seval`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
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
         CHARACTER(LEN=*), PARAMETER :: version           = "1.2.0"
         LOGICAL                     :: test              = .FALSE.
         LOGICAL                     :: generateTest      = .FALSE.
         LOGICAL                     :: didGenerate3DMesh = .FALSE.

         TYPE(testData)              :: tData
         INTEGER                     :: numberOfFailedTests = 0
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
         USE, INTRINSIC :: iso_fortran_env, only : stderr => ERROR_UNIT 
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
            PRINT *, "HOMesh Version ", version
            STOP
         END IF
         
         IF ( CommandLineArgumentIsPresent("-help") )     THEN
            WRITE(stderr,*)  "No help avalable yet. Sorry!"
            ERROR STOP "No help available"
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

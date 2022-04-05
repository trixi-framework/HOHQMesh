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
!      Created: 6/14/21, 6:05 PM
!      By: David Kopriva  
!
!      Main test program
!
!////////////////////////////////////////////////////////////////////////
!
      PROGRAM HOQMeshMain 
         USE SMConstants
         USE ISO_C_BINDING
         USE InteropUtilitiesModule
         USE LibTestModule
         IMPLICIT NONE
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH)             :: pathToRepo
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), EXTERNAL   :: CommandLineArgument
         CHARACTER(KIND=c_char), DIMENSION(:)  , ALLOCATABLE :: cFName
         INTEGER(C_INT)                                      :: nFailed
         
         
         pathToRepo = CommandLineArgument()
         cFName = f_to_c_string(f_string = pathToRepo)
         
         CALL HML_RunTests(cPathToRepo = cFName, numberOfFailedTests = nFailed)

      END PROGRAM HOQMeshMain
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE MeshFromControlFile(controlFileName)
         USE ProjectInterfaceModule
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH)             :: controlFileName
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(KIND=c_char), DIMENSION(:)  , ALLOCATABLE :: cFName
         INTEGER(C_INT)                                      :: flag
         TYPE(c_ptr)                                         :: projCPtr
         
         cFName = f_to_c_string(f_string = controlFileName)
         projCPtr = HML_AllocProject()
         
         CALL HML_InitWithControlFile(cPtr      = projCPtr, &
                                      cFileName = cFName, &
                                      errFlag   = flag)
         CALL StopOnError(flag)
         
         CALL HML_GenerateMesh(cPtr = projCPtr, errFlag = flag)
         CALL StopOnError(flag)
         
         PRINT *, HML_NumberOfNodes(projCPtr, flag),  &
                  HML_NumberOfEdges(projCPtr, flag),  &
                  HML_NumberOfElements(projCPtr, flag)
         
         CALL HML_WriteMesh(cPtr = projCPtr, errFlag = flag )
         CALL StopOnError(flag)
         CALL HML_WritePlotFile(cPtr = projCPtr, errFlag = flag )
         CALL StopOnError(flag)
         
         CALL HML_ReleaseProject(cPtr = projCPtr, errFlag = flag)
         
      END SUBROUTINE MeshFromControlFile
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION CommandLineArgument()  RESULT(controlFileName)
         USE SMConstants
         USE ProgramGlobals
         USE CommandLineReader
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: controlFileName
         
         controlFileName = "none"
         IF ( CommandLineArgumentIsPresent(argument = "-f") )     THEN
            controlFileName = StringValueForArgument(argument = "-f")
         END IF 
        
      END FUNCTION CommandLineArgument

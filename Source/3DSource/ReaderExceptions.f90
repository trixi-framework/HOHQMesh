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
!      ReaderExceptions.f90
!      Created: April 2, 2013 9:49 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module ReaderExceptions 
      USE FTExceptionClass
      USE FTValueDictionaryClass
      IMPLICIT NONE
!
!     -----------------------------------
!     Collect exceptions for file reading
!     -----------------------------------
!
      CONTAINS 
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION ReaderException(exceptionName,message,variableName,whereInCode)
!
!     ---------------------------------------------------------
!     Convenience method to create an exception with a message,
!     value and location
!     ---------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTException), POINTER :: ReaderException
         CHARACTER(LEN=*)            :: exceptionName, variableName, whereInCode, message
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (FTValueDictionary), POINTER :: userDictionary
         CLASS(FTDictionary)     , POINTER :: ptr
         
         ALLOCATE(userDictionary)
         CALL userDictionary % initWithSize(8)
         CALL userDictionary % addValueForKey(message,"message")
         CALL userDictionary % addValueForKey(variableName,"value")
         CALL userDictionary % addValueForKey(whereInCode,"Calling routine")
         
         ALLOCATE(ReaderException)
         ptr => userDictionary
         CALL ReaderException % initFTException(FT_ERROR_FATAL,exceptionName,ptr)
         CALL releaseFTValueDictionary(userDictionary)

      END FUNCTION ReaderException
 
      END Module ReaderExceptions 
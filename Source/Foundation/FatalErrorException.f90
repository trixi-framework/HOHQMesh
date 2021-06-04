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
!      FatalErrorException.f90
!      Created: September 12, 2013 1:51 PM 
!      By: David Kopriva  
!
!      Defines a generic error exception with the following
!      keys and values in the user dictionary:
!
!      (poster, char(len=*))
!      (message, char(len=*))
!
!////////////////////////////////////////////////////////////////////////
!
! 
   MODULE ErrorTypesModule
      INTEGER          , PARAMETER :: ERROR_EXCEPTION_MSG_LENGTH   = 256
      CHARACTER(LEN=21), PARAMETER :: FATAL_ERROR_EXCEPTION    = "Fatal error exception"
      CHARACTER(LEN=23), PARAMETER :: WARNING_ERROR_EXCEPTION  = "Warning error exception"
!
!     ========
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
!
      SUBROUTINE ThrowErrorExceptionOfType(poster,msg,typ)
         USE SharedExceptionManagerModule 
         USE FTValueClass
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=*)  :: msg
         CHARACTER(LEN=*)  :: poster
         INTEGER           :: typ ! = FT_ERROR_FATAL or FT_ERROR_WARNING
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (FTException)   , POINTER :: exception      => NULL()
         CLASS(FTDictionary)  , POINTER :: userDictionary => NULL()
         CLASS(FTObject)      , POINTER :: obj            => NULL()
         TYPE (FTValue)       , POINTER :: v              => NULL()
!
!        -----------------------------------------------------
!        The userDictionary for this exception contains the
!        message to be delivered under the name "message"
!        and the object being created by the name "objectName"
!        -----------------------------------------------------
!
         ALLOCATE(userDictionary)
         CALL userDictionary % initWithSize(4)
         
         ALLOCATE(v)
         CALL v % initWithValue(poster)
         obj => v
         CALL userDictionary % addObjectForKey(obj,"poster")
         CALL releaseFTValue(self = v)
         
         ALLOCATE(v)
         CALL v % initWithValue(msg)
         obj => v
         CALL userDictionary % addObjectForKey(obj,"message")
         CALL releaseFTValue(self = v)
!
!        --------------------
!        Create the exception
!        --------------------
!
         ALLOCATE(exception)
         
         SELECT CASE ( typ )
            CASE( FT_ERROR_FATAL ) 
               CALL exception % initFTException(FT_ERROR_FATAL, &
                                    exceptionName   = FATAL_ERROR_EXCEPTION, &
                                    infoDictionary  = userDictionary)
            CASE DEFAULT 
               CALL exception % initFTException(FT_ERROR_WARNING, &
                                    exceptionName   = WARNING_ERROR_EXCEPTION, &
                                    infoDictionary  = userDictionary)
         END SELECT 
         
         obj => userDictionary
         CALL releaseFTObject(self = obj)
        
!
!        -------------------
!        Throw the exception
!        -------------------
!
         CALL throw(exception)
         CALL releaseFTException(self = exception)
         
      END SUBROUTINE ThrowErrorExceptionOfType
      
   END MODULE ErrorTypesModule
!
!//////////////////////////////////////////////////////////////////////// 
! 
   LOGICAL FUNCTION ReturnOnFatalError()
      USE SharedExceptionManagerModule 
      IMPLICIT NONE
      
      ReturnOnFatalError = catch() .AND. (maximumErrorSeverity() > FT_ERROR_WARNING)
 
   END FUNCTION ReturnOnFatalError
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE trapExceptions  
         USE SharedExceptionManagerModule
         IMPLICIT NONE 

         INTEGER                    :: errorSeverity = FT_ERROR_NONE
         TYPE(FTException), POINTER :: exception
         
         errorSeverity = FT_ERROR_NONE
         
         IF ( catch() )     THEN
            PRINT *
            PRINT *, "------------------------------------------------------------------"
            PRINT *
            PRINT *, "The following errors were found when constructing the project:"
            
            DO
               exception => popLastException()
               IF ( .NOT.ASSOCIATED(exception) )     EXIT
               CALL exception % printDescription(6)
               errorSeverity = MAX(errorSeverity, exception % severity())
            END DO
            PRINT *
            PRINT *, "------------------------------------------------------------------"
            PRINT *
            
            IF ( errorSeverity > FT_ERROR_WARNING )     THEN
               STOP "The Errors were Fatal. Cannot generate mesh." 
            END IF 
         END IF 

      END SUBROUTINE trapExceptions

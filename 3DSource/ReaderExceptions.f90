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
         CLASS(FTValueDictionary), POINTER :: userDictionary
         CLASS(FTDictionary)     , POINTER :: ptr
         
         ALLOCATE(userDictionary)
         CALL userDictionary % initWithSize(8)
         CALL userDictionary % addValueForKey(message,"message")
         CALL userDictionary % addValueForKey(variableName,"value")
         CALL userDictionary % addValueForKey(whereInCode,"Calling routine")
         
         ALLOCATE(ReaderException)
         ptr => userDictionary
         CALL ReaderException % initFTException(FT_ERROR_FATAL,exceptionName,ptr)
         CALL userDictionary % release()

      END FUNCTION ReaderException
 
      END Module ReaderExceptions 
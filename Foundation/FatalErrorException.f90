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
         CLASS(FTException)   , POINTER :: exception      => NULL()
         CLASS(FTDictionary)  , POINTER :: userDictionary => NULL()
         CLASS(FTObject)      , POINTER :: obj            => NULL()
         CLASS(FTValue)       , POINTER :: v              => NULL()
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
         CALL v % release()
         
         ALLOCATE(v)
         CALL v % initWithValue(msg)
         obj => v
         CALL userDictionary % addObjectForKey(obj,"message")
         CALL v % release()
!
!        --------------------
!        Create the exception
!        --------------------
!
         ALLOCATE(exception)
         
         SELECT CASE ( typ )
            CASE( FT_ERROR_FATAL ) 
               CALL exception % initFTException(FT_ERROR_WARNING, &
                                    exceptionName   = WARNING_ERROR_EXCEPTION, &
                                    infoDictionary  = userDictionary)
            CASE DEFAULT 
               CALL exception % initFTException(FT_ERROR_FATAL, &
                                    exceptionName   = FATAL_ERROR_EXCEPTION, &
                                    infoDictionary  = userDictionary)
         END SELECT 
         
         CALL userDictionary % release()
!
!        -------------------
!        Throw the exception
!        -------------------
!
         CALL throw(exception)
         CALL exception % release()
         
      END SUBROUTINE ThrowErrorExceptionOfType
      
   END MODULE ErrorTypesModule

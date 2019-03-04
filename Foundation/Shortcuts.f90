!
!////////////////////////////////////////////////////////////////////////
!
!      Shortcuts.f90
!      Created: February 4, 2019 at 8:57 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   Module ValueSettingModule 
      USE ErrorTypesModule
      USE SharedExceptionManagerModule
      USE FTValueDictionaryClass
      USE SMConstants
      IMPLICIT NONE  
! 
!     -----------------------------------------------------------------
!     Routines for setting values from dictionaries that are used often 
!     -----------------------------------------------------------------
!
!     ======== 
      CONTAINS  
!     ======== 
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE SetRealValueFromDictionary( valueToSet, sourceDict, key, &
                                             errorLevel, message, poster)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP)                :: valueToSet
         CLASS(FTValueDictionary)     :: sourceDict
         INTEGER, INTENT(IN)          :: errorLevel
         CHARACTER(LEN=*), INTENT(IN) :: key
         CHARACTER(LEN=*)             :: message, poster
!
         
         IF( sourceDict % containsKey(key) )     THEN
            valueToSet = sourceDict % doublePrecisionValueForKey(key)
         ELSE
            CALL ThrowErrorExceptionOfType(poster = poster,  &
                                           msg    = message, &
                                           typ    = errorLevel)
         END IF 
         
      END SUBROUTINE SetRealValueFromDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE SetIntegerValueFromDictionary( valueToSet, sourceDict, key, &
                                                errorLevel, message, poster)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                      :: valueToSet
         CLASS(FTValueDictionary)     :: sourceDict
         INTEGER, INTENT(IN)          :: errorLevel
         CHARACTER(LEN=*), INTENT(IN) :: key
         CHARACTER(LEN=*)             :: message, poster
!
         
         IF( sourceDict % containsKey(key) )     THEN
            valueToSet = sourceDict % integerValueForKey(key)
         ELSE
            CALL ThrowErrorExceptionOfType(poster = poster,  &
                                           msg    = message, &
                                           typ    = errorLevel)
         END IF 
         
      END SUBROUTINE SetIntegerValueFromDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE SetStringValueFromDictionary( valueToSet, sourceDict, key, &
                                                errorLevel, message, poster)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=*)             :: valueToSet
         CLASS(FTValueDictionary)     :: sourceDict
         INTEGER, INTENT(IN)          :: errorLevel
         CHARACTER(LEN=*), INTENT(IN) :: key
         CHARACTER(LEN=*)             :: message, poster
!
         
         IF( sourceDict % containsKey(key) )     THEN
            valueToSet = sourceDict % stringValueForKey(key = key,requestedLength = DEFAULT_CHARACTER_LENGTH)
         ELSE
            CALL ThrowErrorExceptionOfType(poster = poster,  &
                                           msg    = message, &
                                           typ    = errorLevel)
         END IF 
         
      END SUBROUTINE SetStringValueFromDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE SetRealArrayValueFromDictionary( arrayToSet, sourceDict, key, &
                                                  errorLevel, message, poster)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP)                :: arrayToSet(3)
         CLASS(FTValueDictionary)     :: sourceDict
         INTEGER, INTENT(IN)          :: errorLevel
         CHARACTER(LEN=*), INTENT(IN) :: key
         CHARACTER(LEN=*)             :: message, poster
!
!        ----------
!        Interfaces
!        ----------
!
         INTERFACE
            FUNCTION GetRealArray( inputLine ) RESULT(x)
               USE SMConstants
               IMPLICIT NONE
               REAL(KIND=RP), DIMENSION(3) :: x
               CHARACTER ( LEN = * ) :: inputLine
            END FUNCTION GetRealArray
         END INTERFACE
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN= DEFAULT_CHARACTER_LENGTH) :: str
!
         IF( sourceDict % containsKey(key) )     THEN
            str = sourceDict % stringValueForKey(key = key,  &
                                                 requestedLength = DEFAULT_CHARACTER_LENGTH)
            arrayToSet = GetRealArray( str )
         ELSE 
            CALL ThrowErrorExceptionOfType(poster = poster,  &
                                           msg    = message, &
                                           typ    = errorLevel)
         END IF
         
      END SUBROUTINE SetRealArrayValueFromDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE SetIntegerArrayValueFromDictionary( arrayToSet, sourceDict, key, &
                                                 errorLevel, message, poster)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                      :: arrayToSet(3)
         CLASS(FTValueDictionary)     :: sourceDict
         INTEGER, INTENT(IN)          :: errorLevel
         CHARACTER(LEN=*), INTENT(IN) :: key
         CHARACTER(LEN=*)             :: message, poster
!
!        ----------
!        Interfaces
!        ----------
!
         INTERFACE
            FUNCTION GetIntArray( inputLine ) RESULT(x)
               IMPLICIT NONE
               INTEGER, DIMENSION(3) :: x
               CHARACTER ( LEN = * ) :: inputLine
            END FUNCTION GetIntArray
         END INTERFACE
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN= DEFAULT_CHARACTER_LENGTH) :: str
!
         IF( sourceDict % containsKey(key) )     THEN
            str = sourceDict % stringValueForKey(key = key,  &
                                                 requestedLength = DEFAULT_CHARACTER_LENGTH)
            arrayToSet = GetIntArray( str )
         ELSE 
            CALL ThrowErrorExceptionOfType(poster = poster,  &
                                           msg    = message, &
                                           typ    = errorLevel)
         END IF
         
      END SUBROUTINE SetIntegerArrayValueFromDictionary
     
   END Module ValueSettingModule
   
   


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
   
   


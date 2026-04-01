!
!////////////////////////////////////////////////////////////////////////
!
!>      SMScanner.f90
!>      Created: March 23, 2026 at 8:34 AM 
!>      By: David Kopriva  
!>
!>      A basic scanner type
!>
!>      Usage:
!>
!>         Initialization
!>            self % initWithString(str, delims)
!>
!>               str    is the string to be scanned
!>               delims is a string of the delimiters, e.g. "[,]" to delimit
!>                      any of the characters [ ] and ,
!>
!>               On initialization the cursor is over the first character of str
!>
!>            self % reset()
!>
!>               Returns the cursor to the first position
!>
!>         Scanning
!>
!>            done = scanUptoString(self, str)
!>
!>               Moves the cursor to just after the string "str"
!>               done is .TRUE. if the end of the string is reached
!>
!>            i   = self % scanInt()
!>
!>                Scans from the cursor position up to the next delimiter
!>                and attempts to convert str(currentPos:delimPos) to an 
!>                integer. If that fails, the value NOT_AN_INTEGER is 
!>                returned. Cursor is placed after the delimiter.
!>
!>            r   = self % scanReal()        
!>
!>                Scans from the cursor position up to the next delimiter
!>                and attempts to convert str(currentPos:delimPos) to a 
!>                real. If that fails, NaN is returned.
!>                Cursor is placed after the delimiter.
!>
!>         Example
!>
!>            CHARACTER(LEN = 12) :: str  = "[124, 2.718]"
!>            CHARACTER(LEN=3)    :: dlms = "[],"
!>            CALL scanner % initWithString(str,dlms)
!>            done       = scanner % scanUpToString("[")
!>            intResult  = scanner % scanInt()  ! = 124
!>            realResult = scanner % scanReal() ! = 2.178
!>
!
!////////////////////////////////////////////////////////////////////////
!
   Module SMScannerClass
   USE, INTRINSIC :: iso_fortran_env, only : stderr => ERROR_UNIT
   USE IEEE_ARITHMETIC
   USE SMConstants, ONLY: RP
   IMPLICIT NONE  
   
   TYPE SMScanner
      CHARACTER(LEN=:), ALLOCATABLE :: str
      CHARACTER(LEN=:), ALLOCATABLE :: delimeters
      INTEGER                       :: cursorPos
      INTEGER                       :: endPos
!
!     ========      
      CONTAINS
!     ========      
!
      PROCEDURE :: initWithString
      PROCEDURE :: reset
      PROCEDURE :: scanUpToString
      PROCEDURE :: scanInt
      PROCEDURE :: scanReal
      PROCEDURE :: isAtEnd
      PROCEDURE :: scanIntsToArray
   END TYPE SMScanner
   
   INTEGER, PARAMETER :: NOT_AN_INTEGER = -HUGE(NOT_AN_INTEGER)
!
!  ========
   CONTAINS  
!  ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE initWithString(self, str, delims)
      IMPLICIT NONE
      CLASS(SMScanner)  :: self
      CHARACTER(LEN=*) :: str
      CHARACTER(LEN=*) :: delims
      
      CALL DestructScanner(self)
      self % str        = str
      self % delimeters = delims
      self % cursorPos  = 1
      self % endPos     = LEN_TRIM(str)
      
   END SUBROUTINE initWithString
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE DestructScanner(self)
      IMPLICIT NONE
      CLASS(SMScanner)  :: self
      self % cursorPos  = 1
      IF(ALLOCATED(self % str)) DEALLOCATE(self % str)
      IF(ALLOCATED(self % delimeters)) DEALLOCATE(self % delimeters)
   END SUBROUTINE DestructScanner
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE reset(self)
      IMPLICIT NONE
      CLASS(SMScanner)  :: self
      self % cursorPos  = 1
   END SUBROUTINE reset
!
!//////////////////////////////////////////////////////////////////////// 
! 
   LOGICAL FUNCTION isAtEnd(self) 
      IMPLICIT NONE  
      CLASS(SMScanner)  :: self
      isAtEnd = .FALSE.
      IF(self % cursorPos > self % endPos) isAtEnd = .TRUE.
   END FUNCTION isAtEnd
!
!//////////////////////////////////////////////////////////////////////// 
! 
   LOGICAL FUNCTION scanUptoString(self, str)
!
!  --------------------------------------------------
!  Scan up to the string str and position the cursor
!  just after it. Returns true if the whole string
!  has been scanned, otherwise false.
!  --------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(SMScanner) :: self
      CHARACTER(LEN=*) :: str
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER :: k
      
      scanUptoString   = .FALSE.
      k                = INDEX(self % str(self % cursorPos: self % endPos), &
                               SUBSTRING = str)
      IF ( k == 0 )     THEN
         scanUptoString = .TRUE.
         RETURN 
      END IF 
      
      self % cursorPos = self % cursorPos + k + LEN_TRIM(str) - 1
            
   END FUNCTION scanUptoString
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE getToken(self, token)  
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(SMScanner) :: self
      CHARACTER(LEN=*) :: token
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER :: scEnd, p, e
      
      p     = self % cursorPos
      e     = self % endPos
! 
      scEnd = SCAN(STRING = self % str(p:e), &
                   SET    = self % delimeters)
                   
      IF(scEnd == 0) THEN 
         scEnd = self % endPos
      ELSE
         scEnd = p + scEnd - 2
      END IF
      
      token = self % str(p:scEnd)
      self % cursorPos = scEnd + 2

   END SUBROUTINE getToken
!
!//////////////////////////////////////////////////////////////////////// 
! 
   FUNCTION scanInt(self) RESULT(intResult)
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(SMScanner) :: self
      INTEGER          :: intResult
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER            :: iErr
      CHARACTER(LEN=128) :: token
      
      CALL getToken(self,token)
      READ(  token, *, IOSTAT = iErr ) intResult
      IF ( iErr /=0 )     THEN
         intResult = -HUGE(intResult)
      END IF
      
   END FUNCTION scanInt
!
!//////////////////////////////////////////////////////////////////////// 
! 
   FUNCTION scanReal(self) RESULT(res)
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(SMScanner)  :: self
      REAL(KIND=RP)     :: res
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER            :: iErr
      CHARACTER(LEN=128) :: token
      
      CALL getToken(self,token)
      READ(  token, *, IOSTAT = iErr ) res
      
      IF ( iErr /=0 )     THEN
         res = IEEE_VALUE(res, IEEE_QUIET_NAN)
      END IF
      
   END FUNCTION scanReal
!
!//////////////////////////////////////////////////////////////////////// 
! 
   FUNCTION scanIntsToArray(self)  RESULT(array)
!
!  ---------------------------------------------------------------
!  Scan through the string collecting integer values into an array
!  On error, the array is of zero size.
!  ---------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(SMScanner)     :: self
      INTEGER, ALLOCATABLE :: array(:)
!
!     ---------------
!     Local varaibles
!     ---------------
!
      INTEGER :: j
      
      ALLOCATE(array(0))
      j = self % scanInt()

      IF ( j == NOT_AN_INTEGER ) RETURN
      array = [j]
            
      DO WHILE(.NOT. self % isAtEnd())
         j = self % scanInt()

         IF ( j == NOT_AN_INTEGER )     THEN
            DEALLOCATE(array)
            ALLOCATE(array(0))
            RETURN
         ELSE 
            array = [array, j]
         END IF 
      END DO

   END FUNCTION scanIntsToArray
!
!  ------------------------------------------------------------------------
!  Testing...
!  ------------------------------------------------------------------------
!
   LOGICAL FUNCTION scanUpToTest()  
      IMPLICIT NONE
!                                      123456789012345
      CHARACTER(LEN = 64)   :: str = "[124, 3.14, ss]"
      INTEGER, DIMENSION(3) :: p = [2,6,12]
      CHARACTER(LEN= 3)   :: dlms = ",[]"
      TYPE(SMScanner)     :: scanner
      LOGICAL             :: done
   
      scanUpToTest = .TRUE. 
      
      CALL scanner % initWithString(str,dlms)
      done = scanner % scanUpToString("[")
      IF(done) scanUpToTest = .FALSE.
      scanUpToTest = scanUpToTest .AND. scanner % cursorPos == p(1)
      
      done         = scanner % scanUpToString(",")
      scanUpToTest = scanUpToTest .AND. .NOT. done
      scanUpToTest = scanUpToTest .AND. scanner % cursorPos == p(2)
            
      done         = scanner % scanUpToString(",")
      scanUpToTest = scanUpToTest .AND. .NOT. done
      scanUpToTest = scanUpToTest .AND. scanner % cursorPos == p(3)
      
      done         = scanner % scanUpToString(",")
      scanUpToTest = scanUpToTest .AND. done
      
   END FUNCTION scanUpToTest
!
!//////////////////////////////////////////////////////////////////////// 
! 
   LOGICAL FUNCTION scanTest()  
      IMPLICIT NONE
!                                   12345678901234567890123456
      CHARACTER(LEN = 64) :: str  = "[124, 14, 3.14, 11, 2.718]"
      CHARACTER(LEN = 64) :: str2 = "[1x4, 3.z4 ]"
      CHARACTER(LEN= 3)   :: dlms = ",[]"
      TYPE(SMScanner)     :: scanner
      LOGICAL             :: done
      INTEGER             :: intResult
      REAL(KIND=RP)       :: realResult
   
      CALL scanner % initWithString(str,dlms)
      done     = scanner % scanUpToString("[")
      scanTest = .NOT.done
!
!     --------
!     Scan 124
!     --------
!
      intResult = scanner % scanInt()
      IF(intResult == NOT_AN_INTEGER)     THEN
         scanTest = .FALSE.
      ELSE
         scanTest = scanTest .AND. intResult == 124
      END IF
!
!     -------
!     Scan 14
!     -------
!
      intResult = scanner % scanInt()
      IF(intResult == NOT_AN_INTEGER)     THEN
         scanTest = .FALSE.
      ELSE
         scanTest = scanTest .AND. intResult == 14
      END IF
!
!     ---------
!     Scan 3.14
!     ---------
!
      realResult = scanner % scanReal()

      IF(IEEE_IS_NAN (realResult))     THEN
         scanTest = .FALSE.
      ELSE
         scanTest = scanTest .AND. ABS(realResult - 3.14d0) < 1.0d-9
      END IF
!
!     -------
!     Scan 11
!     -------
!
      intResult = scanner % scanInt()
      IF(intResult == NOT_AN_INTEGER)     THEN
         scanTest = .FALSE.
      ELSE
         scanTest = scanTest .AND. intResult == 11
      END IF
!
!     ----------
!     Scan 2.718
!     ----------
!
      realResult = scanner % scanReal()

      IF(IEEE_IS_NAN (realResult))     THEN
         scanTest = .FALSE.
      ELSE
         scanTest = scanTest .AND. ABS(realResult - 2.718d0) < 1.0d-9
      END IF
!
!     --------
!     Failures
!     --------
!
      CALL scanner % initWithString(str2,dlms)
      done     = scanner % scanUpToString("[")
      scanTest = .NOT.done
!
!     --------
!     Scan 1x4
!     --------
!
      intResult = scanner % scanInt()
      IF(intResult == NOT_AN_INTEGER)     THEN
         scanTest = scanTest .AND. .TRUE.
      ELSE
         scanTest = .FALSE.
      END IF
!
!     ---------
!     Scan 3.z4
!     ---------
!
      realResult = scanner % scanReal()

      IF(IEEE_IS_NAN (realResult))     THEN
         scanTest = scanTest .AND. .TRUE.
      ELSE
         scanTest = .FALSE.
      END IF
      
   END FUNCTION scanTest
!
!//////////////////////////////////////////////////////////////////////// 
! 
   LOGICAL FUNCTION scanArrayTest()  
      IMPLICIT NONE
!                                         123456789012345678901234567
      CHARACTER(LEN = 64)  :: str      = " breaks = 124, 14,3, 11,2"
      CHARACTER(LEN = 64)  :: failStr  = " breaks = xxy"
      CHARACTER(LEN= 1)    :: dlms = ","
      TYPE(SMScanner)      :: scanner
      LOGICAL              :: done
      INTEGER              :: c(5) = [124,14,3,11,2]
      INTEGER, ALLOCATABLE :: a(:)
!
!     ------------
!     Test success
!     ------------
!
      CALL scanner % initWithString(str,dlms)
      done     = scanner % scanUpToString("=")
      scanArrayTest = .NOT.done
      IF(done) RETURN
      
      a = scanIntsToArray(scanner)
      scanArrayTest = scanArrayTest .AND. MAXVAL(a-c) == 0
      DEALLOCATE(a)
!
!     ------------
!     Test failure
!     ------------
!
      CALL scanner % initWithString(failStr,dlms)
      done     = scanner % scanUpToString("=")
      scanArrayTest = scanArrayTest .AND. .NOT.done
      IF(done) RETURN
      a = scanIntsToArray(scanner)
      scanArrayTest = scanArrayTest .AND. SIZE(a) == 0
      
   END FUNCTION scanArrayTest
   
   END Module SMScannerClass

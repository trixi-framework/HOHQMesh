!
!////////////////////////////////////////////////////////////////////////
!
!      Filename.f
!      Created: 2010-08-27 12:14:31 -0400 
!      By: David Kopriva  
!
! /////////////////////////////////////////////////////////////////////
!
   SUBROUTINE toLower(str)
!
!  ----------------
!  From ResettaCode
!  ----------------
!
     character(*), intent(in out) :: str
     integer :: i
 
     do i = 1, len(str)
       select case(str(i:i))
         case("A":"Z")
           str(i:i) = achar(iachar(str(i:i))+32)
       end select
     end do  
   END SUBROUTINE toLower
!
!///////////////////////////////////////////////////////////////////////
!
!     ----------------------------------------------------------------
!>    "Read" the "value" of a real number declared
!!     after an = sign in a inputLine
!     ----------------------------------------------------------------
!
      FUNCTION GetRealValue( inputLine )
         USE SMConstants
         IMPLICIT NONE 
!
         CHARACTER(len = *) :: inputLine
         REAL( KIND=RP )    :: value, GetRealValue
         INTEGER            :: strLen, leq, k
!
         leq    = INDEX( inputLine, '=' )
         strLen = LEN_TRIM( inputLine )
         READ( inputLine( leq+1:strLen ), *, IOSTAT=k ) VALUE
         IF ( k /=0 )     THEN
            PRINT *, "Bad real value in input line:"
            PRINT *, TRIM(inputLine)
            STOP "Input file synax error"
         END IF 
         GetRealValue = value
!
      END FUNCTION GetRealValue
!
!///////////////////////////////////////////////////////////////////////
!
!     ----------------------------------------------------------------
!>    "Read" the "value" of an integer number declared
!!     after an = sign in a inputLine
!     ----------------------------------------------------------------
!
      FUNCTION GetRealArray( inputLine ) RESULT(x)
         USE SMConstants
         IMPLICIT NONE
!
         REAL(KIND=RP), DIMENSION(3) :: x
         INTEGER                     :: k
         
         CHARACTER ( LEN = * ) :: inputLine
         INTEGER               :: cStart, cEnd
!
         cStart = INDEX(inputLine,'[')
         cEnd   = INDEX(inputLine, ']', .true. )
         READ( inputLine( cStart+1: cEnd-1 ), *, IOSTAT = k  ) x(1), x(2), x(3)
         IF ( k /=0 )     THEN
            PRINT *, "Bad real array syntax in input line:"
            PRINT *, TRIM(inputLine)
            PRINT *, "Syntax is: [real,real,real]"
            STOP "Input file synax error"
         END IF 
!
      END FUNCTION GetRealArray
!
!///////////////////////////////////////////////////////////////////////
!
!     ----------------------------------------------------------------
!>    "Read" the "value" of an integer number declared
!!     after an = sign in a inputLine
!     ----------------------------------------------------------------
!
      INTEGER FUNCTION GetIntValue( inputLine )
         IMPLICIT NONE
!
         CHARACTER ( LEN = * ) :: inputLine
         INTEGER               :: value
         INTEGER               :: strLen, leq, k
!
         leq    = INDEX( inputLine, '=' )
         strLen = LEN_TRIM( inputLine )
         READ( inputLine( leq+1:strLen ), *, IOSTAT = k ) VALUE
         IF ( k /=0 )     THEN
            PRINT *, "Bad integer value in input line:"
            PRINT *, TRIM(inputLine)
            STOP "Input file synax error"
         END IF 
         GetIntValue = VALUE
!
      END FUNCTION GetIntValue
!
!///////////////////////////////////////////////////////////////////////
!
!     ----------------------------------------------------------------
!>    "Read" the "value" of an integer number declared
!!     after an = sign in a inputLine
!     ----------------------------------------------------------------
!
      FUNCTION GetIntArray( inputLine ) RESULT(N)
         IMPLICIT NONE
!
         INTEGER, DIMENSION(3) :: N
         
         CHARACTER ( LEN = * ) :: inputLine
         INTEGER               :: cStart, cEnd, k
!
         cStart = INDEX(inputLine,'[')
         cEnd   = INDEX(inputLine, ']', .true. )
         READ( inputLine( cStart+1: cEnd-1 ), *, IOSTAT = k ) N(1), N(2), N(3)
         IF ( k /=0 )     THEN
            PRINT *, "Bad integer array value in input line:"
            PRINT *, TRIM(inputLine)
            PRINT *, "Syntax is: [integer,integer,integer]"
            STOP "Input file synax error"
         END IF 
!
      END FUNCTION GetIntArray
!
!///////////////////////////////////////////////////////////////////////
!
!     ----------------------------------------------------------------
!>    Extracts the string within the quotes in an input file
!     ----------------------------------------------------------------
!
      CHARACTER( LEN=LINE_LENGTH ) FUNCTION GetStringValue( inputLine )
         USE ProgramGlobals
         IMPLICIT NONE
!
         CHARACTER ( LEN = * ) :: inputLine
         INTEGER               :: cStart, cEnd
!
         cStart = INDEX(inputLine,'"')
         cEnd   = INDEX(inputLine, '"', .true. )
         GetStringValue = inputLine( cStart+1: cEnd-1 )
!
      END FUNCTION GetStringValue
!
!///////////////////////////////////////////////////////////////////////
!
!     ----------------------------------------------------------------
!>    "Read" the "value" of an logical declared
!!     after an = sign in a inputLine
!     ----------------------------------------------------------------
!
      LOGICAL FUNCTION GetLogicalValue( inputLine )
         IMPLICIT NONE
!
         CHARACTER ( LEN = * ) :: inputLine
         LOGICAL               :: value
         INTEGER               :: strLen, leq, k
!
         leq    = INDEX( inputLine, '=' )
         strLen = LEN_TRIM( inputLine )
         READ( inputLine( leq+1:strLen ), *, IOSTAT = k ) value
         GetLogicalValue = value
         IF ( k /=0 )     THEN
            PRINT *, "Bad logical value in input line:"
            PRINT *, TRIM(inputLine)
            STOP "Input file synax error"
         END IF 
!
      END FUNCTION GetLogicalValue
!
! /////////////////////////////////////////////////////////////////////
!
!     ----------------------------------------------------------------
!>    This subroutine returns an unused
!!    unit number. It is assumed that the unit number will be used
!!    immediately after it is assigned. It is also assumed that the
!!    compiler has access to units 1-99. The function returns "0"
!!    if it cannot find an unused unit number.
!     ----------------------------------------------------------------
!
      INTEGER FUNCTION UnusedUnit()
!
         IMPLICIT NONE
         INTEGER :: j
         LOGICAL :: unitIsOpened, unitDoesExist
         
         UnusedUnit = 0
         DO j = 1, 99
           INQUIRE( UNIT = j, OPENED = unitIsOpened, EXIST = unitDoesExist )
           IF ( .NOT.unitIsOpened ) EXIT
         END DO
         
         IF (  j <= 99 .AND. unitDoesExist )     THEN
            UnusedUnit = j
         ELSE
            UnusedUnit = 0
         END IF
!
      END FUNCTION UnusedUnit
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION StdInFileUnitCopy( ) RESULT(fUnit)
!
!     ----------------------------------------------------
!     Since StdIn cannot be rewound, make a temporary file
!     ----------------------------------------------------
!
         IMPLICIT NONE
         INTEGER, EXTERNAL  :: UnusedUnit
         CHARACTER(LEN=132) :: inputLine
         
         fUnit = UnusedUnit( )
         OPEN(UNIT=fUnit, STATUS="scratch")
         
         DO
           READ ( 5, FMT = '(a132)', END = 1000 ) inputLine
           IF( INDEX(inputline, "\end{FILE}") /= 0 )     EXIT
           WRITE( fUnit,  FMT = '(a132)' ) inputLine
         END DO
 1000 CONTINUE 
      rewind(fUnit)
      END FUNCTION StdInFileUnitCopy
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ConvertToPath(str)  
         IMPLICIT NONE  
         CHARACTER(LEN=*) :: str
         INTEGER          :: lc
         
         lc = LEN_TRIM(str)
         IF ( str(lc-1:lc-1) == "/" )     THEN
            str = str(1:lc-1)
         ELSE
            str = str(1:lc-1) // "/"  
         END IF 
          
      END SUBROUTINE ConvertToPath
      

!
!////////////////////////////////////////////////////////////////////////
!
!      CommandLineReader.f90
!      Created: September 13, 2012 11:12 AM 
!      By: David Kopriva  
!
!      A module for reading command line arguments
!
!      Usage
!         Command line arguments are one of two forms, with no
!         value or with a value, as used in the following example:
!         
!         a.out -version -inputFileName f.txt
!
!         For arguments with or without a value, test for presence with
!
!            LOGICAL present
!            present = CommandLineArgumentIsPresent("-version")
!
!         Any shortened form of the argument can also be used:
!
!            present = CommandLineArgumentIsPresent("-v")
!
!         To get arguments with a value, use
!
!            i = IntegerValueForArgument("-np")
!            r = RealValueForArgument("-np")
!            d = DoubleValueForArgument("-np")
!            l = LogicalValueForArgument("-np")
!            
!          depending on the TYPE of the argument. If there is no such value
!          HUGE(TYPE) is returned, or .false. for a logical.
!
!         Alternatively, test for presence of the argument and 
!         immediately get its value. Only one value can be retrieved
!         for each call to CommandLineArgumentIsPresent.
!
!            IF ( CommandLineArgumentIsPresent("-np") )     THEN
!               PRINT *, "np = ", IntegerValueForLastArgument()
!            END IF
!
!         The available functions are:
!
!            IntegerValueForLastArgument()
!            RealValueForLastArgument()
!            DoubleValueForLastArgument()
!            LogicalValueForLastArgument()
!            StringValueForLastArgument()
!
!////////////////////////////////////////////////////////////////////////
!
      Module CommandLineReader 
      IMPLICIT NONE
      
      INTEGER, PARAMETER :: COMMAND_LINE_ARGUMENT_LENGTH = 256
      INTEGER, PRIVATE   :: lastArgumentID
      
      PUBLIC  :: CommandLineArgumentIsPresent, IntegerValueForArgument, &
                 RealValueForArgument, DoubleValueForArgument,          &
                 LogicalValueForArgument, IntegerValueForLastArgument,  &
                 RealValueForLastArgument, DoubleValueForLastArgument,  &
                 LogicalValueForLastArgument

      PRIVATE :: IntegerFromString, RealFromString, LogicalFromString, &
                 DoubleFromString
!
!     ========
      CONTAINS
!     ========
!
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION CommandLineArgumentIsPresent(argument) 
        IMPLICIT NONE
!
!       ---------
!       Arguments
!       ---------
!
        CHARACTER(LEN=*) :: argument
!
!       ---------------
!       Local Variables
!       ---------------
!
        INTEGER                                     :: i
        CHARACTER(LEN=COMMAND_LINE_ARGUMENT_LENGTH) :: arg
        INTEGER                                     :: argumentLength, substringLength
!        
        lastArgumentID               = 0
        CommandLineArgumentIsPresent = .false.
        IF(COMMAND_ARGUMENT_COUNT() == 0)   RETURN
        
        i = 0
        DO 
          CALL GET_COMMAND_ARGUMENT(i, arg, LENGTH = argumentLength)
          IF (argumentLength == 0) EXIT
          
          substringLength = MIN(argumentLength,LEN_TRIM(argument))
          IF ( TRIM(arg) == TRIM(argument) .OR.&
               TRIM(arg) == argument(1:substringLength) )      THEN
             CommandLineArgumentIsPresent = .true.
             lastArgumentID = i
             RETURN
          END IF          
          i = i+1
        END DO
        
      END FUNCTION CommandLineArgumentIsPresent
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION IntegerValueForArgument(argument) 
!
!     -------------------------------------------------------------------------
!     Returns the value of the argument from the next item in the argument list
!     as an integer. If there is no such argument, return HUGE(Integer)
!     -------------------------------------------------------------------------
!
        IMPLICIT NONE 
        CHARACTER(LEN=*)                            :: argument
        CHARACTER(LEN=COMMAND_LINE_ARGUMENT_LENGTH) :: stringValue
        
        stringValue = StringValueForArgument(argument)
        IF ( stringValue == "" )     THEN
           IntegerValueForArgument = HUGE(IntegerValueForArgument)
        ELSE
           IntegerValueForArgument = IntegerFromString(stringValue)
        END IF
        
      END FUNCTION IntegerValueForArgument 
!
!////////////////////////////////////////////////////////////////////////
!
      REAL FUNCTION RealValueForArgument(argument) 
!
!     -------------------------------------------------------------------------
!     Returns the value of the argument from the next item in the argument list
!     as an integer. If there is no such argument, return HUGE(Real)
!     -------------------------------------------------------------------------
!
        IMPLICIT NONE 
        CHARACTER(LEN=*)                            :: argument
        CHARACTER(LEN=COMMAND_LINE_ARGUMENT_LENGTH) :: stringValue
        
        stringValue = StringValueForArgument(argument)
        IF ( stringValue == "" )     THEN
           RealValueForArgument = HUGE(RealValueForArgument)
        ELSE
           RealValueForArgument = RealFromString(stringValue)
        END IF
        
      END FUNCTION RealValueForArgument 
!
!////////////////////////////////////////////////////////////////////////
!
      DOUBLE PRECISION FUNCTION DoubleValueForArgument(argument) 
!
!     -------------------------------------------------------------------------
!     Returns the value of the argument from the next item in the argument list
!     as an integer. If there is no such argument, return HUGE(double precision)
!     -------------------------------------------------------------------------
!
        IMPLICIT NONE 
        CHARACTER(LEN=*)                            :: argument
        CHARACTER(LEN=COMMAND_LINE_ARGUMENT_LENGTH) :: stringValue
        
        stringValue = StringValueForArgument(argument)
        IF ( stringValue == "" )     THEN
           DoubleValueForArgument = HUGE(DoubleValueForArgument)
        ELSE
           DoubleValueForArgument = DoubleFromString(stringValue)
        END IF
        
      END FUNCTION DoubleValueForArgument
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION LogicalValueForArgument(argument) 
!
!     -------------------------------------------------------------------------
!     Returns the value of the argument from the next item in the argument list
!     as an integer. If there is no such argument, return .false.
!     -------------------------------------------------------------------------
!
        IMPLICIT NONE 
        CHARACTER(LEN=*)                            :: argument
        CHARACTER(LEN=COMMAND_LINE_ARGUMENT_LENGTH) :: stringValue
        
        stringValue = StringValueForArgument(argument)
        IF ( stringValue == "" )     THEN
           LogicalValueForArgument = .false.
        ELSE
           LogicalValueForArgument = LogicalFromString(stringValue)
        END IF
        
      END FUNCTION LogicalValueForArgument 
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION IntegerValueForLastArgument() 
!
!     -------------------------------------------------------------------------
!     Returns the value of the argument from the next item in the argument list
!     as an integer. If there is no such argument, return HUGE(Integer)
!     -------------------------------------------------------------------------
!
        IMPLICIT NONE 
        CHARACTER(LEN=COMMAND_LINE_ARGUMENT_LENGTH) :: stringValue
        
        stringValue = StringValueForLastArgument()
        IF ( stringValue == "" )     THEN
           IntegerValueForLastArgument = HUGE(IntegerValueForLastArgument)
        ELSE
           IntegerValueForLastArgument = IntegerFromString(stringValue)
        END IF
        
      END FUNCTION IntegerValueForLastArgument 
!
!////////////////////////////////////////////////////////////////////////
!
      REAL FUNCTION RealValueForLastArgument() 
!
!     -------------------------------------------------------------------------
!     Returns the value of the argument from the next item in the argument list
!     as an integer. If there is no such argument, return HUGE(Real)
!     -------------------------------------------------------------------------
!
        IMPLICIT NONE 
        CHARACTER(LEN=COMMAND_LINE_ARGUMENT_LENGTH) :: stringValue
        
        stringValue = StringValueForLastArgument()
        IF ( stringValue == "" )     THEN
           RealValueForLastArgument = HUGE(RealValueForLastArgument)
        ELSE
           RealValueForLastArgument = RealFromString(stringValue)
        END IF
        
      END FUNCTION RealValueForLastArgument 
!
!////////////////////////////////////////////////////////////////////////
!
      DOUBLE PRECISION FUNCTION DoubleValueForLastArgument() 
!
!     -------------------------------------------------------------------------
!     Returns the value of the argument from the next item in the argument list
!     as an integer. If there is no such argument, return HUGE(double precision)
!     -------------------------------------------------------------------------
!
        IMPLICIT NONE 
        CHARACTER(LEN=COMMAND_LINE_ARGUMENT_LENGTH) :: stringValue
        
        stringValue = StringValueForLastArgument()
        IF ( stringValue == "" )     THEN
           DoubleValueForLastArgument = HUGE(DoubleValueForLastArgument)
        ELSE
           DoubleValueForLastArgument = DoubleFromString(stringValue)
        END IF
        
      END FUNCTION DoubleValueForLastArgument
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION LogicalValueForLastArgument() 
!
!     -------------------------------------------------------------------------
!     Returns the value of the argument from the next item in the argument list
!     as an integer. If there is no such argument, return .false.
!     -------------------------------------------------------------------------
!
        IMPLICIT NONE 
        CHARACTER(LEN=COMMAND_LINE_ARGUMENT_LENGTH) :: stringValue
        
        stringValue = StringValueForLastArgument()
        IF ( stringValue == "" )     THEN
           LogicalValueForLastArgument = .false.
        ELSE
           LogicalValueForLastArgument = LogicalFromString(stringValue)
        END IF
        
      END FUNCTION LogicalValueForLastArgument 
!
!////////////////////////////////////////////////////////////////////////
!
      CHARACTER(LEN=COMMAND_LINE_ARGUMENT_LENGTH) FUNCTION StringValueForArgument(argument) 
      IMPLICIT NONE 
!
!       ---------
!       Arguments
!       ---------
!
        CHARACTER(LEN=*) :: argument
!
!       ---------------
!       Local Variables
!       ---------------
!
        INTEGER                                     :: i
        INTEGER                                     :: argumentLength
        CHARACTER(LEN=COMMAND_LINE_ARGUMENT_LENGTH) :: arg
        
        StringValueForArgument = ""
        IF(COMMAND_ARGUMENT_COUNT() == 0)   RETURN
!
        lastArgumentID = 0
        i              = 0
        DO 
          CALL GET_COMMAND_ARGUMENT(i, arg, argumentLength)
          IF (argumentLength== 0) EXIT
          
          IF ( TRIM(arg) == TRIM(argument) .OR.&
               TRIM(arg) == argument(1:argumentLength) )      THEN
             CALL GET_COMMAND_ARGUMENT(i+1, StringValueForArgument)
             RETURN
          END IF          
          i = i+1
        END DO
        
      END FUNCTION StringValueForArgument
!
!////////////////////////////////////////////////////////////////////////
!
      CHARACTER(LEN=256) FUNCTION StringValueForLastArgument() 
        IMPLICIT NONE 
!
!       ---------------
!       Local Variables
!       ---------------
!        
        StringValueForLastArgument = ""
        IF(COMMAND_ARGUMENT_COUNT() == 0)   RETURN
!
!       ----------------------------------------------------------------
!       If the IsPresent function has been called and found the argument
!       then we don't need to search.
!       ----------------------------------------------------------------
!
        IF ( lastArgumentID /= 0 )     THEN
           CALL GET_COMMAND_ARGUMENT(lastArgumentID + 1, StringValueForLastArgument)
           lastArgumentID = 0
           RETURN
        END IF
        
      END FUNCTION StringValueForLastArgument
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION IntegerFromString(s) RESULT(i)
         IMPLICIT NONE 
         CHARACTER(LEN=*) :: s
         INTEGER          :: i
         READ(s,*) i
      END FUNCTION IntegerFromString 
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION DoubleFromString(s) RESULT(d)
         IMPLICIT NONE 
         CHARACTER(LEN=*)                  :: s
         REAL(KIND=SELECTED_REAL_KIND(15)) :: d
         READ(s,*) d
      END FUNCTION DoubleFromString 
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION RealFromString(s) RESULT(r)
         IMPLICIT NONE 
         CHARACTER(LEN=*)                 :: s
         REAL(KIND=SELECTED_REAL_KIND(6)) :: r
         READ(s,*) r
      END FUNCTION RealFromString 
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION LogicalFromString(s) RESULT(l)
         IMPLICIT NONE 
         CHARACTER(LEN=*) :: s
         LOGICAL          :: l
         READ(s,*) l
      END FUNCTION LogicalFromString 
      
      END Module CommandLineReader 

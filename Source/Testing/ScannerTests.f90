!
!////////////////////////////////////////////////////////////////////////
!
!      ScannerTests.f90
!      Created: March 28, 2026 at 10:41 AM 
!      By: David Kopriva  
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE ScannerTests
      USE SMScannerClass
      USE ScanningModule
      USE FTAssertions
      IMPLICIT NONE
      
      CALL FTAssert(scanTest(),msg = "Scan test failure")
      CALL FTAssert(scanUpToTest(),msg = "Scan up to test failure")
      CALL FTAssert(scanArrayTest(),msg = "Scan array test failure")
      CALL FTAssert(ScanForBreaksIsOK(),msg = "Scan for breaks test failure")
      CALL FTAssert(flaggingIsOK(),msg = "Scan for flagging test failure")

      CALL test_connectFormatCheck()
      CALL test_ScanForBreaksOverlaps()
      
   END SUBROUTINE ScannerTests
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE test_connectFormatCheck()
        USE SMScannerClass
        USE ScanningModule
        USE FTAssertions
        IMPLICIT NONE
!
!       -----------
!       Should pass
!       -----------
!
        CALL FTAssert(connectFormatCheck('1-10')      ,msg = 'CONNECT FORMAT TEST FAILED: 1-10')
        CALL FTAssert(connectFormatCheck('1-10,20-30'),msg = 'CONNECT FORMAT TEST FAILED: 1-10,20-30')
        CALL FTAssert(connectFormatCheck('1-10,20-30'),msg = 'CONNECT FORMAT TEST FAILED: 1-10,20-30,40-5')
        CALL FTAssert(connectFormatCheck('0-1')       ,msg = 'CONNECT FORMAT TEST FAILED: 0-1')
!
!       -----------
!       Should fail
!       -----------
!
        CALL FTAssert(.NOT.connectFormatCheck('1-10,30-20'),msg = 'CONNECT FORMAT TEST FAILED: 1-10,30-20 invalid second range')
        CALL FTAssert(.NOT.connectFormatCheck('10-1')        ,msg = 'CONNECT FORMAT TEST FAILED: 10-1 should be invalid')
        CALL FTAssert(.NOT.connectFormatCheck('1')          ,msg = 'CONNECT FORMAT TEST FAILED: missing second integer')
        CALL FTAssert(.NOT.connectFormatCheck('1-')         ,msg = 'CONNECT FORMAT TEST FAILED: missing second integer')
        CALL FTAssert(.NOT.connectFormatCheck('-10')        ,msg = 'CONNECT FORMAT TEST FAILED: negative integer')
        CALL FTAssert(.NOT.connectFormatCheck('1-10,')      ,msg = 'CONNECT FORMAT TEST FAILED: trailing comma')
        CALL FTAssert(.NOT.connectFormatCheck('1-10,,20-30'),msg = 'CONNECT FORMAT TEST FAILED: consecutive commas')
        CALL FTAssert(.NOT.connectFormatCheck('1-10,abc-20'),msg = 'CONNECT FORMAT TEST FAILED: non-numeric value')
        CALL FTAssert(.NOT.connectFormatCheck('')           ,msg = 'CONNECT FORMAT TEST FAILED: empty string')
      
      END SUBROUTINE test_connectFormatCheck
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE test_ScanForBreaksOverlaps()
!
!     ------------------------------------------------------------------
!     Connect strings that pass connectFormatCheck but flag a joint more
!     than once or reach past the ends of the chain. The breaks must
!     always start at 0, end at 1 and skip only the connected joints.
!     ------------------------------------------------------------------
!
        USE SMConstants
        USE ScanningModule
        USE FTAssertions
        IMPLICIT NONE

        CALL CheckBreaks("1-2,1-2", 3, [0.0_RP, 2.0_RP/3.0_RP, 1.0_RP])
        CALL CheckBreaks("1-3,2-3", 3, [0.0_RP, 1.0_RP])
        CALL CheckBreaks("2-9"    , 3, [0.0_RP, 1.0_RP/3.0_RP, 1.0_RP])
        CALL CheckBreaks("1-10"   , 3, [0.0_RP, 1.0_RP])
        CALL CheckBreaks("0-1"    , 4, [0.0_RP, 0.25_RP, 0.5_RP, 0.75_RP, 1.0_RP])
        CALL CheckBreaks("2-3"    , 4, [0.0_RP, 0.25_RP, 0.75_RP, 1.0_RP])

      CONTAINS

        SUBROUTINE CheckBreaks(connectString, nSegments, expected)
           IMPLICIT NONE
           CHARACTER(LEN=*)           :: connectString
           INTEGER                    :: nSegments
           REAL(KIND=RP)              :: expected(:)
           REAL(KIND=RP), ALLOCATABLE :: breaks(:)
           LOGICAL                    :: same

           CALL ScanForBreaks(connectString, breaks, nSegments)
           same = SIZE(breaks) == SIZE(expected)
           IF ( same )     same = MAXVAL(ABS(breaks - expected)) <= 4.0_RP*EPSILON(1.0_RP)
           CALL FTAssert(test = same, msg = "Breaks for connect = " // connectString)
        END SUBROUTINE CheckBreaks

      END SUBROUTINE test_ScanForBreaksOverlaps

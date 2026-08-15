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

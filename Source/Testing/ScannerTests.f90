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

   END SUBROUTINE ScannerTests

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
      USE FTAssertions
      IMPLICIT NONE
      
      CALL FTAssert(scanTest(),msg = "Scan test failure")
      CALL FTAssert(scanUpToTest(),msg = "Scan up to test failure")
      CALL FTAssert(scanArrayTest(),msg = "Scan array test failure")

   END SUBROUTINE ScannerTests

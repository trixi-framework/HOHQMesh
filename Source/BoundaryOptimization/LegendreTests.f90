!
!////////////////////////////////////////////////////////////////////////
!
!      LegendreTests.f90
!      Created: November 26, 2025 at 11:07 AM 
!      By: David Kopriva  
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE LegendreApproximationTests 
      USE LegendreAlgorithms
      USE FTAssertions
      IMPLICIT NONE
      
      CALL FTAssert(legendreQuadratureIsOK(),msg = "Legendre quadrature")
      CALL FTAssert(gaussInnerProductIsOK(),msg = "Gauss quadrature")
      CALL FTAssert(legendreCoefsForTestAreOK(),msg = "Legendre coefficients")
      CALL FTAssert(legendreSeriesIsOK(),msg = "Legendre series")
      
   END SUBROUTINE LegendreApproximationTests

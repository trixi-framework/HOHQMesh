!
!////////////////////////////////////////////////////////////////////////
!
!      GaussQuadrature.f90
!      Created: November 12, 2025 at 11:46 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   Module GaussQuadratureModule
   USE LegendreAlgorithms
   IMPLICIT NONE  
   
   TYPE GaussQuadratureType
      INTEGER                    :: N
      REAL(KIND=RP), ALLOCATABLE :: nodes(:), weights(:)
   END TYPE GaussQuadratureType
   
   CONTAINS     
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE ConstructGaussQuadrature(self, N)  
      IMPLICIT NONE
      TYPE(GaussQuadratureType) :: self
      INTEGER                   :: N
      
      self % N = N
      ALLOCATE(self % nodes(0:N), self % weights(0:N))
 
   END SUBROUTINE ConstructGaussQuadrature
   
   END Module GaussQuadratureModule

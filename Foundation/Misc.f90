!
!////////////////////////////////////////////////////////////////////////
!
!      Misc.f90
!      Created: September 5, 2012 5:18 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION Loop(k,N) 
         IMPLICIT NONE
         INTEGER k, N
         Loop = k
         IF( Loop > N )     Loop = Loop - N
         IF( Loop < 1 )     Loop = Loop + N
      END FUNCTION Loop
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION Norm2(u) 
         USE SMConstants
         IMPLICIT NONE
         REAL(KIND=RP) :: u(3)
         REAL(KIND=RP) :: Norm2
         norm2 = SQRT( u(1)**2 + u(2)**2 )
      END FUNCTION Norm2
!
!////////////////////////////////////////////////////////////////////////
! 
      INTEGER FUNCTION offsetForIandJ(i,j,N)
!
!     --------------------------------
!     Computes the function 
!     id = i + j*(N+1)
!     for two dimensional array access
!     --------------------------------
!
         IMPLICIT NONE  
         INTEGER :: N, i, j
         offsetForIandJ = i + j*(N+1)
      END FUNCTION offsetForIandJ

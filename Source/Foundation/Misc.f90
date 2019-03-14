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

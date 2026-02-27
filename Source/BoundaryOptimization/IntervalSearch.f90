!
!////////////////////////////////////////////////////////////////////////
!
!      IntervalSearch.f90
!      Created: February 27, 2026 at 8:19 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   Module IntervalSearchModule
      USE ProgramGlobals
      IMPLICIT NONE  

!
!     ========
      CONTAINS  
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION findInterval(array, t) RESULT(s)
!
!     --------------------------------------------------
!     Given the parametric location t, find the interval
!     in which that value of t appears.
!     --------------------------------------------------
!
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP)                 :: array(0:)
         REAL(KIND=RP)                 :: t
         INTEGER                       :: s
 !
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER       :: nSegments
         INTEGER       :: j, lft = 0, rgt, mid
         SAVE          :: lft
         
         nSegments = SIZE(array) - 1
!
!        ------------
!        Find segment
!        ------------
!
         IF( lft >= nSegments ) lft= 0
         IF ( t > array(lft) .AND. t < array(lft+1) )     THEN
            s = lft+1 
         ELSE 
            rgt = nSegments
            lft = 0
            DO j = 1, nSegments 
              mid = (lft+ rgt)/2
              IF ( t < array(mid) )     THEN
                  rgt = mid 
               ELSE 
                  lft = mid 
               END IF 
               IF(rgt-lft == 1) EXIT
            END DO 
            s = lft+1
         END IF 
        
      END FUNCTION findInterval
!
!//////////////////////////////////////////////////////////////////////// 
! 
      REAL(KIND=RP) FUNCTION segmentLength(array, t)  
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP) :: array(0:)
         REAL(KIND=RP) :: t
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER       :: k
         
         k             = findInterval(array, t)
         segmentLength = array(k) - array(k-1)
         
      END FUNCTION segmentLength
 
   END Module IntervalSearchModule

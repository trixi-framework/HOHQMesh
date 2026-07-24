! MIT License
!
! Copyright (c) 2010-present David A. Kopriva and other contributors: AUTHORS.md
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
! FTObjectLibrary contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend,
! https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
!
! --- End License
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
      REAL(KIND=RP) FUNCTION searchArrayForValue(searchArray, array, t)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP) :: searchArray(0:)
         REAL(KIND=RP) :: array(:)
         REAL(KIND=RP) :: t
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER       :: k

         k                   = findInterval(searchArray, t)
         searchArrayForValue = array(k)

      END FUNCTION searchArrayForValue

   END Module IntervalSearchModule

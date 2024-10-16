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
! --- End License
!
!////////////////////////////////////////////////////////////////////////
!
!      FTTimerClass.f90
!      Created: September 2, 2013 1:58 PM 
!      By: NocturnalAviationSoftware  
!
!      Defines a class for timing Fortran program
!      execution
!      
!      Usage
!
!
!         * Starting the timer *
!
!               CALL timer % start()
!
!         * Stopping the timer *
!
!               CALL timer % stop()
!
!         * Reading the time *
!
!               time = timer % elapsedTime(units)
!
!           units (optional) = TC_SECONDS or TC_MINUTES or TC_HOURS
!
!////////////////////////////////////////////////////////////////////////
!
      MODULE FTTimerClass 
      IMPLICIT NONE
      PRIVATE
!
!     -----------------
!     Private constants
!     -----------------
!
      INTEGER, PARAMETER, PRIVATE :: d = 15
!
!     ----------------
!     Public constants
!     ----------------
!
      INTEGER, PARAMETER, PUBLIC  :: TP = SELECTED_REAL_KIND(d)
      INTEGER, PARAMETER, PUBLIC  :: TC_SECONDS = 0, TC_MINUTES = 1, TC_HOURS = 2
!
!     ---------------------
!     Class type definition
!     ---------------------
!
      TYPE, PUBLIC :: FTTimer
         LOGICAL      , PRIVATE :: started = .FALSE., stopped = .FALSE.
         REAL(KIND=TP), PRIVATE :: startTime  = 0.0_TP
         REAL(KIND=TP), PRIVATE :: finishTime = 0.0_TP
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE, PASS :: start => startTimer
         PROCEDURE, PASS :: stop  => stopTimer
         PROCEDURE, PASS :: elapsedTime
         
      END TYPE FTTimer
!
!     ========
      CONTAINS
!     ========
! 
!
!////////////////////////////////////////////////////////////////////////  
!  
      SUBROUTINE startTimer(self)  
         IMPLICIT NONE
         CLASS(FTTimer) :: self
         self % started = .TRUE.
         CALL CPU_TIME(self % startTime)         
      END SUBROUTINE startTimer
!
!////////////////////////////////////////////////////////////////////////  
!  
      SUBROUTINE stopTimer(self)  
         IMPLICIT NONE
         CLASS(FTTimer) :: self
         CALL CPU_TIME(self % finishTime)
         self % stopped = .TRUE.
      END SUBROUTINE stopTimer
!
!//////////////////////////////////////////////////////////////////////// 
! 
       FUNCTION elapsedTime(self,units)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTTimer)    :: self
         INTEGER, OPTIONAL :: units
         REAL(KIND=TP)  :: elapsedTime
!
!        ------------------------------------------
!        Return zero if the timer was never started
!        ------------------------------------------
!
         IF ( .NOT.self % started )     THEN
            elapsedTime = 0.0_TP
            RETURN
         END IF 
!
!        ----------------------------------------------
!        If the timer was not stopped, then return the 
!        current time elapsed
!        ----------------------------------------------
!
         IF ( .NOT.self % stopped )     THEN
            CALL self % stop() 
         END IF 

         elapsedTime =  self % finishTime - self % startTime
!
!        -------------------------------------
!        Convert to requested units if present
!        -------------------------------------
!
         IF ( PRESENT(units) )     THEN
         
            SELECT CASE ( units )
               CASE( TC_MINUTES ) 
                  elapsedTime = elapsedTime/60.0_TP
               CASE( TC_HOURS )
                  elapsedTime = elapsedTime/3600.0_TP
               CASE DEFAULT 
               
            END SELECT 
         END IF 
      
      END FUNCTION elapsedTime
      
      END MODULE FTTimerClass
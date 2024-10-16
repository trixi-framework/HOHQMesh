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

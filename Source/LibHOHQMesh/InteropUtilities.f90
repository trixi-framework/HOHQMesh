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
! HOHQMesh contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend, 
!    https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
! * `fmin`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
! * `spline`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
! * `seval`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
!
! --- End License
!
!////////////////////////////////////////////////////////////////////////
!
!      InteropUtilities.f90
!      Created: June 17, 2021 at 3:46 PM 
!      By: David Kopriva  
!
!      SUBROUTINE C_string_ptr_to_F_string(C_string, F_string)
!      FUNCTION   f_to_c_string(f_string) RESULT(c_string)
!      SUBROUTINE f_to_c_stringSub(fString, cString, cStrLen)
!      FUNCTION   f_to_c_string(f_string) RESULT(c_string)
!
!////////////////////////////////////////////////////////////////////////
!
      Module InteropUtilitiesModule 
      use ISO_C_BINDING
      USE HMLConstants
      IMPLICIT NONE
!
!     --------
      CONTAINS
!     --------
!
!     C_string_ptr_to_F_string is from 
!     http://fortranwiki.org/fortran/show/c_interface_module
!     by Joseph M. Krahn
!
      SUBROUTINE C_string_ptr_to_F_string(C_string, F_string)
          type(C_PTR), intent(in) :: C_string
          character(len=*), intent(out) :: F_string
          character(len=1, kind=C_CHAR), dimension(:), pointer :: p_chars
          integer :: i
          if (.not. C_associated(C_string)) then
            F_string = ' '
          else
            call C_F_pointer(C_string, p_chars, [huge(0)])
            do i = 1, len(F_string)
              if (p_chars(i) == C_NULL_CHAR) exit
              F_string(i:i) = p_chars(i)
            end do
            if (i <= len(F_string)) F_string(i:) = ' '
          end if
      end SUBROUTINE C_string_ptr_to_F_string

      ! Sources:
      ! * https://community.intel.com/t5/Intel-Fortran-Compiler/Converting-c-string-to-Fortran-string/m-p/959515/highlight/true#M94338
      ! * https://stackoverflow.com/a/11443635/1329844
      ! * http://fortranwiki.org/fortran/show/c_interface_module
      FUNCTION c_to_f_string(c_string) RESULT(f_string)
         IMPLICIT NONE  
         CHARACTER(kind=c_char), dimension(*), intent(in) :: c_string
         CHARACTER(len=:)      , allocatable              :: f_string
         INTEGER                                          ::  i, nchars
!
!        ----------------------------------------------------------------
!        Figure out length of C string by counting characters before NULL
!        ----------------------------------------------------------------
!
         i = 1
         do
           if (c_string(i) == c_null_char) exit
           i = i + 1
         end do
         nchars = i - 1  ! Exclude null character from Fortran string
         allocate(character(len=nchars) :: f_string)

         do i=1,nchars
           f_string(i:i) = c_string(i)
         end do
      END FUNCTION c_to_f_string
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION f_to_c_string(f_string) RESULT(c_string)
         IMPLICIT NONE 
         CHARACTER(LEN=*)                                  :: f_string
         CHARACTER(KIND=c_char), DIMENSION(:), ALLOCATABLE :: c_string
         INTEGER                                           :: i, nchars
         
         nchars = LEN_TRIM(f_string)
         ALLOCATE(c_string(nchars+1))
         DO i = 1, nchars 
            c_string(i) = f_string(i:i) 
         END DO 
         c_string(nchars+1) = c_null_char
      END FUNCTION f_to_c_string
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE f_to_c_stringSub(fString, cString, cStrLen)
         IMPLICIT NONE 
         CHARACTER(LEN=*)      , INTENT(IN)                  :: fString
         CHARACTER(KIND=c_char), DIMENSION(*), INTENT(OUT)   :: cString
         INTEGER(C_INT)                      , INTENT(INOUT) :: cStrLen
         ! On input, CStrLen is the buffer size, on output, it is the 
         ! actual length of the string, including the null terminator
         INTEGER                                             :: i, nchars
       
         nchars = LEN_TRIM(fString)
         DO i = 1, nchars 
            cString(i) = fString(i:i) 
         END DO 
         cString(nchars+1) = c_null_char
      END SUBROUTINE f_to_c_stringSub

   END Module InteropUtilitiesModule

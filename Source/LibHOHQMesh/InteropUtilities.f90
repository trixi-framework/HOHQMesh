!
!////////////////////////////////////////////////////////////////////////
!
!      InteropUtilities.f90
!      Created: June 17, 2021 at 3:46 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module InteropUtilitiesModule 
      use ISO_C_BINDING
      USE MeshProjectClass
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
      subroutine C_string_ptr_to_F_string(C_string, F_string)
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
      end SUBROUTINE      
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION IsMeshProjectPtr(ptr)
         IMPLICIT NONE  
         CLASS ( MeshProject ), POINTER :: ptr
         SELECT TYPE (p => ptr)
            TYPE IS(MeshProject)
               IsMeshProjectPtr = .TRUE.
            CLASS DEFAULT
               IsMeshProjectPtr = .FALSE.
         END SELECT
      END FUNCTION IsMeshProjectPtr

      END Module InteropUtilitiesModule

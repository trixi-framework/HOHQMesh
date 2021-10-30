!
!////////////////////////////////////////////////////////////////////////
!
!      HMLConstants.f90
!      Created: June 21, 2021 at 12:01 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module HMLConstants 
      IMPLICIT NONE
!
!  -----------
!  Error Codes
!  -----------
!
!> Error Flag for no error
   INTEGER, PARAMETER :: HML_ERROR_NONE = 0 
!
!> Error Flag when a project has more than one reference and cannot be deallocated
   INTEGER, PARAMETER :: HML_ERROR_MULTIPLE_REFERENCES = 1
!
!> Error Flag if for some reason a MeshProject cannot be deallocated
   INTEGER, PARAMETER :: HML_ERROR_DEALLOCATION = 2  
!
!> Error Flag if a routine is passed a c_ptr that doesn't resolve to a MeshProject
   INTEGER, PARAMETER :: HML_ERROR_NOT_A_PROJECT = 3  
!
!> Error Flag if a memory passed by a caller is not sufficiently large
   INTEGER, PARAMETER :: HML_ERROR_MEMORY_SIZE = 4  
!
!> Error Flag if an accessor is called before data needed for it is available
!> (e.g. asking for node locations before a mesh is allocated)
   INTEGER, PARAMETER :: HML_ERROR_NO_OBJECT_FOR_REQUEST = 5  
!
!> Error Flag if a pointer being passed is null
   INTEGER, PARAMETER :: HML_ERROR_NULL_POINTER = 6  
!
!> Error Flag if a string has been truncated
   INTEGER, PARAMETER :: HML_ERROR_STRING_TRUNCATED = 7  
!
!> Error Flag if a routine is passed a c_ptr that doesn't resolve to a FTDictionary
   INTEGER, PARAMETER :: HML_ERROR_NOT_A_DICT = 8  
!
!> Error Flag if a routine is passed a c_ptr that doesn't resolve to a FTLinkedList
   INTEGER, PARAMETER :: HML_ERROR_NOT_A_LIST = 9 

      END Module HMLConstants

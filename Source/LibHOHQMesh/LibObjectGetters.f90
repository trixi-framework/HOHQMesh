!
!////////////////////////////////////////////////////////////////////////
!
!      LibObjectGetters.f90
!      Created: June 17, 2021 at 1:20 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module ProjectInterfaceGetters 
      USE MeshProjectClass
      USE ISO_C_BINDING
      IMPLICIT NONE
!
!     ________
      CONTAINS 
!     --------
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Getter for number of nodes in the mesh
!>
   INTEGER FUNCTION HML_NumberOfNodes(cPtr)   BIND(C)
      IMPLICIT NONE  
      TYPE(c_ptr)                  :: cPtr
      TYPE( MeshProject ), POINTER :: project
      
      CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
      HML_NumberOfNodes = project % mesh % nodes % count()
   END FUNCTION HML_NumberOfNodes
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Getter for number of elements in the mesh
!>
   INTEGER FUNCTION HML_NumberOfElements(cPtr)   BIND(C)
      IMPLICIT NONE  
      TYPE(c_ptr)                  :: cPtr
      TYPE( MeshProject ), POINTER :: project
      
      CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
      HML_NumberOfElements = project % mesh % elements % count()
   END FUNCTION HML_NumberOfElements
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Getter for number of edges in the mesh
!>
   INTEGER FUNCTION HML_NumberOfEdges(cPtr)   BIND(C)
      IMPLICIT NONE  
      TYPE(c_ptr)                  :: cPtr
      TYPE( MeshProject ), POINTER :: project
      
      CALL C_F_POINTER(cPtr = cPtr, FPTR = project)
      HML_NumberOfEdges = project % mesh % edges % count()
   END FUNCTION HML_NumberOfEdges
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE HML_NodeLocations(cPtr, locationsArray, N)  
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)                  :: cPtr
      INTEGER                      :: N
      REAL(KIND=C_DOUBLE)          :: locationsArray(3,N)
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE( MeshProject ), POINTER :: project
      
   END SUBROUTINE HML_NodeLocations
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE HML_2DElementConnectivity(cPtr, connectivityArray, N)  
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)                  :: cPtr
      INTEGER                      :: N
      REAL(KIND=C_DOUBLE)          :: connectivityArray(4,N)
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE( MeshProject ), POINTER :: project
      
   END SUBROUTINE HML_2DElementConnectivity
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE HML_2DEdgeConnectivity(cPtr, connectivityArray, N)  
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)                  :: cPtr
      INTEGER                      :: N
      REAL(KIND=C_DOUBLE)          :: connectivityArray(2,N)
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE( MeshProject ), POINTER :: project
      
   END SUBROUTINE HML_2DEdgeConnectivity
      END Module ProjectInterfaceGetters
! 
!//////////////////////////////////////////////////////////////////////// 
! 
      !^ 
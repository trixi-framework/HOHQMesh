!
!////////////////////////////////////////////////////////////////////////
!
!      HexMeshObjects.f90
!      Created: April 2, 2013 11:55 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module HexMeshObjectsModule 
      USE SMConstants
      USE SMMeshObjectsModule
      IMPLICIT NONE 
!
!     -------------
!     Derived types
!     -------------
!
      TYPE Node3D
         INTEGER       :: globalID
         REAL(KIND=RP) :: x(3)
      END TYPE Node3D
!
!
!----------------------------------------------------------------------+
!                                                                      |
!  ELEMENT GEOMETRY, 8 NODES                                           |
!  ------------------------------------------------------              |
!                                                                      |
!                                                                      |
!                                   4 --------------- 3                |
!                               -                  -                   |
!                            -     |           -     |                 |
!                         -        |        -        |                 |
!                     -       (2)  |     -           |                 |
!                  -               |  -    (3)       |                 |
!                -                 -                 |                 |
!             8 --------------- 7                    |                 |
!                                  |    (4)          |                 |
!             |        (6)      |                                      |
!             |                 |  1 --------------- 2                 |
!             |                 |                 -       ETA (5)      |
!             |       (5)    -  |              -           |           |
!             |           -     |  (1)      -              |           |
!             |        -        |        -                 |   XI (4)  |
!             |     -           |     -                   /-------     |
!                -                 -                     /             |
!             5 --------------- 6                       / ZETA (6)     |
!----------------------------------------------------------------------|
!
      TYPE Hex8Element
          INTEGER                         :: globalID
          INTEGER, DIMENSION(8)           :: nodeIDs
          INTEGER, DIMENSION(6)           :: faceID
          INTEGER                         :: materialID
          CHARACTER(LEN=32)               :: materialName
          INTEGER, DIMENSION(6)           :: bFaceFlag     ! = ON or OFF. On if there is an interpolant associated with the face.
          CHARACTER(LEN=32), DIMENSION(6) :: bFaceName     ! The boundary face name for each face. Will be "---" for internal faces.
          REAL(KIND=RP)    , POINTER      :: x (:,:,:,:)   ! Nodal values of chebyshev interpolant for the volume.
      END TYPE Hex8Element
      
      TYPE StructuredHexMesh
         INTEGER                                    :: numberofLayers, numberOfQuadElements
         TYPE(Node3D)     , DIMENSION(:,:), POINTER :: nodes
         TYPE(Hex8Element), DIMENSION(:,:), POINTER :: elements
      END TYPE StructuredHexMesh
!
!     ========      
      CONTAINS 
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE InitializeStructuredHexMesh( hexMesh, numberOf2DNodes, numberOfEdges, &
                                              numberOfQuadElements, numberOfLayers, N )
!
!     ---------------------------------
!     Allocate memory for new Hex8 mesh
!     ---------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(StructuredHexMesh), POINTER  :: hexMesh
         INTEGER                           :: numberOfLayers
         INTEGER                           :: numberOf2DNodes
         INTEGER                           :: numberOfQuadElements
         INTEGER                           :: numberOfEdges
         INTEGER                           :: N
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER  :: j, k, l, m
         INTEGER  :: elementID
         
         hexMesh % numberofLayers       = numberOfLayers
         hexMesh % numberOfQuadElements = numberOfQuadElements

         ALLOCATE(hexMesh % nodes   ( numberOf2DNodes     , 0:numberOfLayers) )
         ALLOCATE(hexMesh % elements( numberOfQuadElements,   numberOfLayers) )
!
!        ---------------------------
!        Initialize the hex elements
!        ---------------------------
!
         elementID = 1
         DO l = 1, numberOfLayers 
            DO m = 1, numberOfQuadElements 
               hexMesh % elements(m,l) % faceID    = NONE
               hexMesh % elements(m,l) % globalID  = elementID
               hexMesh % elements(m,l) % bFaceFlag = OFF
               hexMesh % elements(m,l) % bFaceName = NO_BC_STRING
               elementID = elementID + 1
               ALLOCATE(hexMesh % elements(m,l) % x(3,0:N,0:N,0:N) )
            END DO 
         END DO 
         
      END SUBROUTINE InitializeStructuredHexMesh
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE DestructStructuredHexMesh(hexMesh)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(StructuredHexMesh)  :: hexMesh
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER :: i, j
         
         DO j = 1, hexMesh % numberofLayers 
            DO i = 1, hexMesh % numberOfQuadElements 
               DEALLOCATE( hexMesh % elements(i,j) % x) 
            END DO 
         END DO 
         DEALLOCATE(hexMesh % elements)
         DEALLOCATE(hexMesh % nodes)
         
      END SUBROUTINE DestructStructuredHexMesh
      
      END Module HexMeshObjectsModule
      
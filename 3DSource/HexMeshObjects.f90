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
      USE MeshProjectClass
      USE FTValueDictionaryClass
      USE SMMeshObjectsModule
      IMPLICIT NONE 
!
!   Faces:
!      1 = Front, 2 = back, 3 = bottom, 4 - right, 5 = top, 6 = left
!
      INTEGER, PARAMETER :: HEX_FRONT = 1, HEX_BACK = 2, HEX_BOTTOM = 3,&
                            HEX_RIGHT = 4, HEX_TOP  = 5, HEX_LEFT   = 6
                            
      INTEGER, PARAMETER :: NODES_PER_FACE    = 4
      INTEGER, PARAMETER :: NODES_PER_ELEMENT = 8
      INTEGER, PARAMETER :: FACES_PER_ELEMENT = 6
      INTEGER, PARAMETER :: EDGES_PER_ELEMENT = 12
!
!-------------------------------------------------------------------------
!  Definition of the local node numbers for the faces of the master element
!--------------------------------------------------------------------------
!
!
      INTEGER, DIMENSION(4,6) :: localFaceNode = &
     &         RESHAPE( (/                    & 
     &         1, 2, 6, 5,                    & ! Node numbers, face 1
     &         4, 3, 7, 8,                    & ! Node numbers, face 2
     &         1, 2, 3, 4,                    & ! Node numbers, face 3
     &         2, 3, 7, 6,                    & ! Node numbers, face 4
     &         5, 6, 7, 8,                    & ! Node numbers, face 5
     &         1, 4, 8, 5                     & ! Node numbers, face 6
     &         /),(/4,6/))
!
!-------------------------------------------------------------------------
!  Definition of the local node numbers for the edges of the master element
!--------------------------------------------------------------------------
!
      INTEGER, DIMENSION(2,12) :: localEdgeNode = &
     &         RESHAPE( (/                   &
     &         1, 2,                &! Node numbers, edge 1
     &         2, 6,                &! Node numbers, edge 2
     &         5, 6,                &! Node numbers, edge 3
     &         1, 5,                &! Node numbers, edge 4
     &         4, 3,                &! Node numbers, edge 5
     &         3, 7,                &! Node numbers, edge 6
     &         8, 7,                &! Node numbers, edge 7
     &         4, 8,                &! Node numbers, edge 8
     &         1, 4,                &! Node numbers, edge 9
     &         2, 3,                &! Node numbers, edge 10
     &         6, 7,                &! Node numbers, edge 11
     &         5, 8                 &! Node numbers, edge 12
     &         /), (/2,12/) )
     
      INTEGER, DIMENSION(4,6) :: localEdgeForFace = &
     &         RESHAPE( (/          &
     &         1,  2,  3, 4 ,       &! edge numbers, face 1
     &         5,  6,  7, 8,        &! edge numbers, face 2
     &         1, 10,  5, 9  ,      &! edge numbers, face 3
     &         10, 6, 11, 2 ,       &! edge numbers, face 4
     &         3, 11,  7, 12,       &! edge numbers, face 5
     &         9,  8, 12, 4         &! edge numbers, face 6
     &         /), (/4,6/) )
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
! The following definitions are for the PATRAN defined HEX elements
!
      REAL(KIND=RP), PARAMETER :: oth = 1._RP/3._RP
      REAL(KIND=RP), PARAMETER :: tth = 2._RP/3._RP
!
!    ------------------------------------------------------------------
!     Mapping of element ordered nodes to the 6 faces of a HEX8
!     Mapping of the nodes on a face for the face interpolant of a HEX8
!    ------------------------------------------------------------------
!
      INTEGER, DIMENSION(4,6) :: faceMapHex8 = &
      RESHAPE((/1,2,6,5, &
                4,3,7,8, &
                1,2,3,4, &
                2,3,7,6, &
                5,6,7,8, &
                1,4,8,5/),(/4,6/))
!
      INTEGER, DIMENSION(2,2,6) :: intrpMapHex8 = &
      RESHAPE((/1,2,5,6, &
                4,3,8,7, &
                1,2,4,3, &
                2,3,6,7, &
                5,6,8,7, &
                1,4,5,8/),(/2,2,6/))
                
      INTEGER, DIMENSION(4) :: hexFaceForQuadEdge = [1, 4, 2, 6]
!
!     -------------
!     Derived types
!     -------------
!
      TYPE Node3D
         INTEGER       :: globalID
         REAL(KIND=RP) :: x(3)
      END TYPE Node3D
            
      TYPE Face3D
         INTEGER                    :: levelID       ! id of this face on a given level
         INTEGER, DIMENSION(4)      :: nodeIDs       ! ids of the four nodes bounding this face3D
         INTEGER, DIMENSION(2)      :: elementIDs    ! ids of the two hex elements sharing this face3D
         INTEGER, DIMENSION(2)      :: inc           ! Increment for the two directions for the slave element
         INTEGER, DIMENSION(2)      :: faceNumber    ! Face number (1-6) of the two elements associated with this face3D 
         REAL(KIND=RP), ALLOCATABLE :: x (:,:,:)     ! Nodal values of chebyshev interpolant for the face, if there is one.
         CLASS(SMEdge) , POINTER    :: edge          ! The spawning edge, if there is one.
      END TYPE Face3D

      TYPE Hex8Element
          INTEGER                         :: globalID
          INTEGER, DIMENSION(8)           :: nodeIDs
          INTEGER, DIMENSION(6)           :: faceID
          INTEGER                         :: materialID
          CHARACTER(LEN=32)               :: materialName
          INTEGER, DIMENSION(6)           :: bFaceFlag     ! = ON or OFF. On if there is an interpolant associated with the face.
          CHARACTER(LEN=32), DIMENSION(6) :: bFaceName     ! The boundary face name for each face. Will be "---" for internal faces.
      END TYPE Hex8Element
      
      TYPE StructuredHexMesh
         TYPE(Node3D)     , DIMENSION(:,:), ALLOCATABLE :: nodes
         TYPE(Hex8Element), DIMENSION(:,:), ALLOCATABLE :: elements
!
!        ------------------------------------------------------------
!        Faces associated with the edges and faces associated with
!        the top and bottom of an element are stored separately so
!        that the ones associated with the edges are consistent with
!        the source quadMesh.
!        ------------------------------------------------------------
!
         TYPE(Face3D)  , DIMENSION(:,:), ALLOCATABLE :: faces
         TYPE(Face3D)  , DIMENSION(:,:), ALLOCATABLE :: capFaces
      END TYPE StructuredHexMesh
!
!     ========      
      CONTAINS 
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE InitializeStructuredHexMeshFromQuadMesh( hexMesh, quadMesh, numberOfLayers )
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
         TYPE(StructuredHexMesh)  :: hexMesh
         TYPE(SMMesh)             :: quadMesh
         INTEGER                  :: numberOfLayers
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER  :: numberOf2DNodes, numberOfQuadElements, numberOfEdges
         INTEGER  :: j, k
         
         numberOf2DNodes      = quadMesh % nodes % count()
         numberOfQuadElements = quadMesh % elements % count()
         numberOfEdges        = quadMesh % edges % count()
         
         ALLOCATE(hexMesh % nodes   ( numberOf2DNodes     , 0:numberOfLayers) )
         ALLOCATE(hexMesh % elements( numberOfQuadElements,   numberOfLayers) )
         ALLOCATE(hexMesh % faces   ( numberOfEdges       ,   numberOfLayers) )
         ALLOCATE(hexMesh % capFaces( numberOfQuadElements, 0:numberOfLayers) )
        
         
         DO k = 1, SIZE(hexMesh % faces,2)
            DO j = 1, SIZE(hexMesh % faces,1)
               hexMesh % faces(j,k) % edge => NULL()
            END DO   
         END DO  
         
         DO k = 0, UBOUND(hexMesh % capFaces,2)
            DO j = 1, SIZE(hexMesh % capFaces,1)
               hexMesh % capFaces(j,k) % edge => NULL()
            END DO   
         END DO  
         
      END SUBROUTINE InitializeStructuredHexMeshFromQuadMesh
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
         INTEGER :: k, j
         DO k = 1, SIZE(hexMesh % faces,2)
            DO j = 1, SIZE(hexMesh % faces,1)
               IF(ASSOCIATED(hexMesh % faces(j,k) % edge)) THEN
                  CALL hexMesh % faces(j,k) % edge % release()
               END IF 
            END DO   
         END DO  
         
      END SUBROUTINE DestructStructuredHexMesh
      
      END Module HexMeshObjectsModule
      
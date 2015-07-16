!
!////////////////////////////////////////////////////////////////////////
!
!      MeshOutputMethods.f95
!      Created: 2010-09-24 09:32:48 -0400 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module MeshOutputMethods3D
      USE HexMeshObjectsModule
      IMPLICIT NONE
!
!     ========
      CONTAINS 
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE WriteHex8MeshToTecplot( hex8Mesh, fName )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE( StructuredHexMesh ) :: hex8Mesh
         CHARACTER(LEN=*)          :: fName
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER           :: iUnit, j, k
         INTEGER, EXTERNAL :: UnusedUnit
!
!        -----------
!        Open a file
!        -----------
!
         iUnit = UnusedUnit()
         OPEN( UNIT = iUnit, FILE = fName )
!
!        -------------
!        Set up header
!        -------------
!
         WRITE(iUnit,*) 'VARIABLES = "X", "Y", "Z"'
         WRITE(iUnit,*) 'ZONE F=FEPOINT, ET=BRICK, N=',SIZE(hex8Mesh % nodes), &
                        'E=',SIZE(hex8Mesh % elements)
!
!        ---------------
!        Write node data
!        ---------------
!
         DO j = 0, SIZE(hex8Mesh % nodes,2)-1
            DO k = 1, SIZE(hex8Mesh % nodes,1)
               WRITE( iUnit, *) hex8Mesh % nodes(k,j) % x
            END DO   
         END DO  
!
!        --------------------------
!        Write element connectivity
!        --------------------------
!
         DO j = 1, SIZE(hex8Mesh % elements,2)
            DO k = 1, SIZE(hex8Mesh % elements,1)
               WRITE( iUnit, '(8(i7,2x))') hex8Mesh % elements(k,j) % nodeIDs
            END DO   
         END DO  

         CLOSE( iUnit )
         
      END SUBROUTINE WriteHex8MeshToTecplot
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE WriteISMHexMeshFile( mesh, fName, model, N, version )
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         TYPE( StructuredHexMesh )          :: mesh
         CLASS(SMModel)            , POINTER :: model
         CHARACTER(LEN=*)                   :: fName
         INTEGER                            :: N ! The polynomial order of the boundaries.
         INTEGER                            :: version !version number of the ISM format.
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER                    :: iUnit, j, k
         INTEGER                    :: f, jj, ii, faceID
         INTEGER, EXTERNAL          :: UnusedUnit
         
!
!        -----------
!        Open a file
!        -----------
!
         iUnit = UnusedUnit()
         OPEN( UNIT = iUnit, FILE = fName )
!
!        ----------------
!        Print out header
!        ----------------
!
!         WRITE(iUnit, *) SIZE(mesh % nodes), SIZE(mesh % faces) + SIZE(mesh % capFaces), SIZE(mesh % elements), N
         WRITE(iUnit, *) SIZE(mesh % nodes), SIZE(mesh % elements), N
!
!        -----------
!        Print Nodes
!        -----------
!
         DO j = 0, SIZE(mesh % nodes,2)-1
            DO k = 1, SIZE(mesh % nodes,1)
               WRITE( iUnit, *) mesh % nodes(k,j) % x
            END DO   
         END DO  
!
!        ------------------------------
!        Print out the face information
!        ------------------------------
!
!         DO j = 1, SIZE(mesh % faces,2)
!            DO k = 1, SIZE(mesh % faces,1)
!               WRITE( iUnit, "(10(2x,i8))") mesh % faces(k,j) % elementIDs, mesh % faces(k,j) % faceNumber &
!               ,mesh % faces(k,j) % inc, mesh % faces(k,j) % nodeIDs
!            END DO   
!         END DO  
!         
!         DO j = 0, SIZE(mesh % capFaces,2)-1
!            DO k = 1, SIZE(mesh % capFaces,1)
!               WRITE( iUnit, "(10(2x,i8))") mesh % capFaces(k,j) % elementIDs, mesh % capFaces(k,j) % faceNumber &
!               ,mesh % capFaces(k,j) % inc, mesh % capFaces(k,j) % nodeIDs
!            END DO
!         END DO  
!
!        ---------------------------------------------------------
!        Print element connectivity with boundary face information
!        ---------------------------------------------------------
!
         DO j = 1, SIZE(mesh % elements,2)       ! level
            DO k = 1, SIZE(mesh % elements,1)    ! element on original quad mesh
               
               IF ( version == ISM_MM )     THEN
                  WRITE( iUnit, *) mesh % elements(k,j) % nodeIDs, TRIM(mesh % elements(k,j) % materialName)
               ELSE
                  WRITE( iUnit, *) mesh % elements(k,j) % nodeIDs
               END IF
               WRITE( iUnit, *) mesh % elements(k,j) % bFaceFlag
                         
               DO f = 1, 6
                  
                  IF( mesh % elements(k,j) % bFaceFlag(f) == ON )     THEN
                     SELECT CASE ( f )
                        CASE( 3 ) 
                          DO jj = 0, N 
                              DO ii = 0, N
                                 WRITE( iUnit, * ) mesh % capFaces(k,j-1) % x(:,ii,jj)
                              END DO
                           END DO
                        CASE (5) 
                          DO jj = 0, N 
                              DO ii = 0, N
                                 WRITE( iUnit, * ) mesh % capFaces(k,j) % x(:,ii,jj)
                              END DO
                           END DO
                        CASE DEFAULT 
                          faceID = mesh % elements(k,j) % faceID(f)
                          DO jj = 0, N 
                              DO ii = 0, N
                                 WRITE( iUnit, * ) mesh % faces(faceID,j) % x(:,ii,jj)
                              END DO
                           END DO
                     END SELECT 
                  END IF
               END DO
               
               WRITE( iUnit, *) (TRIM(mesh % elements(k,j) % bFaceName(f)), " ", f = 1, 6)
            END DO   
         END DO
         
      END SUBROUTINE WriteISMHexMeshFile
      
      END Module MeshOutputMethods3D
      
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
      SUBROUTINE WriteHex8SkeletonToTecplot( hex8Mesh, fName )
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
         
      END SUBROUTINE WriteHex8SkeletonToTecplot
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE WriteHex8MeshToTecplot( hex8Mesh, fName, N ) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE( StructuredHexMesh ) :: hex8Mesh
         CHARACTER(LEN=*)          :: fName
         INTEGER                   :: N
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER           :: iUnit, i, j, k, m, l, p
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
         WRITE(iUnit,*) ' TITLE = "SEM hex mesh" '
         WRITE(iUnit,*) ' VARIABLES = "x","y", "z"'
                  
         DO l = 1, hex8Mesh % numberofLayers
            DO m = 1, hex8Mesh % numberOfQuadElements
               WRITE(iUnit,*) "ZONE I=", N+1, ",J=",N+1, ",K=",N+1,", F=POINT"
               DO k = 0, N
                  DO j = 0, N 
                     DO i = 0, N 
                        WRITE(iUnit,*) (hex8Mesh % elements(m,l) % x(p,i,j,k), p = 1,3)
                     END DO 
                  END DO 
               END DO 
            END DO   
         END DO  
                  
      END SUBROUTINE WriteHex8MeshToTecplot
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE WriteISMHexMeshFile( mesh, fName, N, version )
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         TYPE( StructuredHexMesh )          :: mesh
         CHARACTER(LEN=*)                   :: fName
         INTEGER                            :: N ! The polynomial order of the boundaries.
         INTEGER                            :: version !version number of the ISM format.
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER                    :: iUnit, j, k
         INTEGER                    :: f, jj, ii
         INTEGER, EXTERNAL          :: UnusedUnit
         INTEGER                    :: eID
         REAL(KIND=RP), ALLOCATABLE :: faceX(:,:,:)
!
!        -----------
!        Open a file
!        -----------
!
         iUnit = UnusedUnit()
         OPEN( UNIT = iUnit, FILE = fName )
!
!        ----------------------------------------------------------
!        To print out face locations, use this as a temporary array
!        ----------------------------------------------------------
!
         ALLOCATE( faceX(3,0:N,0:N) )         
!
!        ----------------
!        Print out header
!        ----------------
!
         IF(version == ISM2)     THEN 
!            WRITE(iUnit, *) SIZE(mesh % nodes), SIZE(mesh % faces) + SIZE(mesh % capFaces), SIZE(mesh % elements), N
         ELSE
            WRITE(iUnit, *) SIZE(mesh % nodes), SIZE(mesh % elements), N
         END IF 
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
         IF( version == ISM2)     THEN 
!
!        --------------------------------------------------------------------------------
!        TODO: This information is no longer available to write out ISM2 format. It needs
!        to be added back in
!        --------------------------------------------------------------------------------
!
!            DO j = 1, SIZE(mesh % faces,2)
!               DO k = 1, SIZE(mesh % faces,1)
!                  WRITE( iUnit, "(10(2x,i8))") mesh % faces(k,j) % elementIDs, mesh % faces(k,j) % faceNumber &
!                  ,mesh % faces(k,j) % inc, mesh % faces(k,j) % nodeIDs
!               END DO   
!            END DO  
!            
!            DO j = 0, SIZE(mesh % capFaces,2)-1
!               DO k = 1, SIZE(mesh % capFaces,1)
!                  WRITE( iUnit, "(10(2x,i8))") mesh % capFaces(k,j) % elementIDs, mesh % capFaces(k,j) % faceNumber &
!                  ,mesh % capFaces(k,j) % inc, mesh % capFaces(k,j) % nodeIDs
!               END DO
!            END DO  
         END IF 
!
!        ---------------------------------------------------------
!        Print element connectivity with boundary face information
!        ---------------------------------------------------------
!
         DO j = 1, mesh % numberofLayers             ! level
            DO k = 1, mesh % numberOfQuadElements    ! element on original quad mesh
               
               eID = mesh % elements(k,j) % globalID
               
               IF ( version == ISM_MM )     THEN
                  WRITE( iUnit, *) mesh % elements(k,j) % nodeIDs, TRIM(mesh % elements(k,j) % materialName)
               ELSE
                  WRITE( iUnit, *) mesh % elements(k,j) % nodeIDs
               END IF
               WRITE( iUnit, *) mesh % elements(k,j) % bFaceFlag
                         
               DO f = 1, 6
                  IF ( mesh % elements(k,j) % bFaceFlag(f) == ON )     THEN
                     CALL FaceFromVolume(xFace   = faceX,                      &
                                         xVolume = mesh % elements(k,j) % x,   &
                                         faceID  = f,                          &
                                         N       = N ) 
                     DO jj = 0, N 
                        DO ii = 0, N 
                           WRITE(iUnit, *) faceX(:,ii,jj) 
                        END DO 
                     END DO 
                  END IF 
               END DO
               
               WRITE( iUnit, *) (TRIM(mesh % elements(k,j) % bFaceName(f)), " ", f = 1, 6)
            END DO   
         END DO
         
      END SUBROUTINE WriteISMHexMeshFile
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE FaceFromVolume(xFace, xVolume, faceID, N)  
!
!        ----------------------------------------------------------------
!        Given a volume array, return the face array for the given faceID
!        ----------------------------------------------------------------
!
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER       :: N
         REAL(KIND=RP) :: xFace(3,0:N,0:N)
         REAL(KIND=RP) :: xVolume(3,0:N,0:N,0:N)
         INTEGER       :: faceID
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER   :: NMap(6)
         INTEGER   :: i, j
         
         NMap  = [0, N, 0, N, N, 0]
         SELECT CASE ( faceID )
            CASE( 1,2 ) 
               DO j = 0, N 
                  DO i = 0, N 
                     xFace(:,i,j) = xVolume(:,i,NMap(faceID),j)
                  END DO 
               END DO 
            CASE( 3,5 )
               DO j = 0, N 
                  DO i = 0, N 
                     xFace(:,i,j) = xVolume(:,i, j, NMap(faceID))
                  END DO 
               END DO 
            CASE( 4,6 )
               DO j = 0, N 
                  DO i = 0, N 
                     xFace(:,i,j) = xVolume(:,NMap(faceID),i,j)
                  END DO 
               END DO 
            CASE DEFAULT 
            ERROR STOP 
         END SELECT          
          
      END SUBROUTINE FaceFromVolume
      
      END Module MeshOutputMethods3D
      
      MODULE Templates
      USE SMConstants
      USE SMMeshObjectsModule
      USE QuadTreeGridClass
      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      INTEGER, PARAMETER :: TEMPLATE0 = 0, TEMPLATE1 = 1, TEMPLATE2A = 21, TEMPLATE2B = 22
      INTEGER, PARAMETER :: TEMPLATE2 = 2
      INTEGER, PARAMETER :: TEMPLATE3 = 3, TEMPLATE4 = 4 !See Schneiders' papers
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructNodesForTemplate1( self, rotation ) 
         IMPLICIT NONE 
         CLASS(QuadTreeGrid), POINTER      :: self
         INTEGER                        :: rotation
         
         IF ( refinementType == REFINEMENT_3 )     THEN
            CALL NodesForTemplate1R3( self, rotation )
         ELSE
            CALL NodesForTemplate1R2( self, rotation )
         END IF
         
      END SUBROUTINE ConstructNodesForTemplate1
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE NodesForTemplate1R3( self, rotation )
!
!     ---------------------------
!     Template 1 for 3-Refinement
!     ---------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER   :: self
         INTEGER                        :: rotation
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                             :: i, j
         INTEGER                             :: N(3)
         REAL(KIND=RP)                       :: x(3)
         REAL(KIND=RP), DIMENSION(3,0:3,0:3) :: nodalPoints
       
         N = self % N
         DO j = 0, N(2) 
            DO i = 0, N(1)
               CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
               nodalPoints(:,i,j) = x
            END DO 
         END DO
!
!        ---------------------------------------------------------------------------
!        In each case, three new nodes must be created, four are re-used, and three 
!        quads are created from them
!        ---------------------------------------------------------------------------
!
         CALL ConstructNodePtr_ForLocation_( self % nodes(0,0)      , nodalPoints(:,0,0) )
         CALL ConstructNodePtr_ForLocation_( self % nodes(N(1),N(2)), nodalPoints(:,N(1),N(2)) )
         CALL ConstructNodePtr_ForLocation_( self % nodes(0,N(2))   , nodalPoints(:,0,N(2)) )
         CALL ConstructNodePtr_ForLocation_( self % nodes(N(1),0)   , nodalPoints(:,N(1),0) )
         
         SELECT CASE ( rotation )
         
            CASE( 1 )
               CALL ConstructNodePtr_ForLocation_( self % nodes(1,0), nodalPoints(:,1,0) )
               CALL ConstructNodePtr_ForLocation_( self % nodes(1,1), nodalPoints(:,1,1) )
               CALL ConstructNodePtr_ForLocation_( self % nodes(0,1), nodalPoints(:,0,1) )
               
            CASE( 2 )
               CALL ConstructNodePtr_ForLocation_( self % nodes(2,0), nodalPoints(:,2,0) )
               CALL ConstructNodePtr_ForLocation_( self % nodes(2,1), nodalPoints(:,2,1) )
               CALL ConstructNodePtr_ForLocation_( self % nodes(3,1), nodalPoints(:,3,1) )
                              
            CASE( 3 )
               CALL ConstructNodePtr_ForLocation_( self % nodes(3,2), nodalPoints(:,3,2) )
               CALL ConstructNodePtr_ForLocation_( self % nodes(2,2), nodalPoints(:,2,2) )
               CALL ConstructNodePtr_ForLocation_( self % nodes(2,3), nodalPoints(:,2,3) )
                            
            CASE( 4 )
               CALL ConstructNodePtr_ForLocation_( self % nodes(0,2), nodalPoints(:,0,2) )
               CALL ConstructNodePtr_ForLocation_( self % nodes(1,2), nodalPoints(:,1,2) )
               CALL ConstructNodePtr_ForLocation_( self % nodes(1,3), nodalPoints(:,1,3) )
               
            CASE DEFAULT
         END SELECT
!
!        -------------
!        Preset levels
!        -------------
!
         DO j = 0, N(2) 
            DO i = 0, N(1) 
               IF(ASSOCIATED(self % nodes(i,j) % node)) self % nodes(i,j) % node % level = self % level
            END DO
         END DO
!
      END SUBROUTINE NodesForTemplate1R3
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE NodesForTemplate1R2( self, rotation )
!
!     ---------------------------
!     Template 1 for 2-Refinement
!     ---------------------------
!      
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER      :: self
         INTEGER                        :: rotation
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                             :: i, j
         INTEGER                             :: N(3)
         REAL(KIND=RP)                       :: x(3)
         REAL(KIND=RP), DIMENSION(3,0:2,0:2) :: nodalPoints
       
         N = self % N
         DO j = 0, N(2) 
            DO i = 0, N(1)
               CALL GetGridPosition( self % x0, self % dx, i , j , x )
               nodalPoints(:,i,j) = x
            END DO 
         END DO
!
!        ---------------------------------------------------------------------------
!        In each case, three new nodes must be created, four are re-used, and three 
!        quads are created from them. The center point is also always used.
!        ---------------------------------------------------------------------------
!
         CALL ConstructNodePtr_ForLocation_( self % nodes(0,0)      , nodalPoints(:,0,0) )
         CALL ConstructNodePtr_ForLocation_( self % nodes(N(1),N(2)), nodalPoints(:,N(1),N(2)) )
         CALL ConstructNodePtr_ForLocation_( self % nodes(0,N(2))   , nodalPoints(:,0,N(2)) )
         CALL ConstructNodePtr_ForLocation_( self % nodes(N(1),0)   , nodalPoints(:,N(1),0) )
         CALL ConstructNodePtr_ForLocation_( self % nodes(1,1)      , nodalPoints(:,1,1) )
         
         SELECT CASE ( rotation )
         
            CASE( 1 )
               CALL ConstructNodePtr_ForLocation_( self % nodes(1,0), nodalPoints(:,1,0) )
               CALL ConstructNodePtr_ForLocation_( self % nodes(0,1), nodalPoints(:,0,1) )
               
            CASE( 2 )
               CALL ConstructNodePtr_ForLocation_( self % nodes(1,0), nodalPoints(:,1,0) )
               CALL ConstructNodePtr_ForLocation_( self % nodes(2,1), nodalPoints(:,2,1) )
                              
            CASE( 3 )
               CALL ConstructNodePtr_ForLocation_( self % nodes(2,1), nodalPoints(:,2,1) )
               CALL ConstructNodePtr_ForLocation_( self % nodes(1,2), nodalPoints(:,1,2) )
                            
            CASE( 4 )
               CALL ConstructNodePtr_ForLocation_( self % nodes(0,1), nodalPoints(:,0,1) )
               CALL ConstructNodePtr_ForLocation_( self % nodes(1,2), nodalPoints(:,1,2) )
               
            CASE DEFAULT
         END SELECT
!
!        -------------
!        Preset levels
!        -------------
!
         DO j = 0, N(2) 
            DO i = 0, N(1) 
               IF(ASSOCIATED(self % nodes(i,j) % node)) self % nodes(i,j) % node % level = self % level
            END DO
         END DO
!
      END SUBROUTINE NodesForTemplate1R2
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructQuadsForTemplate1( self, rotation )
         IMPLICIT NONE 
         CLASS(QuadTreeGrid), POINTER      :: self
         INTEGER                        :: rotation
         
         IF ( refinementType == REFINEMENT_3 )     THEN
            CALL ConstructQuadsForTemplate1R3( self, rotation )
         ELSE
            CALL ConstructQuadsForTemplate1R2( self, rotation )
         END IF
         
      END SUBROUTINE ConstructQuadsForTemplate1
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructQuadsForTemplate1R3( self, rotation )
!
!     ----------------------------
!     Quads in a 3-Refinement grid
!     ----------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER      :: self
         INTEGER                        :: rotation
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                             :: i, j
         INTEGER                             :: N(2) = (/3,3/)
!
!        --------------------------------------------
!        Finally, set the quads to point to the nodes
!        --------------------------------------------
!
         SELECT CASE ( rotation )
         
            CASE( 1 )
               
               i = 1; j = 1 ! #1
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(0,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(1,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(1,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(0,1) )
                
               i = 2; j = 1 ! #2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(1,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(N(1),0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(N(1),N(2)) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(1,1) )
               
               i = 1; j = 2 ! #3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(0,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(1,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(N(1),N(2)) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(0,N(2)) )

            CASE( 2 )
               
               i = 2; j = 1 ! #1
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(0,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(2,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(2,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(0,N(2)) )
                
               i = 3; j = 1 ! #2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(2,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(N(1),0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(3,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(2,1) )
               
               i = 3; j = 2 ! #3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(2,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(3,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(N(1),N(2)) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(0,N(2)) )
               
            CASE( 3 )
               
               i = 3; j = 2 ! #1
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(0,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(N(1),0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(3,2) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(2,2) )
                
               i = 3; j = 3 ! #2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(2,2) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(3,2) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(N(1),N(2)) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(2,3) )
               
               i = 2; j = 3 ! #3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(0,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(2,2) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(2,3) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(0,N(2)) )
             
            CASE( 4 )
               
               i = 1; j = 2 ! #1
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(0,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(N(1),0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(1,2) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(0,2) )
                
               i = 2; j = 3 ! #2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(1,2) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(N(1),0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(N(1),N(2)) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(1,3) )
               
               i = 1; j = 3 ! #3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(0,2) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(1,2) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(1,3) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(0,N(2)) )
            CASE DEFAULT
         END SELECT
         
      END SUBROUTINE ConstructQuadsForTemplate1R3
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructQuadsForTemplate1R2( self, rotation ) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER      :: self
         INTEGER                        :: rotation
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                             :: i, j
         INTEGER                             :: N(2) = (/2,2/)
!
!        --------------------------------------------
!        Finally, set the quads to point to the nodes
!        --------------------------------------------
!
         SELECT CASE ( rotation )
         
            CASE( 1 )
               
               i = 1; j = 1 ! #1
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(0,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(1,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(1,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(0,1) )
                
               i = 2; j = 1 ! #2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(1,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(N(1),0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(N(1),N(2)) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(1,1) )
               
               i = 1; j = 2 ! #3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(0,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(1,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(N(1),N(2)) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(0,N(2)) )

            CASE( 2 )
               
               i = 1; j = 1 ! #1
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(0,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(1,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(1,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(0,N(2)) )
                
               i = 2; j = 1 ! #2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(1,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(N(1),0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(2,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(1,1) )
               
               i = 2; j = 2 ! #3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(1,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(2,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(N(1),N(2)) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(0,N(2)) )
               
            CASE( 3 )
               
               i = 2; j = 1 ! #1
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(0,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(N(1),0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(2,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(1,1) )
                
               i = 2; j = 2 ! #2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(1,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(2,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(N(1),N(2)) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(1,2) )
               
               i = 1; j = 2 ! #3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(0,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(1,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(1,2) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(0,N(2)) )
             
            CASE( 4 )
               
               i = 1; j = 1 ! #1
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(0,0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(N(1),0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(1,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(0,1) )
                
               i = 2; j = 2 ! #2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(1,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(N(1),0) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(N(1),N(2)) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(1,2) )
               
               i = 1; j = 2 ! #3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(1), self % nodes(0,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(2), self % nodes(1,1) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(3), self % nodes(1,2) )
               CALL PointNodePtr_To_( self % quads(i,j) % quad % nodes(4), self % nodes(0,N(2)) )
            CASE DEFAULT
         END SELECT
         
      END SUBROUTINE ConstructQuadsForTemplate1R2
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructNodesForTemplate2( self ) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER      :: self
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                             :: i, j
         INTEGER                             :: N(3)
         REAL(KIND=RP)                       :: x(3)
       
         N = self % N
         DO j = 0, N(2) 
            DO i = 0, N(1)
               CALL GetGridPosition( self % x0, self % dx, i , j , x )
               CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
            END DO 
         END DO
!
!        -------------
!        Preset levels
!        -------------
!
         DO j = 0, N(2) 
            DO i = 0, N(1) 
               self % nodes(i,j) % node % level = self % level
            END DO
         END DO
         
      END SUBROUTINE ConstructNodesForTemplate2
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructNodesForTemplate2A( self, rotation ) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER      :: self
         INTEGER                        :: rotation
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                             :: i, j
         INTEGER                             :: N(3)
         REAL(KIND=RP)                       :: x(3)
         
         N = self % N
         CALL GetGridPosition( self % x0, self % dx, 0, 0, x )
         CALL ConstructNodePtr_ForLocation_( self % nodes(0,0)      , x )
         CALL GetGridPosition( self % x0, self % dx, N(1),N(2), x )
         CALL ConstructNodePtr_ForLocation_( self % nodes(N(1),N(2)), x )
         CALL GetGridPosition( self % x0, self % dx, 0, N(2), x )
         CALL ConstructNodePtr_ForLocation_( self % nodes(0,N(2))   , x )
         CALL GetGridPosition( self % x0, self % dx, N(1),0, x )
         CALL ConstructNodePtr_ForLocation_( self % nodes(N(1),0)   , x )

         SELECT CASE ( rotation )
         
            CASE( 1 )
!
               DO i = 1, 2
                  DO j = 0, 2
                     CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
                     CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
                  END DO
               END DO
               i = 0; j = 1
               CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
               CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
               i = 3; j = 1
               CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
               CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
               
            CASE( 2 )
!
               DO i = 1, 3
                  DO j = 1, 2
                     CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
                     CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
                  END DO
               END DO
               i = 2; j = 0
               CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
               CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
               i = 2; j = 3
               CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
               CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
            CASE( 3 )
!
               DO i = 1, 2
                  DO j = 1,3
                     CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
                     CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
                  END DO
               END DO
               i = 0; j = 2
               CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
               CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
               i = 3; j = 2
               CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
               CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
                            
            CASE( 4 )
!
               DO i = 0, 2
                  DO j = 1,2
                     CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
                     CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
                  END DO
               END DO
               i = 1; j = 0
               CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
               CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
               i = 1; j = 3
               CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
               CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
               
            CASE DEFAULT
         END SELECT
!
!        -------------
!        Preset levels
!        -------------
!
         DO j = 0, N(2) 
            DO i = 0, N(1) 
               IF(ASSOCIATED(self % nodes(i,j) % node)) self % nodes(i,j) % node % level = self % level
            END DO
         END DO
         
      END SUBROUTINE ConstructNodesForTemplate2A
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructQuadsForTemplate2( self ) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER      :: self
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                           :: i, j
         
         DO j = 1, 2 
            DO i = 1, 2 
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
            END DO
         END DO
         
      END SUBROUTINE ConstructQuadsForTemplate2
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructQuadsForTemplate2A( self, rotation ) 
      IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER      :: self
         INTEGER                        :: rotation
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                           :: i, j
       
         i = 2; j = 2
         IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
            ALLOCATE(self % quads(i,j) % quad)
            CALL self % quads(i,j) % quad % init() 
         END IF 
         self % quads(i,j) % quad % domainMark = INSIDE
         CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
         CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
         CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
         CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
         
         SELECT CASE ( rotation )
         
            CASE( 1 )
               j = 1
               DO i = 1, 3
                  IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                     ALLOCATE(self % quads(i,j) % quad)
                     CALL self % quads(i,j) % quad % init() 
                  END IF 
                  self % quads(i,j) % quad % domainMark = INSIDE
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               END DO 
               
               i = 1; j = 2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,3  ))
               
               i = 3; j = 2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  3  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               
               i = 2; j = 3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(3  ,3  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(0  ,3  ))
               
            CASE( 2 )
               i = 3
               DO j = 1, 3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
                  self % quads(i,j) % quad % domainMark = INSIDE
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               END DO 
               
               i = 2; j = 1
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(0,0))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               
               i = 1; j = 2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(0,0))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(0,3  ))
               
               i = 2; j = 3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i  ,j  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(0  ,3  ))
               
            CASE( 3 )
               j = 3
               DO i = 1, 3
                  IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                     ALLOCATE(self % quads(i,j) % quad)
                     CALL self % quads(i,j) % quad % init() 
                  END IF 
                  self % quads(i,j) % quad % domainMark = INSIDE
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               END DO 
               
               i = 2; j = 1
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(0  ,0))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(3  ,0))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j))
               
               i = 1; j = 2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(0  ,0  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               
               i = 3; j = 2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(3  ,0  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i  ,j  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1  ,j))
            CASE( 4 )
               i = 1
               DO j = 1, 3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
                  self % quads(i,j) % quad % domainMark = INSIDE
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               END DO 
               
               i = 2; j = 1
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(3  ,0))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j))
               
               i = 2; j = 3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(3,  3  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               
               i = 3; j = 2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(3  ,0  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(3  ,3  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1  ,j))
            CASE DEFAULT
         END SELECT
         
      END SUBROUTINE ConstructQuadsForTemplate2A
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructNodesForTemplate2B( self, rotation ) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER      :: self
         INTEGER                        :: rotation
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                     :: i, j
         REAL(KIND=RP)               :: x(3)
         
         LOGICAL, DIMENSION(0:3,0:3,2) :: maskNode = RESHAPE( (/ .false., .false. , .true., .false., &
                                                               .false. , .false. , .false., .true. , &
                                                               .true., .false. , .false., .false. , &
                                                               .false., .true., .false., .false., & ! #1
                                                               .false., .true. , .false., .false., &
                                                               .true. , .false. , .false., .false. , &
                                                               .false., .false. , .false., .true. , &
                                                               .false., .false., .true., .false./), (/4,4,2/))
                                                            
       
!
!        -------------------
!        Compute node points
!        -------------------
!
         DO i = 0, 3
            DO j = 0, 3
               IF( maskNode(i,j,rotation) )    CYCLE
               CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
               CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
               self % nodes(i,j) % node % level = self % level
            END DO
         END DO
            
      END SUBROUTINE ConstructNodesForTemplate2B
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructQuadsForTemplate2B( self, rotation ) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER      :: self
         INTEGER                        :: rotation
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                     :: i, j
         INTEGER                     :: N(2) = (/3,3/)
       
!
!        --------------------------------------
!        Now set the nodes of each of the quads
!        --------------------------------------
!
         SELECT CASE ( rotation )
            CASE( 1 )
               DO j = 1, N(2)
                  i = j
                  IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                     ALLOCATE(self % quads(i,j) % quad)
                     CALL self % quads(i,j) % quad % init() 
                  END IF 
                  self % quads(i,j) % quad % domainMark = INSIDE
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               END DO
            CASE( 2 )
               DO j = 1, N(2) 
                  i = N(1) - j + 1
                  IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                     ALLOCATE(self % quads(i,j) % quad)
                     CALL self % quads(i,j) % quad % init() 
                  END IF 
                  self % quads(i,j) % quad % domainMark = INSIDE
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
                  CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               END DO
            CASE DEFAULT
         END SELECT
!
!        -----------------------------
!        Fill in the nonstandard quads
!        ----------------------------
!
         SELECT CASE ( rotation )
         
            CASE( 1 )
               i = 1; j = 2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1 ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1 ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j   ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(0  ,N(2)))
               i = 2; j = 3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1 ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1 ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j   ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(0  ,N(2)))
               i = 2; j = 1
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1 ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(N(1),0  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i   ,  j))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1 ,j  ))
               i = 3; j = 2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1 ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(N(1) ,0))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               
            CASE( 2 )
               i = 2; j = 1
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
                self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(0,0))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               i = 1; j = 2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(0,0))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               i = 3; j = 2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(N(1),N(2)  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               i = 2; j = 3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(N(1),N(2)  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
            CASE DEFAULT
         END SELECT
         
      END SUBROUTINE ConstructQuadsForTemplate2B
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructNodesForTemplate3( self, rotation ) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER      :: self
         INTEGER                        :: rotation
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                     :: i, j
         REAL(KIND=RP)               :: x(3)
         
         LOGICAL, DIMENSION(0:3,0:3,4) :: maskNode = RESHAPE( (/ .false., .false. , .false., .false., &
                                                               .false. , .false. , .false., .false. , &
                                                               .true., .false. , .false., .false. , &
                                                               .false., .true., .false., .false., & ! #1
                                                               .false., .true. , .false., .false., &
                                                               .true. , .false. , .false., .false. , &
                                                               .false., .false. , .false., .false. , &
                                                               .false., .false., .false., .false., & !#2
                                                               .false., .false. , .true., .false., &
                                                               .false. , .false. , .false., .true. , &
                                                               .false., .false. , .false., .false. , &
                                                               .false., .false., .false., .false.,& ! #3
                                                               .false., .false. , .false., .false., &
                                                               .false. , .false. , .false., .false. , &
                                                               .false., .false. , .false., .true. , &
                                                               .false., .false., .true., .false. /), (/4,4,4/))
       
!
!        -------------------
!        Compute node points
!        -------------------
!
         DO i = 0, 3
            DO j = 0, 3
               IF( maskNode(i,j,rotation) )    CYCLE
               CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
               CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
               self % nodes(i,j) % node % level = self % level
            END DO
         END DO
         
      END SUBROUTINE ConstructNodesForTemplate3
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructQuadsForTemplate3( self, rotation ) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER      :: self
         INTEGER                        :: rotation
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                     :: i, j
         INTEGER                     :: N(2) = (/3,3/)
                                                               
         LOGICAL, DIMENSION(3,3,4) :: quadMask = RESHAPE( (/.false.,.false.,.false.,&
                                                            .true.,.false.,.false.,&
                                                            .true.,.true.,.false.,& !#1
                                                            .true.,.true.,.false.,&
                                                            .true.,.false.,.false.,&
                                                            .false.,.false.,.false.,& !#2
                                                            .false.,.true.,.true.,&
                                                            .false.,.false.,.true.,&
                                                            .false.,.false.,.false.,& !#3
                                                            .false.,.false.,.false.,&
                                                            .false.,.false.,.true.,&
                                                            .false.,.true.,.true./), (/3,3,4/))
       
!
!        --------------------------------------
!        Now set the nodes of each of the quads
!        In this template, do them all and then
!        eliminate the one that doesn't exist.
!        --------------------------------------
!
         DO j = 1, N(2) 
            DO i = 1, N(1)
               IF( quadMask(i,j,rotation) ) CYCLE
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
            END DO
         END DO
!
!        -----------------------------
!        Fill in the nonstandard quads
!        ----------------------------
!
         SELECT CASE ( rotation )
         
            CASE( 1 )
               i = 1; j = 2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1 ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1 ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j   ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(0  ,N(2)))
               
               i = 2; j = 3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1 ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1 ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j   ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(0  ,N(2)))
               
            CASE( 2 )
               i = 2; j = 1
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(0,0))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               i = 1; j = 2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(0,0))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               
            CASE( 3 )
               i = 2; j = 1
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(N(1)  ,0))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               i = 3; j = 2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(N(1)  ,0))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i,  j  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               
            CASE( 4 )
               i = 3; j = 2
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(N(1),  N(2)  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
               i = 2; j = 3
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
               self % quads(i,j) % quad % domainMark = INSIDE
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(N(1),  N(2)  ))
               CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
            CASE DEFAULT
         END SELECT
         
   
      END SUBROUTINE ConstructQuadsForTemplate3
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructNodesForTemplate4( self )
!
         CLASS(QuadTreeGrid), POINTER      :: self
         
         INTEGER                       :: i,j
         REAL(KIND=RP)                 :: x(3)
       
!
!        ----------------------------------------------------------------------
!        The template stores all 9 quads.
!        -----------------------------------------------------------------------
!
         DO j = 0, self % N(2) 
            DO i = 0, self % N(1)
               CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
               CALL ConstructNodePtr_ForLocation_( self % nodes(i,j), x )
               self % nodes(i,j) % node % level = self % level
            END DO 
         END DO
         
         
      END SUBROUTINE ConstructNodesForTemplate4
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructQuadsForTemplate4( self )
!
         CLASS(QuadTreeGrid), POINTER      :: self
         INTEGER                        :: i,j
       
!
         DO j = 1, self % N(2) 
            DO i = 1, self % N(1)
               IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                  ALLOCATE(self % quads(i,j) % quad)
                  CALL self % quads(i,j) % quad % init() 
               END IF 
                self % quads(i,j) % quad % domainMark = INSIDE
                CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
                CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
                CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i  ,j  ))
                CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
            END DO 
         END DO
         
         
      END SUBROUTINE ConstructQuadsForTemplate4
      
   END MODULE Templates

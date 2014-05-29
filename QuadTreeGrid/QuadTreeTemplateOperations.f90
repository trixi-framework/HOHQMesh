!
!////////////////////////////////////////////////////////////////////////
!
!      NestedGrid.f95
!      Created: 2010-07-18 14:19:10 -0400 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module QuadTreeTemplateOperations
      
      USE SMConstants
      USE SMMeshObjectsModule
      USE QuadTreeGridClass
      USE Templates
      
      IMPLICIT NONE
!
!     ========
      CONTAINS 
!     ========
!
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructNodesWithTemplate( self, rotation )
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER :: self
         INTEGER                   :: rotation ! Used for all other templates, otherwise ignored
        
         SELECT CASE ( self % templateType )
         
            CASE( TEMPLATE0 )
               !PRINT *, "Level 0 templates should not be called"
               !STOP
               
            CASE( TEMPLATE1 )
               CALL ConstructNodesForTemplate1( self, rotation )
            CASE( TEMPLATE2 )
               CALL ConstructNodesForTemplate2( self )
            CASE( TEMPLATE2A )
               CALL ConstructNodesForTemplate2A ( self, rotation )
            CASE( TEMPLATE2B )
               CALL ConstructNodesForTemplate2B ( self, rotation )
            CASE( TEMPLATE3 )
               CALL ConstructNodesForTemplate3 ( self, rotation )
            CASE( TEMPLATE4 )
               CALL ConstructNodesForTemplate4 ( self )
            CASE DEFAULT
         END SELECT
         
      END SUBROUTINE ConstructNodesWithTemplate
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructQuadsWithTemplate( self )
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER    :: self
         INTEGER                         :: rotation
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER :: i, j
                
         rotation = self % rotation
         SELECT CASE ( self % templateType )
         
            CASE( TEMPLATE0 )
               DO j = 1, self % N(2) 
                  DO i = 1, self % N(1)
                     IF ( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     THEN
                        ALLOCATE(self % quads(i,j) % quad)
                        CALL self % quads(i,j) % quad % init() 
                     END IF 
                     CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(1), self % nodes(i-1,j-1))
                     CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(2), self % nodes(i  ,j-1))
                     CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(3), self % nodes(i  ,j  ))
                     CALL PointNodePtr_To_(self % quads(i,j) % quad % nodes(4), self % nodes(i-1,j  ))
                  END DO
               END DO
               
            CASE( TEMPLATE1 )
               CALL ConstructQuadsForTemplate1 ( self, rotation )
            CASE( TEMPLATE2 )
               CALL ConstructQuadsForTemplate2( self )
            CASE( TEMPLATE2A )
               CALL ConstructQuadsForTemplate2A ( self, rotation )
            CASE( TEMPLATE2B )
               CALL ConstructQuadsForTemplate2B ( self, rotation )
            CASE( TEMPLATE3 )
               CALL ConstructQuadsForTemplate3 ( self, rotation )
            CASE( TEMPLATE4 )
               CALL ConstructQuadsForTemplate4 ( self )
            CASE DEFAULT
         END SELECT
         
      END SUBROUTINE ConstructQuadsWithTemplate
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DoLevelOperation( self, operation ) 
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(QuadTreeGrid), POINTER :: self
      INTEGER                   :: operation
!
!     ---------------
!     Local variables
!     ---------------
!
      CLASS(QuadTreeGrid), POINTER :: currentGrid
      INTEGER                   :: level,k
      INTEGER                   :: iStart, iEnd, iStep
!
!     ----------------
!     Start at level 0
!     ----------------
!
      IF( self % level /= 0 )     THEN !ERROR: Not being called properly
         PRINT *, "hanging node elimination must start at level 0"
         RETURN
      END IF
!
!     -------------------------------------------------------
!     For each level, gather all the quads and subdivide them
!     according to the appropriate template if necessary.
!     -------------------------------------------------------
!
      IF ( operation == FLATTEN_NODE_LEVELS_OPERATION )     THEN
         iStart = highestLevel
         iEnd   = 0
         iStep  = -1
      ELSE
         iStart = 0
         iEnd   = highestLevel
         iStep  = 1
      END IF
      
      DO level = iStart, iEnd, iStep
         CALL AssignNodeLevels( self )
!
!        --------------------------------------------
!        Find how many grids are at the current level
!        --------------------------------------------
!
         numberOfGridsAtLevel = 0
         CALL FindNumberOfGridsIn_AtLevel_( self, level )
!
!        -------------------------------------------------------
!        Store the information about the target quads in arrays.
!        We need the parent and the location in the parent. For
!        each quad, we will check to see if it needs a child
!        grid created by the appropriate template.
!        -------------------------------------------------------
!
         ALLOCATE( gridsAtLevel(numberOfGridsAtLevel) )
!
!        -----------------------------------------------------------------------
!        Find which quads are candidates for subdivision and gather them into
!        a list. We need to subdivide by level at this point, not down the tree.
!        -----------------------------------------------------------------------
!
         globalGridCount = 0
         CALL GatherGridsAtLevel_FromRtGrid_( level, self )
         
         SELECT CASE ( operation )
         
            CASE( REMOVE_HANGING_NODES_OPERATION )
               
               DO k = 1, SIZE(gridsAtLevel)
                  currentGrid => gridsAtLevel(k) % grid
                  IF( refinementType == REFINEMENT_2 )     THEN
                     CALL SetNodeActivation( currentGrid, ACTIVE )
                     CALL Refine( currentGrid )
                     CALL SetNodeActivation( currentGrid, INACTIVE )
                  ELSE
                     CALL Refine( currentGrid )
                  END IF
               END DO
               
            CASE( FLATTEN_NODE_LEVELS_OPERATION )
                DO k = 1, SIZE(gridsAtLevel)
                  currentGrid => gridsAtLevel(k) % grid
                  CALL FlattenNodeLevels( currentGrid )
                  CALL DeleteDuplicateNodesForGrid( currentGrid )
              END DO
           
            CASE DEFAULT
               PRINT *, "Unknown level operation: ", operation!DEBUG
         END SELECT
!
!        -------
!        Cleanup
!        -------
!
         DEALLOCATE( gridsAtLevel )
         numberOfGridsAtLevel = 0

      END DO
      
      END SUBROUTINE DoLevelOperation
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE Refine( self ) 
      IMPLICIT NONE
      
      CLASS(QuadTreeGrid), POINTER :: self, newGrid
      
      INTEGER                      :: level, i, j, k, N, M
      INTEGER                      :: templateType, rotation
      REAL(KIND=RP)                :: dx(3), x0(3)
      INTEGER , DIMENSION(4)       :: nodeLevels, activeStatus
      INTEGER , DIMENSION(2,4)     :: nodeLocs
            
      N     = self % N(1); M = self % N(2)
      level = self % level
      dx    = self % dx/DBLE(refinementType)
!
!     ---------------------------------------------------------------
!     Construct the nodes on each of the children of the current grid
!     ---------------------------------------------------------------
!
      CALL SetNeighborPointers( self )
      DO j = 1, M 
         DO i = 1, N
             IF( ASSOCIATED(self % children(i,j) % grid ) )     cycle ! this grid is not already subdivided.
!
!               -------------------------------------------------
!               Find the nodes for self that make up the i,j quad
!               -------------------------------------------------
!
                CALL NodeLocs_ForTemplate_At( nodeLocs, self % templateType, i, j, self % rotation )
                IF( SUM(nodeLocs) == 0 )     CYCLE ! Template type 2 has no nodelocs??
                
                DO k = 1, 4 
                   nodeLevels(k)   = self % nodes(nodeLocs(1,k),nodeLocs(2,k)) % node % level
                   activeStatus(k) = self % nodes(nodeLocs(1,k),nodeLocs(2,k)) % node % activeStatus
                END DO
                
                IF( refinementType == REFINEMENT_3 )     THEN
                   CALL Determine3TemplateType( nodeLevels, level, templateType, rotation )
                ELSE                
                   CALL Determine2TemplateType( activeStatus, nodeLevels, level, templateType, rotation )
                END IF
                
                IF( templateType == TEMPLATE0 ) CYCLE ! No need to refine
!
!               ----------------------------------
!               Create the new child for this quad
!               ----------------------------------
!
                CALL GetGridPosition( self % x0, self % dx, i-1, j-1, x0 )
                ALLOCATE(self % children(i,j) % grid)
                newGrid => self % children(i,j) % grid
                CALL newGrid % initWithTemplateType( templateType, dx, x0, self, &
                                   (/i,j,0/), self % level+1, rotation )
                CALL ConstructNodesWithTemplate( newGrid, rotation )
                CALL SetNeighborPointers( newGrid )
         END DO
      END DO
      CALL DeleteDuplicateNodesForGrid( self )
      
      END SUBROUTINE Refine
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FlattenNodeLevels( self )
!
!      ------------------------------------------------------------------
!      Cf. Refine & RefineGrid_ToSizeFunction_. This routine goes through 
!      each quad in the grid and
!      looks at the difference in the node levels of the four corners.
!      If the difference is bigger than one at any of the corners, this
!      quad is subdivded. This routine is needed for 2-Refinement meshes.
!      ------------------------------------------------------------------
!
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER :: self
!
!        ---------------
!        local variables
!        ---------------
!
         CLASS(QuadTreeGrid), POINTER :: childGrid
         INTEGER , DIMENSION(3,4)  :: nodeLocs
         
         INTEGER        :: nodeLevel
         LOGICAL        :: needsRefine
         INTEGER        :: i, j, nX, nY, k
         INTEGER        :: N(3)
         REAL(KIND=RP)  :: xMin(3), xMax(3), dX(3)
         
         
         N = refinementType
         nX = self % N(1); nY = self % N(2)
!
!        ----------------
!        Do for each quad
!        ----------------
!
         DO j = 1, nY
            DO i = 1, nX
               IF( ASSOCIATED(self % children(i,j) % grid ) )    CYCLE
               
                nodeLocs(:,1) = (/i-1,j-1,0/)
                nodeLocs(:,2) = (/i  ,j-1,0/)
                nodeLocs(:,3) = (/i  ,j  ,0/)
                nodeLocs(:,4) = (/i-1,j  ,0/)
                
                needsRefine = .false.
                DO k = 1, 4 
                   nodeLevel = self % nodes(nodeLocs(1,k),nodeLocs(2,k)) % node % level - self % level
                   IF ( nodeLevel > 1 )     THEN
                      needsRefine = .true.
                      EXIT
                   END IF
                END DO
                IF( .NOT.needsRefine )   CYCLE
!
               CALL GetGridPosition( self % x0, self % dx, i-1, j-1, xMin )
               CALL GetGridPosition( self % x0, self % dx, i  , j  , xMax )
               dx = self % dx/DBLE(refinementType)
               ALLOCATE(childGrid)
               CALL childGrid % initGridWithParameters( dx, xMin, N, self, (/i,j,0/), self % level+1 )
               self % children(i,j) % grid => childGrid
               CALL SetNeighborPointers( childGrid )
            END DO
         END DO

      END SUBROUTINE FlattenNodeLevels
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE ConstructQuadsWithTemplates( self )
      IMPLICIT NONE 
      CLASS(QuadTreeGrid), POINTER :: self, grid
      INTEGER                   :: i, j, N, M
      
      N = self % N(1)
      M = self % N(2)
      
      CALL ConstructQuadsWithTemplate( self )
!
!     ------------------------
!     Same thing with children
!     ------------------------
!
      DO j = 1, M 
         DO i = 1, N
            grid => self % children(i,j) % grid
            IF( ASSOCIATED(grid) ) CALL ConstructQuadsWithTemplates( grid )
         END DO
      END DO
      
      END SUBROUTINE ConstructQuadsWithTemplates
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE Determine3TemplateType( nodeLevels, level, templateType, rotation )
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER, INTENT(IN)    :: level
      INTEGER, INTENT(OUT)   :: templateType, rotation
      INTEGER , DIMENSION(4) :: nodeLevels
!
!     ---------------
!     Local Variables
!     ---------------
!
      INTEGER , DIMENSION(4) :: rotMap = (/2,3,4,1/)
      INTEGER                :: k, numberOfMarkedNodes
!
!     ------------------------------
!     Mark the nodes for their level
!     ------------------------------
!      
      nodeLevels   = nodeLevels - level ! Finds marked nodes
      numberOfMarkedNodes = 0
      DO k = 1,4 
         IF( nodeLevels(k) /= 0 )  numberOfMarkedNodes = numberOfMarkedNodes + 1
      END DO
      
      SELECT CASE ( numberOfMarkedNodes )
      
         CASE( 0 ) ! No subdivision needed         
            templateType = TEMPLATE0
            rotation     = 0                           
            
         CASE( 1 )
            templateType = TEMPLATE1
            DO k = 1, 4
               rotation = k
               IF( nodeLevels(k) /= 0 ) EXIT
            END DO
                  
         CASE( 2 )
            DO k = 1, 4
               rotation = k
               IF( nodeLevels(k) /= 0 ) EXIT
            END DO
!
!           ----------------------------------
!           Distiguish between types 2A AND 2B
!           ----------------------------------
!
            IF ( ((rotation == 1).OR.(rotation==2)).AND.nodeLevels(rotation+2) /= 0 )     THEN
               templateType = TEMPLATE2B
            ELSE
               templateType = TEMPLATE2A
               IF( rotation == 1 .AND. nodeLevels(4) /= 0 ) rotation = 4
            END IF
            
         CASE( 3 )
            DO k = 1, 4
               rotation = k
               IF( nodeLevels(k) == 0 ) EXIT
            END DO
            templateType = TEMPLATE3
            rotation = rotMap(rotation)
            
         CASE( 4 )
            templateType = TEMPLATE4
            rotation = 0
            
         CASE DEFAULT
            PRINT *, "Whoa, something messed up in Determine3TemplateType"
            templateType = TEMPLATE4
            rotation = 0
      END SELECT
      
      END SUBROUTINE Determine3TemplateType
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE Determine2TemplateType( activeStatus, nodeLevels, level, templateType, rotation )
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER, INTENT(IN)    :: level
      INTEGER, INTENT(OUT)   :: templateType, rotation
      INTEGER , DIMENSION(4) :: activeStatus, nodeLevels
!
!     ---------------
!     Local Variables
!     ---------------
!
      INTEGER                :: k, numberOfMarkedNodes
!
!     ------------------------------
!     Mark the nodes for their level
!     ------------------------------
!
      nodeLevels          = nodeLevels - level
      numberOfMarkedNodes = 0
      DO k = 1,4 
         IF( activeStatus(k) == ACTIVE .AND. nodeLevels(k) == 1 )  &
                  numberOfMarkedNodes = numberOfMarkedNodes + 1
      END DO
      
      SELECT CASE ( numberOfMarkedNodes )
      
         CASE( 0 ) ! No subdivision needed         
            templateType = TEMPLATE0
            rotation     = 0                           
            
         CASE( 1 )
            templateType = TEMPLATE1
            DO k = 1, 4
               rotation = k
               IF( activeStatus(k) == ACTIVE .AND. nodeLevels(k) == 1 ) EXIT
            END DO
            
         CASE( 2 )
            templateType = TEMPLATE2
            rotation     = 0
            
         CASE DEFAULT
            PRINT *, "Whoa, something messed up in Determine2TemplateType"
            templateType = TEMPLATE0
            rotation = 0
      END SELECT
      
      END SUBROUTINE Determine2TemplateType
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetNodeActivation( self, s )
!
!     -------------------------------------------------------------------
!     !
!     !     For 2-Refinement, alternatively activate and deactivate nodes
!     !     on the top level mesh. s is either ACTIVE or INACTIVE
!     !
!     -------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid), POINTER :: self
         INTEGER                      :: s
!
!        ---------------
!        local variables
!        ---------------
!
         INTEGER :: i, j, nX, nY
         
         IF( refinementType /= REFINEMENT_2 )     RETURN
         
         nX = self % N(1); nY = self % N(2)
!
         DO j = 0, nY
            DO i = MOD(j,2), nX, 2
               self % nodes(i,j) % node % activeStatus = s
            END DO
         END DO

      END SUBROUTINE SetNodeActivation
      

      END Module QuadTreeTemplateOperations

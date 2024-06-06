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
! --- End License
!////////////////////////////////////////////////////////////////////////
!
!      QuadTreeGrid.f95
!      Created: 2010-07-18 14:19:10 -0400
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
      Module QuadTreeGridClass

      USE SMConstants
      USE SMMeshObjectsModule
      USE MeshSizerClass
      USE FTObjectClass

      IMPLICIT NONE
!
!     ------------------------
!     QuadTreeGrid
!     ------------------------
!
      TYPE, EXTENDS(FTObject) :: QuadTreeGrid
!
!        ----
!        Self
!        ----
!
         INTEGER         , DIMENSION(3)           :: N            ! Number of quads in each direction
         REAL(KIND=RP)   , DIMENSION(3)           :: dx, x0       ! Grid spacing and location of lower left corner
         INTEGER                                  :: level        ! where the grid is located in the tree.
         INTEGER                                  :: templateType ! One of Schneiders' classifications
         INTEGER                                  :: boundaryTest ! takes on INSIDE, OUTSIDE, ON_BOUNDARY, UNDETERMINED
         INTEGER                                  :: rotation     ! For template construction

         TYPE(SMNodePtr), DIMENSION(:,:), POINTER :: nodes => NULL()        ! Nodes are numbered (0:N(1),0:N(2))
         TYPE(SMQuadPtr), DIMENSION(:,:), POINTER :: quads => NULL()       ! Quads are numbered (1:N(1),1:N(2))
!
!        ------
!        Parent
!        ------
!
         INTEGER            , DIMENSION(3)  :: locInParent ! (i,j,k) location within the parent
         TYPE (QuadTreeGrid), POINTER       :: parent
!
!        --------
!        Children
!        --------
!
         TYPE( NestedQuadTreeGridPtr ), DIMENSION(:,:), POINTER :: children => NULL() ! Children are numbered (1:N(1),1:N(2))
!
!        ----------------------------
!        References to Neighbor grids
!        ----------------------------
!
         TYPE (QuadTreeGrid), POINTER :: neighborL => NULL(), neighborR => NULL(), neighborT => NULL(), neighborB => NULL()
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initGridWithParameters
         PROCEDURE :: initWithTemplateType => TemplateInit
         FINAL     :: destructGrid
         PROCEDURE :: printDescription => printQuadTreeGridDescription

      END TYPE QuadTreeGrid
!
!     -----------------
!     NestedQuadTreeGridPtr
!     -----------------
!
      TYPE NestedQuadTreeGridPtr
         TYPE (QuadTreeGrid), POINTER :: grid => NULL()
      END TYPE NestedQuadTreeGridPtr
!
!     --------------
!     MODULE Globals
!     --------------
!
      INTEGER  :: highestLevel    = 0
      INTEGER  :: globalNodeCount = 0
      INTEGER  :: globalGridCount = 0

      INTEGER                                                :: numberOfGridsAtLevel
      TYPE(NestedQuadTreeGridPtr), DIMENSION(:), ALLOCATABLE :: gridsAtLevel
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initGridWithParameters(self, dx, x0, N, parent, locInParent, level)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid)               :: self
         TYPE (QuadTreeGrid), POINTER      :: parent
         REAL(KIND=RP)      , DIMENSION(3) :: dx, x0
         INTEGER            , DIMENSION(3) :: N , locInParent
         INTEGER                           :: level
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                 :: i,j
         REAL(KIND=RP)           :: x(3)
         TYPE (SMNode) , POINTER :: node => NULL()
!
!        -------------------
!        Self initialization
!        -------------------
!
         CALL self % FTObject % init()

         self % level        = level
         self % templateType = 0 !Subdivision into arbitrary number of quads
         self % N            = N
         self % dx           = dx
         self % x0           = x0
         self % locInParent  = locInParent
         self % boundaryTest = UNDEFINED
         self % rotation     = 0
!
!        ---------------------------------------------------------------------------------
!        Allocate and save the array of nodes for this grid. This is the starting point
!        for the refinement process. If there is no refinement, these are all of the nodes
!        used in the mesh. Nodes can have multiple ownership.
!        ---------------------------------------------------------------------------------
!
         ALLOCATE( self % nodes(0:N(1),0:N(2)) )
         DO j = 0, self % N(2)
            DO i = 0, self % N(1)
               CALL GetGridPosition( self % x0, self % dx, i  , j  , x )
               ALLOCATE(node)
               CALL node % initWithLocationAndID(x,UNDEFINED)
               node % level = level
               self % nodes(i,j) % node => node
            END DO
         END DO
!
!        ----------------------------------------------------
!        Mark nodes now as corner nodes at the highest level
!        in case there is no outer boundary.
!        ----------------------------------------------------
!
         IF( level == 0 )     THEN
            DO i = 0, N(1)
               self % nodes(i,0)    % node % bCurveID      = BOTTOM
               self % nodes(i,N(2)) % node % bCurveID      = TOP
               self % nodes(i,0)    % node % bCurveChainID = OUTER_BOX
               self % nodes(i,N(2)) % node % bCurveChainID = OUTER_BOX
            END DO
            DO j = 0, N(2)
               self % nodes(0,j)    % node % bCurveID      = LEFT
               self % nodes(N(1),j) % node % bCurveID      = RIGHT
               self % nodes(0,j)    % node % bCurveChainID = OUTER_BOX
               self % nodes(N(1),j) % node % bCurveChainID = OUTER_BOX
            END DO
            self % nodes(0,0)       % node % nodeType = CORNER_NODE
            self % nodes(N(1),0)    % node % nodeType = CORNER_NODE
            self % nodes(N(1),N(2)) % node % nodeType = CORNER_NODE
            self % nodes(0,N(2))    % node % nodeType = CORNER_NODE
         END IF
!
!        ----------------------------------------------------------------------------------------
!        Do the same for the quads on this mesh. A quad is owned at its grid level only. For now
!        the nodes are being set so that the locations of the corners are known. Later, after
!        duplicate nodes are removed, the quads will have their node pointers reset.
!        ----------------------------------------------------------------------------------------
!
         ALLOCATE( self % quads(1:N(1), 1:N(2)) )
         DO j = 1, self % N(2)
            DO i = 1, self % N(1)
                self % quads(i,j) % quad => NULL()
            END DO
         END DO
!
!        ---------------------------------------------------------------------------
!        At this point there will be no children for any of the quads. Children will
!        be created during the refinement opteration
!        ---------------------------------------------------------------------------
!
         ALLOCATE( self % children( 1:N(1), 1:N(2) ) )
         DO j = 1, self % N(2)
            DO i = 1, self % N(1)
               NULLIFY(self % children(i,j) % grid)
            END DO
         END DO
!
!        -------------------------------------------
!        Finally just set the Parent if there is one
!        -------------------------------------------
!
         IF( ASSOCIATED(parent) )     THEN
            self % parent => parent ! Weak reference
         ELSE
            NULLIFY( self % parent )
         END IF
!
!        ---------------------------------------------
!        Newly created has no neighbors to be set yet
!        These are not owned by the current grid, only
!        weak references.
!        ---------------------------------------------
!
         NULLIFY( self % neighborL, self % neighborR, &
                  self % neighborT, self % neighborB )

      END SUBROUTINE initGridWithParameters
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE TemplateInit( self, templateType, dx, x0, parent, l, level, rotation )
      IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(QuadTreeGrid)               :: self
         TYPE (QuadTreeGrid), POINTER      :: parent
         REAL(KIND=RP)      , DIMENSION(3) :: dx, x0
         INTEGER            , DIMENSION(3) :: l
         INTEGER                           :: level, rotation, templateType
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                             :: i, j
         INTEGER                             :: N(3) = (/3,3,3/)

         N = refinementType
!
!        ----
!        Self
!        ----
!
         CALL self % FTObject % init()

         self % level        = level
         self % templateType = templateType
         self % N            = N
         self % dx           = dx
         self % x0           = x0
         self % locInParent  = l
         self % boundaryTest = UNDEFINED
         self % rotation     = rotation
!
!        -----------------------------------
!        No further subdivision will be done
!        -----------------------------------
!
         ALLOCATE( self % children( 1:N(1), 1:N(2) ) )
         DO j = 1, self % N(2)
            DO i = 1, self % N(1)
               NULLIFY(self % children(i,j) % grid)
            END DO
         END DO

!
         IF( ASSOCIATED(parent) )     THEN
            self % parent => parent
         ELSE
            NULLIFY( self % parent )
         END IF
!
!        ---------------------------------------------------------------------------------
!        Compute the locations of the nodal points. They won't all be used, and only the
!        ones that are will be saved. Nodes and quads that aren't needed will have their
!        pointers nullified.
!        ---------------------------------------------------------------------------------
!
         ALLOCATE( self % nodes(0:N(1),0:N(2)) )
         DO j = 0, self % N(2)
            DO i = 0, self % N(1)
               NULLIFY( self % nodes(i,j) % node )
            END DO
         END DO

         ALLOCATE( self % quads(1:N(1), 1:N(2)) )
         DO j = 1, self % N(2)
            DO i = 1, self % N(1)
               NULLIFY( self % quads(i,j) % quad )
            END DO
         END DO
!
!        ---------------------------------------------
!        Newly created has no neighbors to be set yet
!        These are not owned by the current grid, only
!        references.
!        ---------------------------------------------
!
         NULLIFY( self % neighborL, self % neighborR, self % neighborT, self % neighborB )

      END SUBROUTINE TemplateInit
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE DestructGrid(self)
         IMPLICIT NONE
         TYPE(QuadTreeGrid)          :: self
         INTEGER                      :: i,j, N, M
         TYPE (SMNode)      , POINTER :: node => NULL()

         N = self % N(1)
         M = self % N(2)
!
!        --------
!        Children
!        --------
!
         IF( ASSOCIATED( self % children ) )     THEN
            DO j = 1, M
               DO i = 1, N
                  CALL releaseGrid(self % children(i,j) % grid)
               END DO
            END DO
            DEALLOCATE( self % children )
         END IF
!
!        ----
!        Self
!        ----
!
         IF( ASSOCIATED(self % quads) )     then
            DO j = 1, self % N(2)
               DO i = 1, self % N(1)
                  CALL releaseSMQuad(self % quads(i,j) % quad)
               END DO
            END DO
            DEALLOCATE(self % quads)
         END IF

         IF( ASSOCIATED(self % nodes) )     THEN
            DO j = 0, self % N(2)
               DO i = 0, self % N(1)
                  CALL releaseSMNode(self % nodes(i,j) % node)
               END DO
            END DO
            DEALLOCATE( self % nodes )
         END IF

         NULLIFY( self % neighborL, self % neighborR, self % neighborT, self % neighborB )

         highestLevel         = 0
         globalNodeCount      = 0
         globalGridCount      = 0
         numberOfGridsAtLevel = 0

      END SUBROUTINE DestructGrid
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE releaseGrid(self)
         IMPLICIT NONE
         TYPE (QuadTreeGrid), POINTER :: self
         CLASS(FTObject)    , POINTER :: obj

         IF(.NOT. ASSOCIATED(self)) RETURN

         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL()
         END IF
      END SUBROUTINE releaseGrid
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE RefineGrid_ToSizeFunction_( self, sizer )
      IMPLICIT NONE
!
!     ----------------------------------------------------
!     Recursively subdivide the QuadTreeGrid until each quad
!     within the grid satisfies the sizeFunction.
!     ----------------------------------------------------
!
!      ---------
!      Arguments
!      ---------
!
      TYPE (QuadTreeGrid), POINTER :: self
      TYPE(MeshSizer)              :: sizer
!
!     ---------------
!     Local Variables
!     ---------------
!
      INTEGER                      :: i, j, nX, nY
      INTEGER                      :: N(3) = 3
      REAL(KIND=RP)                :: hMin, xMin(3), xMax(3), dX(3)
      TYPE (QuadTreeGrid), POINTER :: childGrid
      CHARACTER(LEN=256)           :: limitMsg
      CHARACTER(LEN=32)            :: xMinAsString
!
      N  = refinementType
      nX = self % N(1)
      nY = self % N(2)
      IF(catch() .AND. (maximumErrorSeverity() == FT_ERROR_FATAL)) RETURN
!
!     ----------------
!     Do for each quad
!     ----------------
!
      NULLIFY(childGrid)

      DO j = 1, nY
         DO i = 1, nX
!
!           -----------------------------------------
!           Compute size of this quad within the grid
!           and the size function within the quad.
!           -----------------------------------------
!
            CALL GetGridPosition( self % x0, self % dx, i-1, j-1, xMin )
            CALL GetGridPosition( self % x0, self % dx, i  , j  , xMax )
            hMin = sizeFunctionMinimumOnBox( sizer, xMin, xMax )

            IF ( hMin - MAXVAL(self % dx(1:2)) < -subdivisionRelTol*MAXVAL(self % dx(1:2)))     THEN

               IF ( highestLevel < maxLevelLimit )     THEN

                  dx = self % dx/DBLE(refinementType)
                  ALLOCATE(childGrid)
                  CALL childGrid % initGridWithParameters( dx, xMin, N, self, (/i,j,0/), self % level+1)
                  self % children(i,j) % grid => childGrid
                  CALL SetNeighborPointers( childGrid )

                  CALL RefineGrid_ToSizeFunction_( childGrid, sizer )

                  highestLevel = MAX( highestLevel, self % level+1 )

               ELSE

                  WRITE(xMinAsString, '(F7.2,1x,F7.2)') xMin(1), xMin(2)
                  limitMsg = "Resolution near " // xMinAsString // " needs more subdivisions than the currently allowed. " //&
                              "To override, rerun with the command line flag '-sLimit'. But think before doing this."

                  CALL ThrowErrorExceptionOfType(poster = "RefineGrid_ToSizeFunction_", &
                                                 msg    = limitMsg,       &
                                                 typ    = FT_ERROR_FATAL)
                  RETURN
               END IF

            END IF

         END DO
      END DO

      END SUBROUTINE RefineGrid_ToSizeFunction_
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetNeighborPointers( self )
!
!     -----------------------------------------------------------
!     Set the neighbor pointers to grids at the same level as the
!     current grid, if there are any. The pointers are Weak.
!     -----------------------------------------------------------
!
      IMPLICIT NONE
      TYPE (QuadTreeGrid), POINTER :: self
      TYPE (QuadTreeGrid), POINTER :: neighbor, parent
      INTEGER                      :: i, j, n, m

      IF( self % level == 0 )     RETURN ! Root grid has no neighbors.

      i = self % locInParent(1)
      j = self % locInParent(2)
!
!     -------------
!     Left neighbor
!     -------------
!
      NULLIFY( neighbor )
      IF( i > 1 )     THEN
         neighbor => ChildGridAt_InGrid_(i-1,j,self % parent)
      ELSE
         parent   => self % parent
         neighbor => parent % neighborL
         IF( ASSOCIATED(neighbor) )     THEN
            n        = neighbor % N(1)
            neighbor => ChildGridAt_InGrid_(n,j,neighbor)
         END IF
      END IF
      IF( ASSOCIATED(neighbor) )     THEN
         self % neighborL     => neighbor
         neighbor % neighborR => self
      END IF
!
!     --------------
!     Right neighbor
!     --------------
!
      NULLIFY( neighbor )
      IF( i < self % parent % N(1) )     THEN
         neighbor => ChildGridAt_InGrid_(i+1,j,self % parent)
      ELSE
         parent   => self % parent
         neighbor => parent % neighborR
         IF( ASSOCIATED(neighbor) )     THEN
            n        = 1
            neighbor => ChildGridAt_InGrid_(n,j,neighbor)
         END IF
      END IF
      IF( ASSOCIATED(neighbor) )     THEN
         self % neighborR     => neighbor
         neighbor % neighborL => self
      END IF
!
!     ---------------
!     Bottom neighbor
!     ---------------
!
      NULLIFY( neighbor )
      IF( j > 1 )     THEN
         neighbor => ChildGridAt_InGrid_(i,j-1,self % parent)
      ELSE
         parent   => self % parent
         neighbor => parent % neighborB
         IF( ASSOCIATED(neighbor) )     THEN
            m        = neighbor % N(2)
            neighbor => ChildGridAt_InGrid_(i,m,neighbor)
         END IF
      END IF
      IF( ASSOCIATED(neighbor) )     THEN
         self % neighborB     => neighbor
         neighbor % neighborT => self
      END IF
!
!     ------------
!     Top neighbor
!     ------------
!
      NULLIFY( neighbor )
      IF( j < self % parent % N(2) )     THEN
         neighbor => ChildGridAt_InGrid_(i,j+1,self % parent)
      ELSE
         parent   => self % parent
         neighbor => parent % neighborT
         IF( ASSOCIATED(neighbor) )     THEN
            m        = 1
            neighbor => ChildGridAt_InGrid_(i,m,neighbor)
         END IF
      END IF
      IF( ASSOCIATED(neighbor) )     THEN
         self % neighborT     => neighbor
         neighbor % neighborB => self
      END IF

      END SUBROUTINE SetNeighborPointers

!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION ChildGridAt_InGrid_(i,j,grid) RESULT(p)
         IMPLICIT NONE
         INTEGER                      :: i, j
         TYPE (QuadTreeGrid), POINTER :: grid, p

         IF( ASSOCIATED(grid) .AND. ASSOCIATED(grid % children) )     THEN
            p => grid % children(i,j) % grid
         ELSE
            NULLIFY(p)
         END IF

      END FUNCTION ChildGridAt_InGrid_
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION NodeAt_InGrid_(i,j,grid) RESULT(p)
         IMPLICIT NONE
         INTEGER                     :: i, j
         TYPE (QuadTreeGrid), POINTER :: grid
         TYPE (SMNode)      , POINTER :: p

         IF ( ASSOCIATED(grid) )     THEN
            p=> grid % nodes(i,j) % node
         ELSE
            NULLIFY(p)
         END IF

      END FUNCTION NodeAt_InGrid_
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION QuadAt_InGrid_(i,j,grid) RESULT(p)
      IMPLICIT NONE
      INTEGER                      :: i, j
      TYPE (QuadTreeGrid), POINTER :: grid
      CLASS(SMQuad)    , POINTER   :: p

      IF ( ASSOCIATED(grid) )     THEN
         p=> grid % quads(i,j) % quad
      ELSE
         NULLIFY(p)
      END IF

      END FUNCTION QuadAt_InGrid_
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE AssignNodeIdsForGrid_( self )
      IMPLICIT NONE

      TYPE (QuadTreeGrid) :: self
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER :: i, j, N, M

      N = self % N(1)
      M = self % N(2)
!
!     --------
!     For self
!     --------
!
      DO j = 0, M
         DO i = 0, N
            IF( .NOT.ASSOCIATED(self % nodes(i,j) % node) )    CYCLE
            IF( self % nodes(i,j) % node % refCount()   == 1 )       CYCLE ! Only the main grid, not quads references this
            IF( self % nodes(i,j) % node % activeStatus == REMOVE )  CYCLE ! Marked as outside
            IF( self % nodes(i,j) % node % id           == UNDEFINED )     THEN

               globalNodeCount               = globalNodeCount + 1
               self % nodes(i,j) % node % id = globalNodeCount

            END IF
         END DO
      END DO
!
!     ------------
!     For children
!     ------------
!
      IF( ASSOCIATED(self % children) )     THEN
         DO j = 1, M
            DO i = 1, N
               IF( ASSOCIATED( self % children(i,j) % grid ) ) &
                   CALL AssignNodeIdsForGrid_( self % children(i,j) % grid )
            END DO
         END DO
      END IF

      END SUBROUTINE AssignNodeIdsForGrid_
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE FlagNodeIds( self, flag )
      IMPLICIT NONE
!
!     --------------------------------------------------------------------------
!     Flag a node by setting its id sign to negative. Unflag by making positive.
!     --------------------------------------------------------------------------
!
      TYPE (QuadTreeGrid) :: self
      LOGICAL            :: flag
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER :: i, j, N, M

      N = self % N(1)
      M = self % N(2)
!
!     --------
!     For self
!     --------
!
      IF ( flag )     then
         DO j = 0, M
            DO i = 0, N
               IF(ASSOCIATED(self % nodes(i,j) % node)) self % nodes(i,j) % node % id = -ABS(self % nodes(i,j) % node % id)
            END DO
         END DO
      ELSE
         DO j = 0, M
            DO i = 0, N
               IF(ASSOCIATED(self % nodes(i,j) % node)) self % nodes(i,j) % node % id = ABS(self % nodes(i,j) % node % id)
            END DO
         END DO
      END IF
!
!     ------------
!     For children
!     ------------
!
      IF( .NOT.ASSOCIATED(self % children) )   RETURN

      DO j = 1, M
         DO i = 1, N
            IF( ASSOCIATED( self % children(i,j) % grid ) ) &
                CALL FlagNodeIds( self % children(i,j) % grid, flag )
         END DO
      END DO

      END SUBROUTINE FlagNodeIds
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE DeleteDuplicateNodesForGrid( self )
      IMPLICIT NONE
      TYPE (QuadTreeGrid), POINTER :: self

      INTEGER                      :: i, j, N, M, Nc, Mc
      TYPE (QuadTreeGrid), POINTER :: parent, neighbor

      N = self%N(1); M = self%N(2)
!
!     ------------------------------------------------------
!     Point corner nodes on this grid to corners of quad
!     on the parent, if one exists.
!     ------------------------------------------------------
!

      IF( ASSOCIATED(self%parent) )     THEN
         parent => self%parent
         i = self%locInParent(1); j = self%locInparent(2)

         IF( ASSOCIATED(self%nodes(0,0)%node       ) .AND. &
             ASSOCIATED(parent%nodes(i-1,j-1)%node) )     THEN
            CALL PointNodePtr_To_(self%nodes(0,0), parent%nodes(i-1,j-1) )
         END IF

         IF( ASSOCIATED(self%nodes(N,0)%node       ) .AND. &
             ASSOCIATED(parent%nodes(i,j-1)%node) )     THEN
            CALL PointNodePtr_To_(self%nodes(N,0), parent%nodes(i,j-1) )
         END IF

         IF( ASSOCIATED(self%nodes(N,M)%node       ) .AND. &
             ASSOCIATED(parent%nodes(i,j)%node) )     THEN
            CALL PointNodePtr_To_(self%nodes(N,M), parent%nodes(i,j) )
         END IF

         IF( ASSOCIATED(self%nodes(0,M)%node       ) .AND. &
             ASSOCIATED(parent%nodes(i-1,j)%node) )     THEN
            CALL PointNodePtr_To_(self%nodes(0,M), parent%nodes(i-1,j) )
         END IF

      END IF
!
!     ------------------------------------------
!     Finally, do the same along grid boundaries
!     by finding the neighbors to the current
!     grid.
!     ------------------------------------------
!
!
!     ---------
!     Left side
!     ---------
!

      IF( ASSOCIATED(self%neighborL) )     THEN
         neighbor => self%neighborL
         Nc = neighbor%N(1); Mc = neighbor%N(2)
         i = 0
         DO j = 1, M-1 ! Must assume that grids have the same size (3x3) or (2x2)
            IF( ASSOCIATED(self%nodes(i,j)%node       ) .AND. &
                ASSOCIATED(neighbor%nodes(Nc,j)%node) )     THEN
               CALL PointNodePtr_To_(self%nodes(i,j), neighbor%nodes(Nc,j) )
            END IF
         END DO
      END IF
!
!     ----------
!     Right side
!     ----------
!
      IF( ASSOCIATED(self%neighborR) )     THEN
         neighbor => self%neighborR
         Nc = neighbor%N(1); Mc = neighbor%N(2)
         i = N
         DO j = 1, M-1 ! Must assume that grids have the same size (3x3)  or (2x2)
            IF( ASSOCIATED(self%nodes(i,j)%node       ) .AND. &
                ASSOCIATED(neighbor%nodes(0,j)%node) )     THEN
               CALL PointNodePtr_To_(self%nodes(i,j), neighbor%nodes(0,j) )
            END IF
         END DO
      END IF
!
!     -----------
!     Bottom side
!     -----------
!
      IF( ASSOCIATED(self%neighborB) )     THEN
         neighbor => self%neighborB
         Nc = neighbor%N(1); Mc = neighbor%N(2)
         j = 0
         DO i = 1, N-1 ! Must assume that grids have the same size (3x3) or (2x2)
            IF( ASSOCIATED(self%nodes(i,j)%node       ) .AND. &
                ASSOCIATED(neighbor%nodes(i,Mc)%node) )     THEN
               CALL PointNodePtr_To_(self%nodes(i,j), neighbor%nodes(i,Mc) )
            END IF
         END DO
      END IF
!
!     --------
!     Top side
!     --------
!
      IF( ASSOCIATED(self%neighborT) )     THEN
         neighbor => self%neighborT
         Nc = neighbor%N(1); Mc = neighbor%N(2)
         j = M
         DO i = 1, N-1 ! Must assume that grids have the same size (3x3) or (2x2)
            IF( ASSOCIATED(self%nodes(i,j)%node       ) .AND. &
                ASSOCIATED(neighbor%nodes(i,0)%node) )     THEN
               CALL PointNodePtr_To_(self%nodes(i,j), neighbor%nodes(i,0) )
            END IF
         END DO
      END IF
!
!     ----------------------------------------------------
!     Continue with each of the child grids below this one
!     ----------------------------------------------------
!
      DO j = 1, M
         DO i = 1, N
            IF( ASSOCIATED( self%children(i,j)%grid ) )  THEN
               CALL DeleteDuplicateNodesForGrid( self%children(i,j)%grid )
            END IF
         END DO
      END DO

      END SUBROUTINE DeleteDuplicateNodesForGrid
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE ConstructQuads( self )
!
!     --------------------------------------------------
!     Routine that creates the quads for template0 grids
!     --------------------------------------------------
!
      IMPLICIT NONE
      TYPE (QuadTreeGrid), POINTER :: self
      INTEGER                   :: i, j, N, M

      N = self % N(1)
      M = self % N(2)

      DO j = 1, M
         DO i = 1, N
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
!
!     --------------------------
!     Continue with the children
!     --------------------------
!
      IF( .NOT.ASSOCIATED(self % children) )      RETURN

      DO j = 1, M
         DO i = 1, N
            IF( ASSOCIATED( self % children(i,j) % grid ) )  CALL ConstructQuads( self % children(i,j) % grid )
         END DO
      END DO

      END SUBROUTINE ConstructQuads
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE AssignNodeLevels( self )
      IMPLICIT NONE
      TYPE (QuadTreeGrid), POINTER :: self

      INTEGER                    :: i, j,  N, M

      N = self % N(1)
      M = self % N(2)
!
!     -----------------
!     Set level on self
!     -----------------
!
      DO j = 0, M
         DO i = 0, N
            IF( ASSOCIATED(self % nodes(i,j) % node) ) THEN
               self % nodes(i,j) % node % level = MAX( self % nodes(i,j) % node % level, self % level )
            END IF
         END DO
      END DO
!
!     --------------------------
!     Continue with the children
!     --------------------------
!
      IF( ASSOCIATED(self % children) )     THEN
         DO j = 1, M
            DO i = 1, N
               IF( ASSOCIATED( self % children(i,j) % grid ) )  CALL AssignNodeLevels( self % children(i,j) % grid )
            END DO
         END DO
      END IF

      END SUBROUTINE AssignNodeLevels
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE ClearNodeLevels( self )
      IMPLICIT NONE
      TYPE (QuadTreeGrid), POINTER :: self

      INTEGER                    :: i, j,  N, M

      N = self % N(1)
      M = self % N(2)
!
!     -----------------
!     Set level on self
!     -----------------
!
      DO j = 0, M
         DO i = 0, N
            IF( ASSOCIATED(self % nodes(i,j) % node) ) self % nodes(i,j) % node % level = 0
         END DO
      END DO
!
!     --------------------------
!     Continue with the children
!     --------------------------
!
      IF( ASSOCIATED(self % children) )     THEN
         DO j = 1, M
            DO i = 1, N
               IF( ASSOCIATED( self % children(i,j) % grid ) )  CALL AssignNodeLevels( self % children(i,j) % grid )
            END DO
         END DO
      END IF

      END SUBROUTINE ClearNodeLevels
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE FindNumberOfGridsIn_AtLevel_( self, level )
!
      IMPLICIT NONE
      INTEGER                   :: level
      TYPE (QuadTreeGrid), POINTER :: self

      INTEGER :: N, M, i, j
!
      IF( self % level ==  level )     THEN
         numberOfGridsAtLevel = numberOfGridsAtLevel + 1
      ELSE
         N = self % N(1); M = self % N(2)
!
         IF( ASSOCIATED( self % children ) )    THEN
            DO j = 1, M
               DO i = 1, N
                  IF( ASSOCIATED( self % children(i,j) % grid ) ) THEN
                      CALL FindNumberOfGridsIn_AtLevel_( self % children(i,j) % grid, level )
                  END IF
               END DO
            END DO
         END IF

      END IF

      END SUBROUTINE FindNumberOfGridsIn_AtLevel_
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE GatherGridsAtLevel_FromRtGrid_( level, self )
      IMPLICIT NONE
      INTEGER                      :: level
      TYPE (QuadTreeGrid), POINTER :: self
      INTEGER                      :: N,M, i, j

      N = self % N(1); M = self % N(2)

      IF( self % level ==  level )     THEN
         globalGridCount = globalGridCount + 1
         gridsAtLevel( globalGridCount ) % grid => self
      ELSE
         N = self % N(1); M = self % N(2)

         IF( ASSOCIATED( self % children ) )     then
            DO j = 1, M
               DO i = 1, N
                  IF( ASSOCIATED( self % children(i,j) % grid ) ) &
                      CALL GatherGridsAtLevel_FromRtGrid_( level, self % children(i,j) % grid )
               END DO
            END DO
         END IF

      END IF

      END SUBROUTINE GatherGridsAtLevel_FromRtGrid_
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GetGridPosition( x0, dx, i, j, x )
         IMPLICIT NONE
         REAL(KIND=RP), DIMENSION(3), INTENT(IN)  :: x0, dx
         INTEGER                    , INTENT(IN)  :: i,j
         REAL(KIND=RP), DIMENSION(3), INTENT(OUT) :: x
         x(1) = x0(1) + i*dx(1)
         x(2) = x0(2) + j*dx(2)
         x(3) = 0.0_RP
      END SUBROUTINE GetGridPosition
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GetGridLocation( x0, dx, i, j, x )
         IMPLICIT NONE
         REAL(KIND=RP), DIMENSION(2), INTENT(IN)  :: x0, dx
         REAL(KIND=RP), DIMENSION(2), INTENT(IN)  :: x
         INTEGER                    , INTENT(OUT) :: i,j
         i = NINT((x(1) - x0(1))/dx(1))
         j = NINT((x(2) - x0(2))/dx(2))
      END SUBROUTINE GetGridLocation
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE PrintGridDiagnostics( self )
      IMPLICIT NONE
      TYPE (QuadTreeGrid), POINTER :: self

      INTEGER                :: i, j, k, N, M
      TYPE (SMNode), POINTER :: node
      CLASS(SMQuad), POINTER :: quad

      N = self % N(1)
      M = self % N(2)
!
!     -------
!     Globals
!     -------
!
      PRINT *, "Highest level = ", highestLevel, " Refcount = ", self % refcount()
!
!     -----------------
!     Set level on self
!     -----------------
!
      PRINT *, "Nodes at level ", self % level
      DO j = 0, M
         DO i = 0, N
            IF( .NOT.ASSOCIATED( self % nodes(i,j) % node ) ) CYCLE
            node => self % nodes(i,j) % node
            PRINT *, i, j, self % nodes(i,j) % node % refCount(), node % id, node % level, node % x
         END DO
      END DO

      PRINT *, "Quads at level ", self % level, "Dx = ", self % dx
      DO j = 1, M
         DO i = 1, N
            IF( .NOT.ASSOCIATED(self % quads(i,j) % quad) )     CYCLE
            quad => self % quads(i,j) % quad
            PRINT *, i, j
            DO k = 1, 4
               PRINT *, quad % nodes(k) % node % id, quad % nodes(k) % node % x
            END DO
         END DO
      END DO
      PRINT *, "Neighbors to this grid", self % locInParent
      IF(ASSOCIATED(self % neighborL))   PRINT *, "left", self % neighborL % x0
      IF(ASSOCIATED(self % neighborR))   PRINT *, "right", self % neighborR % x0
      IF(ASSOCIATED(self % neighborT))   PRINT *, "top", self % neighborT % x0
      IF(ASSOCIATED(self % neighborB))   PRINT *, "bottom", self % neighborB % x0
!
!     --------------------------
!     Continue with the children
!     --------------------------
!
      IF (ASSOCIATED( self % children ) )     THEN
         DO j = 1, M
            DO i = 1, N
               IF( ASSOCIATED( self % children(i,j) % grid ) )  CALL PrintGridDiagnostics( self % children(i,j) % grid )
            END DO
         END DO
      END IF

      END SUBROUTINE PrintGridDiagnostics
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE printQuadTreeGridDescription(self,iUnit)
         IMPLICIT NONE
         CLASS(QuadTreeGrid) :: self
         INTEGER             :: iUnit
         WRITE(iUnit, *) "QuadTreeGrid object"
         WRITE(iUnit,*) "Level = ", self % level, "Template type = ", self % templateType
      END SUBROUTINE printQuadTreeGridDescription
      END Module QuadTreeGridClass

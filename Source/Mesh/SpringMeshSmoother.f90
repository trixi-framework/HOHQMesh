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
!
!////////////////////////////////////////////////////////////////////////
!
!      SpringMeshSmoother.f90
!      Created: 2010-10-05 09:54:22 -0400
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
      Module SpringMeshSmootherClass
      USE SMMeshClass
      USE SMModelClass
      USE MeshBoundaryMethodsModule
      USE MeshSmootherClass
      USE Geometry, ONLY: angleBetween
      USE ProgramGlobals, ONLY: chainWasPaved, MAX_PAVED_CHAIN_ID
      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      INTEGER, PARAMETER :: LINEAR_SPRING = 0, CROSS_SPRING = 1, BOTH = 2
!
!     ---------------------
!     Class type definition
!     ---------------------
!
      TYPE, EXTENDS(MeshSmoother) :: SpringMeshSmoother
         REAL(KIND=RP) :: springConstant     ! Spring constant
         REAL(KIND=RP) :: restLength         ! Natural LENGTH
         REAL(KIND=RP) :: dampingCoefficient ! Damping coefficient
         REAL(KIND=RP) :: mass               ! Node mass
         REAL(KIND=RP) :: deltaT
         INTEGER       :: springType
         INTEGER       :: numSmoothingSteps
         INTEGER       :: numberOfSmoothings = 0 ! times smoothMesh has been called
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: init       => initSmoother
         FINAL     :: DestructSpringMeshSmoother
         PROCEDURE :: smoothMesh => SpringSmoothMesh
      END TYPE SpringMeshSmoother
!
!     ----------------------
!     Other type definitions
!     ----------------------
!
      TYPE SpringSmootherParameters
         LOGICAL       :: smoothingOn
         REAL(KIND=RP) :: springConstant     ! Spring constant
         REAL(KIND=RP) :: restLength         ! Natural length
         REAL(KIND=RP) :: dampingCoefficient ! Damping coefficient
         REAL(KIND=RP) :: mass               ! Node mass
         REAL(KIND=RP) :: deltaT
         INTEGER       :: numSteps
         INTEGER       :: springType
      END TYPE SpringSmootherParameters
!
!     ---------------------------
!     Module arrays for smoothing
!     ---------------------------
!
      REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE, PRIVATE :: nodeAcceleration, nodeVelocity
!
!     ----------------------------------------------------------------
!     Nodes next to a sharp corner of an interface curve that can only
!     move along the extension of one side of the corner, see
!     SetInterfaceCornerConstraints
!     ----------------------------------------------------------------
!
      REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE, PRIVATE :: cornerDirection, cornerOrigin
      REAL(KIND=RP), DIMENSION(:)  , ALLOCATABLE, PRIVATE :: cornerMinDistance
      LOGICAL      , DIMENSION(:)  , ALLOCATABLE, PRIVATE :: hasCornerConstraint
!
!     How the acceleration of a node is constrained, see ConstrainBoundaryNodes
!
      INTEGER, PARAMETER, PRIVATE :: FREE_NODE = 0, INACTIVE_NODE = 1, NODE_ON_CURVE = 2, NODE_ON_CORNER_LINE = 3
      REAL(KIND=RP), PARAMETER                  , PRIVATE :: MAX_CONSTRAINED_CORNER_ANGLE = 150.0_RP
!
!     Two nodes of an edge on a paved interface curve may not slide closer
!     than this fraction of their distance before smoothing
!
      REAL(KIND=RP), PARAMETER                  , PRIVATE :: MIN_PAVED_EDGE_FRACTION = 0.75_RP
!
!     Default parameters
!
      REAL(KIND=RP), PRIVATE :: defaultSpringConstant           = 1.0_RP
      REAL(KIND=RP), PRIVATE :: defaultSpringMass               = 1.0_RP
      REAL(KIND=RP), PRIVATE :: defaultSpringRestLength         = 0.0_RP
      REAL(KIND=RP), PRIVATE :: defaultSpringDampingCoefficient = 5.0_RP
      REAL(KIND=RP), PRIVATE :: defaultSpringTimeStep           = 0.1_RP
      INTEGER      , PRIVATE :: defaultSpringIterations         = 15
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initSmoother( self, springConstant, mass, restLength, &
                               dampingCoefficient, springType, deltaT, numSteps  )
         IMPLICIT NONE
         CLASS(SpringMeshSmoother) :: self
         REAL(KIND=RP)             :: springConstant, restLength
         REAL(KIND=RP)             :: dampingCoefficient, mass, deltaT
         INTEGER                   :: springType, numSteps

         self % springConstant     = springConstant
         self % restLength         = restLength
         self % dampingCoefficient = dampingCoefficient
         self % springType         = springType
         self % mass               = mass
         self % deltaT             = deltaT
         self % numSmoothingSteps  = numSteps
         self % numberOfSmoothings = 0
      END SUBROUTINE initSmoother
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DestructSpringMeshSmoother( self )
         IMPLICIT NONE
         TYPE(SpringMeshSmoother) :: self
         IF(self % restLength >= 0.0_RP)     CONTINUE
         !Do nothing
      END SUBROUTINE DestructSpringMeshSmoother
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SpringSmoothMesh( self, mesh, model, errorCode )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS (SpringMeshSmoother)    :: self
         TYPE  (SMMesh)      , POINTER :: mesh
         TYPE  (SMModel)     , POINTER :: model
         INTEGER                       :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                      :: k, nodeID, N
         REAL(KIND=RP)                :: maxV
         INTEGER      , DIMENSION(:,:), ALLOCATABLE :: pavedEdges
         REAL(KIND=RP), DIMENSION(:)  , ALLOCATABLE :: minPavedEdgeLength
         REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE :: xPrevious
         TYPE(SMNode)       , POINTER :: node => NULL()
         CLASS(FTObject)    , POINTER :: obj => NULL()
         CLASS(FTLinkedList), POINTER :: boundaryNodesList => NULL()

         TYPE(SMNodePtr), DIMENSION(:)  , ALLOCATABLE :: nodePointers
         REAL(KIND=RP)  , DIMENSION(:,:), ALLOCATABLE :: x, tangent
         INTEGER        , DIMENSION(:,:), ALLOCATABLE :: edgeNodes, elementNodes
         INTEGER        , DIMENSION(:)  , ALLOCATABLE :: constraintType
         LOGICAL        , DIMENSION(:)  , ALLOCATABLE :: getsEdgeForce, getsCrossForce
!
!        -------------------------
!        Allocate temporary memory
!        -------------------------
!
         CALL renumberObjects(mesh,NODES) ! To guarantee sync.
         N = mesh % nodes % COUNT()
         ALLOCATE( nodeAcceleration(3,N) )
         ALLOCATE( nodeVelocity(3,N) )

         nodeVelocity     = 0.0_RP
         nodeAcceleration = 0.0_RP

         CALL mesh % nodesIterator % setToStart()
         DO WHILE ( .NOT.mesh % nodesIterator % isAtEnd() )
            obj => mesh % nodesIterator % object()
            CALL castToSMNode(obj,node)
            node  %  activeStatus = ACTIVE
            CALL mesh % nodesIterator % moveToNext()
         END DO
         CALL SetNodeActiveStatus( mesh, model, errorCode )

         ALLOCATE( cornerDirection(3,N), cornerOrigin(3,N), cornerMinDistance(N), hasCornerConstraint(N) )
!
!        -------------------------------------------------------------
!        The nodes next to a corner are held on the extensions of its
!        sides only in the first smoothing. By the next smoothing, on
!        interface boundaries, the elements at the corner have been
!        split (SplitInterfaceElements) and holding the new nodes on
!        those lines squeezes the elements between them into slivers.
!        -------------------------------------------------------------
!
         CALL SetInterfaceCornerConstraints( mesh, constrainToSides = self % numberOfSmoothings == 0 )
         self % numberOfSmoothings = self % numberOfSmoothings + 1
!
!        ------------------------
!        Do spring-mass smoothing
!        ------------------------
!
!
!        The mesh does not change while it is smoothed, only the node
!        locations do. So the nodes, the connectivity, and the constraints
!        are gathered into arrays, the steps work on the locations x, and
!        the nodes get their new locations at the end. Nodes, edges and
!        elements are visited in the order of their lists.
!
         CALL GatherSmoothingArrays( mesh, model, nodePointers, x, edgeNodes, elementNodes, &
                                     getsEdgeForce, getsCrossForce, constraintType, tangent )
         CALL GatherPavedCurveEdges( nodePointers, x, edgeNodes, constraintType, pavedEdges, minPavedEdgeLength )
         ALLOCATE( xPrevious(3,N) )

         DO k = 1, self % numSmoothingSteps
            nodeAcceleration = 0.0_RP
            CALL AccumulateAcceleration( self, x, edgeNodes, elementNodes, getsEdgeForce, getsCrossForce )
            CALL ConstrainBoundaryNodes( constraintType, tangent )
            xPrevious = x

            maxV = 0.0_RP
            DO nodeID = 1, N
               x(:,nodeID)            = x(:,nodeID) + self % deltaT*nodeVelocity(:,nodeID)
               nodeVelocity(:,nodeID) = nodeVelocity(:,nodeID) + &
                                        self % deltaT*( nodeAcceleration(:,nodeID) - &
                                        self % dampingCoefficient*nodeVelocity(:,nodeID) )
               IF ( hasCornerConstraint(nodeID) )     CALL KeepNodeAwayFromCorner( nodeID, x(:,nodeID), nodeVelocity(:,nodeID) )
               maxV = MAX(maxV, ABS(nodeVelocity(1,nodeID)), ABS(nodeVelocity(2,nodeID)) )
            END DO
            CALL KeepPavedCurveNodesApart( x, xPrevious, pavedEdges, minPavedEdgeLength )

         END DO

         DO nodeID = 1, N
            nodePointers(nodeID) % node % x = x(:,nodeID)
         END DO
!
!        ---------------------------
!        Deallocate temporary Memory
!        ---------------------------
!
         DEALLOCATE( nodeAcceleration )
         DEALLOCATE( nodeVelocity )
         DEALLOCATE( cornerDirection, cornerOrigin, cornerMinDistance, hasCornerConstraint )
!
!        --------------------------------------------------------------------
!        The boundary nodes have been moved. Find their parameterized location
!        along their respective boundaries
!        --------------------------------------------------------------------
!
         ALLOCATE(boundaryNodesList)
         CALL boundaryNodesList % init()
         CALL CollectBoundaryAndInterfaceNodes( mesh % nodesIterator, boundaryNodesList )
         CALL FindCurveLocationsforNodes      ( boundaryNodesList, model )
         CALL releaseFTLinkedListClass(self = boundaryNodesList)

      END SUBROUTINE SpringSmoothMesh
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE LinearSpringForce( springConstant, restLength, selfLoc, neighborLoc, force )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP)              , INTENT(IN)  :: springConstant, restLength
         REAL(KIND=RP), DIMENSION(3), INTENT(IN)  :: selfLoc, neighborLoc
         REAL(KIND=RP), DIMENSION(3), INTENT(OUT) :: force
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP), DIMENSION(3) ::  v
         REAL(KIND=RP)               :: vNorm

         v     = neighborLoc - selfLoc
         vNorm = SQRT( v(1)*v(1) + v(2)*v(2) ) + restLength
         force = springConstant*v

      END SUBROUTINE LinearSpringForce
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GatherSmoothingArrays( mesh, model, nodePointers, x, edgeNodes, elementNodes, &
                                        getsEdgeForce, getsCrossForce, constraintType, tangent )
         IMPLICIT NONE
!
!        ------------------------------------------------------------------
!        Gather what the smoothing steps use, indexed by node id: the nodes
!        and their locations, x, which nodes get the forces of the edge and
!        the cross springs, and how the acceleration of each node is
!        constrained (see ConstrainBoundaryNodes), with the tangent of the
!        curve for a node on a curve. Also the node ids of the edges and
!        the elements, in the order of their lists. The node ids must be
!        numbered 1..N in the order of the node list.
!        ------------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh)                                 :: mesh
         TYPE (SMModel) , POINTER                      :: model
         TYPE(SMNodePtr), DIMENSION(:)  , ALLOCATABLE  :: nodePointers
         REAL(KIND=RP)  , DIMENSION(:,:), ALLOCATABLE  :: x, tangent
         INTEGER        , DIMENSION(:,:), ALLOCATABLE  :: edgeNodes, elementNodes
         INTEGER        , DIMENSION(:)  , ALLOCATABLE  :: constraintType
         LOGICAL        , DIMENSION(:)  , ALLOCATABLE  :: getsEdgeForce, getsCrossForce
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (SMNode)        , POINTER :: node  => NULL()
         TYPE (SMEdge)        , POINTER :: edge  => NULL()
         TYPE (SMElement)     , POINTER :: e     => NULL()
         CLASS(SMChainedCurve), POINTER :: chain => NULL()
         CLASS(SMCurve)       , POINTER :: c     => NULL()
         CLASS(FTObject)      , POINTER :: obj   => NULL()
         INTEGER                        :: N, id, j, k

         N = mesh % nodes % COUNT()
         ALLOCATE( nodePointers(N), x(3,N), tangent(3,N), constraintType(N), getsEdgeForce(N), getsCrossForce(N) )
         tangent = 0.0_RP

         CALL mesh % nodesIterator % setToStart()
         DO WHILE ( .NOT.mesh % nodesIterator % isAtEnd() )
            obj => mesh % nodesIterator % object()
            CALL castToSMNode(obj,node)
            id                  =  node % id
            nodePointers(id) % node => node
            x(:,id)             =  node % x
            getsEdgeForce(id)   =  node % bCurveID >= NONE
            getsCrossForce(id)  =  .NOT.HasFixedPosition( node )

            IF (node % activeStatus == INACTIVE)     THEN
               constraintType(id) = INACTIVE_NODE
            ELSE IF ( node % distToBoundary == 0.0_RP .AND. node % bCurveID > NONE )     THEN
               constraintType(id) = NODE_ON_CURVE
               c                  => model % curveWithID( node % bCurveID, chain )
               tangent(:,id)      =  c % tangentAt( node % whereOnBoundary )
            ELSE IF ( hasCornerConstraint(id) )     THEN
               constraintType(id) = NODE_ON_CORNER_LINE
            ELSE
               constraintType(id) = FREE_NODE
            END IF
            CALL mesh % nodesIterator % moveToNext()
         END DO

         ALLOCATE( edgeNodes(2, mesh % edges % COUNT()) )
         j = 0
         CALL mesh % edgesIterator % setToStart()
         DO WHILE ( .NOT.mesh % edgesIterator % isAtEnd() )
            obj => mesh % edgesIterator % object()
            CALL castToSMEdge(obj,edge)
            j = j + 1
            edgeNodes(:,j) = [edge % nodes(1) % node % id, edge % nodes(2) % node % id]
            CALL mesh % edgesIterator % moveToNext()
         END DO

         ALLOCATE( elementNodes(4, mesh % elements % COUNT()) )
         j = 0
         CALL mesh % elementsIterator % setToStart()
         DO WHILE ( .NOT.mesh % elementsIterator % isAtEnd() )
            obj => mesh % elementsIterator % object()
            CALL castToSMElement(obj,e)
            j = j + 1
            DO k = 1, 4
               elementNodes(k,j) = e % nodes(k) % node % id
            END DO
            CALL mesh % elementsIterator % moveToNext()
         END DO

      END SUBROUTINE GatherSmoothingArrays
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE AccumulateAcceleration( self, x, edgeNodes, elementNodes, getsEdgeForce, getsCrossForce )
         IMPLICIT NONE
         TYPE (SpringMeshSmoother) :: self
         REAL(KIND=RP)             :: x(:,:)
         INTEGER                   :: edgeNodes(:,:), elementNodes(:,:)
         LOGICAL                   :: getsEdgeForce(:), getsCrossForce(:)

         IF ( self % springType == LINEAR_SPRING )     THEN
            CALL LinearAcceleration( self, x, edgeNodes, getsEdgeForce )
         ELSE IF ( self % springType == CROSS_SPRING )   THEN
            CALL CrossAcceleration( self, x, elementNodes, getsCrossForce )
         ELSE IF ( self % springType == BOTH )     THEN
            CALL LinearAcceleration( self, x, edgeNodes, getsEdgeForce )
            CALL CrossAcceleration( self, x, elementNodes, getsCrossForce )
         END IF

      END SUBROUTINE AccumulateAcceleration
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE LinearAcceleration( self, x, edgeNodes, getsEdgeForce )
         IMPLICIT NONE
!
!        ------------------------------------------------------------
!        Add the forces of the springs along the edges. The nodes of
!        the edges are edgeNodes(1:2,:); getsEdgeForce tells whether a
!        node is moved by them.
!        ------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SpringMeshSmoother) :: self
         REAL(KIND=RP)             :: x(:,:)
         INTEGER                   :: edgeNodes(:,:)
         LOGICAL                   :: getsEdgeForce(:)
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                              :: j, id1, id2
         REAL(KIND=RP)                        :: force(3)

         DO j = 1, SIZE(edgeNodes,2)
            id1 = edgeNodes(1,j)
            id2 = edgeNodes(2,j)
            CALL LinearSpringForce( self % springConstant, self % restLength, x(:,id1), x(:,id2), force )

            IF( getsEdgeForce(id1) )     THEN
               nodeAcceleration(:,id1) = nodeAcceleration(:,id1) + force/self % mass
            END IF
            IF( getsEdgeForce(id2) )     THEN
               nodeAcceleration(:,id2) = nodeAcceleration(:,id2) - force/self % mass
            END IF
         END DO

      END SUBROUTINE LinearAcceleration
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CrossAcceleration( self, x, elementNodes, getsCrossForce )
         IMPLICIT NONE
!
!        ------------------------------------------------------------
!        Add the forces of the springs across the diagonals of the
!        elements, whose nodes are elementNodes(1:4,:), on the nodes
!        that are not fixed (getsCrossForce)
!        ------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SpringMeshSmoother) :: self
         REAL(KIND=RP)             :: x(:,:)
         INTEGER                   :: elementNodes(:,:)
         LOGICAL                   :: getsCrossForce(:)
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER       :: j, k, cK, nID
         REAL(KIND=RP) :: force(3)

         DO j = 1, SIZE(elementNodes,2)
            DO k = 1, 4
               nID = elementNodes(k,j)
               IF( .NOT.getsCrossForce(nID) )     CYCLE ! Is on boundary.
               cK  = k + 2
               IF( cK > 4 ) cK = cK - 4

               CALL LinearSpringForce( self % springConstant, self % restLength, x(:,nID), x(:,elementNodes(cK,j)), force )
               nodeAcceleration(:,nID) = nodeAcceleration(:,nID) + 0.5_RP*force/self % mass
            END DO
         END DO

      END SUBROUTINE CrossAcceleration
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstrainBoundaryNodes( constraintType, tangent )
         IMPLICIT NONE
!
!        -------------------------------------------------------------
!        Inactive nodes do not move. A node on a curve moves along the
!        tangent of the curve at it, and a node held on a line from a
!        corner moves along that line.
!        -------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER       :: constraintType(:)
         REAL(KIND=RP) :: tangent(:,:)
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER :: id

         DO id = 1, SIZE(constraintType)
            SELECT CASE ( constraintType(id) )
               CASE ( INACTIVE_NODE )
                  nodeAcceleration(:,id) = 0.0_RP
               CASE ( NODE_ON_CURVE )
                  nodeAcceleration(:,id) = (nodeAcceleration(1,id)*tangent(1,id) + &
                                            nodeAcceleration(2,id)*tangent(2,id))*tangent(:,id)
               CASE ( NODE_ON_CORNER_LINE )
                  nodeAcceleration(:,id) = (nodeAcceleration(1,id)*cornerDirection(1,id) + &
                                            nodeAcceleration(2,id)*cornerDirection(2,id))*cornerDirection(:,id)
               CASE DEFAULT
            END SELECT
         END DO

      END SUBROUTINE ConstrainBoundaryNodes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetInterfaceCornerConstraints( mesh, constrainToSides )
         IMPLICIT NONE
!
!     ---------------------------------------------------------------------
!     At a sharp corner of an interface curve the corner node is pinned and
!     the region on the reflex angle side of the corner is shared by three
!     elements. The two edges from the corner into that region should
!     continue the two sides of the corner, so that the three elements have
!     angles of ≈90 degrees at the corner. The springs only balance edge
!     lengths and pull those two edges together, which causes the middle
!     element to become nearly triangular.
!
!     For such corners, the two nodes at the other ends of those edges
!     are moved onto the extensions of the sides and constrained to move
!     only along them, like boundary nodes along their curve.
!
!     If the reflex side is shared by only two elements, the one edge
!     between them is moved onto the bisector of the reflex angle and
!     constrained to it, so that the two elements share the reflex angle
!     equally (e.g. 135 + 135 degrees at a 90 degree corner) instead of
!     one element getting nearly all of it and folding.
!
!     Corners where the mesh around the corner do not have this layout
!     are left alone. The constraint to the extensions of the sides is
!     only applied when constrainToSides is .TRUE.
!     ---------------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh), POINTER :: mesh
         LOGICAL                :: constrainToSides
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, PARAMETER :: MAX_NEIGHBORS = 12

         TYPE (SMNodePtr), ALLOCATABLE :: nodeForID(:)
         INTEGER         , ALLOCATABLE :: numNeighbors(:), neighbors(:,:)
         LOGICAL         , ALLOCATABLE :: isCorner(:)
         TYPE (SMNode)   , POINTER     :: node => NULL(), nb => NULL()
         TYPE (SMEdge)   , POINTER     :: edge => NULL()
         CLASS(FTObject) , POINTER     :: obj  => NULL()
         INTEGER                       :: n, id, k, m, chainID
         INTEGER                       :: sideNodes(2), freeNodes(MAX_NEIGHBORS), numSide, numFree, numReflex
         INTEGER                       :: reflexNodes(MAX_NEIGHBORS)
         REAL(KIND=RP)                 :: xC(2), d(2,2), u(2), cornerAngle, a1, a2, dots(2,2), s
         REAL(KIND=RP)                 :: bisector(2), minDistance

         n = SIZE(hasCornerConstraint)
         hasCornerConstraint = .FALSE.
         cornerDirection     = 0.0_RP
         cornerOrigin        = 0.0_RP
         cornerMinDistance   = 0.0_RP
         IF ( .NOT.ALLOCATED(boundaryEdgesType) )     RETURN

         ALLOCATE( nodeForID(n), numNeighbors(n), neighbors(MAX_NEIGHBORS,n), isCorner(n) )
         numNeighbors = 0
         isCorner     = .FALSE.
!
!        --------------------------------------------------------------
!        Corner candidates: interface nodes pinned at a joint of the
!        curve. Whether the curve really turns there is decided below
!        from the angle between the two interface edges at the node.
!        The node type does not tell, since it counts the elements only
!        on one side (a concave corner with two elements on that side
!        is a ROW_SIDE node).
!        --------------------------------------------------------------
!
         CALL mesh % nodesIterator % setToStart()
         DO WHILE ( .NOT.mesh % nodesIterator % isAtEnd() )
            obj => mesh % nodesIterator % object()
            CALL castToSMNode(obj,node)
            nodeForID(node % id) % node => node
            chainID = node % bCurveChainID
            IF ( node % distToBoundary == 0.0_RP .AND. chainID >= 1 .AND. chainID <= SIZE(boundaryEdgesType) )     THEN
               IF ( boundaryEdgesType(chainID) == INTERFACE_EDGES .AND. &
                    (node % whereOnBoundary == 0.0_RP .OR. node % whereOnBoundary == 1.0_RP) )     isCorner(node % id) = .TRUE.
            END IF
            CALL mesh % nodesIterator % moveToNext()
         END DO
         IF ( .NOT.ANY(isCorner) )     RETURN
!
!        -----------------------------
!        Neighbors of the corner nodes
!        -----------------------------
!
         CALL mesh % edgesIterator % setToStart()
         DO WHILE ( .NOT.mesh % edgesIterator % isAtEnd() )
            obj => mesh % edgesIterator % object()
            CALL castToSMEdge(obj,edge)
            DO k = 1, 2
               id = edge % nodes(k) % node % id
               IF ( isCorner(id) .AND. numNeighbors(id) < MAX_NEIGHBORS )     THEN
                  numNeighbors(id) = numNeighbors(id) + 1
                  neighbors(numNeighbors(id),id) = edge % nodes(3-k) % node % id
               END IF
            END DO
            CALL mesh % edgesIterator % moveToNext()
         END DO
!
!        ----------------------
!        Treat each corner node
!        ----------------------
!
         DO id = 1, n
            IF ( .NOT.isCorner(id) )     CYCLE
            node => nodeForID(id) % node
            xC   =  node % x(1:2)
!
!           ----------------------------------------------------
!           The two neighbors along the interface and the others
!           ----------------------------------------------------
!
            numSide = 0
            numFree = 0
            DO m = 1, numNeighbors(id)
               nb => nodeForID(neighbors(m,id)) % node
               IF ( nb % distToBoundary == 0.0_RP .AND. nb % bCurveChainID == node % bCurveChainID )     THEN
                  numSide = numSide + 1
                  IF ( numSide <= 2 )     sideNodes(numSide) = nb % id
               ELSE IF ( nb % distToBoundary /= 0.0_RP .AND. nb % activeStatus /= INACTIVE )     THEN
                  numFree = numFree + 1
                  freeNodes(numFree) = nb % id
               END IF
            END DO
            IF ( numSide /= 2 )     CYCLE
!
!           -----------------------------------------------------
!           Directions continuing the two sides beyond the corner
!           -----------------------------------------------------
!
            minDistance = 0.0_RP
            DO k = 1, 2
               d(:,k)      = xC - nodeForID(sideNodes(k)) % node % x(1:2)
               minDistance = minDistance + 0.25_RP*NORM2(d(:,k))
               d(:,k)      = d(:,k)/NORM2(d(:,k))
            END DO
            cornerAngle = angleBetween(-d(:,1), -d(:,2))
            IF ( cornerAngle > MAX_CONSTRAINED_CORNER_ANGLE )     CYCLE
!
!           ---------------------------------------------------------------
!           Free neighbors on the reflex side, i.e. outside the sharp wedge
!           between the two sides
!           ---------------------------------------------------------------
!
            numReflex = 0
            DO m = 1, numFree
               u  = nodeForID(freeNodes(m)) % node % x(1:2) - xC
               a1 = angleBetween(u, -d(:,1))
               a2 = angleBetween(u, -d(:,2))
               IF ( ABS(a1 + a2 - cornerAngle) > 1.0E-6_RP*cornerAngle )     THEN
                  numReflex = numReflex + 1
                  reflexNodes(numReflex) = freeNodes(m)
               END IF
            END DO
            IF ( numReflex == 1 )     THEN
!
!              ---------------------------------------------------------
!              Two elements on the reflex side. Put an edge between them
!              on the bisector of the reflex angle, which points away
!              from the sharp wedge, i.e., along d(:,1) + d(:,2)
!              ---------------------------------------------------------
!
               bisector = d(:,1) + d(:,2)
               IF ( NORM2(bisector) < 1.0E-6_RP )     CYCLE
               bisector = bisector/NORM2(bisector)
!
!              Project the node onto the bisector, but keep at least half
!              its distance from the corner, so that a node lying nearly at
!              right angles to the bisector does not end up at the corner
!
               nb => nodeForID(reflexNodes(1)) % node
               s  =  MAX( DOT_PRODUCT(nb % x(1:2) - xC, bisector), 0.5_RP*NORM2(nb % x(1:2) - xC), minDistance )
               nb % x(1:2) = xC + s*bisector
               CALL SetCornerConstraint( nb % id, xC, bisector, minDistance )
               CYCLE
            END IF
            IF ( numReflex /= 2 .OR. .NOT.constrainToSides )     CYCLE
!
!           -----------------------------------------------------
!           Pair the two nodes with the two directions, then move
!           each onto its line and constrain it to that line
!           -----------------------------------------------------
!
            DO m = 1, 2
               u = nodeForID(reflexNodes(m)) % node % x(1:2) - xC
               u = u/NORM2(u)
               DO k = 1, 2
                  dots(m,k) = DOT_PRODUCT(u, d(:,k))
               END DO
            END DO
            IF ( dots(1,1) + dots(2,2) < dots(1,2) + dots(2,1) )     THEN
               d = d(:,[2,1])
            END IF

            DO m = 1, 2
               nb => nodeForID(reflexNodes(m)) % node
               s  =  DOT_PRODUCT(nb % x(1:2) - xC, d(:,m))
               IF ( s <= 0.0_RP )     s = NORM2(nb % x(1:2) - xC)
               s  =  MAX( s, minDistance )
               nb % x(1:2) = xC + s*d(:,m)
               CALL SetCornerConstraint( nb % id, xC, d(:,m), minDistance )
            END DO
         END DO

      END SUBROUTINE SetInterfaceCornerConstraints
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetCornerConstraint( id, xC, direction, minDistance )
         IMPLICIT NONE
!
!        ------------------------------------------------------------
!        Constrain node id to the ray from the corner xC along the
!        unit vector direction, at least minDistance from the corner
!        ------------------------------------------------------------
!
         INTEGER       :: id
         REAL(KIND=RP) :: xC(2), direction(2), minDistance

         hasCornerConstraint(id)   = .TRUE.
         cornerDirection(1:2, id)  = direction
         cornerDirection(3  , id)  = 0.0_RP
         cornerOrigin(1:2, id)     = xC
         cornerOrigin(3  , id)     = 0.0_RP
         cornerMinDistance(id)     = minDistance

      END SUBROUTINE SetCornerConstraint
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE KeepNodeAwayFromCorner( id, x, velocity )
         IMPLICIT NONE
!
!        ----------------------------------------------------------------
!        The springs can pull a node that is constrained to a line from
!        a corner (see SetInterfaceCornerConstraints) right up to the
!        corner, which collapses the elements between it and the corner.
!        Stop node id, at x, at its minimum distance from the corner.
!        ----------------------------------------------------------------
!
         INTEGER       :: id
         REAL(KIND=RP) :: x(3), velocity(3)
         REAL(KIND=RP) :: s

         s  = DOT_PRODUCT(x(1:2) - cornerOrigin(1:2,id), cornerDirection(1:2,id))
         IF ( s >= cornerMinDistance(id) )     RETURN

         x(1:2) = cornerOrigin(1:2,id) + cornerMinDistance(id)*cornerDirection(1:2,id)
         IF ( DOT_PRODUCT(velocity(1:2), cornerDirection(1:2,id)) < 0.0_RP )     velocity = 0.0_RP

      END SUBROUTINE KeepNodeAwayFromCorner
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GatherPavedCurveEdges( nodePointers, x, edgeNodes, constraintType, pavedEdges, minLength )
         IMPLICIT NONE
!
!        ----------------------------------------------------------------
!        The edges whose two nodes are on the same paved interface chain,
!        with at least one of them sliding along it (the other can be a
!        fixed joint), and the shortest length each may get while smoothing:
!        MIN_PAVED_EDGE_FRACTION of its length now. Paving spaces those
!        nodes by the size along the curve, but the springs of squeezed
!        elements, e.g. at the tip of a narrow wedge, can pull a node
!        along the curve almost onto its neighbor.
!        ----------------------------------------------------------------
!
         TYPE(SMNodePtr), DIMENSION(:)  , ALLOCATABLE :: nodePointers
         REAL(KIND=RP)  , DIMENSION(:,:)              :: x
         INTEGER        , DIMENSION(:,:)              :: edgeNodes
         INTEGER        , DIMENSION(:)                :: constraintType
         INTEGER        , DIMENSION(:,:), ALLOCATABLE :: pavedEdges
         REAL(KIND=RP)  , DIMENSION(:)  , ALLOCATABLE :: minLength

         INTEGER :: j, k, m, a, b, chainID
         LOGICAL :: onPavedChain

         DO m = 0, 1
            IF ( m == 1 )     ALLOCATE( pavedEdges(2,j), minLength(j) )
            j = 0
            DO k = 1, SIZE(edgeNodes, 2)
               a = edgeNodes(1,k)
               b = edgeNodes(2,k)
               IF ( constraintType(a) /= NODE_ON_CURVE .AND. constraintType(b) /= NODE_ON_CURVE )     CYCLE
               IF ( nodePointers(a) % node % distToBoundary /= 0.0_RP )     CYCLE
               IF ( nodePointers(b) % node % distToBoundary /= 0.0_RP )     CYCLE
               chainID = nodePointers(a) % node % bCurveChainID
               IF ( nodePointers(b) % node % bCurveChainID /= chainID )     CYCLE
               onPavedChain = .FALSE.
               IF ( chainID >= 0 .AND. chainID <= MAX_PAVED_CHAIN_ID )     onPavedChain = chainWasPaved(chainID)
               IF ( .NOT.onPavedChain )     CYCLE
               j = j + 1
               IF ( m == 1 )     THEN
                  pavedEdges(:,j) = [a, b]
                  minLength(j)    = MIN_PAVED_EDGE_FRACTION*NORM2(x(1:2,a) - x(1:2,b))
               END IF
            END DO
         END DO

      END SUBROUTINE GatherPavedCurveEdges
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE KeepPavedCurveNodesApart( x, xPrevious, pavedEdges, minLength )
         IMPLICIT NONE
!
!        ----------------------------------------------------------------
!        Stop the two nodes of a paved curve edge (GatherPavedCurveEdges)
!        that have come closer than its shortest length: put them back to
!        where they were before this step, xPrevious, and stop them
!        ----------------------------------------------------------------
!
         REAL(KIND=RP), DIMENSION(:,:) :: x, xPrevious
         INTEGER      , DIMENSION(:,:) :: pavedEdges
         REAL(KIND=RP), DIMENSION(:)   :: minLength

         INTEGER :: j, a, b

         DO j = 1, SIZE(minLength)
            a = pavedEdges(1,j)
            b = pavedEdges(2,j)
            IF ( NORM2(x(1:2,a) - x(1:2,b)) >= minLength(j) )     CYCLE
            x(:,a) = xPrevious(:,a)
            x(:,b) = xPrevious(:,b)
            nodeVelocity(:,a) = 0.0_RP
            nodeVelocity(:,b) = 0.0_RP
         END DO

      END SUBROUTINE KeepPavedCurveNodesApart
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetSpringSmootherBlock( smootherDict, smp )
         USE FTValueDictionaryClass
         USE ErrorTypesModule
!
!        Example block is:
!
!         \begin{Smoother}
!            smoothing            = "ON" (optional)
!            smoothing type       = "linearSpring"
!            spring constant      = 1.0 (optional)
!            mass                 = 1.0 (optional)
!            rest length          = 1.0 (optional)
!            damping coefficient  = 5.0 (optional)
!            number of iterations = 25 (optional)
!            time step            = 0.1 (optional)
!         \end{Smoother}
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTValueDictionary), POINTER :: smootherDict
         TYPE(SpringSmootherParameters)    :: smp
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=32)          :: str
         REAL(KIND=RP), EXTERNAL    :: GetRealValue
         INTEGER      , EXTERNAL    :: GetIntValue
!
!        -------------------
!        Smoothing On or OFF
!        -------------------
!
         str = "ON"
         IF ( smootherDict % containsKey(key = "smoothing") )     THEN
            str = smootherDict % stringValueForKey(key = "smoothing", requestedLength = 32)
         END IF

         IF( str == "ON" )     THEN
            smp%smoothingON = .true.
         ELSE
            smp%smoothingON = .false.
         END IF
!
!        -----------------
!        Type of smoothing
!        -----------------
!
         IF ( smootherDict % containsKey(key = "smoothing type") )     THEN
            str = smootherDict % stringValueForKey(key = "smoothing type", requestedLength = 32)
         ELSE
            str = "LinearAndCrossBarSpring"
            CALL ThrowErrorExceptionOfType(poster = "SetSpringSmootherBlock", &
                                           msg    = "smoother block is missing smoothing type keyword. &
                                                  &  Using default, smoothing TYPE = LinearAndCrossBarSpring.", &
                                           typ    = FT_ERROR_WARNING)
         END IF

         IF( str == "LinearSpring" )     THEN
            smp%springType = LINEAR_SPRING
         ELSE IF (str == "CrossBarSpring" )     THEN
            smp%springType = CROSS_SPRING
         ELSE
            smp%springType = BOTH
         END IF
!
!        -------------------
!        Smoother parameters
!        -------------------
!
         smp % springConstant = defaultSpringConstant
         IF ( smootherDict % containsKey(key = "spring constant") )     THEN
            smp % springConstant = smootherDict % doublePrecisionValueForKey(key = "spring constant")
         END IF

         smp % mass = defaultSpringMass
         IF ( smootherDict % containsKey(key = "mass") )     THEN
            smp % mass = smootherDict % doublePrecisionValueForKey(key = "mass")
         END IF

         smp % restLength = defaultSpringRestLength
         IF ( smootherDict % containsKey(key = "rest length") )     THEN
            smp % restLength = smootherDict % doublePrecisionValueForKey(key = "rest length")
         END IF

         smp % dampingCoefficient = defaultSpringDampingCoefficient
         IF ( smootherDict % containsKey(key = "damping coefficient") )     THEN
            smp % dampingCoefficient = smootherDict % doublePrecisionValueForKey(key = "damping coefficient")
         END IF

         smp % numSteps = defaultSpringIterations
         IF ( smootherDict % containsKey(key = "number of iterations") )     THEN
            smp % numSteps = smootherDict % integerValueForKey(key = "number of iterations")
         END IF

         smp % deltaT = defaultSpringTimeStep
         IF ( smootherDict % containsKey(key = "time step") )     THEN
            smp % deltaT = smootherDict % doublePrecisionValueForKey(key = "time step")
         END IF

      END SUBROUTINE SetSpringSmootherBlock

      END Module SpringMeshSmootherClass


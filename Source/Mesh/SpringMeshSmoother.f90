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
      REAL(KIND=RP), PARAMETER                  , PRIVATE :: MAX_CONSTRAINED_CORNER_ANGLE = 150.0_RP
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
         IF(self % restLength >= 0.0)     CONTINUE
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
         TYPE(SMNode)       , POINTER :: node => NULL()
         CLASS(FTObject)    , POINTER :: obj => NULL()
         CLASS(FTLinkedList), POINTER :: boundaryNodesList => NULL()
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
         DO k = 1, self % numSmoothingSteps
            nodeAcceleration = 0.0_RP
            CALL AccumulateAcceleration( self, mesh )
            CALL ConstrainBoundaryNodes( self, model, mesh  %  nodesIterator )

            maxV = 0.0_RP
            CALL mesh % nodesIterator % setToStart()
            DO WHILE ( .NOT.mesh % nodesIterator % isAtEnd() )
               obj                => mesh % nodesIterator % object()
               CALL castToSMNode(obj,node)
               nodeID                 = node % id
               node % x               = node % x + self % deltaT*nodeVelocity(:,nodeID)
               nodeVelocity(:,nodeID) = nodeVelocity(:,nodeID) + &
                                        self % deltaT*( nodeAcceleration(:,nodeID) - &
                                        self % dampingCoefficient*nodeVelocity(:,nodeID) )
               IF ( hasCornerConstraint(nodeID) )     CALL KeepNodeAwayFromCorner( node, nodeVelocity(:,nodeID) )
               CALL mesh % nodesIterator % moveToNext()
               maxV = MAX(maxV, ABS(nodeVelocity(1,nodeID)), ABS(nodeVelocity(2,nodeID)) )
            END DO

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
      SUBROUTINE AccumulateAcceleration( self, mesh )
         IMPLICIT NONE
         TYPE (SpringMeshSmoother) :: self
         TYPE (SMMesh)       :: mesh

         IF ( self % springType == LINEAR_SPRING )     THEN
            CALL LinearAcceleration( self, mesh )
         ELSE IF ( self % springType == CROSS_SPRING )   THEN
            CALL CrossAcceleration( self, mesh )
         ELSE IF ( self % springType == BOTH )     THEN
            CALL LinearAcceleration( self, mesh )
            CALL CrossAcceleration( self, mesh )
         END IF

      END SUBROUTINE AccumulateAcceleration
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE LinearAcceleration( self, mesh )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SpringMeshSmoother) :: self
         TYPE (SMMesh)       :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                              :: id1, id2
         REAL(KIND=RP)                        :: force(3), x1(3), x2(3)
         TYPE(SMEdge)               , POINTER :: edge => NULL()
         TYPE(FTLinkedListIterator) , POINTER :: iterator => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()

         iterator => mesh % edgesIterator
         CALL iterator % setToStart()

         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castTosmEdge(obj,edge)

            x1  = edge % nodes(1) % node % x
            x2  = edge % nodes(2) % node % x
            id1 = edge % nodes(1) % node % id
            id2 = edge % nodes(2) % node % id
            CALL LinearSpringForce( self % springConstant, self % restLength, x1, x2, force )

            IF( edge % nodes(1)  %  node  %  bCurveID >= NONE )     THEN
               nodeAcceleration(:,id1) = nodeAcceleration(:,id1) + force/self % mass
            END IF
            IF( edge % nodes(2) % node % bCurveID >= NONE )     THEN
               nodeAcceleration(:,id2) = nodeAcceleration(:,id2) - force/self % mass
            END IF

            CALL iterator % moveToNext()
         END DO

      END SUBROUTINE LinearAcceleration
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CrossAcceleration( self, mesh )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SpringMeshSmoother) :: self
         TYPE (SMMesh)       :: mesh
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER                             :: k, cK, nID
         REAL(KIND=RP)                       :: force(3), x1(3), x2(3)
         TYPE(SMNode)              , POINTER :: node1 => NULL(), node2 => NULL()
         TYPE(SMElement)           , POINTER :: e => NULL()
         TYPE(FTLinkedListIterator), POINTER :: iterator => NULL()
         CLASS(FTObject)           , POINTER :: obj => NULL()

         iterator => mesh % elementsIterator
         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMElement(obj,e)
            DO k = 1, 4
               node1 => e % nodes(k) % node
               IF( HasFixedPosition( node1 ) )     CYCLE ! Is on boundary.
               nID = node1 % id
               cK  = k + 2
               IF( cK > 4 ) cK = cK - 4

               node2 => e % nodes(cK) % node
               x1 = node1 % x
               x2 = node2 % x
               CALL LinearSpringForce( self % springConstant, self % restLength, x1, x2, force )
               nodeAcceleration(:,nID) = nodeAcceleration(:,nID) + 0.5_RP*force/self % mass
            END DO

            CALL iterator % moveToNext()
         END DO

      END SUBROUTINE CrossAcceleration
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstrainBoundaryNodes( self, model, nodeIterator )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS (SpringMeshSmoother)            :: self
         TYPE  (FTLinkedListIterator), POINTER :: nodeIterator
         TYPE  (SMModel)             , POINTER :: model
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (SMNode)        , POINTER :: node => NULL()
         CLASS(SMChainedCurve), POINTER :: chain => NULL()
         CLASS(SMCurve)       , POINTER :: c => NULL()
         CLASS(FTObject)      , POINTER :: obj => NULL()
         INTEGER                        :: id
         REAL(KIND=RP)                  :: tangent(3)

         CALL nodeIterator % setToStart()
         DO WHILE ( .NOT.nodeIterator % isAtEnd() )
            obj => nodeIterator % object()
            CALL castToSMNode(obj,node)

            IF (node  %  activeStatus == INACTIVE)     THEN
               nodeAcceleration(:,node  %  id) = 0.0_RP

            ELSE IF ( node % distToBoundary == 0.0_RP .AND. node  %  bCurveID > NONE )     THEN
              c                      => model % curveWithID( node  %  bCurveID, chain )
              tangent                =  c % tangentAt( node  %  whereOnBoundary )
              id                     =  node  %  id
              nodeAcceleration(:,id) = (nodeAcceleration(1,id)*tangent(1) + &
                                        nodeAcceleration(2,id)*tangent(2))*tangent
            ELSE IF ( hasCornerConstraint(node % id) )     THEN
              id                     =  node  %  id
              nodeAcceleration(:,id) = (nodeAcceleration(1,id)*cornerDirection(1,id) + &
                                        nodeAcceleration(2,id)*cornerDirection(2,id))*cornerDirection(:,id)
            END IF
            CALL nodeIterator % moveToNext()
         END DO

         IF(self % restLength >= 0.0_RP)     CONTINUE

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
         INTEGER                       :: N, id, k, m, chainID
         INTEGER                       :: sideNodes(2), freeNodes(MAX_NEIGHBORS), numSide, numFree, numReflex
         INTEGER                       :: reflexNodes(MAX_NEIGHBORS)
         REAL(KIND=RP)                 :: xC(2), d(2,2), u(2), cornerAngle, a1, a2, dots(2,2), s
         REAL(KIND=RP)                 :: bisector(2), minDistance

         N = SIZE(hasCornerConstraint)
         hasCornerConstraint = .FALSE.
         cornerDirection     = 0.0_RP
         cornerOrigin        = 0.0_RP
         cornerMinDistance   = 0.0_RP
         IF ( .NOT.ALLOCATED(boundaryEdgesType) )     RETURN

         ALLOCATE( nodeForID(N), numNeighbors(N), neighbors(MAX_NEIGHBORS,N), isCorner(N) )
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
         DO id = 1, N
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
            cornerAngle = AngleBetween(-d(:,1), -d(:,2))
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
               a1 = AngleBetween(u, -d(:,1))
               a2 = AngleBetween(u, -d(:,2))
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
               IF ( NORM2(bisector) < 1.0d-6 )     CYCLE
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
      SUBROUTINE KeepNodeAwayFromCorner( node, velocity )
         IMPLICIT NONE
!
!        ----------------------------------------------------------------
!        The springs can pull a node that is constrained to a line from
!        a corner (see SetInterfaceCornerConstraints) right up to the
!        corner, which collapses the elements between it and the corner.
!        Stop it at its minimum distance from the corner.
!        ----------------------------------------------------------------
!
         TYPE (SMNode), POINTER :: node
         REAL(KIND=RP)          :: velocity(3)
         REAL(KIND=RP)          :: s
         INTEGER                :: id

         id = node % id
         s  = DOT_PRODUCT(node % x(1:2) - cornerOrigin(1:2,id), cornerDirection(1:2,id))
         IF ( s >= cornerMinDistance(id) )     RETURN

         node % x(1:2) = cornerOrigin(1:2,id) + cornerMinDistance(id)*cornerDirection(1:2,id)
         IF ( DOT_PRODUCT(velocity(1:2), cornerDirection(1:2,id)) < 0.0_RP )     velocity = 0.0_RP

      END SUBROUTINE KeepNodeAwayFromCorner
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


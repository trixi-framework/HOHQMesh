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
!      MeshCleaner.f90
!      Created: 2011-06-06 14:19:58 -0400
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
      Module MeshCleaner
      USE Geometry
      USE SMMeshObjectsModule
      USE SMMeshClass
      USE MeshQualityAnalysisClass
      USE MeshBoundaryMethodsModule
      USE ConnectionsModule
      USE ElementOperations
      USE InterfaceElementMethods
      USE LineReflectionModule
      IMPLICIT NONE
!
!--------------------------------------------------------------------
!> Apply various mesh improvement techniques to eliminate bad elements
!! from a mesh.
!--------------------------------------------------------------------
!
!@mark -
      CONTAINS
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PerformTopologyCleanup( mesh, errorCode )
         USE SMMeshClass
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        Clean up the interior high valence nodes and the diamonds. The
!        operation depends on the valence of the node:
!
!           valence 7    : CleanUp7ValenceNode_InMesh puts a diamond into
!                          one of its elements, leaving a valence 5 node
!           valence 6, 8 : SplitHighValenceNodes splits the node and puts
!                          a diamond between two of its valence 3
!                          neighbors. A valence 7 node that is left is
!                          split the same way if it can be.
!
!        The order matters. The valence 7 nodes are reduced before the
!        diamonds are removed (removing the diamonds first makes some
!        meshes fail), and the split comes after since its diamonds must
!        stay. A split can leave a valence 7 node, so the valence 7 nodes
!        are reduced once more after it. Interface and boundary nodes are
!        handled later, in CleanUpBoundaryElements.
!        ---------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) :: mesh
         INTEGER       :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER  :: numberOfValenceChanges, numberOfDiamondsRemoved, numberOfSplits
         LOGICAL  :: valenceHasChanged, diamondsHaveBeenRemoved

         valenceHasChanged       = .false.
         diamondsHaveBeenRemoved = .false.
         numberOfValenceChanges  = 0
         numberOfDiamondsRemoved = 0
!
!        ------------------------------------
!        Reduce valence at high valence nodes
!        ------------------------------------
!
         CALL ReduceNodeValences( mesh, numberOfValenceChanges, errorCode, nodesOnInterfaces = .FALSE. )
         IF(errorCode > A_OK_ERROR_CODE)     RETURN

         IF ( numberOfValenceChanges > 0 )     THEN
            valenceHasChanged = .true.
            IF ( printMessage )     THEN
               PRINT *, "      Valences have been modified ", numberOfValenceChanges, " time(s)"
            END IF
         END IF
!
!        ---------------
!        Remove diamonds
!        ---------------
!
         CALL RemoveDiamondElements( mesh, numberOfDiamondsRemoved, errorCode )
         IF(errorCode > A_OK_ERROR_CODE)     RETURN

         IF ( numberOfDiamondsRemoved > 0 )     THEN
            diamondsHaveBeenRemoved = .true.
            IF ( PrintMessage )     THEN
               PRINT *, "      Number of diamond elements removed = ", numberOfDiamondsRemoved
            END IF
         END IF
!
!        ------------------------------------------------
!        Split the valence 6 to 8 nodes next to valence 3
!        nodes left by the quadtree transition templates
!        ------------------------------------------------
!
         CALL SplitHighValenceNodes( mesh, numberOfSplits, errorCode )
         IF(errorCode > A_OK_ERROR_CODE)     RETURN

         IF ( numberOfSplits > 0 )     THEN
            valenceHasChanged = .true.
            IF ( PrintMessage )     THEN
               PRINT *, "      Number of high valence nodes split = ", numberOfSplits
            END IF
!
!           ------------------------------------------------------
!           Reduce the valence 7 nodes that are left. The cleanup
!           uses the edges, which the splits have not updated
!           ------------------------------------------------------
!
            CALL mesh % syncEdges()
            CALL ReduceNodeValences( mesh, numberOfValenceChanges, errorCode, nodesOnInterfaces = .FALSE. )
            IF(errorCode > A_OK_ERROR_CODE)     RETURN

            IF ( numberOfValenceChanges > 0 .AND. printMessage )     THEN
               PRINT *, "      Valences have been modified ", numberOfValenceChanges, " time(s)"
            END IF
         END IF
!
!        -------------------------------------------------
!        If the topology has changed, we must generate new
!        edges
!        -------------------------------------------------
!
         IF ( valenceHasChanged .OR. diamondsHaveBeenRemoved )     THEN
            CALL mesh % renumberObjects(whichIterator = NODES)
            CALL mesh % syncEdges()
            CALL UnmarkNodesNearBoundaries( mesh % nodesIterator )
         END IF

      END SUBROUTINE PerformTopologyCleanup
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReduceNodeValences( mesh, numberOfValenceChanges, errorCode, nodesOnInterfaces )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        Reduce the valence of the high valence nodes that are on an
!        interface curve (nodesOnInterfaces = .TRUE.) or of those that
!        are not on any boundary or interface curve
!        (nodesOnInterfaces = .FALSE.)
!        ---------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) :: mesh
         INTEGER       :: numberOfValenceChanges
         INTEGER       :: errorCode
         LOGICAL       :: nodesOnInterfaces
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                                 :: nodeID
         INTEGER     , DIMENSION(:), ALLOCATABLE :: localNumElementsForNode
         LOGICAL                                 :: wasChanged, connectionsAreCurrent, anyNodeCleaned

         numberOfValenceChanges = 0
!
!        -------------------------------------------------------------
!        Make current connections and keep a copy of the
!        valences since the array will be created and destroyed by the
!        cleanup routines.
!        -------------------------------------------------------------
!
         CALL MakeNodeToElementConnections( mesh, errorCode )
         IF(errorCode > A_OK_ERROR_CODE)     RETURN

         ALLOCATE(localNumElementsForNode(SIZE(numElementsForNode)))
         localNumElementsForNode = numElementsForNode
!
!        ------------------------------------------------------------
!        The cleanup needs the node to element and node to edge
!        connections of the current mesh. They are made by the first
!        cleanup and again after each one that changes the mesh.
!        ------------------------------------------------------------
!
         connectionsAreCurrent = .FALSE.
         anyNodeCleaned        = .FALSE.
!
!        --------------------------------
!        Check and clean up node valences
!        --------------------------------
!
         DO nodeID = 1, SIZE(localNumElementsForNode)
            SELECT CASE ( localNumElementsForNode(nodeID) )
               CASE( 7 )
                  CALL CleanUp7ValenceNode_InMesh(nodeID, mesh, errorCode, nodesOnInterfaces, wasChanged, &
                                                  connectionsAreCurrent)
                  anyNodeCleaned = .TRUE.
                  IF ( wasChanged )     numberOfValenceChanges = numberOfValenceChanges + 1
               CASE( 8 )
!
!                 Interior valence 8 nodes are split by SplitHighValenceNodes.
!                 On an interface curve, the fan of elements on one side of a
!                 valence 8 node can be thinned the same way as for valence 7
!                 (see CleanUpBoundaryElements)
!
                  IF ( nodesOnInterfaces )     THEN
                     CALL CleanUp7ValenceNode_InMesh(nodeID, mesh, errorCode, nodesOnInterfaces, wasChanged, &
                                                     connectionsAreCurrent)
                     anyNodeCleaned = .TRUE.
                     IF ( wasChanged )     numberOfValenceChanges = numberOfValenceChanges + 1
                  END IF
               CASE DEFAULT
                  !Do nothing
            END SELECT
         END DO

         DEALLOCATE(localNumElementsForNode)
         IF ( anyNodeCleaned )     THEN
            CALL deallocateNodeToEdgeConnections()
            CALL deallocateNodeToElementConnections()
         END IF

      END SUBROUTINE ReduceNodeValences
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RemoveDiamondElements( mesh, numberOfDiamondsRemoved, errorCode )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) :: mesh
         INTEGER       :: numberOfDiamondsRemoved
         INTEGER       :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (SMElement)           , POINTER :: currentElement  => NULL()
         CLASS(FTLinkedListIterator), POINTER :: elementIterator => NULL()
         CLASS(FTObject)            , POINTER :: obj             => NULL()
         LOGICAL, ALLOCATABLE                 :: nodeWasChanged(:)
         LOGICAL                              :: touchesChangedNode
         INTEGER                              :: pass, k, id, numberRemovedInPass
         INTEGER, PARAMETER                   :: maxDiamondPasses = 10
!
!        ---------------------------------------------------------------
!        Removing a diamond changes the valences of the nodes of its
!        element and moves the elements around it to a new node that the
!        node to element connections do not know about. Elements that
!        touch those nodes are skipped in this sweep. Sweep again after
!        the deleted elements are gone until no diamonds are left. The
!        sweep uses its own iterator, since making the connections
!        renumbers the elements with the mesh's elements iterator.
!        ---------------------------------------------------------------
!
         numberOfDiamondsRemoved = 0
         DO pass = 1, maxDiamondPasses
            CALL MakeNodeToElementConnections( mesh, errorCode )
            IF ( errorCode > A_OK_ERROR_CODE )     RETURN
            ALLOCATE( nodeWasChanged(SIZE(numElementsForNode)) )
            nodeWasChanged      = .FALSE.
            numberRemovedInPass = 0

            ALLOCATE(elementIterator)
            CALL elementIterator % initWithFTLinkedList(mesh % elements)
            CALL elementIterator % setToStart()
            DO WHILE ( .NOT.elementIterator % isAtEnd() )
               obj => elementIterator % object()
               CALL castToSMElement(obj,currentElement)

               touchesChangedNode = .FALSE.
               DO k = 1, 4
                  id = currentElement % nodes(k) % node % id
                  IF ( id > SIZE(nodeWasChanged) )     THEN
                     touchesChangedNode = .TRUE.
                  ELSE IF ( nodeWasChanged(id) )     THEN
                     touchesChangedNode = .TRUE.
                  END IF
               END DO

               IF ( .NOT.touchesChangedNode )     THEN
                  CALL DeleteElementIfDiamond( currentElement, mesh )
                  IF ( currentElement % remove )     THEN
                     numberRemovedInPass = numberRemovedInPass + 1
                     DO k = 1, 4
                        nodeWasChanged(currentElement % nodes(k) % node % id) = .TRUE.
                     END DO
                  END IF
               END IF

               CALL elementIterator % moveToNext()
            END DO
            CALL releaseFTLinkedListIteratorClass(elementIterator)
            DEALLOCATE( nodeWasChanged )
            CALL deallocateNodeToElementConnections()

            IF ( numberRemovedInPass == 0 )     EXIT
            numberOfDiamondsRemoved = numberOfDiamondsRemoved + numberRemovedInPass
!
!           ------------------------------------------
!           Clean up & remove deleted elements & nodes
!           ------------------------------------------
!
            CALL DoLazyDelete( mesh )
         END DO
!
!        -----------------------------------------------------------
!        Removing a diamond takes an element from each of its other
!        two nodes, so a node that was in three elements is left in
!        two (a doublet). Merge those two elements.
!        -----------------------------------------------------------
!
         IF ( numberOfDiamondsRemoved > 0 )     CALL RemoveDoubletNodes( mesh, errorCode )

      END SUBROUTINE RemoveDiamondElements
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RemoveDoubletNodes( mesh, errorCode )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        An interior node in only two elements, (v,a,p,b) and (v,b,q,a),
!        has an angle above 180 degrees in one of them, which smoothing
!        cannot fix. Merge them into (a,p,b,q), which leaves v unused.
!        Elements that touch a node changed in a sweep are skipped and
!        the sweep is repeated, as in RemoveDiamondElements.
!        ---------------------------------------------------------------
!
         TYPE (SMMesh) :: mesh
         INTEGER       :: errorCode
         TYPE (SMNode)   , POINTER :: v
         TYPE (SMElement), POINTER :: e1, e2
         CLASS(FTObject) , POINTER :: obj
         LOGICAL, ALLOCATABLE      :: nodeWasChanged(:)
         INTEGER                   :: pass, k, i, j, id, numberMerged
         INTEGER, PARAMETER        :: maxDoubletPasses = 10
         INTEGER, EXTERNAL         :: Loop

         DO pass = 1, maxDoubletPasses
            CALL MakeNodeToElementConnections( mesh, errorCode )
            IF ( errorCode > A_OK_ERROR_CODE )     RETURN
            ALLOCATE( nodeWasChanged(SIZE(numElementsForNode)) )
            nodeWasChanged = .FALSE.
            numberMerged   = 0

            CALL mesh % nodesIterator % setToStart()
            DO WHILE ( .NOT.mesh % nodesIterator % isAtEnd() )
               obj => mesh % nodesIterator % object()
               CALL castToSMNode(obj, v)
               CALL mesh % nodesIterator % moveToNext()
               id = v % id
               IF ( id > SIZE(numElementsForNode) )     CYCLE
               IF ( numElementsForNode(id) /= 2 )       CYCLE
               IF ( v % bCurveID /= UNDEFINED )         CYCLE
               e1 => elementsForNodes(1,id) % element
               e2 => elementsForNodes(2,id) % element
               IF ( e1 % remove .OR. e2 % remove )      CYCLE
               IF ( elementTouchesChangedNode(e1, nodeWasChanged) )     CYCLE
               IF ( elementTouchesChangedNode(e2, nodeWasChanged) )     CYCLE
!
!              e1 = (v,a,p,b) from local node i, e2 = (v,b,q,a) from j
!
               i = ElementLocalNodeIDForNodeID( id, e1 )
               j = ElementLocalNodeIDForNodeID( id, e2 )
               IF ( .NOT.ASSOCIATED(e2 % nodes(Loop(j+1,4)) % node, e1 % nodes(Loop(i+3,4)) % node) )     CYCLE
               IF ( .NOT.ASSOCIATED(e2 % nodes(Loop(j+3,4)) % node, e1 % nodes(Loop(i+1,4)) % node) )     CYCLE

               DO k = 1, 4
                  nodeWasChanged(e1 % nodes(k) % node % id) = .TRUE.
                  nodeWasChanged(e2 % nodes(k) % node % id) = .TRUE.
               END DO
               CALL releaseSMNode( e1 % nodes(i) % node )
               e1 % nodes(i) % node => e2 % nodes(Loop(j+2,4)) % node
               CALL e1 % nodes(i) % node % retain()
               e2 % remove  = .TRUE.
               numberMerged = numberMerged + 1
            END DO
            DEALLOCATE( nodeWasChanged )
            CALL deallocateNodeToElementConnections()

            IF ( numberMerged == 0 )     EXIT
            CALL DoLazyDelete( mesh )
         END DO

      END SUBROUTINE RemoveDoubletNodes
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION elementTouchesChangedNode( e, nodeWasChanged )
         IMPLICIT NONE
         TYPE (SMElement), POINTER :: e
         LOGICAL                   :: nodeWasChanged(:)
         INTEGER                   :: k, id

         elementTouchesChangedNode = .FALSE.
         DO k = 1, 4
            id = e % nodes(k) % node % id
            IF ( id > SIZE(nodeWasChanged) )     THEN
               elementTouchesChangedNode = .TRUE.
            ELSE IF ( nodeWasChanged(id) )     THEN
               elementTouchesChangedNode = .TRUE.
            END IF
         END DO
      END FUNCTION elementTouchesChangedNode
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CleanUp7ValenceNode_InMesh(id, mesh, errorCode, nodesOnInterfaces, wasChanged, connectionsAreCurrent)
      USE ErrorTypesModule
!
!     ----------------------------------------------------------
!     A valence 7 node can be reduced to a valence
!     5 node by inserting a new node into one of the
!     elements and inserting a diamond element.
!
!     Only a node that is on an interface curve
!     (nodesOnInterfaces = .TRUE.) or only one that is not on
!     any boundary or interface curve (nodesOnInterfaces = .FALSE.)
!     is changed. wasChanged tells whether the node was changed.
!     The node to element and node to edge connections are made
!     unless connectionsAreCurrent, which is then set. A change to
!     the mesh unsets it. The connections are left for the next
!     call and must be deallocated by the caller.
!     ----------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER       :: id
         TYPE (SMMesh) :: mesh
         INTEGER       :: errorCode
         LOGICAL       :: nodesOnInterfaces
         LOGICAL       :: wasChanged
         LOGICAL       :: connectionsAreCurrent
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                     :: k, localID, localIDForTarget
         INTEGER                     :: j, newID
         INTEGER                     :: cnt
         REAL(KIND=RP)               :: theta, thetaMax
         INTEGER, DIMENSION(4)       :: diagonalMap = [3,4,1,2]
         REAL(KIND=RP)               :: x1(3), x2(3), x(3)
         LOGICAL                     :: nodeIsOnCurve, nearJoint
         INTEGER, EXTERNAL           :: Loop

         TYPE (SMNode)     , POINTER :: node => NULL(), nodeForThisID => NULL(), newNode => NULL()
         TYPE (SMNode)     , POINTER :: neighbor => NULL()
         TYPE (SMElement)  , POINTER :: e => NULL(), eTarget => NULL()
         TYPE (SMEdge)     , POINTER :: edge => NULL()
         CLASS(FTObject)   , POINTER :: obj => NULL()

         TYPE(SMNodePtr), DIMENSION(4) :: elementNodes
         TYPE(SMElementPtr)            :: eNeighbors(2)
         TYPE(SMEdgePtr)               :: sharedEdgePointers(2), newEdgePointers(2)

         CHARACTER(LEN=ERROR_EXCEPTION_MSG_LENGTH) :: msg
         LOGICAL                                   :: isOnNeighbor
!
!        ------------------------
!        Make current connections
!        ------------------------
!
         IF ( .NOT.connectionsAreCurrent )     THEN
            CALL MakeNodeToElementConnections( mesh, errorCode )
            CALL MakeNodeToEdgeConnections   ( mesh )
            connectionsAreCurrent = .TRUE.
         END IF
!
         e    => elementsForNodes(1,id) % element
         node => e % nodes(ElementLocalNodeIDForNodeID(id, e)) % node
         nodeIsOnCurve = node % bCurveID /= UNDEFINED
         wasChanged    = .FALSE.
         IF ( (nodesOnInterfaces .AND. .NOT.isOnInterfaceCurve(node)) .OR. &
              (.NOT.nodesOnInterfaces .AND. nodeIsOnCurve) )     THEN
            RETURN
         END IF
!
!        ---------------------------------------------------------------
!        At a joint of the curve, more elements on one side are expected
!        (e.g. on the reflex side of a corner), and the elements there
!        are constrained by the smoother. Leave nodes at or next to a
!        joint alone.
!        ---------------------------------------------------------------
!
         IF ( nodeIsOnCurve )     THEN
            nearJoint = isAtCurveJoint(node)
            DO k = 1, numElementsForNode(id)
               e       => elementsForNodes(k,id) % element
               localID = ElementLocalNodeIDForNodeID( id, e )
               DO j = -1, 1, 2
                  neighbor => e % nodes(Loop(localID+j,4)) % node
                  IF ( isEdgeAlongCurve(node, neighbor) )     nearJoint = nearJoint .OR. isAtCurveJoint(neighbor)
               END DO
            END DO
            IF ( nearJoint )     THEN
               RETURN
            END IF
         END IF
!
!        ------------------------------------------------------------------
!        Find the element with the largest angle. The target and its two
!        neighbors are moved to the new node, which is not on a curve. For
!        a node on a boundary or interface curve, the two edges of the
!        target at the node must therefore not lie along that curve, so
!        that the edges along the curve stay attached to the node. If no
!        element qualifies, leave the node alone.
!        ------------------------------------------------------------------
!
         thetaMax = 0.0_RP
         eTarget  => NULL()

         DO k = 1, numElementsForNode(id)
            e       => elementsForNodes(k,id) % element
            localID = ElementLocalNodeIDForNodeID( id, e )
            IF ( nodeIsOnCurve )     THEN
               IF ( isEdgeAlongCurve(node, e % nodes(Loop(localID-1,4)) % node) .OR. &
                    isEdgeAlongCurve(node, e % nodes(Loop(localID+1,4)) % node) )     CYCLE
            END IF
            theta   = AngleAtLocalNode_ForElement(localID,e)

            IF(theta > thetaMax)     THEN
               thetaMax         = theta
               eTarget          => e
               localIDForTarget = localID
            END IF
         END DO

         IF ( .NOT.ASSOCIATED(eTarget) )     THEN
            RETURN
         END IF
!
!        -----------------------------------------
!        Find the elements that flank this element
!        -----------------------------------------
!
         cnt = 0
         DO k = 1, numEdgesForNodes(id)
            edge => edgesForNodes(k,id) % edge
!
!           ----------------------------------------------------
!           See if this edge is along the target element
!           A boundary edge has only one element, so elements(2)
!           must be checked before it is used. A boundary edge
!           cannot flank the target.
!           ----------------------------------------------------
!
            IF ( .NOT.ASSOCIATED(edge % elements(2) % element) )             CYCLE

            IF ( edge % elements(1) % element % id == eTarget % id )          THEN
               cnt                       = cnt + 1
               eNeighbors(cnt) % element => edge % elements(2) % element
               sharedEdgePointers(cnt) % edge => edge
            ELSE IF ( edge % elements(2) % element % id == eTarget % id )   THEN
               cnt                     = cnt + 1
               eNeighbors(cnt) % element => edge % elements(1) % element
               sharedEdgePointers(cnt) % edge => edge
            END IF
            IF(cnt == 2) EXIT
         END DO
!
!        --------------------------------------------------------
!        If there aren't two neighbors, then something is wrong.
!        Bail when this happens.
!        --------------------------------------------------------
!
         IF( cnt /= 2 )     THEN
            WRITE(msg,*) "Bad element topology for node at ", &
                           eTarget % nodes(localIDForTarget) % node % x, ". Aborting cleanup."
            CALL ThrowErrorExceptionOfType("CleanUp7ValenceNode_InMesh",msg,FT_ERROR_WARNING)
            RETURN
         END IF
!
!        -------------------------------------------------------------
!        Find the edges that are in the two neighbor elements, but are
!        not the shared edges.
!        -------------------------------------------------------------
!
         cnt = 0
         DO k = 1, numEdgesForNodes(id)
            edge => edgesForNodes(k,id) % edge
            IF ( ASSOCIATED(edge,sharedEdgePointers(1) % edge) .OR. &
                 ASSOCIATED(edge,sharedEdgePointers(2) % edge))              CYCLE
!
!           ------------------------------------------------------
!           See if this edge is along one of the neighbor elements
!           ------------------------------------------------------
!
            DO j = 1, 2
               isOnNeighbor = edge % elements(1) % element % id == eNeighbors(j) % element % id
               IF ( ASSOCIATED(edge % elements(2) % element) )     THEN ! Not a boundary edge
                  isOnNeighbor = isOnNeighbor .OR. edge % elements(2) % element % id == eNeighbors(j) % element % id
               END IF
               IF ( isOnNeighbor ) THEN
                  cnt                       = cnt + 1
                  newEdgePointers(cnt) % edge => edge
               END IF
            END DO
            IF(cnt == 2) EXIT
         END DO
         IF( cnt /= 2 )     THEN
            PRINT *, "Bad element topology in CleanUp7ValenceNode_InMesh for node at ", &
                     eTarget % nodes(localIDForTarget) % node % x
            RETURN
         END IF
!
!        ---------------------------------------------------------------
!        The new element gets these two edges. If both lie along the
!        curve, it would have three nodes in a row on the curve and be
!        degenerate, so leave the node alone.
!        ---------------------------------------------------------------
!
         IF ( nodeIsOnCurve )     THEN
            IF ( isEdgeAlongCurve(newEdgePointers(1) % edge % nodes(1) % node,   &
                                  newEdgePointers(1) % edge % nodes(2) % node) .AND. &
                 isEdgeAlongCurve(newEdgePointers(2) % edge % nodes(1) % node,   &
                                  newEdgePointers(2) % edge % nodes(2) % node) )     THEN
               RETURN
            END IF
         END IF
!
!        -------------------------------------------
!        Create a new node inside the target element
!        -------------------------------------------
!
         node => eTarget % nodes(localIDForTarget) % node

         nodeForThisID => node ! save pointer before overwriting

         x1 = node % x
         x2 = eTarget % nodes(diagonalMap(localIDForTarget)) % node % x
         x  = 0.5_RP*(x1 + x2)

         ALLOCATE(newNode)
         CALL newNode % initWithLocationAndID(x,mesh % newNodeID())
         obj               => newNode
         CALL mesh % nodes % add(obj)
         CALL releaseSMNode(newNode)
!
!        --------------------------------------------
!        Change the corners to point to this new node
!        --------------------------------------------
!
         CALL releaseSMNode(eTarget % nodes(localIDForTarget) % node)
         eTarget % nodes(localIDForTarget) % node => newNode
         CALL newNode % retain()

         localID = ElementLocalNodeIDForNodeID( id, eNeighbors(1) % element )
         CALL releaseSMNode(eNeighbors(1) % element % nodes(localID) % node)
         eNeighbors(1) % element % nodes(localID) % node => newNode
         CALL newNode % retain()

         localID = ElementLocalNodeIDForNodeID( id, eNeighbors(2) % element )
         CALL releaseSMNode(eNeighbors(2) % element % nodes(localID) % node)
         eNeighbors(2) % element % nodes(localID) % node => newNode
         CALL newNode % retain()
!
!        --------------------
!        Create a new element
!        --------------------
!
         elementNodes(1) % node => nodeForThisID

         IF ( newEdgePointers(1) % edge % nodes(1) % node % id == id )     THEN
            elementNodes(2) % node => newEdgePointers(1) % edge % nodes(2) % node
         ELSE
            elementNodes(2) % node => newEdgePointers(1) % edge % nodes(1) % node
         END IF

         elementNodes(3) % node => newNode

         IF ( newEdgePointers(2) % edge % nodes(1) % node % id == id )     THEN
            elementNodes(4) % node => newEdgePointers(2) % edge % nodes(2) % node
         ELSE
            elementNodes(4) % node => newEdgePointers(2) % edge % nodes(1) % node
         END IF

!
!        ----------------------------------------------------------
!        Number the nodes counterclockwise like the other elements
!        ----------------------------------------------------------
!
         IF ( signedQuadArea(elementNodes) < 0.0_RP )     THEN
            node                   => elementNodes(2) % node
            elementNodes(2) % node => elementNodes(4) % node
            elementNodes(4) % node => node
         END IF

         newID = mesh % newElementID()
         ALLOCATE(e)
         CALL e % initWithNodesIDAndType( elementNodes, newID, QUAD )
         obj => e
         CALL mesh % elements % add(obj)
         CALL releaseSMElement(e)
         wasChanged            = .TRUE.
         connectionsAreCurrent = .FALSE.

      END SUBROUTINE CleanUp7ValenceNode_InMesh
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SplitHighValenceNodes( mesh, numberOfSplits, errorCode )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        The quadtree transition templates leave interior valence 6
!        nodes with two valence 3 neighbors (a 3-6-3 pattern), and valence
!        8 nodes with four where two refinement fronts meet. Split each
!        such node of valence 6, 7, or 8 into two and insert a diamond
!        element between two of its valence 3 neighbors, which become
!        regular. The two nodes share the elements of the old node plus
!        the diamond. If they get four each they are regular. The usual
!        2/4 split of a valence 6 node leaves a valence 5 and a valence 3
!        node at opposite corners of the diamond (a kite), and a valence
!        8 node becomes two valence 5 nodes. Valence 7 nodes are mostly
!        reduced before by CleanUp7ValenceNode_InMesh, a valence 7 node
!        that is left is split if it can be. Nodes changed in a pass are
!        not used again in that same pass, and passes are repeated until
!        nothing changes.
!        ---------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) :: mesh
         INTEGER       :: numberOfSplits
         INTEGER       :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, PARAMETER   :: maxSplitPasses = 4
         INTEGER              :: pass, nodeID, numNodes, splitsInPass
         INTEGER, ALLOCATABLE :: valence(:)
         LOGICAL, ALLOCATABLE :: touched(:)
         LOGICAL              :: wasSplit

         numberOfSplits = 0
         DO pass = 1, maxSplitPasses
            CALL MakeNodeToElementConnections( mesh, errorCode )
            IF(errorCode > A_OK_ERROR_CODE)     RETURN

            numNodes = SIZE(numElementsForNode)
            ALLOCATE( valence(numNodes), touched(numNodes) )
            valence      = numElementsForNode
            touched      = .FALSE.
            splitsInPass = 0
            DO nodeID = 1, numNodes
               IF ( valence(nodeID) < 6 .OR. valence(nodeID) > 8 )     CYCLE
               IF ( touched(nodeID) )          CYCLE
               CALL SplitHighValenceNode( nodeID, mesh, valence, touched, wasSplit )
               IF ( wasSplit )     splitsInPass = splitsInPass + 1
            END DO
            DEALLOCATE( valence, touched )
            CALL deallocateNodeToElementConnections()

            numberOfSplits = numberOfSplits + splitsInPass
            IF ( splitsInPass == 0 )     EXIT
         END DO

      END SUBROUTINE SplitHighValenceNodes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SplitHighValenceNode( id, mesh, valence, touched, wasSplit )
         IMPLICIT NONE
!
!        -------------------------------------------------------------
!        Split the interior node with the given id, of valence 6 to 8,
!        if two of its neighbors are interior valence 3 nodes that are
!        not next to each other around it. The node array valence and
!        the touched marks are updated for the next nodes of the pass.
!        -------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER       :: id
         TYPE (SMMesh) :: mesh
         INTEGER       :: valence(:)
         LOGICAL       :: touched(:)
         LOGICAL       :: wasSplit
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, PARAMETER         :: maxSplitValence = 8
         INTEGER                    :: k, m, j, a, c, groupSize, bestA, bestC, bestDeviation, deviation, try, nodeID
         INTEGER                    :: nodeValence
         INTEGER                    :: localID(maxSplitValence), order(maxSplitValence)
         INTEGER                    :: nextID(maxSplitValence), prevID(maxSplitValence), ringLocalID(maxSplitValence)
         LOGICAL                    :: used(maxSplitValence), found, isValenceThreeSpoke(maxSplitValence)
         LOGICAL                    :: inFirstGroup(maxSplitValence), isRightHanded
         REAL(KIND=RP)              :: xNode(3), xFirst(3), xSecond(3), d(3), side(3), t, sideSign
         TYPE (SMNode)    , POINTER :: node => NULL(), newNode => NULL()
         TYPE (SMElement) , POINTER :: e => NULL()
         CLASS(FTObject)  , POINTER :: obj => NULL()
         TYPE(SMElementPtr)         :: ring(maxSplitValence), elements(maxSplitValence)
         TYPE(SMNodePtr)            :: spoke(maxSplitValence), elementNodes(4)
         INTEGER, EXTERNAL          :: Loop

         wasSplit = .FALSE.
         nodeValence = numElementsForNode(id)
         IF ( nodeValence > maxSplitValence )     RETURN

         e    => elementsForNodes(1,id) % element
         node => e % nodes(ElementLocalNodeIDForNodeID(id, e)) % node
         IF ( node % bCurveID /= UNDEFINED )     RETURN
!
!        ------------------------------------------------------------
!        Order the elements counterclockwise around the node. Element
!        m of the ring spans from spoke m to spoke m+1.
!        ------------------------------------------------------------
!
         nextID = 0
         prevID = 0
         DO k = 1, nodeValence
            elements(k) % element => elementsForNodes(k,id) % element
            localID(k) = ElementLocalNodeIDForNodeID( id, elements(k) % element )
            nextID(k)  = elements(k) % element % nodes(Loop(localID(k)+1,4)) % node % id
            prevID(k)  = elements(k) % element % nodes(Loop(localID(k)-1,4)) % node % id
         END DO
         used     = .FALSE.
         order(1) = 1
         used(1)  = .TRUE.
         DO m = 2, nodeValence
            found = .FALSE.
            DO k = 1, nodeValence
               IF ( used(k) )     CYCLE
               IF ( nextID(k) == prevID(order(m-1)) )     THEN
                  order(m) = k
                  used(k)  = .TRUE.
                  found    = .TRUE.
                  EXIT
               END IF
            END DO
            IF ( .NOT.found )     RETURN
         END DO
         IF ( prevID(order(nodeValence)) /= nextID(order(1)) )     RETURN

         DO m = 1, nodeValence
            k = order(m)
            ring(m) % element => elements(k) % element
            ringLocalID(m)    =  localID(k)
            spoke(m) % node   => elements(k) % element % nodes(Loop(localID(k)+1,4)) % node
         END DO
!
!        -------------------------------------------------------
!        Leave the node alone if anything around it was changed
!        in this pass
!        -------------------------------------------------------
!
         DO m = 1, nodeValence
            DO j = 1, 4
               nodeID = ring(m) % element % nodes(j) % node % id
               IF ( nodeID > SIZE(touched) )     RETURN
               IF ( touched(nodeID) )            RETURN
            END DO
         END DO
!
!        -------------------------------------------------------------
!        Find two interior valence 3 spokes. The node keeps the
!        groupSize elements between them plus the diamond, the new node
!        gets the rest plus the diamond. Take the pair for which these
!        two valences are closest to 4. A pair of neighboring spokes
!        would leave a valence 2 node and is not used.
!        -------------------------------------------------------------
!
         DO m = 1, nodeValence
            isValenceThreeSpoke(m) = valence(spoke(m) % node % id) == 3
            IF ( spoke(m) % node % bCurveID /= UNDEFINED )     isValenceThreeSpoke(m) = .FALSE.
         END DO

         bestDeviation = HUGE(1)
         bestA         = 0
         bestC         = 0
         DO a = 1, nodeValence - 1
            IF ( .NOT.isValenceThreeSpoke(a) )     CYCLE
            DO c = a + 1, nodeValence
               IF ( .NOT.isValenceThreeSpoke(c) )     CYCLE
               groupSize = c - a
               IF ( groupSize < 2 .OR. groupSize > nodeValence - 2 )     CYCLE
               deviation = ABS(groupSize + 1 - 4) + ABS(nodeValence - groupSize + 1 - 4)
               IF ( deviation < bestDeviation )     THEN
                  bestDeviation = deviation
                  bestA         = a
                  bestC         = c
               END IF
            END DO
         END DO
         IF ( bestA == 0 )     RETURN
!
!        -----------------------------------------------------------------
!        Ring elements bestA .. bestC-1 keep the node, the others get the
!        new node. The two are moved apart across the line between the
!        two valence 3 spokes, each toward the side of its own elements.
!        Smaller moves are tried until all elements are right handed.
!        -----------------------------------------------------------------
!
         inFirstGroup = .FALSE.
         inFirstGroup(bestA:bestC-1) = .TRUE.

         xNode    = node % x
         d        = spoke(bestC) % node % x - spoke(bestA) % node % x
         side     = [-d(2), d(1), 0.0_RP]
         side     = side/NORM2(side)
         sideSign = 0.0_RP
         DO m = 1, nodeValence
            k = Loop(ringLocalID(m)+2,4)
            IF ( inFirstGroup(m) )     THEN
               sideSign = sideSign + DOT_PRODUCT(side, ring(m) % element % nodes(k) % node % x - xNode)
            ELSE
               sideSign = sideSign - DOT_PRODUCT(side, ring(m) % element % nodes(k) % node % x - xNode)
            END IF
         END DO
         IF ( sideSign < 0.0_RP )     side = -side

         t = 0.25_RP*NORM2(d)
         isRightHanded = .FALSE.
         DO try = 1, 3
            xFirst  = xNode + t*side
            xSecond = xNode - t*side
            isRightHanded = splitIsRightHanded( node, ring(1:nodeValence), inFirstGroup(1:nodeValence), xFirst, xSecond )
            IF ( isRightHanded )     EXIT
            t = 0.5_RP*t
         END DO
         IF ( .NOT.isRightHanded )     RETURN
!
!        ------------------------------------
!        Move the node and make the new node
!        ------------------------------------
!
         node % x = xFirst
         ALLOCATE(newNode)
         CALL newNode % initWithLocationAndID(xSecond, mesh % newNodeID())
         obj => newNode
         CALL mesh % nodes % add(obj)
         CALL releaseSMNode(newNode)

         DO m = 1, nodeValence
            IF ( inFirstGroup(m) )     CYCLE
            k = ringLocalID(m)
            CALL releaseSMNode(ring(m) % element % nodes(k) % node)
            ring(m) % element % nodes(k) % node => newNode
            CALL newNode % retain()
         END DO
!
!        ---------------------------------------------
!        The diamond between the two valence 3 spokes
!        ---------------------------------------------
!
         elementNodes(1) % node => spoke(bestA) % node
         elementNodes(2) % node => newNode
         elementNodes(3) % node => spoke(bestC) % node
         elementNodes(4) % node => node
         IF ( signedQuadArea(elementNodes) < 0.0_RP )     THEN
            elementNodes(2) % node => node
            elementNodes(4) % node => newNode
         END IF
         ALLOCATE(e)
         CALL e % initWithNodesIDAndType( elementNodes, mesh % newElementID(), QUAD )
         obj => e
         CALL mesh % elements % add(obj)
         CALL releaseSMElement(e)
!
!        -----------------------------------------------
!        Valences and marks for the rest of the pass.
!        The new node has an id beyond the touched marks
!        -----------------------------------------------
!
         valence(spoke(bestA) % node % id) = 4
         valence(spoke(bestC) % node % id) = 4
         valence(id)                       = bestC - bestA + 1
         DO m = 1, nodeValence
            DO j = 1, 4
               nodeID = ring(m) % element % nodes(j) % node % id
               IF ( nodeID <= SIZE(touched) )     touched(nodeID) = .TRUE.
            END DO
         END DO
         wasSplit = .TRUE.

      END SUBROUTINE SplitHighValenceNode
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION splitIsRightHanded( node, ring, inFirstGroup, xFirst, xSecond )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        True if no element around a node being split is tangled when
!        the node is put at xFirst for the elements of the first group
!        and at xSecond for the others. The node is put back afterwards.
!        ---------------------------------------------------------------
!
         TYPE (SMNode), POINTER :: node
         TYPE(SMElementPtr)     :: ring(:)
         LOGICAL                :: inFirstGroup(:)
         REAL(KIND=RP)          :: xFirst(3), xSecond(3)
         REAL(KIND=RP)          :: x0(3)
         INTEGER                :: m

         x0 = node % x
         splitIsRightHanded = .TRUE.
         DO m = 1, SIZE(ring)
            IF ( inFirstGroup(m) )     THEN
               node % x = xFirst
            ELSE
               node % x = xSecond
            END IF
            IF ( elementIsTangled(ring(m) % element) )     splitIsRightHanded = .FALSE.
         END DO
         node % x = x0
      END FUNCTION splitIsRightHanded
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION signedQuadArea( quadNodes )
         IMPLICIT NONE
!
!        -----------------------------------------------------------
!        Signed area of the quadrilateral, positive if its nodes are
!        numbered counterclockwise
!        -----------------------------------------------------------
!
         TYPE(SMNodePtr) :: quadNodes(4)
         INTEGER         :: k, kNext

         signedQuadArea = 0.0_RP
         DO k = 1, 4
            kNext = MOD(k, 4) + 1
            signedQuadArea = signedQuadArea + 0.5_RP*( quadNodes(k) % node % x(1)*quadNodes(kNext) % node % x(2) &
                                                     - quadNodes(kNext) % node % x(1)*quadNodes(k) % node % x(2) )
         END DO
      END FUNCTION signedQuadArea
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION isAtCurveJoint( node )
         IMPLICIT NONE
!
!        ---------------------------------------------------------
!        True if the node is pinned at an end of a curve, i.e. at a
!        joint of its chain
!        ---------------------------------------------------------
!
         TYPE (SMNode), POINTER :: node
         isAtCurveJoint = node % whereOnBoundary == 0.0_RP .OR. node % whereOnBoundary == 1.0_RP
      END FUNCTION isAtCurveJoint
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION isEdgeAlongCurve( a, b )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        True if the nodes a and b are both on the same boundary or
!        interface curve chain, i.e. the edge between them follows it
!        ---------------------------------------------------------------
!
         TYPE (SMNode), POINTER :: a, b
         isEdgeAlongCurve = a % bCurveID /= UNDEFINED .AND. b % bCurveID /= UNDEFINED .AND. &
                            a % bCurveChainID == b % bCurveChainID
      END FUNCTION isEdgeAlongCurve
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PerformFinalMeshCleanup( mesh , model, errorCode )
         USE SMModelClass
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) , POINTER :: mesh
         TYPE (SMModel), POINTER :: model
         INTEGER                 :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (FTMutableObjectArray) , POINTER     :: badElements => NULL()
         TYPE (SMElement)            , POINTER     :: e => NULL()
         CLASS(FTObject)             , POINTER     :: obj => NULL()
         REAL(KIND=RP)               , ALLOCATABLE :: shapeMeasures(:,:)
         LOGICAL                     , ALLOCATABLE :: badElementMeasure(:,:)
         INTEGER                                   :: numberOfBadElements
         INTEGER                                   :: k
         INTEGER                                   :: numberOfChevrons
!
!        -----------------------
!        Gather the bad elements
!        -----------------------
!
         badElements => BadElementsInMesh( mesh )
!
!        -----------------------------------------------
!        If there are no bad elements, clean up around
!        the boundaryies anyway. Otherwise go around and
!        clean up the bad interior elements.
!        -----------------------------------------------
!
         IF( .NOT.ASSOCIATED(badElements) )     THEN
!
!           --------------------------
!           Clean up boundary elements
!           --------------------------
!
            CALL CleanUpBoundaryElements( mesh, model, errorCode )
         ELSE
!
!           -----------------------
!           Save the shape measures
!           -----------------------
!
            numberOfBadElements = badElements % COUNT()
            ALLOCATE( shapeMeasures    ( NUMBER_OF_2D_SHAPE_MEASURES, numberOfBadElements ) )
            ALLOCATE( badElementMeasure( NUMBER_OF_2D_SHAPE_MEASURES, numberOfBadElements ) )

            DO k = 1, numberOfBadElements
               obj => badElements % objectAtIndex(k)
               CALL castToSMElement(obj,e)
               CALL ComputeElementShapeMeasures2D( e, shapeMeasures(:,k) )
               CALL ExtractBadElementInfo( shapeMeasures(:,k), badElementMeasure(:,k) )
            END DO
!
!           -----------
!           Fix lefties
!           -----------
!
            DO k = 1, numberOfBadElements
               obj => badElements % objectAtIndex(k)
               CALL castToSMElement(obj,e)
               IF( shapeMeasures(AREA_SIGN,k ) < 0.0_RP ) CALL MakeElement_RightHanded( e )
            END DO
!
!           ------------------------------------------------------------
!           Reordering the nodes changes which side of an element each
!           edge is on, so the edges must be rebuilt before they are used
!           ------------------------------------------------------------
!
            IF ( ANY(shapeMeasures(AREA_SIGN,:) < 0.0_RP) ) CALL mesh % syncEdges()
!
!           -------------------------------
!           Fix triangular/chevron elements
!           -------------------------------
!
            CALL MakeNodeToElementConnections( mesh, errorCode )
            CALL CleanUpChevronElements( badElements, shapeMeasures, numberOfChevrons )
            CALL deallocateNodeToElementConnections
!
!           ---------------
!           Do lazy deletes
!           ---------------
!
            CALL releaseFTMutableObjectArray(badElements)
            DEALLOCATE( shapeMeasures, badElementMeasure )

            IF ( numberOfChevrons > 0 )     THEN
               CALL DoLazyDelete( mesh )
               CALL mesh % syncEdges()
            END IF
!
!           -------------------------------------------
!           Bail if the mesh is so bad that it can't be
!           fixed
!           -------------------------------------------
!
!            badElements => BadElementsInMesh( mesh )
!            IF(ASSOCIATED(badElements))     THEN
!               DO k = 1, numberOfBadElements
!                  obj => badElements % objectAtIndex(k)
!                  CALL cast(obj,e)
!                  IF( shapeMeasures(ASPECT_RATIO_INDEX,k ) < 0.0_RP .OR. &
!                      shapeMeasures(JACOBIAN_INDEX,k) <= 0.0_RP     .OR. &
!                      shapeMeasures(CONDITION_INDEX,k) > 1000.0_RP) THEN
!                      PRINT *,shapeMeasures(ASPECT_RATIO_INDEX,k ),&
!                      shapeMeasures(JACOBIAN_INDEX,k),&
!                      shapeMeasures(CONDITION_INDEX,k)
!                      PRINT *
!                      PRINT *, "     **************************************************************"
!                      PRINT *, "     The mesh generated appears to be so bad that I cannot fix it."
!                      PRINT *, "     Try choosing a smaller mesh size and try again"
!                      PRINT *, "     **************************************************************"
!                      PRINT *
!                      CALL badElements % release()
!                      DEALLOCATE(badElements)
!                      DEALLOCATE(shapeMeasures, badElementMeasure)
!                      RETURN
!                  END IF
!               END DO
!            END IF
!
!           --------------------------
!           Clean up boundary elements
!           --------------------------
!
            CALL UnmarkNodesNearBoundaries( mesh % nodesIterator )
            CALL CleanUpBoundaryElements( mesh, model, errorCode )
         END IF

      END SUBROUTINE PerformFinalMeshCleanup
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MakeElement_RightHanded( e )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMElement) :: e
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE(SMNode), POINTER :: nptr2 => NULL(), nptr4 => NULL()
!
!        ------------------------------------------------------------
!        To make a left handed element right handed, swap nodes 2 & 4
!        ------------------------------------------------------------
!
         nptr2 => e % nodes(2) % node
         nptr4 => e % nodes(4) % node

         e % nodes(2) % node => nptr4
         e % nodes(4) % node => nptr2

      END SUBROUTINE MakeElement_RightHanded
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CleanUpBoundaryElements( mesh, model, errorCode )
         USE SMModelClass
         USE ConnectionsModule
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) , POINTER :: mesh
         TYPE (SMModel), POINTER :: model
         INTEGER                 :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                              :: numBoundaries
         INTEGER                              :: j, numberOfValenceChanges, numberOfDiamondsRemoved
         CLASS(FTLinkedList)        , POINTER :: currentEdgeList => NULL()
         CLASS(FTLinkedList)        , POINTER :: interfaceElements => NULL()
         TYPE (SMEdge)              , POINTER :: currentEdge => NULL()
         CLASS(SMElement)           , POINTER :: e => NULL()
         TYPE (SMNode)              , POINTER :: meshNode => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()
         CLASS(FTLinkedListIterator), POINTER :: edgeListIterator
         CLASS(FTLinkedListIterator), POINTER :: nodesIterator => NULL()

         numBoundaries = model % numberOfInnerCurves &
                       + model % numberOfOuterCurves &
                       + model % numberOfInterfaceCurves
         IF (numBoundaries == 0)     RETURN
!
!        ------------------------------------------------
!        Gather boundary Edges, etc in the following call
!        ------------------------------------------------
!
         CALL SetNodeActiveStatus(mesh,model, errorCode)
!
!        -----------------------------------------------
!        Run through physical boundary elements and make
!        the boundary angles 90 degrees.
!        -----------------------------------------------
!
         IF( errorCode == A_OK_ERROR_CODE)     THEN
            CALL makeNodeToElementConnections(mesh, errorCode)
            DO j = 1, numBoundaries
               IF( boundaryEdgesType(j) == INTERFACE_EDGES ) CYCLE
               ALLOCATE(edgeListIterator)

               obj  => boundaryEdgesArray % objectAtIndex(j)
               CALL cast(obj,currentEdgeList)

               CALL edgeListIterator % initWithFTLinkedListClass(currentEdgeList)
               CALL edgeListIterator % setToStart()

               DO WHILE ( .NOT.EdgeListIterator % isAtEnd() )
                  obj => EdgeListIterator % object()
                  CALL castToSMEdge(obj,currentEdge)
                  IF ( currentEdge % edgeType == ON_BOUNDARY )     THEN
                     e   => currentEdge % elements(1) % element
!               BUG: puts boundary points in the wrong place.
                     CALL CleanUpBoundaryElement( e, model )
                  END IF

                  CALL EdgeListIterator % moveToNext()
               END DO

               CALL releaseFTLinkedListIteratorClass(edgeListIterator)
            END DO
            CALL deallocateNodeToEdgeConnections
         ELSE
            CALL mesh % destroyEdgeArrays()
            CALL deallocateNodeToElementConnections
            RETURN
         END IF
!
!        -------------------------------------------
!        Run through the interface elements
!        and split them as necessary
!        -------------------------------------------
!
         IF ( model % numberOfInterfaceCurves > 0 )     THEN

           ALLOCATE(interfaceElements)
           CALL interfaceElements % init()
!
!          ------------------------------------------------------
!          Gather all elements that have a node on an interface.
!          This includes the elements that only touch it at their
!          corners (one node, or two opposite nodes), which must
!          be split as well to keep the mesh conforming. An
!          element is listed once for each of its interface
!          nodes. Duplicates are skipped when splitting.
!          ------------------------------------------------------
!
            CALL makeNodeToElementConnections(mesh, errorCode)

            IF( errorCode == A_OK_ERROR_CODE)     THEN
               nodesIterator => mesh % nodesIterator
               CALL nodesIterator % setToStart()
               DO WHILE ( .NOT.nodesIterator % isAtEnd() )
                  obj => nodesIterator % object()
                  CALL castToSMNode(obj,meshNode)

                  IF ( isOnInterfaceCurve(meshNode) )     THEN
                     DO j = 1, numElementsForNode( meshNode % id )
                        e   => elementsForNodes(j,meshNode % id) % element
                        obj => e
                        CALL interfaceElements % add(obj)
                     END DO
                  END IF

                  CALL nodesIterator % moveToNext()
               END DO
               CALL deallocateNodeToElementConnections()
            ELSE
               CALL mesh % destroyEdgeArrays()
               CALL deallocateNodeToElementConnections
               RETURN
            END IF

!
!          -------------------------------
!          Split the elements as necessary
!          -------------------------------
!
           CALL splitInterfaceElements( mesh, interfaceElements )
!
           CALL releaseFTLinkedListClass(interfaceElements)
         END IF
!
!        --------
!        Clean up
!        --------
!
         CALL DoLazyDelete( mesh )

         CALL mesh % renumberAllLists()
         CALL mesh % syncEdges()
!
!        ---------------------------------------------------------------
!        Splitting an element with three nodes on an interface curve
!        adds an element at its middle node. Where the interface path
!        turned, this leaves a fan of thin elements around a valence 7
!        node on the curve. Reduce those valences now, keeping the node
!        on the curve (see CleanUp7ValenceNode_InMesh). This must come
!        before destroyEdgeArrays, which deallocates boundaryEdgesType
!        that tells which curves are interfaces.
!        ---------------------------------------------------------------
!
         IF ( model % numberOfInterfaceCurves > 0 )     THEN
            CALL ReduceNodeValences( mesh, numberOfValenceChanges, errorCode, nodesOnInterfaces = .TRUE. )
            IF ( numberOfValenceChanges > 0 )     THEN
               IF ( printMessage )     PRINT *, "      Valences on interfaces have been modified ", &
                                                numberOfValenceChanges, " time(s)"
               CALL mesh % renumberObjects(whichIterator = NODES)
               CALL mesh % syncEdges()
            END IF
!
!           -------------------------------------------------------------
!           Splitting the interface elements leaves diamonds next to the
!           interfaces. Remove those not attached to a curve, as in the
!           topology cleanup.
!           -------------------------------------------------------------
!
            CALL RemoveDiamondElements( mesh, numberOfDiamondsRemoved, errorCode )
            IF(errorCode > A_OK_ERROR_CODE)     RETURN
            IF ( numberOfDiamondsRemoved > 0 )     THEN
               IF ( printMessage )     PRINT *, "      Number of diamond elements removed near interfaces = ", &
                                                numberOfDiamondsRemoved
!
!              The edges of the removed diamonds kept their old nodes in
!              the mesh. Rebuild the edges, then delete the nodes that no
!              element uses (DoLazyDelete also renumbers).
!
               CALL mesh % syncEdges()
               CALL DoLazyDelete( mesh )
            END IF
         END IF

         CALL mesh % destroyEdgeArrays()
         CALL deallocateNodeToElementConnections()

      END SUBROUTINE CleanUpBoundaryElements
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CleanUpChevronElements( badElements, shapeMeasures, numberOfChevrons )

         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTMutableObjectArray) :: badElements
         REAL(KIND=RP)               :: shapeMeasures(:,:)
         INTEGER                     :: numberOfChevrons
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (SMElement), POINTER :: e => NULL(), eNbr => NULL()
         CLASS(FTObject) , POINTER :: obj => NULL()
         TYPE (SMNode)   , POINTER :: node => NULL()
         REAL(KIND=RP)             :: angles(4)
         INTEGER                   :: numberOfBadElements
         INTEGER                   :: k, j, badNodeID
         INTEGER                   :: nbrNodeLocalID , badNodeLocalID

         INTEGER, EXTERNAL :: Loop

         numberOfBadElements = badElements % COUNT()
         numberOfChevrons    = 0

         DO k = 1, numberOfBadElements
            obj => badElements % objectAtIndex(k)
            CALL castToSMElement(obj,e)
            IF( e % remove )     CYCLE
!
!           -------------------------------------
!           For now, look for new 180 degree node
!           -------------------------------------
!
            IF( shapeMeasures(MAX_ANGLE_INDEX,k) > 175.0_RP ) THEN !TODO make 175 a variable
               CALL ElementAngles( e, angles, .true. ) !Lefties have been reversed
               badNodeLocalID = -1
               DO j = 1, 4
                  IF( angles(j) > 175.0_RP )     THEN
                     badNodeLocalID = j
                     EXIT
                  END IF
               END DO
               IF(badNodeLocalID < 0) CYCLE !Bail on this one

               node => e % nodes(badNodeLocalID) % node
               badNodeID = node % id
!
!              -------------------------------------------
!              Check the topology - remove triangles only.
!              -------------------------------------------
!
               IF ( numElementsForNode(badNodeID) == 2 )     THEN
                  IF( ASSOCIATED(e,elementsForNodes(1,badNodeID) % element) )     THEN
                     eNbr => elementsForNodes(2,badNodeID) % element
                  ELSE IF ( ASSOCIATED(e,elementsForNodes(2,badNodeID) % element) )   THEN
                     eNbr => elementsForNodes(1,badNodeID) % element
                  ELSE
                     PRINT *, "Unassociated pointer for node ", badNodeID, " in element ", e % id
                     CYCLE
                  END IF
!
!                 ---------------------------------------------
!                 Replace the bad node in this element by
!                 the opposite node in the neighboring element.
!                 Find where in the neighbor this node is.
!                 ---------------------------------------------
!
                  j = -1
                  DO j = 1,4
!                     obj => eNbr % nodes % objectAtIndex(j)
!                     CALL cast(obj,node)
                     node => eNbr % nodes(j) % node
                     IF( node % id == badNodeID )     THEN
                        nbrNodeLocalID = j
                        EXIT
                     END IF
                  END DO

                  IF ( j < 0 )     THEN
                     PRINT *, "Bad shared element connection, ignoring elements ", e % id, eNbr % id
                     CYCLE
                  END IF
!
!                 ----------------------------------------------------
!                 Get the opposite facing node in the neighbor element
!                 ----------------------------------------------------
!

                  nbrNodeLocalID = Loop(nbrNodeLocalID+2,4)
!
!                 -------------
!                 Make the swap
!                 -------------
!
!                  obj => eNbr % nodes % objectAtIndex(nbrNodeLocalID)
!                  CALL e % nodes % replaceObjectAtIndexWithObject(badNodeLocalID, obj)
                  CALL releaseSMNode(e % nodes(badNodeLocalID) % node)
                  e % nodes(badNodeLocalID) % node => eNbr % nodes(nbrNodeLocalID) % node
                  CALL e % nodes(badNodeLocalID) % node % retain()
                  eNbr % remove = .true.
                  numberOfChevrons = numberOfChevrons + 1

               ELSE !TODO Do something here at some point.
!
!                 ------------------------------------
!                 This usually means a concave element
!                 ------------------------------------
!
!                  PRINT *, "Alternate Topology on bad angle", numElementsForNode(badNodeID)
!                  DO j = 1,4
!                     PRINT *, e % nodes(j) % node % x
!                  END DO
!                  PRINT *, "-----------"
               END IF
            END IF

         END DO
         IF(numberOfChevrons > 0) PRINT *, numberOfChevrons, " chevron elements removed from mesh."

      END SUBROUTINE CleanUpChevronElements
!
!///////////////////////////////////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CleanUpBoundaryElement( e, model )
         USE SMModelClass
         USE ErrorTypesModule
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS (SMElement), POINTER :: e
         TYPE (SMModel)  , POINTER :: model
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, PARAMETER               :: CBE_ON_BOUNDARY = 1, CBE_OFF_BOUNDARY = 0
         CLASS (SMNode)  , POINTER         :: sourceNode => NULL(), node => NULL()
         TYPE(SMNodePtr)                  :: elementNodes(4)  !Temp container for this procedure
         INTEGER                          :: boundaryNodeCount
         INTEGER                          :: k, m, j
         INTEGER                          :: mark(4)

         REAL(KIND=RP)  , DIMENSION(3)    :: p
         REAL(KIND=RP)                    :: t, tSave

         CLASS(SMCurve)       , POINTER   :: c => NULL()
         CLASS(SMChainedCurve), POINTER   :: chain  => NULL()      ! Returned also so that it, too can be used.
         TYPE(SMNode)                     :: nodeCopy(4) ! Two extra but not a big deal.
         TYPE (SMElement)     , POINTER   :: eTest => NULL()
         LOGICAL                          :: badArrayOriginal(6)      ! Node valence <= 6
         INTEGER                          :: nodeID

         CHARACTER(LEN=ERROR_EXCEPTION_MSG_LENGTH) :: msg
!
!        -------------------------------------------------------
!        A boundary element has at least two nodes on a boundary
!        -------------------------------------------------------
!
         boundaryNodeCount = 0
         mark              = CBE_OFF_BOUNDARY
         DO k = 1, 4
            elementNodes(k) % node => e % nodes(k) % node
            IF ( e % nodes(k) % node % bCurveID > UNDEFINED .AND. &
                 e % nodes(k) % node % distToBoundary == 0.0_RP )     THEN
               boundaryNodeCount = boundaryNodeCount + 1
               mark(k) = CBE_ON_BOUNDARY
            END IF
         END DO
!
!        ---------------------------------------------------
!        We check to make sure that this was called properly
!        ---------------------------------------------------
!
         IF( boundaryNodeCount < 2 )   RETURN
!
!        --------------------------------
!        See what we are starting with
!        i.e., see if any of the elements
!        are bad.
!        --------------------------------
!
         badArrayOriginal = .false.
         DO k = 1,4
            IF( mark(k) == CBE_ON_BOUNDARY )     THEN
!
!              --------------------------------------------
!              Find the quality of the neighboring elements
!              --------------------------------------------
!
               nodeID = elementNodes(k) % node % id

               DO j = 1, numElementsForNode(nodeID)
                  eTest => elementsForNodes(j,nodeID) % element
                  IF(.NOT.ASSOCIATED(etest))     THEN
                     WRITE(msg,*) "Neighbors to node ", node % x, "Not associated. Aborting cleanup."
                     CALL ThrowErrorExceptionOfType("CleanUpBoundaryElement",msg,FT_ERROR_WARNING)
                     RETURN
                  END IF
                  badArrayOriginal(j) = elementIsBad(eTest)
               END DO
            END IF
         END DO
!
!        ---------------------
!        Adjust boundary nodes
!        ---------------------
!
         DO k = 1, 4
            IF( mark(k) == CBE_OFF_BOUNDARY            )              CYCLE
            IF(elementNodes(k) % node % activeStatus == INACTIVE) CYCLE
            IF(elementNodes(k) % node % nodeType /= ROW_SIDE)     CYCLE
!
!           -----------------------------------
!           Find which node is off the boundary
!           -----------------------------------
!
            IF ( mark(sourceNodeLocalID(1,k)) == CBE_OFF_BOUNDARY )          THEN
               m = sourceNodeLocalID(1,k)
            ELSE IF ( mark(sourceNodeLocalID(2,k)) == CBE_OFF_BOUNDARY )     THEN
               m = sourceNodeLocalID(2,k)
            ELSE
               node => elementNodes(k) % node
               WRITE(msg,*) "Something wrong with node at ", node % x, ". Aborting cleanup."
               CALL ThrowErrorExceptionOfType("CleanUpBoundaryElement",msg,FT_ERROR_WARNING)
               RETURN
            END IF

            sourceNode  => elementNodes(m) % node
            node        => elementNodes(k) % node
            nodeCopy(k) =  node % typeCopy()

            p      = sourceNode % x
            t      = node % whereOnBoundary
            tSave  = t
!
            c => model % curveWithID(node % bCurveID, chain)
            t = ParametrizationAtPointNear(c,p,t)
!            tStart = t - 0.49_RP
!            tEnd   = t + 0.49_RP
!            tStart = MAX(tStart, 0.0_RP)
!            tEnd   = MIN(tEnd, 1.0_RP)
!!
!            t = fmin(tStart, tEnd, c, p, (/0.0_RP, 0.0_RP, 0.0_RP/), minimizationTolerance )

            node % whereOnBoundary  = t
            node % distToBoundary   = 0.0_RP !This is a boundary node
            node % gWhereOnBoundary = chain % ChainTForCurveTInCurve(t,c)

            node % x                = c % positionAt(t)
         END DO
!
!        --------------------------------------------------------------
!        This element has been adjusted. See if the new element and the
!        neighbors of the nodes have been messed up. If so, go back to
!        the original.
!        --------------------------------------------------------------
!
!         badArrayNew = .false.
!         DO k = 1, 4
!            IF( mark(k) == CBE_OFF_BOUNDARY )     CYCLE
!            IF(elementNodes(k) % node % activeStatus == INACTIVE) CYCLE
!            IF(elementNodes(k) % node % nodeType /= ROW_SIDE)     CYCLE
!!
!!           -----------------------------------
!!           Check the elements around this node
!!           -----------------------------------
!!
!            nodeID   = elementNodes(k) % node % id
!            failed = .false.
!            DO j = 1, numElementsForNode(nodeID)
!               eTest          => elementsForNodes(j,nodeID) % element
!               badArrayNew(j) = elementIsBad(eTest)
!               IF(badArrayNew(j) .AND. (.NOT.badArrayOriginal(j))) failed = .true.
!            END DO
!            IF(failed) THEN
!               nodeCopy(k) % activeStatus = INACTIVE
!               obj => e % nodes % objectAtIndex(k) !Supersceeded with new data structure
!               CALL cast(obj,node)
!               CALL copyOfNodeType(nodeCopy(k),node)
!            END IF
!         END DO
      END SUBROUTINE CleanUpBoundaryElement
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DeleteElementIfDiamond(e, mesh)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMElement), POINTER :: e
         TYPE(SMMesh)              :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                   :: valences(4)
         INTEGER                   :: k, j, id, idNbr, localIds(2)
         LOGICAL                   :: hasA3ValenceNode, isDiamond
         REAL(KIND=RP)             :: x(3), corners(3,4)
         TYPE (SMElement), POINTER :: eNbr    => NULL()
         CLASS(FTObject) , POINTER :: obj     => NULL()
         TYPE (SMNode)   , POINTER :: newNode => NULL()
         TYPE(SMNodePtr)           :: elementNodes(4)  !Temp container for this procedure
!
!        --------------------------------------------
!        Gather the valences of the four corners of e
!        --------------------------------------------
!
         hasA3ValenceNode = .false.
         DO k = 1,4
            elementNodes(k) % node => e % nodes(k) % node
            id                      = e % nodes(k) % node  % id
            valences(k) = numElementsForNode(id)
            IF(valences(k) == 3) hasA3ValenceNode = .true.
         END DO
         IF(.NOT.hasA3ValenceNode) RETURN
!
!        -------------------------------------------------------
!        A diamond has two non-adjacent 3-valence nodes, n1 & n2
!        -------------------------------------------------------
!
         isDiamond = .false.
         DO k = 1,2
            IF ( valences(k) == 3 )          THEN
               IF ( valences(k+2) == 3 )     THEN
                  isDiamond = .true.
                  localIds(1)  = k
                  localIds(2)  = k + 2
                  EXIT
               END IF
            END IF
         END DO

         IF(.NOT.isDiamond) RETURN
!
!        --------------------------------------
!        Do not remove a diamond if one node is
!        attached to a boundary
!        --------------------------------------
!
         IF ( elementNodes(localIDs(1)) % node % bCurveID /= UNDEFINED .OR. &
              elementNodes(localIDs(2)) % node % bCurveID /= UNDEFINED )     THEN
            RETURN
         END IF

         e % remove = .true.
!
!        ---------------------------------
!        Create a node inside this element
!        ---------------------------------
!
         DO k = 1, 4
            corners(:,k) = elementNodes(k) % node % x
         END DO
         CALL ComputeCentroid(corners,x)
         ALLOCATE(newNode)
         CALL newNode % initWithLocationAndID(x,mesh % newNodeID())
         obj => newNode
         CALL mesh % nodes % add(obj)
         CALL releaseSMNode(newNode)
!
!        ----------------------------------------------
!        Find the elements that share the two nodes,
!        Find the local ids of them within the elements
!        and point those to the new node
!        ----------------------------------------------
!
         DO j = 1, 2
            id = elementNodes(localIDs(j)) % node  % id
            DO k = 1, numelementsForNode(id)
               eNbr => elementsForNodes(k,id) % element
               IF(ASSOCIATED(eNbr,e)) CYCLE
!
!              ----
!              Find
!              ----
!
               idNbr = ElementLocalNodeIDForNodeID( id, eNbr )
!
!              -------------------
!              Release and repoint
!              -------------------
!
               CALL releaseSMNode( eNbr % nodes(idNbr) % node)
               eNbr % nodes(idNbr) % node => newNode
               CALL newNode % retain()
            END DO
         END DO

      END SUBROUTINE DeleteElementIfDiamond
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE UntangleInvertedElements( mesh, errorCode )
         IMPLICIT NONE
!
!        -----------------------------------------------------------------
!        After smoothing, an element can still have a corner where the
!        Jacobian is not positive, typically where curves come close to
!        each other or meet at a corner. Each corner Jacobian is linear in
!        the position of any one of its nodes, so the positions of a node
!        for which all corners of its elements are positive form a convex
!        polygon. Move each node of an inverted element that is not on a
!        curve toward the point farthest inside that polygon. Repeat a few
!        times since moving one node changes the polygons of its neighbors.
!        -----------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) , POINTER :: mesh
         INTEGER                 :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         CLASS(FTObject)            , POINTER :: obj      => NULL()
         TYPE (SMElement)           , POINTER :: e        => NULL()
         TYPE (SMNode)              , POINTER :: node     => NULL()
         INTEGER                              :: sweep, k, numInverted, numInvertedAtStart
         INTEGER                              :: numOpened
         REAL(KIND=RP), ALLOCATABLE           :: xAtStart(:,:)
!
!        ----------------------------------------------------------------------
!        Note, these two parameters are kept here rather than in ProgramGlobals
!        because placing them there gives a spurious warning in gfortran 15
!        that copyOfNodeClass (SMMeshObjects.f90) may call itself recursively
!        while compiling MeshOutputMethods. Adding any two variables to
!        ProgramGlobals does the same, so the warning seems to depend on the
!        layout of the module symbols, not on the code.
!        ----------------------------------------------------------------------
!
         INTEGER      , PARAMETER :: numUntanglePasses = 10       ! sweeps over the inverted elements
         REAL(KIND=RP), PARAMETER :: maxAcceptedAngle  = 165.0_RP ! degrees, larger angles are opened up

         iterator => mesh % elementsIterator
         CALL MakeNodeToElementConnections( mesh, errorCode )
         IF ( errorCode > A_OK_ERROR_CODE )     RETURN
!
!        -----------------------------------------------------------
!        Node positions before untangling. Each node may only move
!        within the box of its star at these positions so that
!        repeated moves cannot carry it away from its neighbors
!        -----------------------------------------------------------
!
         ALLOCATE( xAtStart(2, SIZE(numElementsForNode)) )
         CALL mesh % nodesIterator % setToStart()
         DO WHILE ( .NOT.mesh % nodesIterator % isAtEnd() )
            obj => mesh % nodesIterator % object()
            CALL castToSMNode(obj,node)
            xAtStart(:, node % id) = node % x(1:2)
            CALL mesh % nodesIterator % moveToNext()
         END DO
!
         numInvertedAtStart = -1
         DO sweep = 1, numUntanglePasses
            numInverted = 0
            CALL iterator % setToStart()
            DO WHILE ( .NOT.iterator % isAtEnd() )
               obj => iterator % object()
               CALL castToSMElement(obj,e)
               IF ( elementIsTangled(e) )     THEN
                  numInverted = numInverted + 1
                  DO k = 1, 4
                     node => e % nodes(k) % node
                     IF ( node % distToBoundary == 0.0_RP )     CYCLE
                     CALL MoveNodeIntoItsKernel( node, 0.5_RP, xAtStart )
                  END DO
               END IF
               CALL iterator % moveToNext()
            END DO
            IF ( numInvertedAtStart < 0 )     numInvertedAtStart = numInverted
            IF ( numInverted == 0 )     EXIT
         END DO

!
!        ---------------------------------------------------------------
!        An element with an angle close to 180 degrees is valid but
!        nearly degenerate. The springs cannot always open such a
!        corner, e.g. at a valence 3 node between straight rows of
!        edges. Move the free nodes of such elements part of the way to
!        where the corners around them are as far from flat as possible.
!        ---------------------------------------------------------------
!
         numOpened = 0
         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMElement(obj,e)
            IF ( .NOT.elementIsTangled(e) .AND. elementMaxAngle(e) > maxAcceptedAngle )     THEN
               DO k = 1, 4
                  node => e % nodes(k) % node
                  IF ( node % distToBoundary == 0.0_RP )     CYCLE
                  CALL MoveNodeIntoItsKernel( node, 0.5_RP, xAtStart )
               END DO
               IF ( elementMaxAngle(e) <= maxAcceptedAngle )     numOpened = numOpened + 1
            END IF
            CALL iterator % moveToNext()
         END DO

         IF ( printMessage .AND. numOpened > 0 )     &
            PRINT *, "   Untangling: ", numOpened, " elements with angles above ", &
                     NINT(maxAcceptedAngle), " degrees opened up"
         IF ( printMessage .AND. numInvertedAtStart > 0 )     THEN
            numInverted = 0
            CALL iterator % setToStart()
            DO WHILE ( .NOT.iterator % isAtEnd() )
               obj => iterator % object()
               CALL castToSMElement(obj,e)
               IF ( elementIsTangled(e) )     numInverted = numInverted + 1
               CALL iterator % moveToNext()
            END DO
            PRINT *, "   Untangling: ", numInvertedAtStart - numInverted, " of ", &
                     numInvertedAtStart, " inverted elements fixed"
         END IF

         CALL deallocateNodeToElementConnections()

      END SUBROUTINE UntangleInvertedElements
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION elementIsTangled( e )
         IMPLICIT NONE
!
!        -------------------------------------------------------------
!        Some corner Jacobian is not positive. An element with all four
!        negative is turned over completely, and is tangled as well.
!        -------------------------------------------------------------
!
         TYPE (SMElement), POINTER :: e
         INTEGER                   :: k
         REAL(KIND=RP)             :: cornerJacobians(4)
         DO k = 1, 4
            cornerJacobians(k) = elementCornerJacobian(e, k)
         END DO
         elementIsTangled = MINVAL(cornerJacobians) <= 0.0_RP
      END FUNCTION elementIsTangled
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION elementMaxAngle( e )
         IMPLICIT NONE
!
!        ---------------------------------------------------------
!        Largest interior angle of a counterclockwise element, in
!        degrees (above 180 at a reflex corner)
!        ---------------------------------------------------------
!
         TYPE (SMElement), POINTER :: e
         INTEGER                   :: k
         REAL(KIND=RP)             :: xPrev(3), x(3), xNext(3), angle
         INTEGER, EXTERNAL         :: Loop

         elementMaxAngle = 0.0_RP
         DO k = 1, 4
            xPrev = e % nodes(Loop(k-1,4)) % node % x
            x     = e % nodes(k) % node % x
            xNext = e % nodes(Loop(k+1,4)) % node % x
            angle = ATAN2( cornerJacobian(xPrev, x, xNext), DOT_PRODUCT(xNext(1:2) - x(1:2), xPrev(1:2) - x(1:2)) )
            angle = MODULO(angle*180.0_RP/PI, 360.0_RP)
            elementMaxAngle = MAX(elementMaxAngle, angle)
         END DO
      END FUNCTION elementMaxAngle
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MoveNodeIntoItsKernel( node, fraction, xAtStart )
         IMPLICIT NONE
!
!        -----------------------------------------------------------------
!        Each corner Jacobian of the elements around the node that involves
!        the node is written as gx + h, normalized so that it is the signed
!        distance of x from a line. The point that maximizes the smallest
!        of them is found at a vertex where three are equal. The node is
!        moved from its current position toward that point only as far as
!        needed for the smallest distance to cover the given fraction of
!        the way from its current value (or zero, if that is negative) to
!        its best value.
!        -----------------------------------------------------------------
!
         TYPE (SMNode), POINTER :: node
         REAL(KIND=RP)          :: fraction
         REAL(KIND=RP)          :: xAtStart(:,:)
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (SMElement), POINTER :: e => NULL()
         REAL(KIND=RP)             :: g(2,3*maxValence+4), h(3*maxValence+4), starRadius
         REAL(KIND=RP)             :: x0(3), xBest(2), current, best, value, sLow, sHigh, s, target
         REAL(KIND=RP)             :: lhs(3,3), rhs(3), sol(3), jacobianAtOrigin, dJacobianDx, dJacobianDy, gNorm
         INTEGER                   :: id, i, k, n, i1, i2, i3, iter
         INTEGER, EXTERNAL         :: Loop

         id = node % id
         x0 = node % x
         n  = 0
         DO i = 1, numElementsForNode(id)
            e => elementsForNodes(i,id) % element
            DO k = 1, 4
               IF ( .NOT.(ASSOCIATED(e % nodes(Loop(k-1,4)) % node, node) .OR. &
                          ASSOCIATED(e % nodes(k) % node, node)           .OR. &
                          ASSOCIATED(e % nodes(Loop(k+1,4)) % node, node)) )     CYCLE
               node % x(1:2) = 0.0_RP
               jacobianAtOrigin = elementCornerJacobian(e, k)
               node % x(1:2) = [1.0_RP, 0.0_RP]
               dJacobianDx = elementCornerJacobian(e, k) - jacobianAtOrigin
               node % x(1:2) = [0.0_RP, 1.0_RP]
               dJacobianDy = elementCornerJacobian(e, k) - jacobianAtOrigin
               gNorm = SQRT(dJacobianDx**2 + dJacobianDy**2)
               IF ( gNorm == 0.0_RP )     CYCLE
               n = n + 1
               g(:,n) = [dJacobianDx, dJacobianDy]/gNorm
               h(n)   = jacobianAtOrigin/gNorm
            END DO
         END DO
         node % x = x0
         IF ( n < 3 )     RETURN
!
!        -------------------------------------------------------------
!        Keep the node within the box spanned by its star before
!        untangling started, so that it is not thrown far away when
!        the polygon is large or open, nor carried away by repeated
!        moves
!        -------------------------------------------------------------
!
         starRadius = 0.0_RP
         DO i = 1, numElementsForNode(id)
            e => elementsForNodes(i,id) % element
            DO k = 1, 4
               starRadius = MAX(starRadius, NORM2(xAtStart(:, e % nodes(k) % node % id) - xAtStart(:,id)))
            END DO
         END DO
         g(:,n+1) = [ 1.0_RP, 0.0_RP]; h(n+1) = starRadius - xAtStart(1,id)
         g(:,n+2) = [-1.0_RP, 0.0_RP]; h(n+2) = starRadius + xAtStart(1,id)
         g(:,n+3) = [ 0.0_RP, 1.0_RP]; h(n+3) = starRadius - xAtStart(2,id)
         g(:,n+4) = [ 0.0_RP,-1.0_RP]; h(n+4) = starRadius + xAtStart(2,id)
         n = n + 4

         current = distanceInside( g, h, n, x0(1:2) )
         best    = current
         xBest   = x0(1:2)
         DO i1 = 1, n-2
            DO i2 = i1+1, n-1
               DO i3 = i2+1, n
                  lhs(1,:) = [g(1,i1), g(2,i1), -1.0_RP]; rhs(1) = -h(i1)
                  lhs(2,:) = [g(1,i2), g(2,i2), -1.0_RP]; rhs(2) = -h(i2)
                  lhs(3,:) = [g(1,i3), g(2,i3), -1.0_RP]; rhs(3) = -h(i3)
                  IF ( .NOT.solve3x3(lhs, rhs, sol) )     CYCLE
                  value = distanceInside( g, h, n, sol(1:2) )
                  IF ( value > best )     THEN
                     best  = value
                     xBest = sol(1:2)
                  END IF
               END DO
            END DO
         END DO
!
!        ---------------------------------------------------------------
!        Ignore improvements at roundoff level, which can come from a
!        vertex where the node would sit on top of one of its neighbors
!        ---------------------------------------------------------------
!
         IF ( best <= MAX(current, 0.0_RP) + 1.0E-3_RP*starRadius )     RETURN
!
!        ----------------------------------------------------------
!        The smallest distance is concave along the segment from x0
!        to xBest, so bisect for the first point where it reaches
!        the target value
!        ----------------------------------------------------------
!
         target = MAX(current, 0.0_RP) + fraction*(best - MAX(current, 0.0_RP))
         sLow  = 0.0_RP
         sHigh = 1.0_RP
         DO iter = 1, 30
            s = 0.5_RP*(sLow + sHigh)
            IF ( distanceInside( g, h, n, x0(1:2) + s*(xBest - x0(1:2)) ) >= target )     THEN
               sHigh = s
            ELSE
               sLow = s
            END IF
         END DO
         node % x(1:2) = x0(1:2) + sHigh*(xBest - x0(1:2))

      END SUBROUTINE MoveNodeIntoItsKernel
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION elementCornerJacobian( e, k )
         IMPLICIT NONE
!
!        ----------------------------------------
!        Corner Jacobian of element e at corner k
!        ----------------------------------------
!
         TYPE (SMElement), POINTER :: e
         INTEGER                   :: k
         INTEGER, EXTERNAL         :: Loop
         elementCornerJacobian = cornerJacobian( e % nodes(Loop(k-1,4)) % node % x, &
                                                 e % nodes(k) % node % x,           &
                                                 e % nodes(Loop(k+1,4)) % node % x )
      END FUNCTION elementCornerJacobian
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION distanceInside( g, h, n, x )
         IMPLICIT NONE
!
!        -------------------------------------------------------------
!        Smallest of the n normalized constraints g(:,i)*x + h(i) at x
!        -------------------------------------------------------------
!
         INTEGER       :: n
         REAL(KIND=RP) :: g(:,:), h(:), x(2)
         distanceInside = MINVAL( g(1,1:n)*x(1) + g(2,1:n)*x(2) + h(1:n) )
      END FUNCTION distanceInside
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION solve3x3( a, b, x )
         IMPLICIT NONE
!
!        ----------------------------------------------------------
!        Solve a x = b by Cramer's rule. False if a is (nearly)
!        singular, in which case x is not set
!        ----------------------------------------------------------
!
         REAL(KIND=RP) :: a(3,3), b(3), x(3)
         REAL(KIND=RP) :: det, cramerMatrix(3,3)
         INTEGER       :: col
         det = determinant3x3(a)
         solve3x3 = ABS(det) > 1.0E-12_RP
         IF ( .NOT.solve3x3 )     RETURN
         DO col = 1, 3
            cramerMatrix = a
            cramerMatrix(:,col) = b
            x(col) = determinant3x3(cramerMatrix)/det
         END DO
      END FUNCTION solve3x3
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION determinant3x3( a )
         IMPLICIT NONE
         REAL(KIND=RP) :: a(3,3)
         determinant3x3 = a(1,1)*(a(2,2)*a(3,3) - a(2,3)*a(3,2)) &
                          - a(1,2)*(a(2,1)*a(3,3) - a(2,3)*a(3,1)) &
                          + a(1,3)*(a(2,1)*a(3,2) - a(2,2)*a(3,1))
      END FUNCTION determinant3x3

!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ImproveElementShapes( mesh, errorCode, symmetryLine )
         IMPLICIT NONE
!
!        -----------------------------------------------------------------
!        The spring smoother leaves some elements with poor angles, mostly
!        at irregular nodes such as the valence 5 and 3 nodes of the kites
!        made by SplitHighValenceNodes. Each node of an element whose
!        smallest scaled corner Jacobian is below minAcceptedScaledJacobian
!        and that is not on a boundary is moved by a compass search to where
!        the worst element around it has the best shape, see
!        MoveNodeToImproveShapes. Repeat a few sweeps since moving one
!        node changes the elements of its neighbors.
!
!        If the mesh was reflected across a symmetry line, given as the
!        coefficients a, b, c of a*x + b*y + c = 0, nodes are moved in
!        mirror pairs: the node on the positive side of the line is
!        optimized, and its partner is put at its reflection so that the
!        mesh stays symmetric. A node without a partner is not moved.
!        -----------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh) , POINTER  :: mesh
         INTEGER                  :: errorCode
         REAL(KIND=RP), OPTIONAL  :: symmetryLine(3)
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedListIterator), POINTER     :: iterator => NULL()
         CLASS(FTObject)            , POINTER     :: obj      => NULL()
         TYPE (SMElement)           , POINTER     :: e        => NULL()
         TYPE (SMNode)              , POINTER     :: node     => NULL(), partner => NULL()
         TYPE (SMNodePtr)           , ALLOCATABLE :: nodeArray(:)
         INTEGER                    , ALLOCATABLE :: partnerID(:)
         REAL(KIND=RP)              , ALLOCATABLE :: areaAtStart(:)
         REAL(KIND=RP)                            :: shapeMeasures(NUMBER_OF_2D_SHAPE_MEASURES), x(3)
         INTEGER                                  :: sweep, k, numMoves, numElementsTreated
         LOGICAL                                  :: isSymmetric, moved
!
!        ---------------------------------------------------------------------
!        Note, these parameters are kept here rather than in ProgramGlobals to
!        avoid the same spurious warning in gfortran 15 the same reason as
!        described in UntangleInvertedElements
!        ---------------------------------------------------------------------
!
         INTEGER      , PARAMETER :: numShapeSweeps            = 6
         REAL(KIND=RP), PARAMETER :: minAcceptedScaledJacobian = 0.8_RP

         isSymmetric = PRESENT(symmetryLine)
         iterator => mesh % elementsIterator
         CALL MakeNodeToElementConnections( mesh, errorCode )
         IF ( errorCode > A_OK_ERROR_CODE )     RETURN
!
!        --------------------------------------------------------------
!        Element areas before the moves. A node may not change the size
!        of its elements much, so that the size grading is kept
!        --------------------------------------------------------------
!
         ALLOCATE( areaAtStart(mesh % elements % COUNT()) )
         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMElement(obj,e)
            CALL ComputeElementShapeMeasures2D( e, shapeMeasures )
            areaAtStart(e % id) = shapeMeasures(SIGNED_AREA_INDEX)
            CALL iterator % moveToNext()
         END DO

         IF ( isSymmetric )     THEN
            ALLOCATE( nodeArray(SIZE(numElementsForNode)), partnerID(SIZE(numElementsForNode)) )
            partnerID = 0
            CALL mesh % nodesIterator % setToStart()
            DO WHILE ( .NOT.mesh % nodesIterator % isAtEnd() )
               obj => mesh % nodesIterator % object()
               CALL castToSMNode(obj,node)
               nodeArray(node % id) % node => node
               CALL mesh % nodesIterator % moveToNext()
            END DO
         END IF

         DO sweep = 1, numShapeSweeps
            numMoves           = 0
            numElementsTreated = 0
            CALL iterator % setToStart()
            DO WHILE ( .NOT.iterator % isAtEnd() )
               obj => iterator % object()
               CALL castToSMElement(obj,e)
               IF ( elementMinScaledJacobian(e) < minAcceptedScaledJacobian )     THEN
                  numElementsTreated = numElementsTreated + 1
                  DO k = 1, 4
                     node => e % nodes(k) % node
                     IF ( node % distToBoundary == 0.0_RP )     CYCLE
                     IF ( node % bCurveID /= UNDEFINED )        CYCLE

                     IF ( isSymmetric )     THEN
                        partner => mirrorPartner( node, nodeArray, partnerID, symmetryLine )
                        IF ( .NOT.ASSOCIATED(partner) )     CYCLE
                        IF ( DOT_PRODUCT(symmetryLine(1:2), node % x(1:2)) + symmetryLine(3) < 0.0_RP )     THEN
                           partner => node
                           node    => nodeArray(partnerID(partner % id)) % node
                        END IF
                     END IF

                     CALL MoveNodeToImproveShapes( node, areaAtStart, minAcceptedScaledJacobian, moved )
                     IF ( .NOT.moved )     CYCLE
                     numMoves = numMoves + 1
                     IF ( isSymmetric )     THEN
                        x = reflectAboutLine(node % x, symmetryLine(1), symmetryLine(2), symmetryLine(3))
                        partner % x(1:2) = x(1:2)
                     END IF
                  END DO
               END IF
               CALL iterator % moveToNext()
            END DO
            IF ( sweep == 1 .AND. printMessage .AND. numElementsTreated > 0 )     &
               PRINT *, "   Improving the shapes of ", numElementsTreated, " elements"
            IF ( numMoves == 0 )     EXIT
         END DO

         CALL deallocateNodeToElementConnections()

      END SUBROUTINE ImproveElementShapes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MoveNodeToImproveShapes( node, areaAtStart, minAcceptedScaledJacobian, moved )
         IMPLICIT NONE
!
!        -----------------------------------------------------------------
!        Compass search for the position of the node that maximizes the
!        smallest shape quality of the elements around it, see
!        StarQualities. Steps in eight directions are tried, the best
!        that improves is taken, otherwise the step is halved. A step is
!        only taken if the smallest scaled Jacobian of the elements around
!        the node does not fall below its value at the start, or below
!        minAcceptedScaledJacobian if that is smaller, so that the angles
!        are not traded for sizes.
!        -----------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMNode), POINTER :: node
         REAL(KIND=RP)          :: areaAtStart(:)
         REAL(KIND=RP)          :: minAcceptedScaledJacobian
         LOGICAL                :: moved
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (SMElement), POINTER :: e
         REAL(KIND=RP)             :: x0(3), xBest(2), best, quality, stepSize, minStepSize
         REAL(KIND=RP)             :: edgeLength, scaledJacobian, scaledJacobianFloor
         INTEGER                   :: i, k, direction, bestDirection, numIterations
         INTEGER, EXTERNAL         :: Loop
         INTEGER      , PARAMETER  :: maxSearchIterations = 60
         REAL(KIND=RP), PARAMETER  :: s = 0.7071067811865476_RP ! 1/sqrt(2)
         REAL(KIND=RP), PARAMETER  :: directions(2,8) = RESHAPE( [ 1.0_RP, 0.0_RP,  s,  s,  0.0_RP,  1.0_RP, -s,  s, &
                                                                  -1.0_RP, 0.0_RP, -s, -s,  0.0_RP, -1.0_RP,  s, -s], [2,8] )

         moved = .FALSE.
         x0    = node % x
!
!        ----------------------------------------------------
!        Step sizes relative to the shortest edge at the node
!        ----------------------------------------------------
!
         edgeLength = HUGE(1.0_RP)
         DO i = 1, numElementsForNode(node % id)
            e => elementsForNodes(i, node % id) % element
            k =  ElementLocalNodeIDForNodeID( node % id, e )
            edgeLength = MIN(edgeLength, NORM2(e % nodes(Loop(k+1,4)) % node % x(1:2) - x0(1:2)), &
                                         NORM2(e % nodes(Loop(k-1,4)) % node % x(1:2) - x0(1:2)))
         END DO
         stepSize    = 0.1_RP * edgeLength
         minStepSize = 1.0e-3_RP * edgeLength

         CALL StarQualities( node, areaAtStart, best, scaledJacobianFloor )
         scaledJacobianFloor = MIN(scaledJacobianFloor, minAcceptedScaledJacobian)
         xBest               = x0(1:2)
         numIterations       = 0
         DO WHILE ( stepSize > minStepSize .AND. numIterations < maxSearchIterations )
            numIterations = numIterations + 1
            bestDirection = 0
            DO direction = 1, 8
               node % x(1:2) = xBest + stepSize*directions(:,direction)
               CALL StarQualities( node, areaAtStart, quality, scaledJacobian )
               IF ( quality > best + 1.0e-6_RP .AND. scaledJacobian >= scaledJacobianFloor )     THEN
                  best          = quality
                  bestDirection = direction
               END IF
            END DO
            IF ( bestDirection == 0 )     THEN
               stepSize = 0.5_RP*stepSize
            ELSE
               xBest = xBest + stepSize*directions(:,bestDirection)
               moved = .TRUE.
            END IF
         END DO
         node % x(1:2) = xBest

      END SUBROUTINE MoveNodeToImproveShapes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE StarQualities( node, areaAtStart, shapeQuality, minScaledJacobian )
         IMPLICIT NONE
!
!        ----------------------------------------------------------------
!        Smallest shape quality and smallest scaled Jacobian of the
!        elements around the node. The shape quality of an element is the
!        inverse of its condition number, which is 1 for a square and
!        lower for skewed corners or edges of unequal length, times
!        min(r, 1/r) where r is the ratio of its area to its area at the
!        start, so that sizes are kept.
!        ----------------------------------------------------------------
!
         TYPE (SMNode), POINTER    :: node
         REAL(KIND=RP)             :: areaAtStart(:)
         REAL(KIND=RP)             :: shapeQuality, minScaledJacobian

         TYPE (SMElement), POINTER :: e
         REAL(KIND=RP)             :: shapeMeasures(NUMBER_OF_2D_SHAPE_MEASURES), areaRatio, quality
         INTEGER                   :: i

         shapeQuality      = HUGE(1.0_RP)
         minScaledJacobian = HUGE(1.0_RP)
         DO i = 1, numElementsForNode(node % id)
            e => elementsForNodes(i, node % id) % element
            CALL ComputeElementShapeMeasures2D( e, shapeMeasures )
            areaRatio = shapeMeasures(SIGNED_AREA_INDEX)/areaAtStart(e % id)
            IF ( areaRatio <= 0.0_RP )     THEN
               quality = -1.0_RP
            ELSE
               quality = MIN(areaRatio, 1.0_RP/areaRatio)/shapeMeasures(CONDITION_INDEX)
            END IF
            shapeQuality      = MIN(shapeQuality, quality)
            minScaledJacobian = MIN(minScaledJacobian, elementMinScaledJacobian(e))
         END DO
      END SUBROUTINE StarQualities
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION elementMinScaledJacobian( e )
         IMPLICIT NONE
!
!        ------------------------------------------------------------
!        Smallest over the corners of the corner Jacobian divided by
!        the lengths of the two edges, the sine of the corner angle
!        ------------------------------------------------------------
!
         TYPE (SMElement), POINTER :: e
         INTEGER                   :: k
         INTEGER, EXTERNAL         :: Loop
         REAL(KIND=RP)             :: x(2)

         elementMinScaledJacobian = HUGE(1.0_RP)
         DO k = 1, 4
            x = e % nodes(k) % node % x(1:2)
            elementMinScaledJacobian = MIN( elementMinScaledJacobian, elementCornerJacobian(e, k)       &
                                            /( NORM2(e % nodes(Loop(k-1,4)) % node % x(1:2) - x)        &
                                              *NORM2(e % nodes(Loop(k+1,4)) % node % x(1:2) - x) ) )
         END DO
      END FUNCTION elementMinScaledJacobian
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION mirrorPartner( node, nodeArray, partnerID, symmetryLine ) RESULT(partner)
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        The node at the reflection of the given node across the
!        symmetry line, or NULL if there is none. Found by a search over
!        all nodes the first time it is asked for, and remembered in
!        partnerID for both nodes (-1 means there is no partner).
!        ---------------------------------------------------------------
!
         TYPE (SMNode)  , POINTER :: node
         TYPE (SMNodePtr)         :: nodeArray(:)
         INTEGER                  :: partnerID(:)
         REAL(KIND=RP)            :: symmetryLine(3)
         TYPE (SMNode)  , POINTER :: partner

         REAL(KIND=RP)            :: xMirror(3), tolerance, distance, closest
         INTEGER                  :: j, closestID

         partner => NULL()
         IF ( partnerID(node % id) == 0 )     THEN
            xMirror   = reflectAboutLine(node % x, symmetryLine(1), symmetryLine(2), symmetryLine(3))
            tolerance = 1.0e-8_RP*MAX(1.0_RP, MAXVAL(ABS(node % x(1:2))))
            closest   = HUGE(1.0_RP)
            closestID = -1
            DO j = 1, SIZE(nodeArray)
               IF ( j == node % id )     CYCLE
               distance = NORM2(nodeArray(j) % node % x(1:2) - xMirror(1:2))
               IF ( distance < closest )     THEN
                  closest   = distance
                  closestID = j
               END IF
            END DO
            IF ( closest > tolerance )     closestID = -1
            partnerID(node % id) = closestID
            IF ( closestID > 0 )     partnerID(closestID) = node % id
         END IF
         IF ( partnerID(node % id) > 0 )     partner => nodeArray(partnerID(node % id)) % node
      END FUNCTION mirrorPartner

      END Module MeshCleaner

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
!      InterfaceElementMethods.f90
!      Created: May 16, 2013 11:17 AM
!      By: David Kopriva
!
!Interface boundary steps
!
!GenerateQuadMeshFromGrid
!  MarkInterfaceElements
!    For each curve
!      For each element
!        Use winding fn to mark 4 corners as inside or outside
!        Mark as interface element if element straddles interface curve
!        Mark each node bCurveSide as INSIDE or OUTSIDE
!        Set node bCurveChainID to the curve ID
!  CollectBoundaryEdges
!    For each edge in the mesh
!      If both nodes on edge are associated with a curve &
!      If that curve is an interface curve &
!      If BOTH nodes are INSIDE the curve
!        Mark edges as ON_INTERFACE
!  LocateEdgeImagesOnBoundaries
!    For each boundary curve
!        If first edge node of first edges bCurveSide /= INSIDE isInnerBoundaryCurve = .FALSE. ELSE
!  isInnerBoundaryCurve = .TRUE. ??? Why is this ??? (isInnerBoundaryCurve determines /pm on nHat)
!  AssociateBoundaryEdgesToCurve
!    For each node in edges list for this boundary curve
!      find t value for closest point on the curve and the distToBoundary
!
!   CleanUpBoundaryCurves
!     For each interface boundary edge list
!        MoveInterfaceNodesToBoundary
!          For each edge in boundary edge list
!            For both nodes on edge, set x value to curve location for t
!
!////////////////////////////////////////////////////////////////////////
!
      Module InterfaceElementMethods
      USE MeshProjectClass
      IMPLICIT NONE
!
!-------------------------------------------------------------------
! Define procedures for modifying elements in the neighborhood of
! material interface boundaries
!-------------------------------------------------------------------
!
      CONTAINS
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SplitInterfaceElements( mesh, interfaceElements )
!
!        --------------------------------------------------------------
!        When the nodes are moved onto an interface boundary,
!        three element shapes are created.
!        (1) A triangle with two edges/three nodes on the interface
!        (2) A wedge with one node on the interface
!        (3) A regular element with one edge/two nodes

!        The triangle elements must be split, and the neighbor elements
!        must be then split to make the mesh conforming.
!        --------------------------------------------------------------
!

         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh)      , POINTER :: mesh
         CLASS(FTLinkedList), POINTER :: interfaceElements
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (SMElement)           , POINTER :: e               => NULL()
         CLASS(FTLinkedList)        , POINTER :: newElementsList => NULL()
         CLASS(FTObject)            , POINTER :: obj             => NULL()
         TYPE (SMNode)              , POINTER :: node            => NULL()
         CLASS(FTLinkedListIterator), POINTER :: interfaceElementIterator => NULL()
         INTEGER                              :: interfaceNodeCount, k, side
         INTEGER                              :: boundaryNodeNumber, oppositeNodeNumber
         INTEGER                              :: interfaceNodes(4)

         INTEGER, EXTERNAL                    :: Loop
!
!        ---------------------------------------------
!        New elements will be created when splitting
!        put them into a list and add them to the mesh
!        when we are done
!        ---------------------------------------------
!
         ALLOCATE(newElementsList)
         CALL newElementsList % init()
         CALL makeElementToEdgeConnections(mesh)
!
!        -----------------------------------------------
!        Step through each element on this interface and
!        split according to its shape
!        -----------------------------------------------
!
         ALLOCATE(interfaceElementIterator)
         CALL interfaceElementIterator % initWithFTLinkedListClass(interfaceElements)
         CALL interfaceElementIterator % setToStart()
         DO WHILE ( .NOT.interfaceElementIterator % isAtEnd() )
            obj => interfaceElementIterator % object()
            CALL castToSMElement(obj,e)
            IF ( e % remove )     THEN
               CALL interfaceElementIterator % moveToNext()
               CYCLE !in case elements are doubled up
            END IF
!
!           ------------------------------
!           See what element shape this is
!           ------------------------------
!
            interfaceNodeCount = 0
            boundaryNodeNumber = 0
            oppositeNodeNumber = 0
            interfaceNodes     = 0
            DO k = 1, 4
               node => e % nodes(k) % node
               IF ( IsInterfaceNode(node) )     THEN
                  interfaceNodeCount = interfaceNodeCount + 1
                  interfaceNodes(interfaceNodeCount) = k
!
!                 ------------------------------------------------------------
!                 If there is only one interface node, then boundaryNodeNumber
!                 marks where that is
!                 ------------------------------------------------------------
!
                  boundaryNodeNumber    = k
               ELSE
!
!                 ---------------------------------------------------
!                 If there is only one node not on the boundary, then
!                 oppositeNodeNumber will mark where it is
!                 ---------------------------------------------------
!
                  oppositeNodeNumber    = k
               END IF
            END DO
!
!           --------------------------------------------------------
!           Split according to the number of nodes on the interface.
!           Every edge with exactly one interface node is split at
!           its midpoint, which keeps the mesh conforming.
!
!           Note, when `interfaceNodeCount = 0 or 4 no edge needs
!           to be split and the element is kept as is.
!           --------------------------------------------------------
!
            SELECT CASE ( interfaceNodeCount )
!              ---------
               CASE( 1 )
!              ---------
!
!                 -------------------------
!                 Split into three elements
!                 -------------------------
!
                  CALL SplitElementIntoThree( boundaryNodeNumber, e, newElementsList, mesh )
                  e % remove = .true.

!              ---------
               CASE( 2 )
!              ---------

                  IF ( interfaceNodes(2) - interfaceNodes(1) == 2 )     THEN
!
!                    -----------------------------------------------
!                    The interface nodes are at opposite corners, so
!                    all four edges are split.
!                    -----------------------------------------------
!
                     CALL SplitElementIntoFour( e, newElementsList, mesh )
                  ELSE
!
!                    -----------------------
!                    Split into two elements
!                    -----------------------
!
                     IF ( interfaceNodes(1) == 1 .AND. interfaceNodes(2) == 4 )     THEN
                        side = 4
                     ELSE
                        side = interfaceNodes(1)
                     END IF
                     CALL SplitElementIntoTwo( side, e, newElementsList, mesh )
                  END IF
                  e % remove = .true.

!              ---------
               CASE( 3 )
!              ---------
!
!                 -------------------------
!                 Split into three elements
!                 -------------------------
!
                  CALL SplitElementIntoThree( oppositeNodeNumber, e, newElementsList, mesh )
                  e % remove = .true.

               CASE DEFAULT
            END SELECT

            CALL interfaceElementIterator % moveToNext()
         END DO
!
!        -------------------------------------------------
!        Remove deleted elements and add the newly created
!        ones to the mesh
!        -------------------------------------------------
!
         CALL mesh % elements % addObjectsFromList(newElementsList)
         CALL releaseFTLinkedListIteratorClass(interfaceElementIterator)
         CALL releaseFTLinkedListClass(newElementsList)

      END SUBROUTINE SplitInterfaceElements
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SplitElementIntoTwo( side, e, newElementsList, mesh )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                      :: side
         TYPE (SMMesh)      , POINTER :: mesh
         TYPE (SMElement)   , POINTER :: e
         CLASS(FtLinkedList), POINTER :: newElementsList
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE(SMNodePtr), DIMENSION(4) :: elementNodes, nodes
         TYPE(SMNodePtr)               :: newNodePtr1, newNodePtr2
         TYPE (SMEdge)   , POINTER     :: edge => NULL()
         TYPE (SMNode)   , POINTER     :: swapNodePtr => NULL(), node => NULL()

         INTEGER                       :: id, k
         INTEGER                       :: sideP, sideM
         REAL(KIND=RP)                 :: x(3)
         INTEGER, EXTERNAL             :: Loop
!
!        ----------------------------------------------
!        The interface edge is the side whose two nodes
!        are both on the interface
!        ----------------------------------------------
!
         id   = e % id
!
!        ----------------------------------------------------
!        Weak reference the four corner nodes for convenience
!        ----------------------------------------------------
!
         DO k = 1,4
            nodes(k) % node => e % nodes(k) % node
         END DO
!
!        -----------------------------------------------------------
!        Find the edges on either side. These edges will be split in
!        half. Create two new nodes at the centers of these edges.
!        Since the new nodes will be shared, we store a pointer to
!        them on the edge. If the pointer is already set, it means
!        that we don't have to compute it again.
!        -----------------------------------------------------------
!
         sideP =  Loop(side+1,4)
         edge  => edgesForElements(sideP,id) % edge
         IF ( ASSOCIATED(edge % auxiliaryNode) )     THEN
            newNodePtr1 % node =>  edge % auxiliaryNode !Weak reference
         ELSE
            x     =  0.5_RP*(edge % nodes(1) % node % x + edge % nodes(2) % node % x)
            CALL constructNewNode(mesh,x,edge,node)
            newNodePtr1 % node => node !Weak reference
         END IF

         sideM =  Loop(side-1,4)
         edge  => edgesForElements(sideM,id) % edge
         IF ( ASSOCIATED(edge % auxiliaryNode) )     THEN
            newNodePtr2 % node =>  edge % auxiliaryNode !Weak reference
         ELSE
            x     =  0.5_RP*(edge % nodes(1) % node % x + edge % nodes(2) % node % x)
            CALL constructNewNode(mesh,x,edge,node)
            newNodePtr2 % node => node !Weak reference
         END IF
!
!        ---------------------------------------------
!        Make the node on left #1 and the node on the
!        top or right #2
!        ---------------------------------------------
!
         IF ( side == 2 .OR. side == 1 )     THEN
            swapNodePtr        => newNodePtr1 % node
            newNodePtr1 % node => newNodePtr2 % node
            newNodePtr2 % node => swapNodePtr
         END IF
!
!        ----------------------------------------------
!        Split the current element to use the two new
!        nodes. This is a template type operation.
!        ----------------------------------------------
!
         SELECT CASE ( side )
!
!           -----------
            CASE( 1,3 )
!           -----------
!
!              --------
!              Top side
!              --------
!
               elementNodes(1) % node => newNodePtr1 % node
               elementNodes(2) % node => newNodePtr2 % node
               elementNodes(3) % node => nodes(3) % node
               elementNodes(4) % node => nodes(4) % node

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newElementsList)
!
!              -----------
!              Bottom side
!              -----------
!
               elementNodes(3) % node => newNodePtr2 % node
               elementNodes(1) % node => nodes(1) % node
               elementNodes(2) % node => nodes(2) % node
               elementNodes(4) % node => newNodePtr1 % node

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newElementsList)
!
!           -----------
            CASE( 2,4 )
!           -----------
!
!              ---------
!              Left side
!              ---------
!
               elementNodes(1) % node => nodes(1) % node
               elementNodes(2) % node => newNodePtr1 % node
               elementNodes(3) % node => newNodePtr2 % node
               elementNodes(4) % node => nodes(4) % node

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newElementsList)
!
!              ----------
!              Right side
!              ----------
!
               elementNodes(1) % node => newNodePtr1 % node
               elementNodes(2) % node => nodes(2) % node
               elementNodes(3) % node => nodes(3) % node
               elementNodes(4) % node => newNodePtr2 % node

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newElementsList)

            CASE DEFAULT
         END SELECT

      END SUBROUTINE SplitElementIntoTwo
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SplitElementIntoThree( localNodeNumber, e, newElementsList, mesh )
         USE, INTRINSIC :: iso_fortran_env, only : stderr => ERROR_UNIT
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMmesh)      , POINTER :: mesh
         TYPE (SMElement)   , POINTER :: e
         CLASS(FTLinkedList), POINTER :: newElementsList
         INTEGER                      :: localNodeNumber
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE(SMNodePtr), DIMENSION(4) :: elementNodes, nodes
         TYPE (SMNode)   , POINTER     :: nodeP => NULL(), nodeM => NULL(), nodeC => NULL()
         TYPE (SMEdge)   , POINTER     :: edge  => NULL()
         TYPE (SMNode)   , POINTER     :: node  => NULL()
         CLASS(FTObject) , POINTER     :: obj   => NULL()

         INTEGER                       :: id, k
         INTEGER                       :: sideP, sideM
         REAL(KIND=RP)                 :: x(3)
         INTEGER, EXTERNAL             :: Loop
!
!        ----------------------------------------------------
!        Weak reference the four corner nodes for convenience
!        ----------------------------------------------------
!
         DO k = 1,4
            nodes(k) % node => e % nodes(k) % node
         END DO
!
!        -----------------------------------------------------------
!        Find the edges on either side. These edges will be split in
!        half. Create two new nodes at the centers of these edges.
!        Since the new nodes will be shared, we store a pointer to
!        them on the edge. If the pointer is already set, it means
!        that we don't have to compute it again.
!        -----------------------------------------------------------
!
         id    = e % id
         sideP =  localNodeNumber
         edge  => edgesForElements(sideP,id) % edge

         IF ( .NOT.ASSOCIATED(edge) )     THEN
            WRITE(stderr,*)  "Edge not associated for element ",  id, " and side ", sideP!TODO post exception
            ERROR STOP "Edge not associated. See stderr"
         END IF

         IF ( ASSOCIATED(edge % auxiliaryNode) )     THEN
            nodeP =>  edge % auxiliaryNode
         ELSE
            x     =  0.5_RP*(edge % nodes(1) % node % x + edge % nodes(2) % node % x)
            CALL constructNewNode(mesh,x,edge,node)
            nodeP => node
         END IF

         sideM =  Loop(localNodeNumber-1,4)
         edge  => edgesForElements(sideM,id) % edge

         IF ( .NOT.ASSOCIATED(edge) )     THEN
            WRITE(stderr,*)  "Edge not associated for element ",  id, " and side ", sideP
            ERROR STOP "Edge not associated. See stderr"
         END IF

         IF ( ASSOCIATED(edge % auxiliaryNode) )     THEN
            nodeM =>  edge % auxiliaryNode
         ELSE
            x     =  0.5_RP*(edge % nodes(1) % node % x + edge % nodes(2) % node % x)
            CALL constructNewNode(mesh,x,edge,node)
            nodeM => node
         END IF
!
!        -----------------------------------------------------------------
!        Create the center node at the average of the corners. Unlike the
!        area centroid, this stays well defined when the element is nearly
!        folded after its nodes have been moved onto the interface, and it
!        lies close to the interface nodes so that the new elements at a
!        reflex corner are not folded.
!        -----------------------------------------------------------------
!
         x = 0.0_RP
         DO k = 1, 4
            x = x + 0.25_RP*nodes(k) % node % x
         END DO
         ALLOCATE(node)
         CALL node % initWithLocationAndID(x, mesh % newNodeID())
         obj   => node
         nodeC => node
         CALL mesh % nodes % add(obj)
         CALL releaseSMNode(node)
!
!        ------------------------------------------------------------------
!        Construct three new elements depending on what the orientation is.
!        This is a template operation.
!        ------------------------------------------------------------------
!
         SELECT CASE ( localNodeNumber )

!           ---------
            CASE( 1 )
!           ---------

               elementNodes(1) % node => nodes(1) % node
               elementNodes(2) % node => nodeP
               elementNodes(3) % node => nodeC
               elementNodes(4) % node => nodeM

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newelementsList)

               elementNodes(1) % node => nodeP
               elementNodes(2) % node => nodes(2) % node
               elementNodes(3) % node => nodes(3) % node
               elementNodes(4) % node => nodeC

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newelementsList)

               elementNodes(1) % node => nodeM
               elementNodes(2) % node => nodeC
               elementNodes(3) % node => nodes(3) % node
               elementNodes(4) % node => nodes(4) % node

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newelementsList)

!           ---------
            CASE( 2 )
!           ---------

               elementNodes(1) % node => nodes(1) % node
               elementNodes(2) % node => nodeM
               elementNodes(3) % node => nodeC
               elementNodes(4) % node => nodes(4) % node

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newelementsList)

               elementNodes(1) % node => nodeM
               elementNodes(2) % node => nodes(2) % node
               elementNodes(3) % node => nodeP
               elementNodes(4) % node => nodeC

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newelementsList)

               elementNodes(1) % node => nodeC
               elementNodes(2) % node => nodeP
               elementNodes(3) % node => nodes(3) % node
               elementNodes(4) % node => nodes(4) % node

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newelementsList)

!           ---------
            CASE( 3 )
!           ---------

               elementNodes(1) % node => nodes(1) % node
               elementNodes(2) % node => nodes(2) % node
               elementNodes(3) % node =>  nodeM
               elementNodes(4) % node =>  nodeC

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newelementsList)

               elementNodes(1) % node =>  nodeC
               elementNodes(2) % node =>  nodeM
               elementNodes(3) % node =>  nodes(3) % node
               elementNodes(4) % node =>  nodeP

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newelementsList)

               elementNodes(1) % node => nodes(1) % node
               elementNodes(2) % node => nodeC
               elementNodes(3) % node => nodeP
               elementNodes(4) % node => nodes(4) % node

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newelementsList)

!           ---------
            CASE( 4 )
!           ---------

               elementNodes(1) % node => nodes(1) % node
               elementNodes(2) % node => nodes(2) % node
               elementNodes(3) % node => nodeC
               elementNodes(4) % node => nodeP

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newelementsList)

               elementNodes(1) % node => nodeC
               elementNodes(2) % node => nodes(2) % node
               elementNodes(3) % node => nodes(3) % node
               elementNodes(4) % node => nodeM

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newelementsList)

               elementNodes(1) % node => nodeP
               elementNodes(2) % node => nodeC
               elementNodes(3) % node => nodeM
               elementNodes(4) % node => nodes(4) % node

               CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newelementsList)

            CASE DEFAULT
         END SELECT

      END SUBROUTINE SplitElementIntoThree
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SplitElementIntoFour( e, newElementsList, mesh )
         IMPLICIT NONE
!
!        ------------------------------------------------------------
!        Split an element whose two interface nodes are at opposite
!        corners. All four edges are split at their midpoints and a
!        node is added at the centroid:
!
!              4 ---- m3 ---- 3
!              |      |       |
!              m4 --- C ----- m2
!              |      |       |
!              1 ---- m1 ---- 2
!        ------------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh)      , POINTER :: mesh
         TYPE (SMElement)   , POINTER :: e
         CLASS(FTLinkedList), POINTER :: newElementsList
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE(SMNodePtr), DIMENSION(4) :: elementNodes, nodes, midNodes
         TYPE (SMEdge)   , POINTER     :: edge  => NULL()
         TYPE (SMNode)   , POINTER     :: node  => NULL(), nodeC => NULL()
         CLASS(FTObject) , POINTER     :: obj   => NULL()
         INTEGER                       :: id, k
         REAL(KIND=RP)                 :: x(3), corners(3,4)

         id = e % id
         DO k = 1,4
            nodes(k) % node => e % nodes(k) % node
            corners(:,k)    =  nodes(k) % node % x
         END DO
!
!        ---------------------------------------------
!        Midpoint nodes, shared with neighbor elements
!        ---------------------------------------------
!
         DO k = 1, 4
            edge => edgesForElements(k,id) % edge
            IF ( ASSOCIATED(edge % auxiliaryNode) )     THEN
               midNodes(k) % node => edge % auxiliaryNode
            ELSE
               x = 0.5_RP*(edge % nodes(1) % node % x + edge % nodes(2) % node % x)
               CALL constructNewNode(mesh, x, edge, node)
               midNodes(k) % node => node
            END IF
         END DO
!
!        --------------------------------------------
!        Create a node at the centroid of the element
!        --------------------------------------------
!
         CALL ComputeCentroid(corners, x)
         ALLOCATE(node)
         CALL node % initWithLocationAndID(x, mesh % newNodeID())
         obj   => node
         nodeC => node
         CALL mesh % nodes % add(obj)
         CALL releaseSMNode(node)
!
!        -----------------------------------------------
!        The four new elements. Edges 1, 2, 3, 4 are the
!        bottom, right, top, and left sides
!        -----------------------------------------------
!
         elementNodes(1) % node => nodes(1) % node
         elementNodes(2) % node => midNodes(1) % node
         elementNodes(3) % node => nodeC
         elementNodes(4) % node => midNodes(4) % node
         CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newElementsList)

         elementNodes(1) % node => midNodes(1) % node
         elementNodes(2) % node => nodes(2) % node
         elementNodes(3) % node => midNodes(2) % node
         elementNodes(4) % node => nodeC
         CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newElementsList)

         elementNodes(1) % node => nodeC
         elementNodes(2) % node => midNodes(2) % node
         elementNodes(3) % node => nodes(3) % node
         elementNodes(4) % node => midNodes(3) % node
         CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newElementsList)

         elementNodes(1) % node => midNodes(4) % node
         elementNodes(2) % node => nodeC
         elementNodes(3) % node => midNodes(3) % node
         elementNodes(4) % node => nodes(4) % node
         CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newElementsList)

      END SUBROUTINE SplitElementIntoFour
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION IsInterfaceNode( node )
         IMPLICIT NONE
!
!        ------------------------------------------------------
!        True if the node has been moved onto an interface curve
!        (and not onto a physical boundary curve)
!        ------------------------------------------------------
!
         TYPE (SMNode), POINTER :: node
         INTEGER                :: chainID

         IsInterfaceNode = .FALSE.
         IF ( node % distToBoundary /= 0.0_RP )     RETURN

         chainID = node % bCurveChainID
         IF ( chainID < 1 .OR. chainID > SIZE(boundaryEdgesType) )     RETURN
         IsInterfaceNode = boundaryEdgesType(chainID) == INTERFACE_EDGES

      END FUNCTION IsInterfaceNode
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CreateAndAddElement(elementID, e, elementNodes, newelementsList)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(SMNodePtr), DIMENSION(4)           :: elementNodes
         TYPE (SMElement)              , POINTER :: e
         CLASS(FTLinkedList)           , POINTER :: newElementsList
         INTEGER                                 :: elementID
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (SMElement), POINTER :: eNew => NULL()
         CLASS(FTObject) , POINTER :: obj  => NULL()

         ALLOCATE(eNew)
         CALL eNew % initWithNodesIDAndType(elementNodes, elementID, QUAD)
         eNew % materialID   =  e % materialID

         obj => eNew
         CALL newElementsList % add(obj)
         CALL releaseSMElement(eNew)

      END SUBROUTINE CreateAndAddElement
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE constructNewNode(mesh,x,edge,node)
         IMPLICIT NONE
         TYPE (SMMesh)  , POINTER :: mesh
         TYPE (SMEdge)  , POINTER :: edge
         TYPE (SMNode)  , POINTER :: node
         CLASS(FTObject), POINTER :: obj => NULL()
         REAL(KIND=RP)            :: x(3)

         ALLOCATE(node)
         CALL node % initWithLocationAndID(x, mesh % newNodeID())

         obj => node
         CALL mesh % nodes % add(obj)
         CALL setAuxiliaryNode( edge, node )
         CALL releaseSMNode(node)

      END SUBROUTINE constructNewNode

      END Module InterfaceElementMethods

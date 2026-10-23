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
!Templates that split the elements next to an interface curve after its
!nodes have been moved onto it, so that the mesh stays conforming. The
!complete interface meshing procedure, and where these templates fit
!into it, is described in Documentation/docs/interface-meshing.md.
!
!////////////////////////////////////////////////////////////////////////
!
      Module InterfaceElementMethods
      USE MeshProjectClass
      IMPLICIT NONE
!
!     An element with three interface nodes whose middle node is pinned at a
!     joint of the interface curve and has an angle smaller than this (in
!     degrees) is at a corner of the curve, see SplitCornerElementIntoTwo
!
      REAL(KIND=RP), PARAMETER, PRIVATE :: MAX_CORNER_ANGLE = 135.0_RP
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
               IF ( IsOnInterfaceCurve(node) )     THEN
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
!                 --------------------------------------------------------------
!                 Split into three elements, unless the middle interface node is
!                 pinned at a corner of the interface curve. Then split into two
!                 so that the corner angle is not cut in half.
!                 --------------------------------------------------------------
!
                  IF ( e % nodes(Loop(oppositeNodeNumber+2,4)) % node % nodeType /= ROW_SIDE .AND. &
                       InterfaceCornerAngle( e, oppositeNodeNumber ) < MAX_CORNER_ANGLE )     THEN
                     CALL SplitCornerElementIntoTwo( oppositeNodeNumber, e, newElementsList, mesh )
                  ELSE
                     CALL SplitElementIntoThree( oppositeNodeNumber, e, newElementsList, mesh )
                  END IF
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
         TYPE (SMNode)  , POINTER      :: swapNodePtr => NULL()

         INTEGER                       :: k
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
!        ------------------------------------------------------
!        The edges on either side are split in half at their
!        (shared) midpoint nodes. These are weak references.
!        ------------------------------------------------------
!
         CALL GetEdgeMidpointNode( mesh, e, Loop(side+1,4), newNodePtr1 % node )
         CALL GetEdgeMidpointNode( mesh, e, Loop(side-1,4), newNodePtr2 % node )
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
         TYPE (SMNode)  , POINTER      :: nodeP => NULL(), nodeM => NULL(), nodeC => NULL()
         TYPE (SMNode)  , POINTER      :: node  => NULL()
         CLASS(FTObject), POINTER      :: obj   => NULL()

         INTEGER                       :: k
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
!        ------------------------------------------------------
!        The two edges at the split corner are split in half at
!        their (shared) midpoint nodes
!        ------------------------------------------------------
!
         CALL GetEdgeMidpointNode( mesh, e, localNodeNumber          , nodeP )
         CALL GetEdgeMidpointNode( mesh, e, Loop(localNodeNumber-1,4), nodeM )
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
         TYPE (SMNode)  , POINTER      :: node  => NULL(), nodeC => NULL()
         CLASS(FTObject), POINTER      :: obj   => NULL()
         INTEGER                       :: k
         REAL(KIND=RP)                 :: x(3), corners(3,4)

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
            CALL GetEdgeMidpointNode( mesh, e, k, midNodes(k) % node )
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
      REAL(KIND=RP) FUNCTION InterfaceCornerAngle( e, freeNode )
         IMPLICIT NONE
!
!        -----------------------------------------------------------
!        Interior angle (in degrees) of the element at the interface
!        node opposite to freeNode, i.e. the middle one of the three
!        interface nodes
!        -----------------------------------------------------------
!
         TYPE (SMElement), POINTER :: e
         INTEGER                   :: freeNode
         INTEGER, EXTERNAL         :: Loop
         REAL(KIND=RP)             :: a(2), b(2), c(2)

         a = e % nodes(Loop(freeNode+1,4)) % node % x(1:2)
         b = e % nodes(Loop(freeNode+2,4)) % node % x(1:2)
         c = e % nodes(Loop(freeNode+3,4)) % node % x(1:2)
         InterfaceCornerAngle = AngleBetween( a - b, c - b )

      END FUNCTION InterfaceCornerAngle
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SplitCornerElementIntoTwo( freeNode, e, newElementsList, mesh )
         IMPLICIT NONE
!
!        -----------------------------------------------------------------
!        Split an element with three interface nodes that turn a sharp
!        corner. The two edges at freeNode are split at their midpoints,
!        P (towards freeNode+1) and M (towards freeNode-1), as for the
!        three-way split, so the neighbors still conform. The resulting
!        hexagon (f, P, f+1, f+2, f+3, M) is cut into two quads by the
!        diagonal P-(f+3) or M-(f+1), whichever gives the better elements.
!        The corner node f+2 then belongs to one element only, keeping its
!        full angle.
!        -----------------------------------------------------------------
!
         TYPE (SMMesh)      , POINTER :: mesh
         TYPE (SMElement)   , POINTER :: e
         CLASS(FTLinkedList), POINTER :: newElementsList
         INTEGER                      :: freeNode

         TYPE(SMNodePtr), DIMENSION(4) :: elementNodes, quadA1, quadA2, quadB1, quadB2
         TYPE (SMNode)  , POINTER      :: nodeP => NULL(), nodeM => NULL()
         INTEGER                       :: f, f1, f2, f3
         INTEGER, EXTERNAL             :: Loop

         f  = freeNode
         f1 = Loop(f+1,4); f2 = Loop(f+2,4); f3 = Loop(f+3,4)
!
!        -----------------------------------------------------
!        Midpoints of the two edges at the free node,
!        shared with the neighbors (see SplitElementIntoThree)
!        -----------------------------------------------------
!
         CALL GetEdgeMidpointNode( mesh, e, f          , nodeP )
         CALL GetEdgeMidpointNode( mesh, e, Loop(f-1,4), nodeM )
!
!        --------------------------
!        Two candidate subdivisions
!        --------------------------
!
         quadA1(1) % node => nodeP; quadA1(2) % node => e % nodes(f1) % node
         quadA1(3) % node => e % nodes(f2) % node; quadA1(4) % node => e % nodes(f3) % node
         quadA2(1) % node => e % nodes(f) % node; quadA2(2) % node => nodeP
         quadA2(3) % node => e % nodes(f3) % node; quadA2(4) % node => nodeM

         quadB1(1) % node => e % nodes(f1) % node; quadB1(2) % node => e % nodes(f2) % node
         quadB1(3) % node => e % nodes(f3) % node; quadB1(4) % node => nodeM
         quadB2(1) % node => e % nodes(f) % node; quadB2(2) % node => nodeP
         quadB2(3) % node => e % nodes(f1) % node; quadB2(4) % node => nodeM

         IF ( MIN(QuadMinScaledJacobian(quadA1), QuadMinScaledJacobian(quadA2)) >= &
              MIN(QuadMinScaledJacobian(quadB1), QuadMinScaledJacobian(quadB2)) )     THEN
            elementNodes = quadA1
            CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newElementsList)
            elementNodes = quadA2
            CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newElementsList)
         ELSE
            elementNodes = quadB1
            CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newElementsList)
            elementNodes = quadB2
            CALL CreateAndAddElement( mesh % newElementID(), e, elementNodes, newElementsList)
         END IF

      END SUBROUTINE SplitCornerElementIntoTwo
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION QuadMinScaledJacobian( q )
         IMPLICIT NONE
!
!        -------------------------------------------------------
!        Smallest sine of the corner angles (negative if folded)
!        -------------------------------------------------------
!
         TYPE(SMNodePtr), DIMENSION(4) :: q
         INTEGER                       :: k
         INTEGER, EXTERNAL             :: Loop
         REAL(KIND=RP)                 :: u(2), v(2)

         QuadMinScaledJacobian = HUGE(1.0_RP)
         DO k = 1, 4
            u = q(Loop(k+1,4)) % node % x(1:2) - q(k) % node % x(1:2)
            v = q(Loop(k-1,4)) % node % x(1:2) - q(k) % node % x(1:2)
            QuadMinScaledJacobian = MIN( QuadMinScaledJacobian,                                 &
                                         CornerJacobian( q(Loop(k-1,4)) % node % x, q(k) % node % x, &
                                                         q(Loop(k+1,4)) % node % x )/(NORM2(u)*NORM2(v)) )
         END DO

      END FUNCTION QuadMinScaledJacobian
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
      SUBROUTINE GetEdgeMidpointNode( mesh, e, side, node )
         USE, INTRINSIC :: iso_fortran_env, only : stderr => ERROR_UNIT
         IMPLICIT NONE
!
!        -------------------------------------------------------------
!        Node at the midpoint of the given side of element e. The node
!        is shared with the neighbor element, so it is stored on the
!        edge (auxiliaryNode) when it is created, and taken from there
!        if it already exists. On return, node is a weak reference.
!        -------------------------------------------------------------
!
         TYPE (SMMesh)   , POINTER :: mesh
         TYPE (SMElement), POINTER :: e
         INTEGER                   :: side
         TYPE (SMNode)   , POINTER :: node

         TYPE (SMEdge)   , POINTER :: edge => NULL()
         CLASS(FTObject) , POINTER :: obj  => NULL()
         REAL(KIND=RP)             :: x(3)

         edge => edgesForElements(side, e % id) % edge
         IF ( .NOT.ASSOCIATED(edge) )     THEN
            WRITE(stderr,*)  "Edge not associated for element ", e % id, " and side ", side !TODO post exception
            ERROR STOP "Edge not associated. See stderr"
         END IF

         IF ( ASSOCIATED(edge % auxiliaryNode) )     THEN
            node => edge % auxiliaryNode
            RETURN
         END IF

         x = 0.5_RP*(edge % nodes(1) % node % x + edge % nodes(2) % node % x)
         ALLOCATE(node)
         CALL node % initWithLocationAndID(x, mesh % newNodeID())

         obj => node
         CALL mesh % nodes % add(obj)
         CALL setAuxiliaryNode( edge, node )
         CALL releaseSMNode(node)

      END SUBROUTINE GetEdgeMidpointNode

      END Module InterfaceElementMethods

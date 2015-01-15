!
!////////////////////////////////////////////////////////////////////////
!
!      InterfaceElementMethods.f90
!      Created: May 16, 2013 11:17 AM 
!      By: David Kopriva  
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
         CLASS(SMMesh)      , POINTER :: mesh
         CLASS(FTLinkedList), POINTER :: interfaceElements
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMElement)           , POINTER :: e               => NULL()
         CLASS(FTLinkedList)        , POINTER :: newElementsList => NULL()
         CLASS(FTObject)            , POINTER :: obj             => NULL()
         CLASS(SMNode)              , POINTER :: node            => NULL()
         CLASS(FTLinkedListIterator), POINTER :: elementIterator => NULL()
         INTEGER                              :: interfaceNodeCount, k
         INTEGER                              :: boundaryNodeNumber, oppositeNodeNumber
         
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
         ALLOCATE(elementIterator)
         CALL elementIterator % initWithFTLinkedList(interfaceElements)
         CALL elementIterator % setToStart()
         DO WHILE ( .NOT.elementIterator % isAtEnd() )
            obj => elementIterator % object()
            CALL cast(obj,e)
            IF ( e % remove )     THEN
               CALL elementIterator % moveToNext()
               CYCLE !DEBUG - in case elements are doubled up
            END IF
!
!           ------------------------------
!           See what element shape this is
!           ------------------------------
!
            interfaceNodeCount = 0
            boundaryNodeNumber = 0
            oppositeNodeNumber = 0
            DO k = 1, 4
               obj => e % nodes % objectAtIndex(k)
               CALL cast(obj,node)
               IF ( node % distToBoundary == 0.0_RP )     THEN
                  interfaceNodeCount = interfaceNodeCount + 1
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
!           --------------------------------------
!           Split according to the number of nodes
!           on the interface
!           --------------------------------------
!
            SELECT CASE ( interfaceNodeCount )
               CASE( 1 )
!
!                 -------------------------
!                 Split into three elements
!                 -------------------------
!
                  CALL SplitElementIntoThree( boundaryNodeNumber, e, newElementsList, mesh )
                  e % remove = .true.
                  
               CASE( 2 ) 
!
!                 -----------------------
!                 Split into two elements
!                 -----------------------
!
                  CALL SplitElementIntoTwo( e, newElementsList, mesh )
                  e % remove = .true.
                  
               CASE( 3 ) 
!
!                 -------------------------
!                 Split into three elements
!                 -------------------------
!
                  CALL SplitElementIntoThree( oppositeNodeNumber, e, newElementsList, mesh )
                  e % remove = .true.
                  
               CASE DEFAULT 
                  PRINT *, "For some reason an interface element has no interface nodes!"
            END SELECT 
            
            CALL elementIterator % moveToNext()
         END DO
!
!        -------------------------------------------------
!        Remove deleted elements and add the newly created
!        ones to the mesh
!        -------------------------------------------------
!
         CALL mesh % elements % addObjectsFromList(newElementsList)
         CALL newElementsList % release()
         IF(newElementsList % isUnreferenced()) DEALLOCATE(newElementsList)
         CALL elementIterator % release()
         IF(elementIterator % isUnreferenced()) DEALLOCATE(elementIterator)

         CALL DoLazyDelete( mesh )
         CALL mesh % renumberAllLists()
         CALL mesh % syncEdges()
         CALL deallocateElementToEdgeConnections()

      END SUBROUTINE SplitInterfaceElements
!
!//////////////////////////////////////////////////////////////////////// 
!
      SUBROUTINE SplitElementIntoTwo( e, newElementsList, mesh )  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMMesh)      , POINTER :: mesh
         CLASS(SMElement)   , POINTER :: e
         CLASS(FtLinkedList), POINTER :: newElementsList
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE(SMNodePtr), DIMENSION(4) :: elementNodes, nodes
         TYPE(SMNodePtr)               :: newNodePtr1, newNodePtr2
         CLASS(SMEdge)   , POINTER     :: edge => NULL()
         CLASS(SMNode)   , POINTER     :: swapNodePtr => NULL(), node => NULL()
         CLASS(FTObject), POINTER      :: obj => NULL()
         
         INTEGER                       :: side, id, k
         INTEGER                       :: sideP, sideM
         REAL(KIND=RP)                 :: x(3)
         INTEGER, EXTERNAL             :: Loop
!
!        -----------------------
!        Find the interface edge
!        -----------------------
!
         id   = e%id
         side = 0
         DO k = 1, 4
            edge => edgesForElements(k,id)%edge 
            IF ( edge%edgeType == ON_INTERFACE )     THEN
               side = k
               EXIT  
            END IF 
         END DO
         IF(side == 0)     RETURN
!
!        ----------------------------------------------------
!        Weak reference the four corner nodes for convenience
!        ----------------------------------------------------
!
         DO k = 1,4
            obj => e % nodes % objectAtIndex(k)
            CALL cast(obj,node)
            nodes(k) % node => node
         END DO  
!
!        ----------------------------------------------------------
!        Find the edges on either side These edges will be split in
!        half. Create two new nodes at the centers of these edges.
!        Since the new nodes will be shared, we store a pointer to
!        them on the edge. If the pointer is already set, it means
!        that we don't have to compute it again.
!        ----------------------------------------------------------
!
         sideP =  Loop(side+1,4)
         edge  => edgesForElements(sideP,id) % edge
         IF ( ASSOCIATED(edge % auxiliaryNode) )     THEN
            newNodePtr1 % node =>  edge % auxiliaryNode
         ELSE 
            x     =  0.5_RP*(edge % nodes(1) % node % x + edge % nodes(2) % node % x)
            CALL constructNewNode(mesh,x,edge,node)
            newNodePtr1 % node => node
         END IF 
        
         sideM =  Loop(side-1,4)
         edge  => edgesForElements(sideM,id) % edge
         IF ( ASSOCIATED(edge % auxiliaryNode) )     THEN
            newNodePtr2 % node =>  edge % auxiliaryNode
         ELSE 
            x     =  0.5_RP*(edge % nodes(1) % node % x + edge % nodes(2) % node % x)
            CALL constructNewNode(mesh,x,edge,node)
            newNodePtr2 % node => node
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
               elementNodes(1) = newNodePtr1 
               elementNodes(2) = newNodePtr2 
               elementNodes(3) % node => nodes(3) % node
               elementNodes(4) % node => nodes(4) % node
               
               CALL constructNewElement(mesh,elementNodes,newElementsList,e)
!
!              -----------
!              Bottom side
!              -----------
!
               elementNodes(3) % node => newNodePtr2 % node
               elementNodes(1) % node => nodes(1) % node
               elementNodes(2) % node => nodes(2) % node
               elementNodes(4) % node => newNodePtr1 % node
               
               CALL constructNewElement(mesh,elementNodes,newElementsList,e)
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
               
               CALL constructNewElement(mesh,elementNodes,newElementsList,e)
!
!              ----------
!              Right side
!              ----------
!
               elementNodes(1) % node => newNodePtr1 % node
               elementNodes(2) % node => nodes(2) % node
               elementNodes(3) % node => nodes(3) % node
               elementNodes(4) % node => newNodePtr2 % node
               
               CALL constructNewElement(mesh,elementNodes,newElementsList,e)
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
         CLASS(SMmesh)      , POINTER :: mesh
         CLASS(SMElement)   , POINTER :: e
         CLASS(FTLinkedList), POINTER :: newElementsList
         INTEGER                      :: localNodeNumber
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE(SMNodePtr), DIMENSION(4) :: elementNodes, nodes
         CLASS(SMNode)   , POINTER     :: nodeP => NULL(), nodeM => NULL(), nodeC => NULL()
         CLASS(SMEdge)   , POINTER     :: edge  => NULL()
         CLASS(SMNode)   , POINTER     :: node  => NULL()
         CLASS(FTObject) , POINTER     :: obj   => NULL()
         
         INTEGER                       :: id, k
         INTEGER                       :: sideP, sideM
         REAL(KIND=RP)                 :: x(3), corners(3,4)
         INTEGER, EXTERNAL             :: Loop
!
!        ----------------------------------------------------
!        Weak reference the four corner nodes for convenience
!        ----------------------------------------------------
!
         DO k = 1,4
            obj => e % nodes % objectAtIndex(k)
            CALL cast(obj,node)
            nodes(k) % node => node
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
            PRINT *, "Edge not associated for element ",  id, " and side ", sideP!TODO post exception
            STOP
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
            PRINT *, "Edge not associated for element ",  id, " and side ", sideP
            STOP
         END IF 
         
         IF ( ASSOCIATED(edge % auxiliaryNode) )     THEN
            nodeM =>  edge % auxiliaryNode
         ELSE 
            x     =  0.5_RP*(edge % nodes(1) % node % x + edge % nodes(2) % node % x)
            CALL constructNewNode(mesh,x,edge,node)
            nodeM => node
         END IF
!
!        ------------------------------------------
!        Create node at the centroid of the element
!        ------------------------------------------
!
         DO k = 1,4
            corners(:,k) = nodes(k) % node % x 
         END DO  
         CALL ComputeCentroid(corners,x)
         ALLOCATE(node)
         CALL node % initWithLocationAndID(x, mesh % newNodeID())
         obj   => node
         nodeC => node
         CALL mesh % nodes % add(obj)
         CALL node % release()
!
!        ------------------------------------------------------------------
!        Construct three new elements depending on what the orientation is.
!        This is a template operation.
!        ------------------------------------------------------------------
!
         SELECT CASE ( localNodeNumber )
            CASE( 1 ) 
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
              
            CASE( 2 ) 
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
              
            CASE( 3 ) 
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
              
            CASE( 4 ) 
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
      SUBROUTINE CreateAndAddElement(elementID, e, elementNodes, newelementsList)  
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(SMNodePtr), DIMENSION(4)           :: elementNodes
         CLASS(SMElement)              , POINTER :: e
         CLASS(FTLinkedList)           , POINTER :: newElementsList
         INTEGER                                 :: elementID
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMElement), POINTER :: eNew => NULL()
         CLASS(FTObject) , POINTER :: obj  => NULL()
         
         ALLOCATE(eNew)
         CALL eNew % initWithNodesIDAndType(elementNodes, elementID, QUAD)
         eNew % materialID   =  e % materialID
         eNew % materialName =  e % materialName
         
         obj => eNew
         CALL newElementsList % add(obj)
         CALL eNew % release()
           
      END SUBROUTINE CreateAndAddElement
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE constructNewElement(mesh,elementNodes,newElementsList,e)
         IMPLICIT NONE  
         CLASS(SMMesh)      , POINTER :: mesh
         CLASS(SMElement)   , POINTER :: e, eNew => NULL()
         CLASS(FtLinkedList), POINTER :: newElementsList
         TYPE(SMNodePtr)              :: elementNodes(4)
         CLASS(FTObject)    , POINTER :: obj => NULL()
         
         ALLOCATE(eNew)
         CALL eNew % initWithNodesIDandType(elementNodes, mesh % newElementID(), QUAD)
         eNew % materialID   =  e % materialID
         eNew % materialName =  e % materialName
         
         obj => eNew
         CALL newElementsList % add(obj)
         CALL eNew % release()
         
      END SUBROUTINE constructNewElement
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE constructNewNode(mesh,x,edge,node)
         IMPLICIT NONE  
         CLASS(SMMesh)  , POINTER :: mesh
         CLASS(SMEdge)  , POINTER :: edge
         CLASS(SMNode)  , POINTER :: node
         CLASS(FTObject), POINTER :: obj => NULL()
         REAL(KIND=RP)            :: x(3)
         
         ALLOCATE(node)
         CALL node % initWithLocationAndID(x, mesh % newNodeID())
         
         obj => node
         CALL mesh % nodes % add(obj)
         CALL setAuxiliaryNode( edge, node )
         CALL node % release()
         
      END SUBROUTINE constructNewNode
      
      END Module InterfaceElementMethods

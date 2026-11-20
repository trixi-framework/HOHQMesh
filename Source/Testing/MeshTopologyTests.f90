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
!      MeshTopologyTests.f90
!      Created: 2026-11-19
!      Unit tests of the mesh cleanup and interface splitting routines
!      on small meshes built by hand, each with exactly the defect the
!      routine is meant to fix:
!        1. RemoveDoubletNodes merges the two elements around an
!           interior node of valence two
!        2. CleanUpChevronElements removes a near 180 degree corner
!        3. UntangleInvertedElements unfolds elements around a node
!           that was moved out of its star
!        4. SplitInterfaceElements splits an element whose interface
!           nodes are at opposite corners into four, and keeps the
!           mesh conforming
!      All coordinates are exact binary fractions.
!////////////////////////////////////////////////////////////////////////
      Module MeshTopologyTestsModule
      USE SMConstants
      USE FTAssertions
      USE FTObjectClass
      USE FTLinkedListClass
      USE FTMutableObjectArrayClass
      USE SMMeshObjectsModule
      USE SMMeshClass
      USE ConnectionsModule
      USE MeshQualityAnalysisClass
      USE MeshCleaner
      USE InterfaceElementMethods, ONLY : SplitInterfaceElements
      USE ProgramGlobals         , ONLY : printMessage, QUAD, A_OK_ERROR_CODE, UNDEFINED
      IMPLICIT NONE

      REAL(KIND=RP), PARAMETER, PRIVATE :: AREA_TOLERANCE = 1.0E-12_RP
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE BuildMesh( mesh, xy, connectivity, nodes )
!
!     --------------------------------------------------------------
!     A mesh with nodes at the columns of xy and quad elements given
!     by the columns of connectivity. Node k has id k. The nodes are
!     returned (as weak references) so that tests can mark them.
!     --------------------------------------------------------------
!
         IMPLICIT NONE
         TYPE (SMMesh)  , POINTER                    :: mesh
         REAL(KIND=RP)                               :: xy(:,:)
         INTEGER                                     :: connectivity(:,:)
         TYPE(SMNodePtr), DIMENSION(:), ALLOCATABLE  :: nodes

         TYPE (SMNode)   , POINTER :: node
         TYPE (SMElement), POINTER :: e
         CLASS(FTObject) , POINTER :: obj
         TYPE(SMNodePtr)           :: quadNodes(4)
         INTEGER                   :: k, j

         ALLOCATE(mesh)
         CALL mesh % init()

         IF ( ALLOCATED(nodes) )     DEALLOCATE(nodes)
         ALLOCATE(nodes(SIZE(xy,2)))
         DO k = 1, SIZE(xy,2)
            ALLOCATE(node)
            CALL node % initWithLocationAndID([xy(1,k), xy(2,k), 0.0_RP], mesh % newNodeID())
            obj => node
            CALL mesh % nodes % add(obj)
            nodes(k) % node => node
            CALL releaseSMNode(node)
         END DO

         DO k = 1, SIZE(connectivity,2)
            DO j = 1, 4
               quadNodes(j) % node => nodes(connectivity(j,k)) % node
            END DO
            ALLOCATE(e)
            CALL e % initWithNodesIDAndType(quadNodes, mesh % newElementID(), QUAD)
            obj => e
            CALL mesh % elements % add(obj)
            CALL releaseSMElement(e)
         END DO

      END SUBROUTINE BuildMesh
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION elementSignedArea( e )
         IMPLICIT NONE
         TYPE (SMElement), POINTER :: e
         INTEGER                   :: k, kp
         elementSignedArea = 0.0_RP
         DO k = 1, 4
            kp = MOD(k,4) + 1
            elementSignedArea = elementSignedArea + e % nodes(k) % node % x(1)*e % nodes(kp) % node % x(2) &
                                                  - e % nodes(kp) % node % x(1)*e % nodes(k) % node % x(2)
         END DO
         elementSignedArea = 0.5_RP*elementSignedArea
      END FUNCTION elementSignedArea
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MeshAreaSummary( mesh, totalArea, minArea, numberOfElements )
!
!     -----------------------------------------------------
!     Sum and minimum of the signed element areas. A mesh
!     that covers its domain once with right handed
!     elements has minArea > 0 and the area of the domain.
!     -----------------------------------------------------
!
         IMPLICIT NONE
         TYPE (SMMesh), POINTER :: mesh
         REAL(KIND=RP)          :: totalArea, minArea
         INTEGER                :: numberOfElements

         CLASS(FTObject) , POINTER :: obj
         TYPE (SMElement), POINTER :: e
         REAL(KIND=RP)             :: a

         totalArea        = 0.0_RP
         minArea          = HUGE(1.0_RP)
         numberOfElements = 0
         CALL mesh % elementsIterator % setToStart()
         DO WHILE ( .NOT.mesh % elementsIterator % isAtEnd() )
            obj => mesh % elementsIterator % object()
            CALL castToSMElement(obj, e)
            IF ( .NOT.e % remove )     THEN
               a                = elementSignedArea(e)
               totalArea        = totalArea + a
               minArea          = MIN(minArea, a)
               numberOfElements = numberOfElements + 1
            END IF
            CALL mesh % elementsIterator % moveToNext()
         END DO

      END SUBROUTINE MeshAreaSummary
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MarkAsBoundaryNode( node )
         IMPLICIT NONE
         TYPE (SMNode), POINTER :: node
         node % bCurveID       = 1
         node % bCurveChainID  = 1
         node % distToBoundary = 0.0_RP
      END SUBROUTINE MarkAsBoundaryNode
!
!////////////////////////////////////////////////////////////////////////
!
!     --------------------------------------------------------------
!     The doublet: interior node v (5) is only in two elements that
!     share the edges v-a and v-b. Together they cover the square
!     a, p, b, q with diagonals of length 2, area 2:
!
!                    q (0,1)
!                  /    \
!          a (-1,0) - v - b (1,0)      v = (0, h)
!                  \    /
!                    p (0,-1)
!     --------------------------------------------------------------
!
      SUBROUTINE BuildDoubletMesh( mesh, nodes, h )
         IMPLICIT NONE
         TYPE (SMMesh)  , POINTER                   :: mesh
         TYPE(SMNodePtr), DIMENSION(:), ALLOCATABLE :: nodes
         REAL(KIND=RP)                              :: h
         REAL(KIND=RP) :: xy(2,5)
         INTEGER       :: connectivity(4,2), k

         xy(:,1) = [-1.0_RP,  0.0_RP] ! a
         xy(:,2) = [ 0.0_RP, -1.0_RP] ! p
         xy(:,3) = [ 1.0_RP,  0.0_RP] ! b
         xy(:,4) = [ 0.0_RP,  1.0_RP] ! q
         xy(:,5) = [ 0.0_RP,  h     ] ! v
         connectivity(:,1) = [5, 1, 2, 3] ! (v,a,p,b)
         connectivity(:,2) = [5, 3, 4, 1] ! (v,b,q,a)
         CALL BuildMesh(mesh, xy, connectivity, nodes)
         DO k = 1, 4
            CALL MarkAsBoundaryNode(nodes(k) % node)
         END DO

      END SUBROUTINE BuildDoubletMesh
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckMergedDoublet( mesh, label )
!
!     ---------------------------------------------------------------
!     After the merge there is one right handed element a, p, b, q in
!     cyclic order, and v is no longer in the mesh
!     ---------------------------------------------------------------
!
         IMPLICIT NONE
         TYPE (SMMesh)  , POINTER :: mesh
         CHARACTER(LEN=*)         :: label

         CLASS(FTObject) , POINTER :: obj
         TYPE (SMElement), POINTER :: e
         REAL(KIND=RP)             :: totalArea, minArea
         INTEGER                   :: numberOfElements, k, first, j
         LOGICAL                   :: cyclicOrder

         CALL MeshAreaSummary(mesh, totalArea, minArea, numberOfElements)
         CALL FTAssertEqual(expectedValue = 1, actualValue = numberOfElements, msg = label // ": one element is left")
         CALL FTAssertEqual(expectedValue = 4, actualValue = mesh % nodes % COUNT(), msg = label // ": the node v is removed")
         CALL FTAssert(test = ABS(totalArea - 2.0_RP) <= AREA_TOLERANCE .AND. minArea > 0.0_RP, &
                       msg  = label // ": the element is right handed and covers the doublet")

         IF ( numberOfElements /= 1 )     RETURN
         CALL mesh % elementsIterator % setToStart()
         obj => mesh % elementsIterator % object()
         CALL castToSMElement(obj, e)
!
!        ---------------------------------------------
!        a, p, b, q (ids 1, 2, 3, 4) in cyclic order
!        ---------------------------------------------
!
         first = 0
         DO k = 1, 4
            IF ( e % nodes(k) % node % id == 1 )     first = k
         END DO
         cyclicOrder = first > 0
         IF ( cyclicOrder )     THEN
            DO j = 0, 3
               cyclicOrder = cyclicOrder .AND. e % nodes(MOD(first - 1 + j, 4) + 1) % node % id == j + 1
            END DO
         END IF
         CALL FTAssert(test = cyclicOrder, msg = label // ": the element is (a, p, b, q)")

      END SUBROUTINE CheckMergedDoublet
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DoubletRemovalTest(optData)
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)
         TYPE (SMMesh)  , POINTER                   :: mesh
         TYPE(SMNodePtr), DIMENSION(:), ALLOCATABLE :: nodes
         INTEGER                                    :: errorCode
         REAL(KIND=RP)                              :: totalArea, minArea
         INTEGER                                    :: numberOfElements

         errorCode = A_OK_ERROR_CODE
         CALL BuildDoubletMesh(mesh, nodes, 0.25_RP)
         CALL RemoveDoubletNodes(mesh, errorCode)
         CALL FTAssertEqual(expectedValue = A_OK_ERROR_CODE, actualValue = errorCode, msg = "RemoveDoubletNodes error code")
         CALL CheckMergedDoublet(mesh, "Doublet removal")
         CALL releaseMesh(mesh)
!
!        -------------------------------------------------
!        A 2 x 2 block has a valence four center node and
!        must not change
!        -------------------------------------------------
!
         CALL BuildBlockMesh(mesh, nodes, 2)
         CALL RemoveDoubletNodes(mesh, errorCode)
         CALL MeshAreaSummary(mesh, totalArea, minArea, numberOfElements)
         CALL FTAssertEqual(expectedValue = 4, actualValue = numberOfElements, msg = "No doublet: elements are kept")
         CALL FTAssertEqual(expectedValue = 9, actualValue = mesh % nodes % COUNT(), msg = "No doublet: nodes are kept")
         CALL releaseMesh(mesh)

      END SUBROUTINE DoubletRemovalTest
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ChevronRemovalTest(optData)
!
!     -----------------------------------------------------------
!     With v just above the line a-b, the element (v,b,q,a) has a
!     corner of about 177 degrees at v and is a chevron
!     -----------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)
         TYPE (SMMesh)  , POINTER                   :: mesh
         TYPE(SMNodePtr), DIMENSION(:), ALLOCATABLE :: nodes
         TYPE (FTMutableObjectArray), POINTER       :: badElements
         CLASS(FTObject)            , POINTER       :: obj
         TYPE (SMElement)           , POINTER       :: e
         REAL(KIND=RP)              , ALLOCATABLE   :: shapeMeasures(:,:)
         INTEGER                                    :: errorCode, numberOfChevrons, k

         errorCode = A_OK_ERROR_CODE
         CALL BuildDoubletMesh(mesh, nodes, 0.015625_RP)

         badElements => BadElementsInMesh(mesh)
         CALL FTAssert(test = badElements % COUNT() > 0, msg = "The chevron is a bad element")
         ALLOCATE( shapeMeasures(NUMBER_OF_2D_SHAPE_MEASURES, badElements % COUNT()) )
         DO k = 1, badElements % COUNT()
            obj => badElements % objectAtIndex(k)
            CALL castToSMElement(obj, e)
            CALL ComputeElementShapeMeasures2D(e, shapeMeasures(:,k))
         END DO

         CALL MakeNodeToElementConnections(mesh, errorCode)
         CALL CleanUpChevronElements(badElements, shapeMeasures, numberOfChevrons)
         CALL deallocateNodeToElementConnections()
         CALL DoLazyDelete(mesh)

         CALL FTAssertEqual(expectedValue = 1, actualValue = numberOfChevrons, msg = "One chevron is removed")
         CALL CheckMergedDoublet(mesh, "Chevron removal")

         CALL releaseFTMutableObjectArray(badElements)
         CALL releaseMesh(mesh)

      END SUBROUTINE ChevronRemovalTest
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE BuildBlockMesh( mesh, nodes, n )
!
!     -----------------------------------------------------------------
!     An n x n block of unit squares on [0,n]^2. Node (i,j), i, j = 0..n,
!     has id i + (n+1)*j + 1. The nodes on the outside are boundary nodes.
!     -----------------------------------------------------------------
!
         IMPLICIT NONE
         TYPE (SMMesh)  , POINTER                   :: mesh
         TYPE(SMNodePtr), DIMENSION(:), ALLOCATABLE :: nodes
         INTEGER                                    :: n

         REAL(KIND=RP), ALLOCATABLE :: xy(:,:)
         INTEGER      , ALLOCATABLE :: connectivity(:,:)
         INTEGER                    :: i, j, k

         ALLOCATE(xy(2,(n+1)**2), connectivity(4,n**2))
         DO j = 0, n
            DO i = 0, n
               xy(:, i + (n+1)*j + 1) = [REAL(i,RP), REAL(j,RP)]
            END DO
         END DO
         k = 0
         DO j = 0, n-1
            DO i = 0, n-1
               k = k + 1
               connectivity(:,k) = [i + (n+1)*j + 1, i + 1 + (n+1)*j + 1, i + 1 + (n+1)*(j+1) + 1, i + (n+1)*(j+1) + 1]
            END DO
         END DO
         CALL BuildMesh(mesh, xy, connectivity, nodes)
         DO j = 0, n
            DO i = 0, n
               IF ( i == 0 .OR. j == 0 .OR. i == n .OR. j == n )     THEN
                  CALL MarkAsBoundaryNode(nodes(i + (n+1)*j + 1) % node)
               ELSE
                  nodes(i + (n+1)*j + 1) % node % distToBoundary = 1.0_RP
               END IF
            END DO
         END DO

      END SUBROUTINE BuildBlockMesh
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION numberOfTangledElements( mesh )
         IMPLICIT NONE
         TYPE (SMMesh), POINTER    :: mesh
         CLASS(FTObject) , POINTER :: obj
         TYPE (SMElement), POINTER :: e
         numberOfTangledElements = 0
         CALL mesh % elementsIterator % setToStart()
         DO WHILE ( .NOT.mesh % elementsIterator % isAtEnd() )
            obj => mesh % elementsIterator % object()
            CALL castToSMElement(obj, e)
            IF ( elementIsTangled(e) )     numberOfTangledElements = numberOfTangledElements + 1
            CALL mesh % elementsIterator % moveToNext()
         END DO
      END FUNCTION numberOfTangledElements
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE UntangleTest(optData)
!
!     -----------------------------------------------------------------
!     The center node of a 2 x 2 block moved to (2.5,1.25), outside the
!     block, folds an element: a corner Jacobian is negative, although
!     the signed area of the element is still positive. Untangling must move it back
!     inside so that all elements are right handed again.
!     -----------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)
         TYPE (SMMesh)  , POINTER                   :: mesh
         TYPE(SMNodePtr), DIMENSION(:), ALLOCATABLE :: nodes
         INTEGER                                    :: errorCode, numberOfElements, numberTangled
         REAL(KIND=RP)                              :: totalArea, minArea
         LOGICAL                                    :: savedPrintMessage

         CALL BuildBlockMesh(mesh, nodes, 2)
         nodes(5) % node % x = [2.5_RP, 1.25_RP, 0.0_RP]
         CALL FTAssert(test = numberOfTangledElements(mesh) > 0, msg = "Untangle: the moved node folds an element")

         savedPrintMessage = printMessage
         printMessage      = .TRUE.  ! Also exercises the progress messages
         errorCode         = A_OK_ERROR_CODE
         CALL UntangleInvertedElements(mesh, errorCode)
         printMessage      = savedPrintMessage
         CALL FTAssertEqual(expectedValue = A_OK_ERROR_CODE, actualValue = errorCode, msg = "Untangle error code")

         numberTangled = numberOfTangledElements(mesh)
         CALL MeshAreaSummary(mesh, totalArea, minArea, numberOfElements)
         CALL FTAssertEqual(expectedValue = 0, actualValue = numberTangled, msg = "Untangle: no element is tangled")
         CALL FTAssert(test = minArea > 0.0_RP .AND. ABS(totalArea - 4.0_RP) <= AREA_TOLERANCE, &
                       msg  = "Untangle: all elements are right handed and cover the block")
         CALL FTAssert(test = nodes(5) % node % x(1) > 0.0_RP .AND. nodes(5) % node % x(1) < 2.0_RP .AND. &
                              nodes(5) % node % x(2) > 0.0_RP .AND. nodes(5) % node % x(2) < 2.0_RP,     &
                       msg  = "Untangle: the center node is back inside the block")
         CALL FTAssert(test = ALL(nodes(1) % node % x == [0.0_RP, 0.0_RP, 0.0_RP]), &
                       msg  = "Untangle: boundary nodes do not move")
         CALL releaseMesh(mesh)

      END SUBROUTINE UntangleTest
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE InterfaceSplitTest(optData)
!
!     -----------------------------------------------------------------
!     A 3 x 3 block whose center element has interface nodes at the
!     opposite corners (1,1) and (2,2). The center element is split
!     into four, the six elements with one interface node into three
!     each, and the two corner elements without interface nodes are
!     kept: 4 + 18 + 2 = 24 elements. The split must keep the mesh
!     conforming, i.e. the edges with only one element are exactly the
!     outer boundary of length 12.
!     -----------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)
         TYPE (SMMesh)      , POINTER                   :: mesh
         TYPE(SMNodePtr)    , DIMENSION(:), ALLOCATABLE :: nodes
         CLASS(FTLinkedList), POINTER                   :: interfaceElements
         CLASS(FTObject)    , POINTER                   :: obj
         TYPE (SMElement)   , POINTER                   :: e
         TYPE (SMEdge)      , POINTER                   :: edge
         INTEGER                                        :: errorCode, numberOfElements, k, j, nodeID, numberInCenter
         INTEGER, PARAMETER                             :: interfaceNodeIDs(2) = [6, 11] ! (1,1) and (2,2)
         REAL(KIND=RP)                                  :: totalArea, minArea, boundaryLength, c(2)

         CALL BuildBlockMesh(mesh, nodes, 3)
         DO k = 1, 2
            CALL MarkAsBoundaryNode(nodes(interfaceNodeIDs(k)) % node)
         END DO
!
!        -----------------------------------------------------------
!        Chain 1 is an interface. The outer nodes are not on a curve
!        here so that only the two marked nodes are interface nodes.
!        -----------------------------------------------------------
!
         DO k = 1, SIZE(nodes)
            IF ( ANY(interfaceNodeIDs == k) )     CYCLE
            IF ( nodes(k) % node % distToBoundary == 0.0_RP )     THEN
               nodes(k) % node % bCurveID       = UNDEFINED
               nodes(k) % node % bCurveChainID  = UNDEFINED
               nodes(k) % node % distToBoundary = 1.0_RP
            END IF
         END DO
         IF ( ALLOCATED(boundaryEdgesType) )     DEALLOCATE(boundaryEdgesType)
         ALLOCATE(boundaryEdgesType(1))
         boundaryEdgesType(1) = INTERFACE_EDGES

         CALL mesh % buildEdgeList()
!
!        ------------------------------------------------------------
!        The elements around each interface node, as gathered by
!        CleanUpBoundaryElements, so the center element is listed twice
!        ------------------------------------------------------------
!
         ALLOCATE(interfaceElements)
         CALL interfaceElements % init()
         errorCode = A_OK_ERROR_CODE
         CALL MakeNodeToElementConnections(mesh, errorCode)
         DO k = 1, 2
            nodeID = interfaceNodeIDs(k)
            DO j = 1, numElementsForNode(nodeID)
               obj => elementsForNodes(j,nodeID) % element
               CALL interfaceElements % add(obj)
            END DO
         END DO
         CALL deallocateNodeToElementConnections()

         CALL SplitInterfaceElements(mesh, interfaceElements)
         CALL releaseFTLinkedListClass(interfaceElements)
         CALL DoLazyDelete(mesh)
         CALL mesh % renumberAllLists()
         CALL mesh % syncEdges()

         CALL MeshAreaSummary(mesh, totalArea, minArea, numberOfElements)
         CALL FTAssertEqual(expectedValue = 24, actualValue = numberOfElements, msg = "Interface split: element count")
         CALL FTAssert(test = minArea > 0.0_RP .AND. ABS(totalArea - 9.0_RP) <= AREA_TOLERANCE, &
                       msg  = "Interface split: right handed elements cover the block")
!
!        -----------------------------------
!        Four elements inside the center one
!        -----------------------------------
!
         numberInCenter = 0
         CALL mesh % elementsIterator % setToStart()
         DO WHILE ( .NOT.mesh % elementsIterator % isAtEnd() )
            obj => mesh % elementsIterator % object()
            CALL castToSMElement(obj, e)
            c = 0.0_RP
            DO k = 1, 4
               c = c + 0.25_RP*e % nodes(k) % node % x(1:2)
            END DO
            IF ( ALL(c > 1.0_RP) .AND. ALL(c < 2.0_RP) )     numberInCenter = numberInCenter + 1
            CALL mesh % elementsIterator % moveToNext()
         END DO
         CALL FTAssertEqual(expectedValue = 4, actualValue = numberInCenter, &
                            msg = "Interface split: the center element is split into four")
!
!        -------------------------------------------------------
!        Conformity: a hanging node would leave interior edges
!        with only one element, adding to the boundary length
!        -------------------------------------------------------
!
         boundaryLength = 0.0_RP
         CALL mesh % edgesIterator % setToStart()
         DO WHILE ( .NOT.mesh % edgesIterator % isAtEnd() )
            obj => mesh % edgesIterator % object()
            CALL castToSMEdge(obj, edge)
            IF ( .NOT.ASSOCIATED(edge % elements(2) % element) )     THEN
               boundaryLength = boundaryLength + NORM2(edge % nodes(2) % node % x - edge % nodes(1) % node % x)
            END IF
            CALL mesh % edgesIterator % moveToNext()
         END DO
         CALL FTAssert(test = ABS(boundaryLength - 12.0_RP) <= AREA_TOLERANCE, &
                       msg  = "Interface split: the mesh is conforming")

         DEALLOCATE(boundaryEdgesType)
         CALL releaseMesh(mesh)

      END SUBROUTINE InterfaceSplitTest

      END Module MeshTopologyTestsModule

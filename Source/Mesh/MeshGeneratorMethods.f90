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
!      MeshGeneratorMethods.f90
!      Created: August 21, 2013 1:01 PM
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
   Module MeshGenerationMethods
      USE QuadTreeGridGeneratorModule
      USE MeshProjectClass
      USE MeshOperationsModule
      USE ProgramGlobals
      USE MeshBoundaryMethodsModule
      USE ErrorTypesModule
      USE MeshOutputMethods
      USE MeshSizerClass
      USE BoundaryErrorModule
      IMPLICIT NONE
!
!     ---------------------------------------------------------------
!     Interface paving: what paving one interface chain
!     would change in the mesh (PaveInterfaceWithBand), so that several
!     band widths can be tried and the best one applied (CommitPavePlan)
!     without meshing it again. bandEdge(:,side) are the nodes on the
!     edge of the remaining mesh, local nodes nc+1..nc+nfr(side) of sx.
!     nDropped counts the elements of large islands that the band cut
!     off from the rest of the mesh and that are paved over instead.
!     ---------------------------------------------------------------
!
      TYPE PavePlan
         LOGICAL                         :: ok       = .FALSE.
         REAL(KIND=RP)                   :: score    = 0.0_RP
         INTEGER                         :: nDropped = 0
         CLASS(SMChainedCurve), POINTER  :: chain => NULL()
         INTEGER                         :: chainID, nCurves, bCurveSide, nc, nfr(2), nAll(2), nQ(2)
         LOGICAL                         :: ccw
         TYPE(SMElementPtr), ALLOCATABLE :: removedElements(:)
         TYPE(SMNodePtr)   , ALLOCATABLE :: seq(:), bandEdge(:,:)
         REAL(KIND=RP)     , ALLOCATABLE :: cg(:), sx(:,:,:)
         INTEGER           , ALLOCATABLE :: quads(:,:,:)
      END TYPE PavePlan
!
!     Whether a pair of nodes can be joined by a chord (see isBridge)
!
      INTEGER, PARAMETER, PRIVATE :: BRIDGE_UNCHECKED = 0, BRIDGE_VALID = 1, BRIDGE_INVALID = -1

!
!     ========
      CONTAINS
!     ========
!
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GenerateQuadMesh(project, errorCode)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MeshProject), POINTER :: project
         INTEGER                     :: errorCode
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER :: k
         LOGICAL :: needsRemesh
         INTEGER :: numberOfTries = 3
         INTEGER :: j

         needsRemesh = .FALSE.

         DO j = 1, numberOfTries
            CALL GenerateAQuadMesh( project, errorCode )
!
!           --------------------------------------------------------
!           A failed attempt leaves the boundary edge lists in an
!           unknown state. Skip the boundary approximations and let
!           the retry with a smaller background grid below handle it
!           --------------------------------------------------------
!
            IF ( errorCode > A_OK_ERROR_CODE )     EXIT
!
!           -----------------------------------
!           Gather and set boundary information
!           and from that, create boundary
!           polynomial approximations to the
!           boundary chains.
!           -----------------------------------
!
            CALL ComputeBoundaryApproximations(project, .FALSE.)
            CALL ComputeBoundaryErrors(project)
            CALL ResetInverseScales(project, needsRemesh)
!
!        -----------------------------------------------
!        If the mesh needs to be remeshed because errors
!        are too large someplace, do so
!        -----------------------------------------------
!
            IF ( .NOT. needsRemesh )     EXIT

            IF( j < numberOfTries) CALL ResetProject(project)
            IF(printMessage)       PRINT *, "Remeshing for accuracy..."

         END DO
!
!        ------------------------------------------------------------------------
!        If there is a problem, usually it is because the initial background grid
!        is too large. Try again with a smaller background grid, just in case.
!        ------------------------------------------------------------------------
!
         IF ( errorCode > A_OK_ERROR_CODE )     THEN ! Try again at most two times
            DO k = 1, 2

               errorCode = A_OK_ERROR_CODE
               IF(printMessage)     THEN
                  PRINT *, "Background grid is too large. Trying again with 1/2 size" ! Throw exception here
               END IF

               project % backgroundParams % dx           =   project % backgroundParams % dx/2.0_RP
               project % backgroundParams % N            = 2*project % backgroundParams % N
               project % sizer % baseSize                = 0.5_RP*project % sizer % baseSize
               project % backgroundParams % backgroundGridSize = 0.5_RP*project % backgroundParams % backgroundGridSize

               CALL ResetProject(project)
               CALL clearBoundaryCurves(project % sizer)
               CALL BuildSizerBoundaryCurves(project)

               CALL GenerateAQuadMesh(project,errorCode)

               IF( errorCode == A_OK_ERROR_CODE)     EXIT
            END DO
        END IF
!
!       ------------------------------------------------------
!       Still no valid mesh: the caller reports the error, see
!       ReportMeshGenerationFailure
!       ------------------------------------------------------
!
        IF ( errorCode > A_OK_ERROR_CODE )     RETURN
!
!       -------------------------------------------------------------
!       Re-compute the boundary polynomials with the optimized curves
!       if that has been requested
!       -------------------------------------------------------------
!
        CALL ResetProjectBoundaryObjects(project)
        CALL ComputeBoundaryApproximations(project, .TRUE.)

      END SUBROUTINE GenerateQuadMesh
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GenerateAQuadMesh(project, errorCode)
         USE MeshCleaner
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MeshProject), POINTER :: project
         INTEGER                     :: errorCode
!
!        ---------------
!        Local Variables
!        ---------------
!
         TYPE (SMModel)       , POINTER     :: model
!
         IF(PrintMessage) PRINT *, "Generate 2D mesh..."
!
!        --------------------------------------------------------------
!        Determining exterior quads requires a curve defined in terms
!        of an array. Convert and temporarily save the boundary curves.
!        ConvertBoundaryCurvesToArrays allocates and fills temporaries
!        in the baseMethods module.
!        --------------------------------------------------------------
!
!         CALL generateTemporaryBoundaryArrays( project % sizer )

         errorCode = A_OK_ERROR_CODE
         CALL GenerateQuadMeshFromGrid( project, errorCode )
         CALL trapExceptions !Abort on fatal exceptions
         IF(errorCode > A_OK_ERROR_CODE)     RETURN
!
!        ------------------------
!        Perform topology cleanup
!        ------------------------
!
         CALL PerformTopologyCleanup(project % mesh, errorCode)
         IF(errorCode > A_OK_ERROR_CODE)     RETURN
!
!        ---------------------------------------
!        Redistribute nodes along the boundaries
!        as a smoothing technique
!        ---------------------------------------
!
         CALL RedistributeNodesAlongBoundaries(project % mesh, project % model )
!
!        ------------------------
!        Smooth mesh if requested
!        ------------------------
!
         IF(Associated(project % smoother))     THEN
            IF(PrintMessage) PRINT *, "   Begin Smoothing..."
            CALL project % smoother % smoothMesh(  project % mesh, project % model, errorCode )
            IF(PrintMessage) PRINT *, "   Smoothing done."
         END IF
!
!        -------------
!        Clean up mesh
!        -------------
!
         IF(PrintMessage) PRINT *, "   Performing final mesh cleanup..."
            CALL PerformFinalMeshCleanup( project % mesh, project % model, errorCode )
            IF(errorCode > A_OK_ERROR_CODE)     RETURN
         IF(PrintMessage) PRINT *, "   Mesh cleanup done."
!
!        --------------------------------------
!        Smooth mesh one more time if requested
!        --------------------------------------
!
         IF(Associated(project % smoother))     THEN
            IF(PrintMessage) PRINT *, "   Begin Final Smoothing..."
            CALL project % smoother % smoothMesh(  project % mesh, project % model, errorCode )
            IF(errorCode > A_OK_ERROR_CODE)     RETURN
            IF(PrintMessage) PRINT *, "   final Smoothing done."
         END IF
!
!        --------------------------------------------------------------
!        Move interior nodes out of any inverted elements that are left
!        --------------------------------------------------------------
!
         CALL UntangleInvertedElements( project % mesh, errorCode )
         IF(errorCode > A_OK_ERROR_CODE)     RETURN
!
!        ------------------------------------------------
!        Improve the shapes of the elements that are poor
!        after smoothing, mostly those at irregular nodes
!        ------------------------------------------------
!
         CALL ImproveElementShapes( project % mesh, errorCode )
         IF(errorCode > A_OK_ERROR_CODE)     RETURN
!
!        ----------------------------------------------------------
!        If this is a multi-material mesh, then set each element's
!        materialID and materialName
!        ----------------------------------------------------------
!
         IF ( ASSOCIATED(interfaceCurves) .AND. &
            project % runParams % meshFileFormat == ISM_MM)     THEN
            CALL SetMaterialProperties(mesh = project % mesh, model = project % model)
         END IF
!
!        -------------------------------------
!        We no longer need the boundary arrays
!        -------------------------------------
!
         CALL destroyTemporaryBoundaryArrays

      END SUBROUTINE GenerateAQuadMesh
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GenerateQuadMeshFromGrid( project, errorCode )
      USE BoundaryEdgeCleaningModule
!
!     ------------------------------------------------------------
!     This is the main procedure that actually generates the mesh.
!     ------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS( MeshProject ), POINTER :: project
      INTEGER                       :: errorCode
!
!     ---------------
!     Local variables
!     ---------------
!
      LOGICAL                       :: paved
      TYPE (QuadTreeGrid), POINTER :: grid      => NULL()
      TYPE (SMMesh)      , POINTER :: mesh      => NULL()
      TYPE (SMModel)     , POINTER :: model     => NULL()
      TYPE (MeshSizer)   , POINTER :: sizer     => NULL()
      CLASS(FTObject)    , POINTER :: obj       => NULL()
      CLASS(FTLinkedList), POINTER :: list      => NULL()
      TYPE (FTException) , POINTER :: exception => NULL()

      INTEGER                                          :: numberOfBoundaries,numBoundaryEdgeLists
      INTEGER                                          :: j
      INTEGER                                          :: idOfOuterBoundary
      INTEGER                                          :: numberOfMaterials
      INTEGER                                          :: numberOfDetachedElements, pass
      INTEGER, PARAMETER                               :: MAX_BOUNDARY_CLEANUP_PASSES = 5
      CHARACTER(LEN=SM_CURVE_NAME_LENGTH)              :: materialName
      CHARACTER(LEN=SM_CURVE_NAME_LENGTH), ALLOCATABLE :: materialNames(:)

      TYPE (FTLinkedListIterator), POINTER :: iterator
      CLASS(SMChainedCurve)      , POINTER :: chain
!
!     ---------------------
!     Generate the quadtree
!     ---------------------
!
      IF(PrintMessage) PRINT *, "   Generate quadtree..."
         chainWasPaved = .FALSE.
         CALL GenerateGridWithSizerAndType( project % grid, project % sizer, &
                                            project % meshParams % meshType)
         project % numberOfLevelsUsed = highestLevel
         IF(catch() .AND. (maximumErrorSeverity() == FT_ERROR_FATAL)) RETURN
      IF(PrintMessage) PRINT *, "   Quadtree grid generated"
!
!     ---------------
!     Create the mesh
!     ---------------
!
      ALLOCATE( project % mesh )
      CALL project % mesh % init( )

      mesh  => project % mesh
      model => project % model
      grid  => project % grid
      sizer => project % sizer
!
!     --------------------------------------------------------------------
!     Save the polynomial order and the material names to the mesh.
!     The background material has BACKGROUND_MATERIAL_ID = 0. Each
!     interface curve encloses a material, given by its optional
!     "material name", if not present the curve is named after the chain.
!     Each distinct material name gets the next ID, 1, 2,..., in the order
!     the interfaces appear in the control file, so interfaces with the
!     with the same material name share an ID. An interface whose material
!     is the background material gets BACKGROUND_MATERIAL_ID.
!     --------------------------------------------------------------------
!
      mesh % polynomialOrder =  project % runParams % polynomialOrder
      numberOfBoundaries = model % numberOfInnerCurves + model % numberOfOuterCurves &
                         + model % numberOfInterfaceCurves

      IF ( model % numberOfInterfaceCurves > 0 )     THEN

            ALLOCATE( materialNames(0:model % numberOfInterfaceCurves) )
            materialNames(BACKGROUND_MATERIAL_ID) = project % backgroundMaterialName
            numberOfMaterials = 0

            iterator => model % interfaceBoundariesIterator
            CALL iterator % setToStart()
            DO WHILE( .NOT.iterator % isAtEnd() )
               obj => iterator % object()
               CALL castToSMChainedCurve(obj,chain)

               IF ( LEN_TRIM(chain % materialName) > 0 )     THEN
                  materialName = chain % materialName
               ELSE
                  materialName = chain % curveName()
               END IF

               chain % materialID = -1
               DO j = 0, numberOfMaterials
                  IF ( materialNames(j) == materialName )     THEN
                     chain % materialID = j
                     EXIT
                  END IF
               END DO
               IF ( chain % materialID < 0 )     THEN
                  numberOfMaterials                = numberOfMaterials + 1
                  materialNames(numberOfMaterials) = materialName
                  chain % materialID               = numberOfMaterials
               END IF

               CALL iterator % moveToNext()
            END DO

            ALLOCATE(CHARACTER(SM_CURVE_NAME_LENGTH) :: mesh % materialNameForID(0:numberOfMaterials))
            mesh % materialNameForID = materialNames(0:numberOfMaterials)
            DEALLOCATE( materialNames )
      ELSE
!
!           ----------------------------------------------------------
!           Without interfaces all elements are the background
!           material, which the ISM-MM format still writes out
!           ----------------------------------------------------------
!
            ALLOCATE(CHARACTER(SM_CURVE_NAME_LENGTH) :: mesh % materialNameForID(0:0))
            mesh % materialNameForID(BACKGROUND_MATERIAL_ID) = project % backgroundMaterialName
      END IF
!
!     -------------------------------------
!     Create the nodes, elements, and edges
!     from the nested grid
!     -------------------------------------
!
      IF(PrintMessage) PRINT *, "   Generate nodes and elements..."
         CALL GenerateNodesAndElements( mesh, grid )
      IF(PrintMessage) PRINT *, "   Nodes and elements were generated..."
!
!     ---------------------------------------------
!     Free up memory since grid is no longer needed
!     ---------------------------------------------
!
      CALL releaseGrid(grid)
      NULLIFY(project % grid)
!
!     --------------------------------------------------------------
!     Now make the mesh conform to exterior or interface boundaries.
!
!     Determining exterior quads requires a curve defined in terms
!     of an array. Convert and temporarily save the boundary curves.
!     ConvertBoundaryCurvesToArrays allocates and fills temporaries
!     in the baseMethods module.
!     --------------------------------------------------------------
!
      CALL generateTemporaryBoundaryArrays( sizer )
!
!     --------------------------------------------
!     Mark the elements outside or near boundaries
!     then remove.
!     --------------------------------------------
!
      IF( numberOfBoundaries > 0 )     THEN
         ALLOCATE( mesh % curveTypeForID(numberOfBoundaries) )
         CALL flagBoundaryTypes(mesh % curveTypeForID)
      END IF

      CALL MarkExteriorElements ( mesh, project % backgroundParams )
      CALL MarkInterfaceElements( mesh )
!
!     ----------------------------------------
!     Actually do the deletion. At this point,
!     any temporaries are out of sync.
!     ----------------------------------------
!
      CALL DoLazyDelete( mesh )
!
!     ------------------------------------------------------------
!     Elements left on their own or attached only at a node (e.g.
!     near a sharp tip) cannot be part of a boundary edge loop
!     ------------------------------------------------------------
!
      CALL RemoveDetachedElements( mesh, numberOfDetachedElements )
      IF ( PrintMessage .AND. numberOfDetachedElements > 0 )     THEN
         PRINT *, "      Elements removed that were detached from the rest of the mesh: ", numberOfDetachedElements
      END IF

      IF( model % curveCount == 0 )     THEN
         CALL mesh % buildEdgeList()
         RETURN
      END IF

      numBoundaryEdgeLists = model%numberOfInnerCurves + &
                             model%numberOfOuterCurves + &
                             model%numberOfInterfaceCurves

      idOfOuterBoundary = UNDEFINED
      IF( ASSOCIATED( sizer % outerBoundary ) ) idOfOuterBoundary = sizer % outerBoundary % id
!
!     --------------------------------------------------------------
!     Removing the elements too close to the boundaries can detach
!     others again, so repeat until the boundary edges form loops
!     --------------------------------------------------------------
!
      DO pass = 1, MAX_BOUNDARY_CLEANUP_PASSES
!
!        -------------------------------------------
!        Prepare for generating boundary elements by
!        building, collecting and ordering boundary
!        edges
!        -------------------------------------------
!
         IF ( pass == 1 )     THEN
            CALL mesh % buildEdgeList()
         ELSE
            CALL mesh % syncEdges()
         END IF

         CALL AllocateBoundaryEdgesArray(numBoundaryEdgeLists)
         CALL CollectBoundaryEdges( mesh, errorCode, useElementSides = .TRUE. )

         IF( errorCode > A_OK_ERROR_CODE)     RETURN
         IF ( .NOT.boundaryEdgeLoopsAreClosed() )     THEN
            errorCode = BOUNDARY_LOOP_OPEN_ERROR_CODE
            RETURN
         END IF

         CALL OrderBoundaryEdges( mesh )

         IF ( catch(WARNING_ERROR_EXCEPTION) )     THEN  ! Pass the error up the chain
            exception => errorObject()
            CALL throw(exception)
            RETURN
         END IF
!
!        -------------------------------------------------------
!        Smooth the edges along exterior and interior boundaries
!        before generating the boundary elements.
!        -------------------------------------------------------
!
         CALL smoothBoundaryEdges
!
!        -------------------------------------------------------------
!        Associate the boundary edges with elements and see if any
!        elements must be eliminated for having nodes too close to
!        a boundary, etc.
!        -------------------------------------------------------------
!
         CALL LocateEdgeImagesOnBoundaries( mesh, model, idOfOuterBoundary, skipInterfaces = .FALSE. )

         IF ( catch(FATAL_ERROR_EXCEPTION) )     THEN  ! Pass the error up the chain
            CALL destroyTemporaryBoundaryArrays
            exception => errorObject()
            CALL throw(exception)
            RETURN
         END IF

         CALL CleanUpBoundaryCurves( mesh, model, errorCode )
         IF(errorCode > A_OK_ERROR_CODE)     RETURN

         CALL RemoveDetachedElements( mesh, numberOfDetachedElements )
         IF ( numberOfDetachedElements == 0 )     EXIT
         IF ( PrintMessage )     THEN
            PRINT *, "      Elements removed that were detached from the rest of the mesh: ", numberOfDetachedElements
         END IF
      END DO
!
!     ------------------------------------------
!     The edge lists are also no longer in sync.
!     They may have had edges added or deleted.
!     Re-configure and smooth the boundary edges
!     ------------------------------------------
!
      CALL OrderBoundaryEdges( mesh )
      CALL smoothBoundaryEdges
!
!     ----------------------------------------------------------------
!     Next, find where the edges should project onto the
!     boundary. Try to keep them uniformly distributed if possible
!     by moving the proposed node locations along the boundary, except
!     for the end nodes, which will be attached to corners.
!     ----------------------------------------------------------------
!
      CALL LocateEdgeImagesOnBoundaries( mesh, model, idOfOuterBoundary, skipInterfaces = .true. )
      IF ( catch(FATAL_ERROR_EXCEPTION) )     THEN  ! Pass the error up the chain
         CALL destroyTemporaryBoundaryArrays
         exception => errorObject()
         CALL throw(exception)
         RETURN
      END IF

      DO j = 1, boundaryEdgesArray % COUNT()
         obj => boundaryEdgesArray % objectAtIndex(j)
         CALL cast(obj,list)
         CALL FlagEndNodes( list, model )
         CALL SpreadCollapsedBoundaryLocations( list, model )
         IF( boundaryEdgesType(j) == INTERFACE_EDGES )     THEN
!
!           ---------------------------------------------------------
!           With interfacePaving, pave the band around the interface
!           chain. Otherwise, or if paving fails, move the nearby nodes
!           onto the curve.
!           ---------------------------------------------------------
!
            paved = .FALSE.
            IF ( interfacePaving )     CALL PaveInterface( mesh, model, sizer, list, paved )
            IF ( .NOT.paved )     CALL MoveInterfaceNodesToImages( list, model )
         END IF
      END DO
!
!     ------------------------------------------
!     Create new elements by projecting onto the
!     boundary curves.
!     ------------------------------------------
!
      DO j = 1, boundaryEdgesArray % COUNT()
         IF( boundaryEdgesType(j) == INTERFACE_EDGES ) CYCLE

         obj => boundaryEdgesArray % objectAtIndex(j)
         CALL cast(obj,list)
         CALL GenerateBoundaryElements( mesh, model, list )
      END DO
!
!     -------------------------------
!     The edges are no longer in sync
!     -------------------------------
!
      CALL mesh % destroyEdgeArrays()
      CALL DoLazyDelete( mesh )
      CALL unmarkNodesNearBoundaries( mesh % nodesIterator )
      CALL mesh % syncEdges()
      CALL mesh % renumberAllLists()

      IF(PrintMessage) PRINT *, "   Nodes and elements generated"

      END SUBROUTINE GenerateQuadMeshFromGrid
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE GenerateNodesAndElements( mesh, grid )
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE (QuadTreeGrid), POINTER :: grid
      TYPE (SMMesh)      , POINTER :: mesh
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER                        :: i, j, k, N, M
      INTEGER                        :: elementID
      TYPE (SMElement), POINTER      :: e   => NULL()
      CLASS(FTObject) , POINTER      :: obj => NULL()
      TYPE( SMNodePtr), DIMENSION(4) :: eNodes
!
!     -------------------------------------------------------
!     Top level operations: Remove flags and number the nodes
!     -------------------------------------------------------
!
      IF( grid % level == 0 ) THEN
         CALL AssignNodeIdsForGrid_( grid )
         CALL FlagNodeIds( grid, .false. )
      END IF
!
!     ---------------------------------------------
!     Generate the nodes: For now this makes copies
!     ---------------------------------------------
!
      N = grid % N(1)
      M = grid % N(2)
!
!     ----------------------------------------------------------------------------
!     For grid: Write out nodes in the order in which they were created <=> nodeID
!     Flag with a sign change to ignore aliases as having already been written out.
!     ----------------------------------------------------------------------------
!
      DO j = 0, M
         DO i = 0, N
            IF(.NOT.ASSOCIATED(grid % nodes(i,j) % node) )            CYCLE
            IF( grid % nodes(i,j) % node % refCount() == 1 )          CYCLE  ! => Only the main grid, not quads references this
            IF( grid % nodes(i,j) % node % activeStatus == REMOVE )   CYCLE  ! Marked as outside
            IF( grid % nodes(i,j) % node % id > 0 )     THEN

               obj => grid % nodes(i,j) % node
               CALL mesh % nodes % add(obj)
               grid % nodes(i,j) % node % id = -ABS(grid % nodes(i,j) % node % id)
            END IF

         END DO
      END DO
!
!     -------------------------------------
!     Now generate the element connectivity
!     -------------------------------------
!
      DO j = 1, M
         DO i = 1, N
            IF ( ASSOCIATED(grid % quads(i,j) % quad) )     THEN
               IF ( ASSOCIATED(grid % children(i,j) % grid) )  CYCLE
               elementID = mesh % newElementID()
               DO k = 1, 4
                  eNodes(k) % node => grid % quads(i,j) % quad % nodes(k) % node
               END DO
               ALLOCATE(e)
               CALL e % initWithNodesIDAndType(eNodes, elementID, QUAD )
               obj => e
               CALL mesh % elements % add(obj)
               CALL releaseSMElement(e)
            END IF
         END DO
      END DO
!
!     ------------
!     For children
!     ------------
!
      IF( ASSOCIATED(grid % children) )     THEN
         DO j = 1, M
            DO i = 1, N
               IF( ASSOCIATED( grid % children(i,j) % grid ) ) &
                   CALL GenerateNodesAndElements( mesh, grid % children(i,j) % grid )
            END DO
         END DO
      END IF
!
!     --------------------
!     Remove node flagging
!     --------------------
!
      IF( grid % level == 0 ) CALL FlagNodeIds( grid, .false. )

      END SUBROUTINE GenerateNodesAndElements
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GenerateBoundaryElements( mesh, model, list )
         USE MeshOutputMethods, ONLY: WriteSkeletonToTecplot
         USE, INTRINSIC :: iso_fortran_env, only : stderr => ERROR_UNIT
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh)      , POINTER :: mesh
         TYPE (SMModel)     , POINTER :: model
         CLASS(FTLinkedList), POINTER :: list
!
!        ---------------
!        Local Variables
!        ---------------
!
         TYPE(SMNodePtr), DIMENSION(:), POINTER :: nodeArray => NULL()

         INTEGER, EXTERNAL :: Loop


         TYPE (SMNode)  , POINTER       :: node => NULL(), prevNode => NULL(), jointNode => NULL(), nextNode => NULL()
         TYPE (SMNode)  , POINTER       :: startNode => NULL()
         TYPE(SMNodePtr), DIMENSION(2)  :: newNodes
         TYPE(SMNodePtr), DIMENSION(2)  :: boundaryEdgeNodes
         TYPE(SMNodePtr), DIMENSION(4)  :: elementNodes

         CLASS(SMCurve)       , POINTER :: cStart => NULL(), cEnd => NULL()
         CLASS(SMChainedCurve), POINTER :: chain => NULL()
         TYPE (SMElement)     , POINTER :: e => NULL()
         CLASS(FTObject)      , POINTER :: obj => NULL()

         INTEGER                        :: k, nodeArraySize, shift
         INTEGER                        :: jointType, bCurveSide

         REAL(KIND=RP)                  :: t, p(3)
         REAL(KIND=RP)                  :: tStart, tEnd
!
!        ---------
!        Externals
!        ---------
!
!
!        ---------------------------------------
!        Step through each edge on this boundary
!        and add a new node along the boundary
!        itself to make new elements.
!        ---------------------------------------
!
         nodeArray     => GatheredNodes( list )
         nodeArraySize = SIZE(nodeArray)
!
!        -----------------------------------------------------------------
!        The templates at a joint use nodes past the joint, up to three for
!        a row reversal, and the corner and reversal templates move the
!        boundary node made for the node before the joint. So that no
!        template wraps around the end of the loop, start the loop where
!        the first two and the last three nodes are not joints.
!        -----------------------------------------------------------------
!
         shift = boundaryLoopStartShift( nodeArray )
         IF ( shift /= 0 )     nodeArray = CSHIFT( nodeArray, shift )
!
!        --------------------------
!        Get the first node started
!        --------------------------
!
         node       => nodeArray(1) % node
         bCurveSide =  node % bCurveSide
         t          =  node % whereOnBoundary
         cStart     => model % curveWithID(node % bCurveID, chain)

         ALLOCATE(node)
         CALL initBoundaryNode( cStart, chain, t, bCurveSide, mesh % newNodeID(), node )
         obj => node
         CALL mesh % nodes % add(obj)
         CALL releaseSMNode(node)
         prevNode  => node
         startNode => node
!
!        ------------------------
!        Now work around the loop
!        ------------------------
!
         k = 1
         DO WHILE ( k <= nodeArraySize )

            IF ( k < nodeArraySize )     THEN
               jointType = nodeArray(k+1) % node % nodeType
            ELSE
               jointType = nodeArray(1) % node % nodeType
            END IF

            SELECT CASE ( jointType )

               CASE( NONE, ROW_SIDE )
!
!                 ---------------------------------------------------------------
!                 Create one element by projecting the edge to the boundary curve
!                 ---------------------------------------------------------------
!
                  newNodes(1) % node          => prevNode
                  boundaryEdgeNodes(1) % node => nodeArray(k) % node
!
!                 -----------------------------------------------------------------
!                 Before the end of the loop, create a new node along the boundary,
!                 otherwise, wrap around to the beginning.
!                 -----------------------------------------------------------------
!
                  IF( k < nodeArraySize )     THEN
                     node                        => nodeArray(k+1) % node
                     boundaryEdgeNodes(2) % node => nodeArray(k+1) % node
                     t                           =  node % whereOnBoundary
                     cEnd                        => model % curveWithID(node % bCurveID, chain)

                     ALLOCATE(node)
                     CALL initBoundaryNode( cEnd, chain, t, bCurveSide, mesh % newNodeID(), node )

                     newNodes(2) % node => node
                     obj => node
                     CALL mesh % nodes % add(obj)
                     CALL releaseSMNode(node)
                  ELSE
                     boundaryEdgeNodes(2) % node => nodeArray(1) % node
                     newNodes(2) % node          => startNode
                  END IF
!
!                 ----------------------------------------------------
!                 Create element and add element and nodes to the mesh
!                 ----------------------------------------------------
!
                  e   => boundaryElementForNodes( mesh % newElementID(), boundaryEdgeNodes, newNodes )
                  obj => e
                  CALL mesh % elements % add(obj)
                  CALL releaseSMElement(e)
                  prevNode => newNodes(2) % node

               CASE( ROW_END )
!
!                 ---------------------------------------------------------------
!                 A row-end sees the insertion of at least three elements. One is
!                 a wedge element whose corner is at the joint between the two
!                 curves. The (small) angle is not split this way.
!
!                 First Projection Element - like a row side
!                 ---------------------------------------------------------------
!
                  newNodes(1) % node          => prevNode ! first corner node
                  boundaryEdgeNodes(1) % node => nodeArray(k) % node
                  IF( k < nodeArraySize )     THEN
                     jointNode => nodeArray(k+1) % node
                  ELSE ! Wrap around
                     jointNode => nodeArray(1) % node
                  END IF
                  boundaryEdgeNodes(2) % node => jointNode
                  p = jointNode % x

                  tStart = nodeArray(k) % node % whereOnBoundary
                  tEnd   = 1.0_RP
                  cStart => model % curveWithID(nodeArray(k) % node % bCurveID, chain)

                  t = 0.5_RP*(tStart + tEnd)

                  ALLOCATE(node)
                  CALL initBoundaryNode( cStart, chain, t, bCurveSide, mesh % newNodeID(), node )

                  newNodes(2) % node => node
                  obj => node
                  CALL mesh % nodes % add(obj)
                  CALL releaseSMNode(node)
!
                  e => boundaryElementForNodes( mesh % newElementID(), boundaryEdgeNodes, newNodes )
                  obj => e
                  CALL mesh % elements % add(obj)
                  CALL releaseSMElement(e)
                  prevNode => newNodes(2) % node
!
!                 -------------
!                 Wedge Element
!                 -------------
!
                  elementNodes(1) % node => jointNode
                  elementNodes(2) % node => prevNode
!
!                 ------------
!                 Joint corner
!                 ------------
!
                  t    =  1.0_RP

                  ALLOCATE(node)
                  CALL initBoundaryNode( cStart, chain, t, bCurveSide, mesh % newNodeID(), node )
                  elementNodes(3) % node => node
                  obj => node
                  CALL mesh % nodes % add(obj)
                  CALL releaseSMNode(node)
                  elementNodes(3) % node % nodeType = ROW_END
!
!                 ----------------------------------------------
!                 Closest point on the *next* curve in the chain
!                 ----------------------------------------------
!
                  IF ( k+2 <= nodeArraySize )     THEN
                     nextNode => nodeArray(k+2) % node
                  ELSE
                     nextNode => nodeArray(1) % node !Wrap around
                  END IF

                  cEnd   => model % curveWithID(nextNode % bCurveID, chain)
                  tStart = 0.0_RP
                  tEnd   = nextNode % whereOnBoundary ! No further than the next node along the chain

                  t = 0.5_RP*(tStart + tEnd)

                  ALLOCATE(node)
                  CALL initBoundaryNode( cEnd, chain, t, bCurveSide, mesh % newNodeID(), node )

                  elementNodes(4) % node => node
                  obj => node
                  CALL mesh % nodes % add(obj)
                  CALL releaseSMNode(node)
!
!                 ----------------------------------------
!                 Create wedge element and add to the mesh
!                 ----------------------------------------
!
                  e   => boundaryElementFor4Nodes( mesh % newElementID(), elementNodes )
                  obj => e
                  CALL mesh % elements % add(obj)
                  CALL releaseSMElement(e)
!
!                 ----------------------------------------------------------------
!                 Prepare for the next element, which should be a row-side element
!                 ----------------------------------------------------------------
!
                  prevNode => elementNodes(4) % node
!

               CASE( ROW_CORNER )
!
!                 -----------------------------------------------------
!                 A row corner node needs a single wedge element
!                 to avoid creating triangles. The wedge element
!                 uses the previous and next nodes along the boundary
!                 edge plus the reversal point along the boundary. This
!                 requires the prev % node % x to be moved to that point.
!                 -----------------------------------------------------
!
                  elementNodes(1) % node => nodeArray(k) % node
                  IF ( k < nodeArraySize )     THEN
                     elementNodes(4) % node => nodeArray(k+1) % node
                  ELSE
                     elementNodes(4) % node => nodeArray(1) % node
                  END IF
                  IF ( k < nodeArraySize-1 )     THEN
                     elementNodes(3) % node => nodeArray(k+2) % node
                  ELSE IF ( k == nodeArraySize-1 )     THEN
                     elementNodes(3) % node => nodeArray(1) % node
                  ELSE
                     elementNodes(3) % node => nodeArray(2) % node
                  END IF
!
!                 -------------------------------------------------
!                 Find the location of the joint along the boundary
!                 and move the prevNode position to that point. Use
!                 that as a corner in the wedge element.
!                 -------------------------------------------------
!
                  cStart => model % curveWithID(nodeArray(k) % node % bCurveID, chain)
                  p                           = cStart % positionAt(1.0_RP)
                  prevNode % x                = p
                  prevNode % whereOnBoundary  = 1.0_RP
                  prevNode % gWhereOnBoundary = chain % ChainTForCurveTInCurve( 1.0_RP, cStart )
                  prevNode % nodeType         = ROW_CORNER

                  elementNodes(2) % node => prevNode

                  e => boundaryElementFor4Nodes( mesh % newElementID(), elementNodes )
                  obj => e
                  CALL mesh % elements % add(obj)
                  CALL releaseSMElement(e)
!
!                 ----------------------------------------
!                 Get ready to move on - skip the k+1 node
!                 ----------------------------------------
!
                  prevNode => elementNodes(2) % node
                  k = k + 1

               CASE( ROW_REVERSAL )
!
!                 --------------------------------------------------------------
!                 A Row reversal node needs TWO elements to be created from four
!                 consecutive edges.
!                 --------------------------------------------------------------
!
                  elementNodes(1) % node => nodeArray(k) % node
                  elementNodes(2) % node => nodeArray(Loop(k+1,nodeArraySize)) % node
                  elementNodes(3) % node => nodeArray(Loop(k+2,nodeArraySize)) % node
!
!                 -------------------------------------------------
!                 Find the location of the joint along the boundary
!                 and move the prevNode position to that point. Use
!                 that as a corner in the wedge element.
!                 -------------------------------------------------
!
                  cStart => model % curveWithID(nodeArray(k) % node % bCurveID, chain)
                  p                           = cStart % positionAt(1.0_RP)
                  prevNode % x                = p
                  prevNode % whereOnBoundary  = 1.0_RP
                  prevNode % gWhereOnBoundary = chain % ChainTForCurveTInCurve( 1.0_RP, cStart )
                  prevNode % nodeType         = ROW_REVERSAL
                  elementNodes(4) % node => prevNode
                  e => boundaryElementFor4Nodes( mesh % newElementID(), elementNodes )
                  obj => e
                  CALL mesh % elements % add(obj)
                  CALL releaseSMElement(e)
!
!                 ----------------------------------------------
!                 Get ready to move on - skip the next two nodes
!                 ----------------------------------------------
!
                  prevNode => elementNodes(4) % node
                  k = k + 2
!
!                 --------------------
!                 Second Wedge Element
!                 --------------------
!
                  elementNodes(1) % node => prevNode
                  elementNodes(2) % node => nodeArray(k) % node
                  elementNodes(3) % node => nodeArray(Loop(k+1,nodeArraySize)) % node
                  elementNodes(4) % node => nodeArray(Loop(k+2,nodeArraySize)) % node

                  e => boundaryElementFor4Nodes( mesh % newElementID(), elementNodes )
                  obj => e
                  CALL mesh % elements % add(obj)
                  CALL releaseSMElement(e)

                  prevNode => elementNodes(1) % node
                  k = k + 1

               CASE DEFAULT
                 WRITE(stderr,*) " "
                 WRITE(stderr,*) "**************************************************************************"
                 WRITE(stderr,*) "An unknown row type has appeared in GenerateBoundaryElements"
                 WRITE(stderr,*) "Plot the file 'DebugPlot.tec' to check on the mesh topology"
                 WRITE(stderr,*) "**************************************************************************"
                 WRITE(stderr,*) " "
                  CALL WriteSkeletonToTecplot(mesh = mesh,fName = "DebugPlot.tec")
                  ERROR STOP "Meshing Terminated. See stderr"
            END SELECT
            k = k + 1
         END DO

         DEALLOCATE( nodeArray )

      END SUBROUTINE GenerateBoundaryElements
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION boundaryLoopStartShift( nodeArray )
         IMPLICIT NONE
!
!        ------------------------------------------------------------------
!        For GenerateBoundaryElements: the cyclic shift of the boundary
!        nodes so that the first two and the last three are not joints. It
!        is zero if the nodes are already in such an order.
!
!        If the joints are too close together for that (only a few nodes
!        apart), use the first shift at which the templates tile the loop
!        instead (see templatesTileLoop). If there is none, it is zero.
!        ------------------------------------------------------------------
!
         TYPE(SMNodePtr), DIMENSION(:) :: nodeArray
         INTEGER                       :: n, shift, j, nodeType
         LOGICAL                       :: isFree
         INTEGER                       :: types(SIZE(nodeArray))

         n = SIZE(nodeArray)
         boundaryLoopStartShift = 0
         DO shift = 0, n-1
            isFree = .TRUE.
            DO j = -2, 2
               nodeType = nodeArray(MODULO(shift + j - 1, n) + 1) % node % nodeType
               IF ( nodeType /= NONE .AND. nodeType /= ROW_SIDE )     THEN
                  isFree = .FALSE.
                  EXIT
               END IF
            END DO
            IF ( isFree )     THEN
               boundaryLoopStartShift = shift
               RETURN
            END IF
         END DO

         DO j = 1, n
            types(j) = nodeArray(j) % node % nodeType
         END DO
         DO shift = 0, n-1
            IF ( templatesTileLoop( CSHIFT(types, shift) ) )     THEN
               boundaryLoopStartShift = shift
               RETURN
            END IF
         END DO

      END FUNCTION boundaryLoopStartShift
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION templatesTileLoop( types )
         IMPLICIT NONE
!
!        ----------------------------------------------------------------
!        For GenerateBoundaryElements: true if, starting at the first
!        node, the templates use the boundary nodes exactly once around
!        the loop. The template at node k is chosen by the type of node
!        k+1 and moves on by one node (row side or end), two (corner) or
!        four (reversal). The first node must be a row side, so that the
!        boundary node made for it can close the loop. The second node
!        must not be a corner or reversal, whose templates would move that
!        boundary node to the joint. The walk must reach the last node,
!        whose row side template then closes the loop.
!        ----------------------------------------------------------------
!
         INTEGER, DIMENSION(:) :: types
         INTEGER               :: n, k, nextType

         n = SIZE(types)
         templatesTileLoop = .FALSE.
         IF ( types(1) /= NONE .AND. types(1) /= ROW_SIDE )     RETURN
         IF ( types(2) == ROW_CORNER .OR. types(2) == ROW_REVERSAL )     RETURN

         k = 1
         DO WHILE ( k < n )
            nextType = types(k+1)
            SELECT CASE ( nextType )
               CASE( ROW_CORNER )
                  k = k + 2
               CASE( ROW_REVERSAL )
                  k = k + 4
               CASE DEFAULT
                  k = k + 1
            END SELECT
         END DO
         templatesTileLoop = k == n

      END FUNCTION templatesTileLoop
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE flagBoundaryTypes(curveTypeForID)
!
!     ------------------------------------------------------------------------
!     Assign a boundary type (EXTERIOR, INTERIOR, INTERFACE) according to the
!     curve's id
!     ------------------------------------------------------------------------
!
         IMPLICIT NONE
         INTEGER, DIMENSION(:) :: curveTypeForID
         INTEGER :: k

         IF(ASSOCIATED( outerBoundaryCurve ))     THEN
            curveTypeForID(outerBoundaryCurve % id) = OUTER
         END IF

          IF(ASSOCIATED( interiorCurves ))     THEN
            DO k = 1, SIZE(interiorCurves)
               curveTypeForID(interiorCurves(k) % curveArray % id) = INNER
            END DO
         END IF

         IF( ASSOCIATED( interfaceCurves ) )     THEN
             DO k = 1, SIZE(interfaceCurves)
               curveTypeForID(interfaceCurves(k) % curveArray % id) = INTERIOR_INTERFACE
            END DO
         END IF

      END SUBROUTINE flagBoundaryTypes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MarkExteriorElements( mesh, backgroundParams )
      USE Geometry
      IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh)                 , POINTER :: mesh
         TYPE(BackgroundGridParameters)          :: backgroundParams
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedList)        , POINTER :: elements => NULL()
         TYPE (SMElement)           , POINTER :: e => NULL()
         CLASS(SegmentedCurveArray) , POINTER :: curveArray => NULL()
         TYPE (SMNode)              , POINTER :: node1 => NULL(), node2 => NULL(), node => NULL()
         CLASS(FTLinkedListIterator), POINTER :: elementIterator => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()

         INTEGER                             :: k, l, j, m, mSteps
         INTEGER                             :: w
         REAL(KIND=RP)                       :: nodes(3,4)
         REAL(KIND=RP)                       :: x1(3), x2(3), xRight, yTop, xLeft, yBottom
         REAL(KIND=RP)                       :: h, hMin, dz, z(3), segmentLength
         LOGICAL                             :: removedHere

         elements        => mesh % elements
         elementIterator => mesh % elementsIterator
!
!        ------------------------------------------------
!        Mark elements outside or near the outer boundary
!        ------------------------------------------------
!
         IF( ASSOCIATED( outerBoundaryCurve ) )    THEN

            CALL elementIterator % setToStart()
            DO WHILE(.NOT.elementIterator % isAtEnd())
               obj => elementIterator % object()
               CALL castToSMElement(obj,e)
!
!              -----------------------------------------------
!              Remove element if it has a node that is outside
!              the outer boundary curve (winding number =0)
!              -----------------------------------------------
!
               removedHere = .false.

               DO k = 1, 4

                  w = outerBoundaryCurve % windingNumber( e % nodes(k) % node % x, throughLastPoint = .TRUE. )
                  IF ( abs(w) == 0 ) THEN
                     e % remove    = .true.
                     removedHere   = .true.
                     EXIT
                  END IF

               END DO
!
!              --------------------------------------------
!              Let the nodes know which curve they are near
!              --------------------------------------------
!
               IF( e % remove .AND. removedHere )     THEN
                  DO k = 1, 4
                     node => e % nodes(k) % node
                     node % bCurveChainID = outerBoundaryCurve % id
                     node % bCurveSide    = INSIDE
                  END DO
               END IF

               CALL elementIterator % moveToNext()
            END DO

         ELSE ! we just have the outer box

            xRight  = backgroundParams % x0(1) + backgroundParams % dx(1)*backgroundParams % N(1)
            yTop    = backgroundParams % x0(2) + backgroundParams % dx(2)*backgroundParams % N(2)
            xLeft   = backgroundParams % x0(1)
            yBottom = backgroundParams % x0(2)

            CALL elementIterator % setToStart()
            DO WHILE ( .NOT.elementIterator % isAtEnd() )
               obj => elementIterator % object()
               CALL castToSMElement(obj,e)
               removedHere = .false.

               DO k = 1, 4
                  node1 => e % nodes(edgeMap(1,k)) % node
                  node2 => e % nodes(edgeMap(2,k)) % node

                  x1    = node1 % x
                  x2    = node2 % x
                  IF( AlmostEqual(x1(2),yBottom) .AND. AlmostEqual(x2(2),yBottom) )      THEN
                      node1 % bCurveID      = BOTTOM
                      node1 % bCurveChainID = OUTER_BOX
                      node1 % bCurveSide    = INSIDE
                      node2 % bCurveID      = BOTTOM
                      node2 % bCurveChainID = OUTER_BOX
                      node2 % bCurveSide    = INSIDE
                      node1 % distToBoundary = 0.0_RP
                      node2 % distToBoundary = 0.0_RP
                      IF( AlmostEqual(x1(1),xLeft) .OR. AlmostEqual(x1(1), xRight) )     THEN
                          node1 % nodeType = CORNER_NODE
                      END IF
                      IF( AlmostEqual(x2(1), xLeft) .OR. AlmostEqual(x2(1), xRight) )    THEN
                          node2 % nodeType = CORNER_NODE
                      END IF
                  ELSE IF ( AlmostEqual(x1(2),yTop) .AND. AlmostEqual(x2(2), yTop) )     THEN
                      node1 % bCurveID      = TOP
                      node1 % bCurveChainID = OUTER_BOX
                      node1 % bCurveSide    = INSIDE
                      node2 % bCurveID      = TOP
                      node2 % bCurveChainID = OUTER_BOX
                      node2 % bCurveSide    = INSIDE
                      node1 % distToBoundary = 0.0_RP
                      node2 % distToBoundary = 0.0_RP
                      IF( AlmostEqual(x1(1),xLeft) .OR. AlmostEqual(x1(1),xRight))       THEN
                          node1 % nodeType = CORNER_NODE
                      END IF
                      IF( AlmostEqual(x2(1),xLeft) .OR. AlmostEqual(x2(1),xRight))       THEN
                          node2 % nodeType = CORNER_NODE
                      END IF
                  ELSE IF ( AlmostEqual(x1(1),xLeft) .AND. AlmostEqual(x2(1),xLeft))     THEN
                      node1 % bCurveID      = LEFT
                      node1 % bCurveChainID = OUTER_BOX
                      node1 % bCurveSide    = INSIDE
                      node2 % bCurveID      = LEFT
                      node2 % bCurveChainID = OUTER_BOX
                      node2 % bCurveSide    = INSIDE
                      node1 % distToBoundary = 0.0_RP
                      node2 % distToBoundary = 0.0_RP
                      IF( AlmostEqual(x1(2),yTop) .OR. AlmostEqual(x1(2),yBottom) )      THEN
                          node1 % nodeType = CORNER_NODE
                      END IF
                      IF( AlmostEqual(x2(2), yTop) .OR. AlmostEqual(x2(2), yBottom) )    THEN
                          node2 % nodeType = CORNER_NODE
                      END IF
                  ELSE IF ( AlmostEqual(x1(1),xRight) .AND. AlmostEqual(x2(1),xRight) )  THEN
                      node1 % bCurveID      = RIGHT
                      node1 % bCurveChainID = OUTER_BOX
                      node1 % bCurveSide    = INSIDE
                      node2 % bCurveID      = RIGHT
                      node2 % bCurveChainID = OUTER_BOX
                      node2 % bCurveSide    = INSIDE
                      node1 % distToBoundary = 0.0_RP
                      node2 % distToBoundary = 0.0_RP
                      IF( AlmostEqual(x1(2),yTop) .OR. &
                          AlmostEqual(x1(2),yBottom) )     THEN
                          node1 % nodeType = CORNER_NODE
                      END IF
                      IF( AlmostEqual(x2(2),yTop) .OR. AlmostEqual(x2(2),yBottom) )     THEN
                          node2 % nodeType = CORNER_NODE
                      END IF
                  END IF

               END DO

               CALL elementIterator % moveToNext()
            END DO
         END IF
!
!        --------------------------------
!        Do the same for inner boundaries
!        --------------------------------
!
         IF( ASSOCIATED( interiorCurves ) )    THEN
            DO l = 1, SIZE(interiorCurves)
               curveArray => interiorCurves(l) % curveArray

               CALL elementIterator % setToStart()

               DO WHILE ( .NOT.elementIterator % isAtEnd() )
                  obj => elementIterator % object()
                  CALL castToSMElement(obj,e)
                  removedHere = .FALSE.

                  IF( .NOT.e % remove )     THEN

                        DO k = 1, 4

                          node => e % nodes(k) % node
                          w = curveArray % windingNumber( node % x, throughLastPoint = .TRUE. )
                          IF ( ABS(w) >= 1 ) THEN
                             e % remove  = .true.
                             removedHere = .true.
                             EXIT
                          END IF
                        END DO
!
!                       ----------------------------------------------------------------
!                       For thin, sharp interior objects like an airfoil, it is possible
!                       for all points in an element to be outside, yet boundary points
!                       fall within an element. Check for this, too.
!                       TODO: This is just a linear search through all elements. Should
!                       use a better data structure like a spatial tree.
!                       ----------------------------------------------------------------
!
                        IF( .NOT.e % remove )     THEN

                           DO k = 1, 4
                              nodes(:,k) = e % nodes(k) % node % x
                           END DO
!
!                          --------------------------
!                          See how big the element is
!                          --------------------------
!
                           hMin = SQRT( (nodes(1,1) - nodes(1,4))**2 + (nodes(2,1) - nodes(2,4))**2)
                           DO k = 1, 3
                              h = SQRT( (nodes(1,k+1) - nodes(1,k))**2 + (nodes(2,k+1) - nodes(2,k))**2)
                              hMin = MIN( h, hMin )
                           END DO
!
!                          --------------------------------------------------------------------
!                          See if any of the nodes are inside the element. If the elements
!                          are small compared to the spacing on the curve, linearly interpolate
!                          the curve segment and see if those fall inside the element. HACK:
!                          the decision on how big is too big is arbitrary at the moment.
!                          --------------------------------------------------------------------
!

                           DO j = 0, curveArray % nSegments
!
!                             ---------------------------------------------------
!                             See if one of the boundary points is in the element
!                             ---------------------------------------------------
!
                              IF ( PointInQuad( nodes, curveArray % x(:,j) ) )     THEN
                                 e % remove  = .true.
                                 removedHere = .true.
                                 EXIT
                              END IF
!!
!                             ------------------------------------------------------------------
!                             If not, interpolate (currently see how much smaller the element is
!                             and divide by 5 )
!                             ------------------------------------------------------------------
!
                              x1 = curveArray % x(:,j)
                              IF ( j == curveArray % nSegments  )     THEN
                                 x2 = curveArray % x(:,0)
                              ELSE
                                 x2 = curveArray % x(:,j+1)
                              END IF
                              segmentLength = SQRT( (x2(1) - x1(1))**2 + (x2(2) - x1(2))**2)
                              IF( segmentLength > 0.25_RP*hMin )     THEN
                                 mSteps = NINT(segmentLength/hMin)*5
                                 dz = 1.0_RP/REAL(mSteps, RP)
                                 DO m = 1, mSteps-1
                                    z = x1 + m*dz*(x2 - x1)
                                    IF ( PointInQuad( nodes, z ) )     THEN
                                       e % remove    = .true.
                                       removedHere   = .true.
                                       EXIT
                                    END IF
                                 END DO
                                 IF( e % remove )     EXIT
                              END IF
                           END DO
                        END IF
!
!                       --------------------------------------------
!                       Let the nodes know which curve they are near
!                       --------------------------------------------
!
                        IF ( e % remove .AND. removedHere )     THEN
                          DO k = 1, 4
                              node => e % nodes(k) % node
                              node % bCurveChainID = curveArray % id
                              node % bCurveSide    = OUTSIDE
                          END DO
                        END IF

                  END IF

                  CALL elementIterator % moveToNext()
               END DO

            END DO
         END IF

      END SUBROUTINE MarkExteriorElements
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MarkFloaters( mesh )
!
!        ------------------------------------------------------------------
!        TODO: A floater is an element that is not attached to any other element,
!        which happens occasionally during the marking process
!        ------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh), POINTER :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedList)        , POINTER :: elements        => NULL()
!         TYPE (SMElement)           , POINTER :: e               => NULL()
!         TYPE (SMNode)              , POINTER :: node            => NULL()
         CLASS(FTLinkedListIterator), POINTER :: elementIterator => NULL()
!         CLASS(FTObject)            , POINTER :: obj             => NULL()
!         INTEGER                              :: k
!
         elements        => mesh % elements
         elementIterator => mesh % elementsIterator

         CALL elementIterator % setToStart()
         DO WHILE(.NOT.elementIterator % isAtEnd())
!            obj => elementIterator % object()
!            CALL cast(obj,e)

!            DO k = 1, 4
!
!            END DO
            CALL elementIterator % moveToNext()
         END DO

      END SUBROUTINE MarkFloaters
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MarkInterfaceElements( mesh )
      USE Geometry
      IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh)                 , POINTER :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedList)        , POINTER :: elements        => NULL()
         TYPE (SMElement)           , POINTER :: e               => NULL()
         CLASS(SegmentedCurveArray) , POINTER :: curveArray      => NULL()
         TYPE (SMNode)              , POINTER :: node            => NULL()
         CLASS(FTLinkedListIterator), POINTER :: elementIterator => NULL()
         CLASS(FTObject)            , POINTER :: obj             => NULL()

         INTEGER                              :: k, l, j, i, c, ix, iy, ix0, ix1, iy0, iy1
         REAL(KIND=RP)                        :: w
         REAL(KIND=RP)                        :: nodes(3,4)
         REAL(KIND=RP)                        :: h, hMin
         LOGICAL                              :: isInterfaceElement
         INTEGER                              :: numInside, numOutside, location(4)
         REAL(KIND=RP)                        :: elementBox(4), margin

         IF( .NOT.ASSOCIATED( interfaceCurves ) )    RETURN
!
         elements        => mesh % elements
         elementIterator => mesh % elementsIterator
!
!        --------------------------------------
!        Mark elements and nodes for each curve
!        --------------------------------------
!
         DO l = 1, SIZE(interfaceCurves)
            curveArray => interfaceCurves(l) % curveArray

            CALL elementIterator % setToStart()
!
!           -------------------------
!           Loop through each element
!           -------------------------
!
            DO WHILE ( .NOT.elementIterator % isAtEnd() )
               obj => elementIterator % object()
               CALL castToSMElement(obj,e)
               isInterfaceElement = .false.

               IF( .NOT.e % remove )     THEN
!
!                 -----------------------------------------------------
!                 Mark elements that have both inside and outside nodes
!                 -----------------------------------------------------
!
                  numInside  = 0
                  numOutside = 0
                  location   = UNDEFINED
                  DO k = 1, 4
                     node => e % nodes(k) % node
                     w = curveArray % windingNumber( node % x )
                     IF ( ABS(w) > 0.6_RP ) THEN
                        location(k)                   = INSIDE
                        numInside                     = numInside + 1
                      ELSE IF ( abs(w) <= EPSILON(w) ) THEN
                        location(k)                   = OUTSIDE
                        numOutside                    = numOutside + 1
                      END IF
                  END DO
!
!                 -------------------------------------------------------
!                 Those that do straddle the curve are interface elements
!                 Otherwise, check if the element is too small and do
!                 an interpolation to see if a point is inside.
!                 -------------------------------------------------------
!
                  IF ( numInside > 0 .AND. numOutside > 0 )     THEN
                     isInterfaceElement = .TRUE.
                     DO k = 1,4
                        e % nodes(k) % node % bCurveSide = location(k) ! INSIDE or OUTSIDE
                     END DO

                  ELSE
!
!                    ------------------------------------
!                    Grab the four corners of the element
!                    ------------------------------------
!
                     DO k = 1, 4
                        nodes(:,k) = e % nodes(k) % node % x
                     END DO
!
!                    --------------------------
!                    See how big the element is
!                    --------------------------
!
                     hMin = SQRT( (nodes(1,1) - nodes(1,4))**2 + (nodes(2,1) - nodes(2,4))**2)
                     DO k = 1, 3
                        h = SQRT( (nodes(1,k+1) - nodes(1,k))**2 + (nodes(2,k+1) - nodes(2,k))**2)
                        hMin = MIN( h, hMin )
                     END DO
!
!                    -----------------------------------------------------------
!                    Bounding box of the element. A point that PointInQuad finds
!                    inside lies in it, up to the tolerance of PointInQuad and
!                    rounding, which the margin covers. Curve segments that do
!                    not reach the box are skipped below.
!                    -----------------------------------------------------------
!
                     elementBox = [MINVAL(nodes(1,:)), MAXVAL(nodes(1,:)), MINVAL(nodes(2,:)), MAXVAL(nodes(2,:))]
                     margin     = 1.0E-6_RP*MAX(elementBox(2) - elementBox(1), elementBox(4) - elementBox(3)) &
                                + 8.0_RP*EPSILON(1.0_RP)/MAX(hMin, TINY(hMin))                                &
                                + 8.0_RP*SPACING(MAXVAL(ABS(elementBox)))
                     elementBox = elementBox + margin*[-1.0_RP, 1.0_RP, -1.0_RP, 1.0_RP]
!
!                    ----------------------------------------------------------------
!                    See if the curve passes through the element. Only the segments
!                    in the cells of the curve's segment grid that the box overlaps
!                    can reach it. Without a grid, look at all of the segments.
!                    ----------------------------------------------------------------
!
                     IF ( curveArray % nCellsX == 0 )     THEN
                        DO j = 1, curveArray % nSegments-1
                           IF ( segmentPassesThroughElement(curveArray, j, nodes, elementBox, hMin) )     THEN
                              isInterfaceElement = .TRUE.
                              EXIT
                           END IF
                        END DO
                     ELSE
                        CALL curveArray % cellRangeForBox( elementBox, ix0, ix1, iy0, iy1 )
                        DO iy = iy0, iy1
                           DO ix = ix0, ix1
                              c = ix + (iy - 1)*curveArray % nCellsX
                              DO i = curveArray % cellStart(c), curveArray % cellStart(c+1) - 1
                                 j = curveArray % cellSegments(i)
                                 IF ( j < 1 )     CYCLE
                                 IF ( segmentPassesThroughElement(curveArray, j, nodes, elementBox, hMin) )     THEN
                                    isInterfaceElement = .TRUE.
                                    EXIT
                                 END IF
                              END DO
                              IF ( isInterfaceElement )     EXIT
                           END DO
                           IF ( isInterfaceElement )     EXIT
                        END DO
                     END IF
                  END IF
               END IF !(.NOT.e%remove)
!
!              --------------------------------------------
!              Let the nodes know which curve they are near
!              --------------------------------------------
!
               IF ( isInterfaceElement )     THEN
                  DO k = 1, 4
                     node => e % nodes(k) % node
                     node % bCurveChainID = curveArray % id
                     IF(node % bCurveSide == UNDEFINED )     THEN
                        w = curveArray % windingNumber( node % x )

                        IF ( ABS(w) > 0.6_RP ) THEN
                           node % bCurveSide    = INSIDE
                        ELSE IF ( abs(w) <= EPSILON(w) ) THEN
                           node % bCurveSide    = OUTSIDE
                        END IF
                     END IF
                  END DO
               END IF

               CALL elementIterator % moveToNext()
            END DO !WHILE ( .NOT.elementIterator % isAtEnd )

         END DO !l = 1, SIZE(interfaceCurves)

      END SUBROUTINE MarkInterfaceElements
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION segmentPassesThroughElement( curveArray, j, nodes, elementBox, hMin )
      USE Geometry
      IMPLICIT NONE
!
!     ----------------------------------------------------------------------
!     True if segment j of the curve, from x(:,j) to x(:,j+1), reaches into
!     the element with corners nodes: if its first point is inside, or, if
!     the segment is long compared to the element, a point along it is.
!     Segments that do not reach the box of the element are skipped. HACK:
!     the decision on how long is too long is arbitrary at the moment.
!     ----------------------------------------------------------------------
!
         CLASS(SegmentedCurveArray), POINTER :: curveArray
         INTEGER                             :: j
         REAL(KIND=RP)                       :: nodes(3,4), elementBox(4), hMin
         INTEGER                             :: m, mSteps
         REAL(KIND=RP)                       :: x1(3), x2(3), dz, z(3), segmentLength

         segmentPassesThroughElement = .FALSE.
         IF ( MAX(curveArray % x(1,j), curveArray % x(1,j+1)) < elementBox(1) .OR. &
              MIN(curveArray % x(1,j), curveArray % x(1,j+1)) > elementBox(2) .OR. &
              MAX(curveArray % x(2,j), curveArray % x(2,j+1)) < elementBox(3) .OR. &
              MIN(curveArray % x(2,j), curveArray % x(2,j+1)) > elementBox(4) )     RETURN
!
!        ---------------------------------------------------
!        See if one of the boundary points is in the element
!        ---------------------------------------------------
!
         IF ( PointInQuad( nodes, curveArray % x(:,j) ) )     THEN
            segmentPassesThroughElement = .TRUE.
            RETURN
         END IF
!
!        ------------------------------------------------------------------
!        If not, interpolate (currently see how much smaller the element is
!        and divide by 5 )
!        ------------------------------------------------------------------
!
         x1 = curveArray % x(:,j)
         x2 = curveArray % x(:,j+1)
         segmentLength = SQRT( (x2(1) - x1(1))**2 + (x2(2) - x1(2))**2)

         IF( segmentLength > 0.25_RP*hMin )     THEN
            mSteps = NINT(segmentLength/hMin)*5
            dz = 1.0_RP/REAL(mSteps, RP)
            DO m = 1, mSteps-1
               z = x1 + m*dz*(x2 - x1)
               IF ( PointInQuad( nodes, z ) )     THEN
                  segmentPassesThroughElement = .TRUE.
                  RETURN
               END IF
            END DO
         END IF

      END FUNCTION segmentPassesThroughElement
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetMaterialProperties( mesh, model )
!
!     ---------------------------------------------------------------------
!     This procedure is intended to be called after any topology/element
!     modifications are done. This means the procedure assumes that all
!     elements have nodes either inside, outside or on any interface
!     curves. the temporary interfaceCurves curves array must be available.
!     ---------------------------------------------------------------------
!
      USE Geometry
      IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh)              , POINTER :: mesh
         TYPE (SMModel)             , POINTER :: model
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedList)        , POINTER :: elements        => NULL()
         TYPE (SMElement)           , POINTER :: e               => NULL()
         CLASS(SegmentedCurveArray) , POINTER :: curveArray      => NULL()
         TYPE (SMNode)              , POINTER :: node            => NULL()
         CLASS(FTLinkedListIterator), POINTER :: elementIterator => NULL()
         CLASS(FTObject)            , POINTER :: obj             => NULL()

         INTEGER                              :: k, l
         INTEGER                              :: w
         INTEGER                              :: numInside, numOutside, location(4)
         CLASS(SMChainedCurve)      , POINTER :: chain           => NULL()
         REAL(KIND=RP)                        :: center(3)

         IF( .NOT.ASSOCIATED( interfaceCurves ) )    RETURN
!
         elements => mesh % elements
         elementIterator => mesh % elementsIterator
!
         DO l = 1, SIZE(interfaceCurves)
            curveArray => interfaceCurves(l) % curveArray
!
!           ---------------------------------------------------------------
!           The material ID of the interface was set from the material
!           names in GenerateQuadMeshFromGrid
!           ---------------------------------------------------------------
!
            chain => model % chainWithID( curveArray % id )

            CALL elementIterator % setToStart()
!
!           -------------------------
!           Loop through each element
!           -------------------------
!
            DO WHILE ( .NOT.elementIterator % isAtEnd() )
               obj => elementIterator % object()
               CALL castToSMElement(obj,e)
!
!              -----------------------------------------------------
!              Mark elements that have only inside or boundary nodes
!              -----------------------------------------------------
!
               IF(.NOT.e % remove)     THEN
                  numInside  = 0
                  numOutside = 0
                  location   = UNDEFINED
!
!                 -----------------------------------------------------------------
!                 Count the nodes inside and outside of 'curveArray', the interface
!                 curve treated in this pass of the loop. Nodes that were moved
!                 onto curveArray are not counted. The winding number test uses a
!                 polygon through points of the curve, and a node on the curve
!                 itself is on the outside of the polygon when the curve is convex
!                 and on the inside where it is concave. Simply counting those nodes
!                 would mislabel elements along the concave parts of the curve.
!                 -----------------------------------------------------------------
!
                  DO k = 1, 4
                     node => e % nodes(k) % node
                     IF ( node % distToBoundary == 0.0_RP .AND. &
                          node % bCurveChainID == curveArray % id )     CYCLE

                     w = curveArray % windingNumber( node % x )
                     IF ( abs(w) == 0 ) THEN
                        location(k)                   = OUTSIDE
                        numOutside                    = numOutside + 1
                     ELSE
                        location(k)                   = INSIDE
                        numInside                     = numInside + 1
                     END IF
                  END DO
!
!                 --------------------------------------------------------
!                 If the remaining nodes do not decide, e.g. all nodes are
!                 on the interface, use the center of the element
!                 --------------------------------------------------------
!
                  IF ( numInside == numOutside )     THEN
                     center = 0.0_RP
                     DO k = 1, 4
                        center = center + 0.25_RP*e % nodes(k) % node % x
                     END DO
                     w = curveArray % windingNumber( center )
                     IF ( abs(w) /= 0 )     numInside = numInside + 1
                  END IF
!
!                 ------------------------------------------------------------
!                 Set the material ID of the region inside the interface curve
!                 ------------------------------------------------------------
!
                  IF ( numInside > numOutside )     THEN
                     e % materialID   = chain % materialID
                  END IF
               END IF

               CALL elementIterator % moveToNext()
            END DO !WHILE ( .NOT.elementIterator % isAtEnd )

         END DO !l = 1, SIZE(interfaceCurves)

      END SUBROUTINE SetMaterialProperties
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initBoundaryNode( c, chain, t, bCurveSide, id, node )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP)                  :: t
         CLASS(SMCurve)       , POINTER :: c
         CLASS(SMChainedCurve), POINTER :: chain
         TYPE (SMNode)        , POINTER :: node
         INTEGER                        :: bCurveSide, id
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP) :: p(3)

         p = c % positionAt(t)
         CALL node % initWithLocationAndID(p,id)

         node % whereOnBoundary  = t
         node % gWhereOnBoundary = chain % ChainTForCurveTInCurve( t, c )
         node % distToBoundary   = 0.0_RP
         node % bCurveID         = c % id()
         node % bCurveChainID    = chain % id()
         node % bCurveSide       = bCurveSide
         node % nodeType         = ROW_SIDE !Most will be, change later if necessary

      END SUBROUTINE initBoundaryNode
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION boundaryElementForNodes( elementID, oldNodes, newNodes ) RESULT(e)
         USE Geometry
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                       :: elementID
         TYPE(SMNodePtr), DIMENSION(2) :: oldNodes, newNodes
         TYPE(SMElement), POINTER      :: e
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP)  , DIMENSION(3,2) :: oldX, newX
         REAL(KIND=RP)  , DIMENSION(3)   :: u, v
         TYPE(SMNodePtr), DIMENSION(4)   :: eNodes


         oldX(:,1) = oldNodes(1) % node % x
         oldX(:,2) = oldNodes(2) % node % x
         newX(:,1) = newNodes(1) % node % x
         newX(:,2) = newNodes(2) % node % x

         u = oldX(:,2) - oldX(:,1)
         v = newX(:,1) - oldX(:,1)
         IF( CrossProductDirection(u,v) == UP )     THEN
            eNodes(1) % node => oldNodes(1) % node
            eNodes(2) % node => oldNodes(2) % node
            eNodes(3) % node => newNodes(2) % node
            eNodes(4) % node => newNodes(1) % node
         ELSE
            eNodes(1) % node => oldNodes(1) % node
            eNodes(2) % node => newNodes(1) % node
            eNodes(3) % node => newNodes(2) % node
            eNodes(4) % node => oldNodes(2) % node
         END IF

         ALLOCATE(e)
         CALL e % initWithNodesIDAndType(eNodes, elementID, QUAD )

      END FUNCTION boundaryElementForNodes
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION boundaryElementFor4Nodes( elementID, newNodes ) RESULT(e)
         USE Geometry
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(SMNodePtr) , DIMENSION(4)   :: newNodes
         TYPE (SMElement), POINTER        :: e
         INTEGER                          :: elementID
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP)  , DIMENSION(3)   :: x1, x2, x4
         REAL(KIND=RP)  , DIMENSION(3)   :: u, v
         TYPE(SMNodePtr), DIMENSION(4)   :: eNodes

         ALLOCATE(e)

         x1 = newNodes(1) % node % x
         x2 = newNodes(2) % node % x
         x4 = newNodes(4) % node % x

         u = x2 - x1
         v = x4 - x1
         IF( CrossProductDirection(u,v) == UP )     THEN
            CALL e % initWithNodesIDAndType(newNodes, elementID, QUAD )
         ELSE
            eNodes(1) % node => newNodes(1) % node
            eNodes(2) % node => newNodes(4) % node
            eNodes(3) % node => newNodes(3) % node
            eNodes(4) % node => newNodes(2) % node

            CALL e % initWithNodesIDAndType(eNodes, elementID, QUAD )
         END IF

      END FUNCTION boundaryElementFor4Nodes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FlagEndNodes( list, model )
      IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList), POINTER :: list
         TYPE (SMModel)     , POINTER :: model
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMChainedCurve)         , POINTER   :: boundaryCurve => NULL()
         CLASS(SMCurve)                , POINTER   :: currentCurve  => NULL()
         TYPE (SMNodePtr), DIMENSION(:), POINTER   :: nodeArray     => NULL()
         TYPE (SMNode)                 , POINTER   :: node          => NULL()
         INTEGER                                   :: nodeArraySize
         INTEGER                                   :: j, k, jNode, id, jNode0, jNode1
         REAL(KIND=RP)                             :: t, dt, t0, t1, x, dMin, d
         CHARACTER(LEN=ERROR_EXCEPTION_MSG_LENGTH) :: msg
         LOGICAL                                   :: isInterface

         nodeArray     => GatheredNodes( list )
         nodeArraySize = SIZE(nodeArray)
!
!        -------------------------
!        Set the default node type
!        -------------------------
!
         DO j = 1, nodeArraySize
            nodeArray(j) % node % nodeType = ROW_SIDE
         END DO
!
!        ------------------------
!        Get the associated chain
!        ------------------------
!
         id            = nodeArray(1) % node % bCurveChainID
         boundaryCurve => model % chainWithID(id)
!
!        ----------------------------------------------------------------
!        Interface nodes are not replaced by new boundary nodes, they keep
!        their (chain) parameter values, so the chain parameter of the
!        node pinned to the start of the chain must be set as well
!        ----------------------------------------------------------------
!
         isInterface = boundaryEdgesType(id) == INTERFACE_EDGES
!
!        -----------------------------------------------------------
!        For each joint in the boundary curve, find the closest
!        node in terms of "t" to flag as an end node. This will also
!        set the location on the boundary to that joint location
!
!        TODO: If this gets's expensive, can use a quadtree to
!        bin the nodes so that obviously far nodes aren't checked.
!        -----------------------------------------------------------
!
         dt = 1.0_RP/boundaryCurve % numberOfCurvesInChain
!
!        -------------------
!        Start/End point...
!        TODO: Should bisect
!        -------------------
!
         t0 = HUGE(t0)
         t1 = -10.0_RP !Doesn't really matter...

         jnode0 = -1
         jnode1 = -1
         DO j = 1, nodeArraySize
            t = nodeArray(j) % node % gWhereOnBoundary
            IF( t < t0 )     THEN
               t0     = t
               jNode0 = j
            END IF
            IF( t > t1 )     THEN
               t1     = t
               jNode1 = j
            END IF
         END DO

         IF( t0 < 1.0_RP - t1 )     THEN
           IF ( jNode0 < 0 )     THEN
              WRITE(msg,*) "Joint node 0 not found"
              CALL ThrowErrorExceptionOfType("FlagEndNodes", msg, FT_ERROR_FATAL)
              RETURN
           END IF
           nodeArray(jNode0) % node % whereOnBoundary = 0.0_RP
           nodeArray(jNode0) % node % nodeType        = boundaryCurve % jointClassification(0)
           nodeArray(jNode0) % node % bCurveID        = boundaryCurve % myCurveIDs(1)
           IF ( isInterface )     nodeArray(jNode0) % node % gWhereOnBoundary = 0.0_RP
         ELSE
           IF ( jNode1 < 0 )     THEN
              WRITE(msg,*) "Joint node 1 not found"
              CALL ThrowErrorExceptionOfType("FlagEndNodes", msg, FT_ERROR_FATAL)
              RETURN
           END IF
           nodeArray(jNode1) % node % whereOnBoundary = 1.0_RP
           nodeArray(jNode1) % node % nodeType        = boundaryCurve % jointClassification(0)
           IF ( isInterface )     THEN
              nodeArray(jNode1) % node % gWhereOnBoundary = 1.0_RP
              nodeArray(jNode1) % node % bCurveID         = boundaryCurve % myCurveIDs(boundaryCurve % numberOfCurvesInChain)
           END IF
         END IF
!
!       --------------------
!       All the other points
!       --------------------
!
         DO k = 1, boundaryCurve % numberOfCurvesInChain-1
            t = k*dt
!
!           ----------------------
!           Find the closest point:
!           TODO should bisect
!           ----------------------
!
            dMin  = HUGE(dMin)
            jNode = -1
            DO j = 1, SIZE(nodeArray)
               x = nodeArray(j) % node % gWhereOnBoundary
               d = ABS(t - x)
               IF( d < dMin )     THEN
                  dMin  = d
                  jNode = j
               END IF
            END DO
           IF ( jNode < 0 )     THEN
              WRITE(msg,*) "Joint node not found"
              CALL ThrowErrorExceptionOfType("FlagEndNodes", msg, FT_ERROR_FATAL)
              RETURN
           END IF
!
!           ------------------------
!           Set the node information
!           ------------------------
!
            nodeArray(jNode) % node % bCurveID         = boundaryCurve % myCurveIDs(k+1)
            nodeArray(jNode) % node % whereOnBoundary  = 0.0_RP
            nodeArray(jNode) % node % gWhereOnBoundary = t
            nodeArray(jNode) % node % nodeType         = boundaryCurve % jointClassification(k)
         END DO
!
!        ---------------------------------------------------------------------------------
!        At Row-Reversal points along a curve, it is possible that the row-reversal point
!        is the closest point along the curve to more than one node. To account for this,
!        mark all points within the tolerance used to find the closest point by the joint
!        classification.
!        ---------------------------------------------------------------------------------
!
         DO k = 0, boundaryCurve % COUNT()-1

            IF( boundaryCurve % jointClassification(k) == ROW_REVERSAL )     THEN
               currentCurve => boundaryCurve % curveAtIndex(k+1)
               t            = boundaryCurve % ChainTForCurveTInCurve(0.0_RP,currentCurve)
               t1           = boundaryCurve % ChainTForCurveTInCurve(1.0_RP,currentCurve)

               DO j = 1, SIZE(nodeArray)
                  node => nodeArray(j) % node

                  IF ( ABS(node % gWhereOnBoundary - t) <= minimizationTolerance )     THEN
                     node % whereOnBoundary  = 0.0_RP
                     node % gWhereOnBoundary = t
                     node % nodeType         = ROW_REVERSAL
                  ELSE IF ( ABS(t1 - node % whereOnBoundary) < minimizationTolerance )     THEN
                     node % whereOnBoundary  = 1.0_RP
                     node % gWhereOnBoundary = t1
                     node % nodeType         = ROW_REVERSAL
                  END IF
               END DO

            END IF

         END DO
!
!        --------
!        Clean up
!        --------
!
         DEALLOCATE( nodeArray )

      END SUBROUTINE FlagEndNodes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE setElementBoundaryInfo(project)
!
!     ------------------------------------------------------------------------
!     Go through the mesh and gather the boundary information for each element
!     ------------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(MeshProject) :: project
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER                              :: N
         TYPE (SMMesh)              , POINTER :: mesh
         CLASS(FTLinkedListIterator), POINTER :: iterator
         CLASS(FTObject)            , POINTER :: obj
         TYPE (SMElement)           , POINTER :: e
         TYPE (SMModel)             , POINTER :: model

         N     =  project % runParams % polynomialOrder
         mesh  => project % mesh
         model => project % model

         iterator => mesh % elementsIterator
         CALL iterator % setToStart()

         DO WHILE ( .NOT.iterator % isAtEnd() )

            obj => iterator % object()
            CALL castToSMElement(obj,e)
            CALL ElementBoundaryInfoInit( e % boundaryInfo, N)
            CALL gatherElementBoundaryInfo( e, project )

            CALL iterator % moveToNext()
         END DO

      END SUBROUTINE setElementBoundaryInfo
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE gatherElementBoundaryInfo( e, project  )
!
!     --------------------------------------------------------------------------
!     Gather together the boundary information for the four
!     edges of the element. This info is
!
!     nodeIDs(4)    = integer id # of the 4 corner nodes
!     bCurveFlag(4) = integer ON or OFF of whether a boundary
!                     curve is defined or NOT
!     bCurveName(4) = Name of the 4 boundary curves. Equals "---" IF
!                     the element side is interior,
!     x(3,0:N,4)    = location (x,y) of the j=0:N Chebyshev-Lobatto points
!                     along boundary k = 1,2,3,4. The kth entry will be zero if
!                     bCurveFlag(k) = OFF.
!     -------------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMElement)         , POINTER :: e
         TYPE (MeshProject)        :: project
!
!        ---------------
!        Local Variables
!        ---------------
!
         TYPE (SMModel)       , POINTER :: model
         INTEGER                        :: j, k
         INTEGER                        :: N
         TYPE (SMNode)        , POINTER :: node1 => NULL(), node2 => NULL()
         CLASS(SMCurve)       , POINTER :: c     => NULL()
         CLASS(SMChainedCurve), POINTER :: chain => NULL()
         CLASS(FTMutableObjectArray), POINTER  :: boundaryPolynomials !An alias
         CLASS(MultiSegmentCurve)   , POINTER  :: boundaryPolynomial
         CLASS(FTObject)            , POINTER  :: obj

         REAL(KIND=RP)            :: tStart(4), tEnd(4), t_j, deltaT
         INTEGER                  :: curveId(4)
         CHARACTER(LEN=32)        :: noCurveName(-4:-1) = (/"Right ", "Left  ", "Bottom", "Top   " /)

         model => project % model
         boundaryPolynomials => project % boundaryPolynomialsArray
         N = SIZE(e % boundaryInfo%x,2)-1
!
!        -----
!        Nodes
!        -----
!
         DO k = 1, 4
            node1 => e % nodes(k) % node
            e % boundaryInfo % nodeIDs(k) = node1 % id
         END DO
!
!        -----------------------------------
!        Gather up boundary edge information
!        -----------------------------------
!
         e % boundaryInfo % bCurveName = NO_BC_STRING
         e % boundaryInfo % bCurveFlag = OFF

         DO k = 1, 4
            node1 => e % nodes(edgeMap(1,k)) % node
            node2 => e % nodes(edgeMap(2,k)) % node
!
!           ---------------------------------------------------------------------------
!           See if this edge is on a boundary. One of the two nodes should be
!           a ROW_SIDE, and that one is on a curve rather than the joint of two curves.
!           The edge could be on an outer box, in which case it is a straight line, but
!           still needs boundary name information
!           ---------------------------------------------------------------------------
!
            IF( IsOnBoundaryCurve(node1) .AND. IsOnBoundaryCurve(node2) )     THEN
!
!              -----------------------------------------------------------
!              Mark as on a boundary curve needing interpolant information
!              -----------------------------------------------------------
!
               e % boundaryInfo % bCurveFlag(k) = ON
               IF( node2 % whereOnBoundary == 0.0_RP .OR. node2 % whereOnBoundary == 1.0_RP )     THEN
                  c             => model % curveWithID(node1 % bCurveID, chain)
                  curveID(k)    = chain % id()
               ELSE
                  c             => model % curveWithID(node2 % bCurveID, chain)
                  curveID(k)    = chain % id()
               END IF

               e % boundaryInfo % bCurveName(k) = c % curveName()
               tStart(k)                        = node1 % gWhereOnBoundary
               tEnd(k)                          = node2 % gWhereOnBoundary

            ELSE IF ( IsOnOuterBox(node1) .AND. IsOnOuterBox(node2) )     THEN
!
!              --------------------------------------------------------------
!              Only mark the boundary names for output, no interpolant needed
!              --------------------------------------------------------------
!
               IF( node1 % nodeType == CORNER_NODE )     THEN
                  e % boundaryInfo % bCurveName(k) = noCurveName(node2 % bCurveID)
               ELSE
                  e % boundaryInfo % bCurveName(k) = noCurveName(node1 % bCurveID)
               END IF

            END IF
         END DO
!
!        --------------------------------------------
!        Construct boundary information
!        Use a Chebyshev interpolant for the points.
!        --------------------------------------------
!
         DO k = 1, 4

            IF( e % boundaryInfo % bCurveFlag(k) == ON )     THEN ! Use boundary curves to compute interpolant

              obj => boundaryPolynomials % objectAtIndex(curveID(k))
              CALL castObjToMultiSegmentCurve(obj,boundaryPolynomial)

              deltaT = tEnd(k) - tStart(k)
              IF( deltaT > maxParameterChange )     THEN !Crossing over the start
                 deltaT = deltaT - 1.0_RP
              ELSE IF (deltaT < -maxParameterChange ) THEN
                 deltaT = 1.0_RP + deltaT
              END IF


              DO j = 0, N

                  t_j = tStart(k) + deltaT*(1.0_RP - COS(j*PI/N))/2.0_RP
                  IF( t_j > 1.0_RP )     THEN
                     t_j = t_j - 1.0_RP
                  ELSE IF( t_j < 0.0_RP )     THEN
                     t_j = t_j + 1.0_RP
                  END IF

                  e % boundaryInfo % x(:,j,k) = boundaryPolynomial % PositionAt( t_j )

                END DO
             ELSE ! Use a straight line between end nodes

               node1 => e % nodes(edgeMap(1,k)) % node
               node2 => e % nodes(edgeMap(2,k)) % node

               DO j = 0, N
                  t_j = (1.0_RP - COS(j*PI/N))/2.0_RP
                  e % boundaryInfo % x(:,j,k) = (1.0_RP - t_j)*node1 % x + t_j*node2 % x
               END DO

             END IF
         END DO

      END SUBROUTINE gatherElementBoundaryInfo
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MakeBoundaryInfoRightHanded(bInfo, a, b, c)
         USE LineReflectionModule
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (ElementBoundaryInfo) :: bInfo
         REAL(KIND=RP)              :: a, b, c
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                    :: idSwap
         CHARACTER(LEN=32)          :: strSwap
         REAL(KIND=RP), ALLOCATABLE :: x(:,:,:)
         INTEGER                    :: j, k, N
!
!        ---------------------
!        Two nodes swap places
!        ---------------------
!
         idSwap             = bInfo % nodeIDs(2)
         bInfo % nodeIDs(2) = bInfo % nodeIDs(4)
         bInfo % nodeIDs(4) = idSwap
!
!        --------------------------
!        All four edges swap places
!        --------------------------
!
         idSwap                = bInfo % bCurveFlag(2)
         bInfo % bCurveFlag(2) = bInfo % bCurveFlag(3)
         bInfo % bCurveFlag(3) = idSwap

         idSwap                = bInfo % bCurveFlag(1)
         bInfo % bCurveFlag(1) = bInfo % bCurveFlag(4)
         bInfo % bCurveFlag(4) = idSwap
!
!        --------------------------------
!        All four edges swap places names
!        --------------------------------
!
         strSwap               = bInfo % bCurveName(2)
         IF(strSwap .ne. NO_BC_STRING) strSwap = TRIM(strSwap)  //"_R"
         bInfo % bCurveName(2) = bInfo % bCurveName(3)
         IF(bInfo % bCurveName(2) .ne. NO_BC_STRING) bInfo % bCurveName(2) = TRIM(bInfo % bCurveName(2))//"_R"
         bInfo % bCurveName(3) = strSwap

         strSwap               = bInfo % bCurveName(1)
         IF(strSwap .ne. NO_BC_STRING) strSwap = TRIM(strSwap)  //"_R"
         bInfo % bCurveName(1) = bInfo % bCurveName(4)
         IF(bInfo % bCurveName(1) .ne. NO_BC_STRING) bInfo % bCurveName(1) = TRIM(bInfo % bCurveName(1))//"_R"
         bInfo % bCurveName(4) = strSwap
!
!        --------------------------------------------------------
!        Boundary curves swap and change direction. At the same
!        time, might as well perform the reflection on the points
!        --------------------------------------------------------
!
         IF ( ALLOCATED(bInfo % x) )     THEN

            N = UBOUND(bInfo % x,2)
            ALLOCATE(x, source = bInfo % x)

            DO k = 1, 3, 2
               DO j = 0,N
                  x(:,j,k) = reflectAboutLine(bInfo % x(:,j,k), a, b, c)
               END DO
            END DO
            DO k = 2, 4, 2
               DO j = 0,N
                  x(:,j,k) = reflectAboutLine(bInfo % x(:,N-j,k), a, b, c)
               END DO
            END DO

            bInfo % x(:,:,4) = x(:,:,1)
            bInfo % x(:,:,1) = x(:,:,4)
            bInfo % x(:,:,3) = x(:,:,2)
            bInfo % x(:,:,2) = x(:,:,3)

         END IF


      END SUBROUTINE MakeBoundaryInfoRightHanded
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeFacePatches(project)
!
!     ---------------------------------------------------------------
!     Compute the boundary and interior face patch interpolant points
!     ---------------------------------------------------------------
!
         USE CurveInterpolantClass
         USE TransfiniteMapClass
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(MeshProject)     :: project
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                              :: N, j, k
         TYPE (SMMesh)              , POINTER :: mesh
         CLASS(FTLinkedListIterator), POINTER :: iterator
         CLASS(FTObject)            , POINTER :: obj
         TYPE (SMElement)           , POINTER :: e
         TYPE(TransfiniteQuadMap)             :: quadMap

         TYPE(CurveInterpolant)       , POINTER     :: boundaryCurves(:)
         REAL(KIND=RP), DIMENSION(:)  , ALLOCATABLE :: nodes
         REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE :: values
!
!        -------------------
!        Interior face patch
!        -------------------
!
         N     =  project % runParams % polynomialOrder
         mesh  => project % mesh

         iterator => mesh % elementsIterator
         CALL iterator % setToStart()

         ALLOCATE( boundaryCurves(4) )
         ALLOCATE( nodes(0:N) )
         ALLOCATE( values(0:N,3) )

         DO j = 0, N
            nodes(j) = -COS(j*PI/N)
         END DO
         values = 0.0_RP
         DO k = 1, 4
            CALL Construct( boundaryCurves(k), N, nodes, values )
         END DO
         quadMap = NewTransfiniteQuadMap( boundaryCurves )

         DO WHILE ( .NOT.iterator % isAtEnd() )

            obj => iterator % object()
            CALL castToSMElement(obj,e)

            e % N = N
            CALL ComputeElementFacePatch(e, quadMap, nodes, N)

            CALL iterator % moveToNext()
         END DO

         ! Deallocate all the internal memory on each curve
         ! then release the other memory
         DO k = 1, 4
            CALL Destruct( boundaryCurves(k) )
         END DO
         DEALLOCATE( boundaryCurves )

      END SUBROUTINE ComputeFacePatches
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE Perform2DMeshTransformations(project)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(MeshProject)     :: project
!
!        ---------------
!        Local Variables
!        ---------------
!
         TYPE (SMMesh)              , POINTER :: mesh

         mesh => project % mesh

         IF ( .NOT. isIdentityRotation(self = project % rotationTransformer) )     THEN
            CALL RotationTransformMesh(mesh = mesh, rotationTransformer = project % rotationTransformer)
         END IF

         IF ( .NOT. isIdentityScale(self = project % scaleTransformer) )     THEN
            CALL scaleMesh(mesh = mesh, scalingTransformer = project % scaleTransformer)
         END IF

      END SUBROUTINE Perform2DMeshTransformations
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReflectMesh( mesh, symmetryCurve)
         USE ConnectionsModule
         USE LineReflectionModule
         USE MeshCleaner, ONLY: MakeElement_RightHanded
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(SMMesh)  , POINTER :: mesh
         CLASS(SMCurve), POINTER :: symmetryCurve
!
!        ---------------
!        Local Variables
!        ---------------
!
         REAL(KIND=RP)                       :: x0(3), x1(3), x(3)
         REAL(KIND=RP)                       :: a, b, c
         INTEGER                             :: nodeID, elemID
         INTEGER                             :: bCurveID
         INTEGER                             :: errorCode
         CLASS(FTLinkedList)       , POINTER :: newNodes
         CLASS(FTLinkedList)       , POINTER :: savedElements
         TYPE(FTLinkedListIterator), POINTER :: nodeItr, elemItr
         CLASS(FTObject)           , POINTER :: obj
         TYPE(SMNode)              , POINTER :: oldNode, newNode
         TYPE(SMElement)           , POINTER :: oldElement, newElement, e
         INTEGER                             :: i, j
         CHARACTER(SM_CURVE_NAME_LENGTH)     :: str
!
!        -------------------------------------------------
!        Determine the mapping function for the reflection
!        -------------------------------------------------
!
         x0 = symmetryCurve % positionAt(t = 0.0_RP)
         x1 = symmetryCurve % positionAt(t = 1.0_RP)
         CALL ComputeLineCoefs(x0,x1,a,b,c)

         bCurveID = symmetryCurve % id()
!
!        -----------------------------------------------------------
!        Copy current elements to save for adding back into the mesh
!        While we're at it, flag nodes along the symmetry boundary
!        with the symmetry boundary ID, since corner points might
!        be associated with another boundary.
!        -----------------------------------------------------------
!
         ALLOCATE(savedElements)
         CALL savedElements % init()

         elemItr => mesh % elementsIterator
         elemID  = newElementID(mesh)

         CALL elemItr % setToStart()

         DO WHILE(.NOT. elemItr % isAtEnd())

            obj => elemItr % object()
            CALL castToSMElement(obj,oldElement)

            newElement => SMElementCopy(oldElement)
            obj        => newElement
            newElement % id = elemID

            CALL savedElements % add(obj)
            CALL release(obj)
!
!           -----------------------------------------------------
!           Flag symmetry points and clear the associated edge as
!           being a boundary anymore.
!           -----------------------------------------------------
!
            DO j = 1, 4
               str = oldElement % boundaryInfo % bCurveName(j)
               CALL toLower(str)
               IF ( str == SYMMETRY_CURVE_NAME )     THEN
                  oldElement % nodes(edgeMap(1,j)) % node % bCurveID = bCurveID
                  oldElement % nodes(edgeMap(2,j)) % node % bCurveID = bCurveID
                  oldElement % boundaryInfo % bCurveName(j)          = NO_BC_STRING
                  newElement % boundaryInfo % bCurveName(j)          = NO_BC_STRING
                  oldElement % boundaryInfo % bCurveFlag(j)          = NONE
                  newElement % boundaryInfo % bCurveFlag(j)          = NONE
!
!                 -------------------------------------------------------------------------
!                 Mark that the nodes that were on the symmetry boundary are off of it now.
!                 The amount doesn't matter.
!                 -------------------------------------------------------------------------
!
                  oldElement % nodes(edgeMap(1,j)) % node % distToBoundary = 1.0_RP
                  oldElement % nodes(edgeMap(2,j)) % node % distToBoundary = 1.0_RP
                  newElement % nodes(edgeMap(1,j)) % node % distToBoundary = 1.0_RP
                  newElement % nodes(edgeMap(2,j)) % node % distToBoundary = 1.0_RP
               END IF
            END DO

            elemID = elemID + 1
            CALL elemItr % MoveToNext()

         END DO
!
!        ---------------------------------------------------
!        Copy nodes, except those along the symmetryCurve
!        and reassign node pointers in the elements
!        ---------------------------------------------------
!
         CALL deallocateNodeToElementConnections
         !(Ignore error code since ReflectMesh won't be called otherwise.)
         CALL makeNodeToElementConnections(mesh, errorCode)

         ALLOCATE(newNodes)
         CALL newNodes % init()

         nodeItr => mesh % nodesIterator
         nodeID  = newNodeID(mesh)

         CALL nodeItr % setToStart()

         DO WHILE( .NOT. nodeItr % isAtEnd() )

            CALL castToSMNode(nodeItr % object(),oldNode)
!
            IF ( oldNode % bCurveID /= bCurveID )     THEN !Skip over nodes along the symmetry axis
!
!              -----------------------------------------------------
!              Make a new node that is the reflection of the old one
!              and add it to the list.
!              -----------------------------------------------------
!
               ALLOCATE(newNode)
               CALL newNode % init()
               CALL copyNodeType(oldNode,newNode)

               newNode % x  = reflectAboutLine(oldNode % x, a, b, c)
               newNode % id = nodeID
               nodeID       = nodeID + 1

               obj => newNode
               CALL newNodes % add(obj)
               CALL release(obj)
!
!              ---------------------------------------------------------------------
!              Reassign pointers in the elements that use the oldNode to the newNode
!              ---------------------------------------------------------------------
!
               DO i = 1, numElementsForNode(oldNode % id)
                  e => elementsForNodes(i, oldNode % id) % element
                  DO j = 1, e % eType
                     IF ( ASSOCIATED(e % nodes(j) % node, oldNode) )     THEN
                        CALL releaseSMNode(e % nodes(j) % node)
                        e % nodes(j) % node => newNode
                        e % boundaryInfo % nodeIDs(j) = newNode % id
                        CALL newNode % retain()
                     END IF
                  END DO
               END DO
            END IF

            CALL nodeItr % moveToNext()
         END DO
!
!        ---------------------------------------------------------------
!        Finish up by
!        1. Adding the new nodes to the mesh's node list
!        2. Making the flipped elements right handed
!        3. Fixing the flipped element's boundaryInfo block due to #2
!        4. Flipping the element's face patch
!        5. Adding back the original elements to the mesh
!        6. Rebuild the edge list
!        ---------------------------------------------------------------
!
         CALL mesh % nodes % addObjectsFromList(newNodes)
         CALL releaseFTLinkedListClass(newNodes)

         CALL elemItr % setToStart()
         DO WHILE(.NOT. elemItr % isAtEnd())

            obj => elemItr % object()
            CALL castToSMElement(obj,e)
            DO j = 1, e % eType
               e % boundaryInfo % nodeIDs(j) = e % nodes(j) % node % id
            END DO
            IF ( .NOT. elementIsRightHanded(e) )     THEN
               CALL MakeElement_RightHanded(e)
               CALL MakeBoundaryInfoRightHanded(e % boundaryInfo, a, b, c)
            END IF

            IF ( ALLOCATED(e % xPatch) )     THEN
               DO j = 0, UBOUND(e % xPatch,3)
                  DO i = 0, UBOUND(e % xPatch, 2)
                     x = reflectAboutLine(e % xPatch(:,i,j),a,b,c)
                     e % xPatch(:,i,j) = x
                  END DO
               END DO
            END IF

            CALL elemItr % moveToNext()
         END DO

         CALL mesh % elements % addObjectsFromList(savedElements)

         CALL releaseFTLinkedListIterator(mesh % edgesIterator)
         CALL releaseFTLinkedList(mesh % edges)
         ALLOCATE(mesh % edges)
         ALLOCATE(mesh % edgesIterator)
         CALL mesh % edges % init()
         CALL mesh % edgesIterator % initWithFTLinkedList(mesh % edges)
         CALL buildEdgeList(mesh)
!
!        --------
!        Clean up
!        --------
!
         CALL releaseFTLinkedListClass(savedElements)
         CALL deallocateNodeToElementConnections

      END SUBROUTINE ReflectMesh
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeElementFacePatch(e, quadMap, nodes, N)
         USE TransfiniteMapClass
         USE CurveInterpolantClass
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMElement), POINTER :: e
         INTEGER                   :: N
         TYPE(TransfiniteQuadMap)  :: quadMap
         REAL(KIND=RP)             :: nodes(0:N)
!
!        ---------------
!        Local Variables
!        ---------------
!
         REAL(KIND=RP) :: values(0:N,3)
         INTEGER       :: i, j, k

         IF(.NOT. ALLOCATED(e % xPatch)) ALLOCATE(e % xPatch(3,0:N,0:N))
!
!        -------------------
!        Set up the quad map
!        -------------------
!
         DO k = 1, 4
            DO j = 0, N
               DO i = 1,3
                  values(j,i) = e % boundaryInfo % x(i,j,k)
               END DO
            END DO

            CALL SetValues( quadMap % boundaryCurves(k), values )
         END DO
         nodes = quadMap % boundaryCurves(1) % nodes

         DO j = 0, N
            DO i = 0, N
               CALL EvaluateTransfiniteMapAt(this = quadMap,  &
                                             xi   = nodes(i), &
                                             eta  = nodes(j), &
                                             res  = e % xPatch(:,i,j) )
            END DO
         END DO

      END SUBROUTINE ComputeElementFacePatch
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE scaleMesh(mesh, scalingTransformer)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh)        :: mesh
         TYPE(ScaleTransform) :: scalingTransformer
!
!        ---------------
!        Local Variables
!        ---------------
!
         TYPE (FTLinkedListIterator), POINTER :: nodeIterator, elementIterator
         CLASS(FTObject)            , POINTER :: obj  => NULL()
         TYPE (SMNode)              , POINTER :: node => NULL()
         TYPE (SMElement)           , POINTER :: e
         REAL(KIND=RP)                        :: xFormed(3)
         INTEGER                              :: i, j, k, N
!
!        ---------------
!        Scale the nodes
!        ---------------
!
         nodeIterator => mesh % nodesIterator
         CALL nodeIterator % setToStart()
         DO WHILE( .NOT.nodeIterator % isAtEnd() )
            obj => nodeIterator % object()
            CALL castToSMNode(obj,node)

            xFormed = PerformScaleTransformation(x              = node % x, &
                                                 transformation = scalingTransformer)
            node % x = xFormed
            CALL nodeIterator % moveToNext()
         END DO
!
!        ---------------------------
!        Scale element-stored values
!        ---------------------------
!
         elementIterator => mesh % elementsIterator
         CALL elementIterator % setToStart()

         DO WHILE(.NOT.elementIterator % isAtEnd())

            obj => elementIterator % object()
            CALL castToSMElement(obj,e)
            N = e % N
!
!           -------------
!           Patch scaling
!           -------------
!
            DO j = 0, N
               DO i = 0, N
                  xFormed = PerformScaleTransformation(x              = e % xPatch(:,i,j), &
                                                       transformation = scalingTransformer)
                  e % xPatch(:,i,j) = xFormed
               END DO
            END DO
!
!           ----------------------
!           Boundary point scaling
!           ----------------------
!
            DO k = 1,4
               DO j = 0, N
                  xFormed = PerformScaleTransformation(x              = e % boundaryInfo % x(:,j,k), &
                                                       transformation = scalingTransformer)
                  e % boundaryInfo % x(:,j,k) = xFormed
               END DO
            END DO

            CALL elementIterator % moveToNext()
         END DO

      END SUBROUTINE scaleMesh
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RotationTransformMesh(mesh, rotationTransformer)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh)         :: mesh
         TYPE(RotationTransform) :: rotationTransformer
!
!        ---------------
!        Local Variables
!        ---------------
!
         TYPE (FTLinkedListIterator), POINTER :: nodeIterator, elementIterator
         CLASS(FTObject)            , POINTER :: obj  => NULL()
         TYPE (SMNode)              , POINTER :: node => NULL()
         TYPE (SMElement)           , POINTER :: e
         REAL(KIND=RP)                        :: xFormed(3)
         INTEGER                              :: i, j, k, N
!
!        ---------------
!        Scale the nodes
!        ---------------
!
         nodeIterator => mesh % nodesIterator
         CALL nodeIterator % setToStart()
         DO WHILE( .NOT.nodeIterator % isAtEnd() )
            obj => nodeIterator % object()
            CALL castToSMNode(obj,node)

            xFormed = PerformRotationTransform(x              = node % x, &
                                               transformation = rotationTransformer)
            node % x = xFormed
            CALL nodeIterator % moveToNext()
         END DO
!
!        ---------------------------
!        Scale element-stored values
!        ---------------------------
!
         elementIterator => mesh % elementsIterator
         CALL elementIterator % setToStart()

         DO WHILE(.NOT.elementIterator % isAtEnd())

            obj => elementIterator % object()
            CALL castToSMElement(obj,e)
            N = e % N
!
!           -------------
!           Patch scaling
!           -------------
!
            DO j = 0, N
               DO i = 0, N
                  xFormed = PerformRotationTransform(x              = e % xPatch(:,i,j), &
                                                     transformation = rotationTransformer)
                  e % xPatch(:,i,j) = xFormed
               END DO
            END DO
!
!           ----------------------
!           Boundary point scaling
!           ----------------------
!
            DO k = 1,4
               DO j = 0, N
                  xFormed = PerformRotationTransform(x              = e % boundaryInfo % x(:,j,k), &
                                                     transformation = rotationTransformer)
                  e % boundaryInfo % x(:,j,k) = xFormed
               END DO
            END DO

            CALL elementIterator % moveToNext()
         END DO

      END SUBROUTINE RotationTransformMesh
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeBoundaryPolynomials( project, chainNodesArray, performOptimization)
!
!     -----------------------------------------------------------------
!     Generate multisegment curves that span the curve segments in the
!     chainNodesArray. This procedure creates polynomial approximations
!     to the model's curves referenced in allCurves and stores them in
!     the project's boundaryPolynomialsArray. The approximation error
!     can be computed from the difference between the two.
!     -----------------------------------------------------------------
!
         USE MultiSegmentNodalCurveClass
         USE MultiSegmentModalCurveClass
         USE CurveOptimization
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(MeshProject)     :: project
         TYPE(JaggedNodeArray) :: chainNodesArray(:) !Array of arrays of nodes along the boundary
         LOGICAL               :: performOptimization
!
!        ---------------
!        Local variables
!        ---------------
!

         CLASS(SMModel)                , POINTER  :: model               !An alias
         CLASS(FTMutableObjectArray)   , POINTER  :: boundaryPolynomials !An alias
         TYPE(ObjectPointerWrapper)    , POINTER  :: modelChains(:)      !An alias
         CLASS(FTObject)               , POINTER  :: obj                 !An Alias

         CLASS(SMChainedCurve)         , POINTER  :: modelChain
         CLASS(MultiSegmentNodalCurve) , POINTER  :: boundaryPolynomial
         CLASS(MultiSegmentModalCurve) , POINTER  :: optimizedCurve
         CLASS(SMCurve)                , POINTER  :: crv, crvPtr

         TYPE(OptimizerOptions)                   :: options
         REAL(KIND=RP)              , ALLOCATABLE :: nodeTs(:), chebyPoints(:)
         REAL(KIND=RP)              , ALLOCATABLE :: values(:,:)
         INTEGER                    , ALLOCATABLE :: ends(:)

         INTEGER                                  :: j, N, m, shift
         INTEGER                                  :: nChains
!
!        -----------
!        Set aliases
!        -----------
!
         model               => project % model
         modelChains         => model % allChains
         boundaryPolynomials => project % boundaryPolynomialsArray

         nChains = model % numberOfChains()
         N       = project % runParams % polynomialOrder
!
!        ---------------------------------------
!        Allocate Chebyshev Gauss-Lobatto points
!        for use with non-optimized boundary
!        curves.
!        ---------------------------------------
!
         ALLOCATE(chebyPoints(0:N))
         ALLOCATE(values(0:N,3))
         DO m = 0, N
            chebyPoints(m) = (1.0_RP - COS(m*PI/N))/2.0_RP
         END DO
!
!        ---------------------------------------------------------------------------------
!        For each chained curve, generate a new curve from the
!        curves in the source chain. The new curves added to the chain includes
!        (a) pointers to a new SMCurve of type MultiSegmentModalCurve if the curve
!            is to be optimized. This may include coalescing several curves in the chain
!            if the user requests them to be connected
!        (b) pointers to a new MultiSegmentNodalCurve subclasses for polynomials defined
!            between two pairs of nodes along boundaries.
!        ---------------------------------------------------------------------------------
!
         DO j = 1, nChains ! For each boundary curve chain
            boundaryPolynomial => NULL()
            optimizedCurve     => NULL()
!
!           -----------------------------------------------------
!           Get the curve chain from the model.
!           modelChain is the exact chain.
!           boundaryPolynomial is its approximation by a (PW) polynomial
!           which is saved in the boundaryPolynomialsArray of the
!           project at the same index.
!           -----------------------------------------------------
!
            obj => modelChains(j) % object
            CALL castToSMChainedCurve(obj, modelChain)
!
!           ----------------------------------------------------
!           Collect all the parametric locations along the chain
!           ----------------------------------------------------
!
            CALL GatherNodeTsAndEnds(chainNodesArray = chainNodesArray(j) % array, &
                                     nCurves         = modelChain % COUNT(),       &
                                     nodeTs          = nodeTs,                     &
                                     ends            = ends,                       &
                                     shift           = shift)
!
!           -------------------------------------------------------------
!           Create a polynomial approximation for each curve in the chain
!           depending on if optimization is requested or not.
!           -------------------------------------------------------------
!
            crv => modelChain
            IF ( modelChain % optimization /= NONE .AND. performOptimization)     THEN

               CALL SetDefaultOptions(options)
               options % whichNorm = modelChain % optimization
               CALL OptimizeCurve(curve              = crv,                     &
                                  polyOrder          = N,                       &
                                  cuts               = nodeTs,                  &
                                  breakIndices       = ends,                    &
                                  options            = options,                 &
                                  newName            = modelChain % curveName(),&
                                  newID              = modelChain % id(),       &
                                  optimized          = crvPtr)
               CALL castToMultiSegmentModalCurve(crvPtr, optimizedCurve)
               obj => optimizedCurve
               CALL boundaryPolynomials % addObject(obj)
            ELSE
               ALLOCATE(boundaryPolynomial)
               CALL boundaryPolynomial % ConstructMultiSegmentNodalCurve(crv, nodeTs, N, &
                                                      modelChain % curveName(), modelChain % id() )
               obj => boundaryPolynomial
               CALL boundaryPolynomials % addObject(obj)
            END IF
!
!           -------
!           Cleanup
!           -------
!
            DEALLOCATE(ends)
            DEALLOCATE(nodeTs)
            IF(ASSOCIATED(boundaryPolynomial)) CALL releaseMultiSegmentNodalCurve(boundaryPolynomial)
            IF(ASSOCIATED(optimizedCurve))     CALL releaseMultiSegmentModalCurve(optimizedCurve)

         END DO !All boundary curves

      END SUBROUTINE ComputeBoundaryPolynomials
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GatherNodeTsAndEnds( chainNodesArray, nCurves, nodeTs, ends, shift )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(SMNodePtr)            :: chainNodesArray(:) !Array of nodes along a boundary
         REAL(KIND=RP), ALLOCATABLE :: nodeTs(:)
         INTEGER      , ALLOCATABLE :: ends(:)
         INTEGER                    :: nCurves
         INTEGER                    :: shift
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER       :: nNodes
         INTEGER       :: k, m
         REAL(KIND=RP) :: dt, t
         REAL(KIND=RP) :: nodeTol = 1.0E-12_RP ! TODO: put into a constant.

         nNodes = SIZE(chainNodesArray)
         ALLOCATE(nodeTs(0:nNodes))

         IF ( chainNodesArray(1) % node % gWhereOnBoundary == 0.0_RP )     THEN
            DO k = 0, nNodes-1
               nodeTs(k) = chainNodesArray(k+1) % node % gWhereOnBoundary
            END DO
            nodeTs(nNodes) = 1.0_RP
            shift = 1
         ELSE
            nodeTs(0) = 0.0_RP
            DO k = 1, nNodes
               nodeTs(k) = chainNodesArray(k) % node % gWhereOnBoundary
            END DO
            shift = 0
         END IF
!
!        ---------------------------------------------
!        Find the indices of the ends of each segment
!        in the model chain.
!        ---------------------------------------------
!
         dt = 1.0_RP/REAL(nCurves, RP)

         ALLOCATE(ends(0:nCurves), source = 0)
         ends(0)       = 0
         ends(nCurves) = nNodes

         m = 1
         DO k = 1, nNodes-1
            t = m*dt
            IF ( ABS(nodeTs(k) - t) <= nodeTol )     THEN
               ends(m)   = k
               nodeTs(k) = t
               m = m + 1
            END IF
         END DO

      END SUBROUTINE GatherNodeTsAndEnds
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE MakeMeshSymmetric(project, symmetryCurve)
      USE MeshCleaner, ONLY: ImproveElementShapes
      USE LineReflectionModule
      IMPLICIT NONE
!
!     -----------------------------------------------------------
!     Perform symmetric transform if there is a symmetry boundary
!     -----------------------------------------------------------
!
!
!      ---------
!      Arguments
!      ---------
!
       CLASS(SMCurve)      , POINTER :: symmetryCurve
       CLASS( MeshProject ), POINTER :: project
!
!      ---------------
!      Local variables
!      ---------------
!
       INTEGER       :: errorCode
       REAL(KIND=RP) :: x0(3), x1(3), a, b, c

       IF ( allSymmetryCurvesAreColinear(project % model) )     THEN
          CALL ReflectMesh(project % mesh, symmetryCurve)
!
!         --------------------------------
!         Smooth mesh (again) if requested
!         --------------------------------
!
          IF(ASSOCIATED(project % smoother))     THEN
            IF(PrintMessage) PRINT *, "   Begin Smoothing (after reflection across a symmetry boundary)..."
            CALL project % smoother % smoothMesh(  project % mesh, project % model, errorCode )
            IF(PrintMessage) PRINT *, "   Smoothing done."
          END IF
!
!         -------------------------------------------------------------
!         The smoothing may undo the shape improvements made before the
!         reflection, so improve the shapes again, in mirror pairs
!         -------------------------------------------------------------
!
          x0 = symmetryCurve % positionAt(t = 0.0_RP)
          x1 = symmetryCurve % positionAt(t = 1.0_RP)
          CALL ComputeLineCoefs(x0, x1, a, b, c)
          CALL ImproveElementShapes( project % mesh, errorCode, symmetryLine = [a, b, c] )
       ELSE
           CALL ThrowErrorExceptionOfType(poster = "HOHQMesh", &
                                          msg    = "A symmetry curve is is not straight or colinear. Ignoring...", &
                                          typ    = FT_ERROR_WARNING)
       END IF

   END SUBROUTINE MakeMeshSymmetric
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE ComputeBoundaryApproximations(project, optimize)
      IMPLICIT NONE

      CLASS(MeshProject), POINTER :: project
      LOGICAL                     :: optimize
!
!     ---------------
!     Local Variables
!     ---------------
!
      TYPE(JaggedNodeArray), ALLOCATABLE :: chainNodesArray(:)
      TYPE (SMModel)       , POINTER     :: model
      INTEGER                            :: numBoundaryChains
!
!     -----------------------------------
!     Gather and set boundary information
!     and from that, create boundary
!     polynomial approximations to the
!     boundary chains.
!     -----------------------------------
!
      model => project % model
      numBoundaryChains = model % numberOfChains()
      ALLOCATE(chainNodesArray(numBoundaryChains))
      CALL SortBoundaryNodesToChains(project % mesh % nodesIterator, &
                                     numBoundaryChains, chainNodesArray)
      CALL ComputeBoundaryPolynomials( project, chainNodesArray, optimize)
      CALL setElementBoundaryInfo(project)
!
!     -----------------------------------------
!     Compute the patch of interpolated points
!     inside the elements
!     -----------------------------------------
!
      CALL ComputeFacePatches(project)

      DEALLOCATE(chainNodesArray)

   END SUBROUTINE ComputeBoundaryApproximations
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RedistributeNodesAlongBoundaries( mesh, model )
         USE SMModelClass
         IMPLICIT NONE

         TYPE(SMMesh)  :: mesh
         TYPE(SMModel) :: model
!
!        ---------------
!        Local Variables
!        ---------------
!
         TYPE(JaggedNodeArray), ALLOCATABLE :: chainNodesArray(:)
         INTEGER                            :: numBoundaryChains
         REAL(KIND=RP)        , ALLOCATABLE :: nodeTs(:)
         INTEGER              , ALLOCATABLE :: ends(:)

         TYPE(ObjectPointerWrapper), POINTER  :: modelChains(:)      !An alias
         CLASS(FTObject)           , POINTER  :: obj                 !An Alias
         CLASS(SMChainedCurve)     , POINTER  :: modelChain

         INTEGER :: nCurves
         INTEGER :: j, k, shift
!
!        --------------------------------
!        Gather boundary node information
!        --------------------------------
!
         numBoundaryChains = model % numberOfChains()
         ALLOCATE(chainNodesArray(numBoundaryChains))
         CALL SortBoundaryNodesToChains(mesh % nodesIterator, &
                                        numBoundaryChains, chainNodesArray)
!
!        -----------------------------
!        For each boundary curve chain
!        -----------------------------
!
         modelChains => model % allChains

         DO j = 1, numBoundaryChains
            obj => modelChains(j) % object
            CALL castToSMChainedCurve(obj, modelChain)
            nCurves = modelChain % COUNT()
!
!           ------------------------------------------------------------
!           A paved interface already has its nodes placed by size, with
!           nodes on its corners. Smoothing them would move those off.
!           ------------------------------------------------------------
!
            k = modelChain % id()
            IF ( k >= 0 .AND. k <= MAX_PAVED_CHAIN_ID )     THEN
               IF ( chainWasPaved(k) )     CYCLE
            END IF

            CALL GatherNodeTsAndEnds(chainNodesArray = chainNodesArray(j) % array, &
                                     nCurves         = nCurves,                    &
                                     nodeTs          = nodeTs,                     &
                                     ends            = ends,                       &
                                     shift           = shift)
!
!           ----------------------------------------------------------------
!           Smooth the node locations along the boundary within each segment
!           ----------------------------------------------------------------
!
            DO k = 1, nCurves
               CALL SmoothSegments(chainNodesArray = chainNodesArray(j) % array, &
                                   nodeTs          = nodeTs                    , &
                                   startID         = ends(k-1),                  &
                                   endID           = ends(k),                    &
                                   shift           = shift,                      &
                                   chain           = modelChain)
            END DO
!
!           --------------------------------------------
!           Release local memory for next boundary chain
!           --------------------------------------------
!
            DEALLOCATE(ends)
            DEALLOCATE(nodeTs)
         END DO

         DEALLOCATE(chainNodesArray)
!         DEALLOCATE(modelChains)

      END SUBROUTINE RedistributeNodesAlongBoundaries
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SmoothSegments( chainNodesArray, nodeTs, startID, endID, shift, chain)
!        --------------------------------------------------------------------------------
!        This routine applies a 1-2-1 filter to move the boundary nodes along a curve.
!        The movement is anchored by the initial position to avoid excessive movement
!        in regions of high curvature. The `anchorStrength` is set as a global parameter.
!        --------------------------------------------------------------------------------
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(SMNodePtr)       :: chainNodesArray(:) !Array of nodes along a boundary
         REAL(KIND=RP)         :: nodeTs(0:)
         INTEGER               :: startID, endID, shift
         CLASS(SMChainedCurve) :: chain
!
!        ---------------
!        local Variables
!        ---------------
!
         INTEGER                                 :: j, k
         REAL(KIND=RP)                           :: tm, tp, t
         REAL(KIND=RP), DIMENSION(startID:endID) :: t0
!
!        ------------------------
!        Copy the existing values
!        ------------------------
!
         t0 = nodeTs(startID:endID)
!
!        ----------------------------------
!        Smooth the parametrized locations
!        Values for numBoundarySmoothPasses
!        and anchorStrength are defined in
!        ProgramGlobals
!        ----------------------------------
!
         DO k = 1, numBoundarySmoothPasses
            DO j = startID+1, endID-1
               tm = nodeTs(j-1)
               t  = nodeTs(j)
               tp = nodeTs(j+1)

               nodeTs(j) = anchorStrength*t0(j) + (1.0_RP - anchorStrength)*(0.25_RP*(tm + 2.0_RP*t + tp))
            END DO
            DO j = endID-1, startID+1, -1
               tm = nodeTs(j-1)
               t  = nodeTs(j)
               tp = nodeTs(j+1)

               nodeTs(j) = anchorStrength*t0(j) + (1.0_RP - anchorStrength)*(0.25_RP*(tm + 2.0_RP*t + tp))
            END DO
         END DO
!
!        ---------------
!        Reset the nodes
!        ---------------
!
         DO j = startID+1, endID-1
            t = nodeTs(j)

            chainNodesArray(j+shift) % node % gWhereOnBoundary = t
            chainNodesArray(j+shift) % node % x                = chain % positionAt(t)
            chainNodesArray(j+shift) % node % whereOnBoundary  = chain % curveTForChainT(t)
            chainNodesArray(j+shift) % node % bCurveID         = chain % myCurveIDs(chain % curveNumberForLocation(t))
         END DO

      END SUBROUTINE SmoothSegments
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION smallestCornerSine( corners )
         IMPLICIT NONE
!
!        ------------------------------------------------------------
!        Smallest sine of the corner angles of a counterclockwise quad.
!        It is negative if a corner is reflex, i.e. the quad is not
!        convex. Zero if a side has zero length.
!        ------------------------------------------------------------
!
         REAL(KIND=RP) :: corners(2,4)
         REAL(KIND=RP) :: a(2), d(2), la, ld
         INTEGER       :: c
         INTEGER, EXTERNAL :: Loop
         smallestCornerSine = HUGE(smallestCornerSine)
         DO c = 1, 4
            a  = corners(:, Loop(c+3,4)) - corners(:, c)
            d  = corners(:, Loop(c+1,4)) - corners(:, c)
            la = NORM2(a)
            ld = NORM2(d)
            IF ( la == 0.0_RP .OR. ld == 0.0_RP )     THEN
               smallestCornerSine = 0.0_RP
               RETURN
            END IF
            smallestCornerSine = MIN(smallestCornerSine, (d(1)*a(2) - d(2)*a(1))/(la*ld))
         END DO
      END FUNCTION smallestCornerSine
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION elementWithDirectedEdge( a, b, elems, nodeEl, nodeElCount )
         IMPLICIT NONE
!
!        -------------------------------------------------------------
!        Index of the element that has the nodes a, b in this order in
!        its counterclockwise node list, or 0 if there is none
!        -------------------------------------------------------------
!
         INTEGER            :: a, b
         TYPE(SMElementPtr) :: elems(:)
         INTEGER            :: nodeEl(:,:), nodeElCount(:)
         INTEGER            :: j, c, ie
         INTEGER, EXTERNAL  :: Loop

         elementWithDirectedEdge = 0
         DO j = 1, nodeElCount(a)
            ie = nodeEl(j,a)
            IF ( elems(ie) % element % remove )     CYCLE
            DO c = 1, 4
               IF ( elems(ie) % element % nodes(c) % node % id /= a )              CYCLE
               IF ( elems(ie) % element % nodes(Loop(c+1,4)) % node % id == b )    THEN
                  elementWithDirectedEdge = ie
                  RETURN
               END IF
            END DO
         END DO
      END FUNCTION elementWithDirectedEdge
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReconnectIsland( elems, nE, nodeEl, nodeElCount, removed, comp, island, mainland, &
                                  otherSide, nodeDistance, minDistance, found )
         IMPLICIT NONE
!
!        -------------------------------------------------------------
!        An island of kept elements (comp = island) that the band of an
!        interface curve cut off from the rest of the mesh on the same
!        side (comp = mainland): find the shortest path across which they
!        meet, through band elements whose nodes are at least minDistance
!        local sizes from the curve (nodeDistance) and through other
!        islands, but not through the mesh on the other side of the curve
!        (comp = otherSide). Put the band elements of the path back
!        (removed = .FALSE.) and join the path to the mainland
!        (comp = mainland). found = .FALSE. if there is no such path.
!        -------------------------------------------------------------
!
         TYPE(SMElementPtr) :: elems(:)
         INTEGER            :: nE, nodeEl(:,:), nodeElCount(:), comp(:), island, mainland, otherSide
         LOGICAL            :: removed(:), found
         REAL(KIND=RP)      :: nodeDistance(:), minDistance
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, ALLOCATABLE      :: parent(:), queue(:)
         TYPE(SMElement), POINTER  :: e
         INTEGER                   :: f, g, k, m, head, tail
         LOGICAL                   :: farFromCurve
         INTEGER, EXTERNAL         :: Loop

         found = .FALSE.
         ALLOCATE( parent(nE), queue(nE) )
         parent = 0
         tail   = 0
         DO f = 1, nE
            IF ( removed(f) )            CYCLE
            IF ( comp(f) /= island )     CYCLE
            parent(f)   = f
            tail        = tail + 1
            queue(tail) = f
         END DO
!
!        -------------------------------------------------------------
!        Breadth first search from the island through band elements and
!        other islands
!        -------------------------------------------------------------
!
         head = 0
         DO WHILE ( head < tail )
            head = head + 1
            f    = queue(head)
            e    => elems(f) % element
            DO k = 1, 4
               g = elementWithDirectedEdge( e % nodes(Loop(k+1,4)) % node % id, e % nodes(k) % node % id, &
                                            elems, nodeEl, nodeElCount )
               IF ( g == 0 )     CYCLE
               IF ( parent(g) /= 0 )     CYCLE
               IF ( .NOT.removed(g) )     THEN
                  IF ( comp(g) == otherSide )     CYCLE
                  IF ( comp(g) == mainland )     THEN
!
!                    -----------------------------------------------
!                    Reached the mainland: put the band elements of
!                    the path back
!                    -----------------------------------------------
!
                     found = .TRUE.
                     DO WHILE ( parent(f) /= f )
                        removed(f) = .FALSE.
                        comp(f)    = mainland
                        f          = parent(f)
                     END DO
                     RETURN
                  END IF
               ELSE
                  farFromCurve = .TRUE.
                  DO m = 1, 4
                     IF ( nodeDistance(elems(g) % element % nodes(m) % node % id) < minDistance )     farFromCurve = .FALSE.
                  END DO
                  IF ( .NOT.farFromCurve )     CYCLE
               END IF
               parent(g)   = f
               tail        = tail + 1
               queue(tail) = g
            END DO
         END DO

      END SUBROUTINE ReconnectIsland
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION newInteriorNode( mesh, x2 ) RESULT(node)
         IMPLICIT NONE
         TYPE (SMMesh), POINTER :: mesh
         REAL(KIND=RP)          :: x2(2)
         TYPE (SMNode), POINTER :: node
         CLASS(FTObject), POINTER :: obj
         ALLOCATE(node)
         CALL node % initWithLocationAndID([x2(1), x2(2), 0.0_RP], mesh % newNodeID())
         obj => node
         CALL mesh % nodes % add(obj)
         CALL releaseSMNode(node)
      END FUNCTION newInteriorNode
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE AddQuad4( mesh, quadNodes, reverse )
         IMPLICIT NONE
!
!        ----------------------------------------------------------
!        Add an element with these nodes, in reverse order if asked.
!        The order comes from the topology, not the node locations.
!        ----------------------------------------------------------
!
         TYPE (SMMesh), POINTER    :: mesh
         TYPE(SMNodePtr)           :: quadNodes(4)
         LOGICAL                   :: reverse
         TYPE(SMNodePtr)           :: ordered(4)
         TYPE (SMElement), POINTER :: e
         CLASS(FTObject) , POINTER :: obj

         IF ( reverse )     THEN
            ordered = [quadNodes(1), quadNodes(4), quadNodes(3), quadNodes(2)]
         ELSE
            ordered = quadNodes
         END IF
         ALLOCATE(e)
         CALL e % initWithNodesIDAndType(ordered, mesh % newElementID(), QUAD)
         obj => e
         CALL mesh % elements % add(obj)
         CALL releaseSMElement(e)
      END SUBROUTINE AddQuad4
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE AppendUnique( list, n, id )
         IMPLICIT NONE
!
!        --------------------------------------------
!        Append id unless it is already the last entry
!        --------------------------------------------
!
         INTEGER :: list(:), n, id
         IF ( n > 0 )     THEN
            IF ( list(n) == id )     RETURN
         END IF
         n = n + 1
         list(n) = id
      END SUBROUTINE AppendUnique
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE SplitFill( x, nAll, ids, nIds, h, quads, nQuads, ok, nChords )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        Fill a counterclockwise polygon with an even number of nodes
!        with quads by cutting it along the chord that best splits the
!        corner angles at its ends. The chord gets new nodes about h apart,
!        as many as make both halves even. New nodes are appended to x.
!        ---------------------------------------------------------------
!
         REAL(KIND=RP) :: x(:,:), h
         INTEGER       :: nAll, nIds, nQuads
         INTEGER       :: ids(nIds), quads(:,:), nChords(:)
         LOGICAL       :: ok

         INTEGER       :: i, j, d, k, kBest, iBest, jBest, n1, n2, c
         INTEGER       :: half1(nIds+2*nIds), half2(nIds+2*nIds)
         REAL(KIND=RP) :: len, score, best, bound, angI(2), angJ(2), interior(nIds), dirNext(nIds)
         REAL(KIND=RP), PARAMETER :: valencePenalty = 15.0_RP
         REAL(KIND=RP), PARAMETER :: BOUND_SLACK    = 1.0d-6
         INTEGER, EXTERNAL :: Loop

         ok = .FALSE.
         IF ( nIds < 4 .OR. MODULO(nIds,2) /= 0 )     RETURN
         IF ( nIds == 4 )     THEN
            nQuads = nQuads + 1
            quads(:,nQuads) = ids
            ok = .TRUE.
            RETURN
         END IF

         DO i = 1, nIds
            interior(i) = cornerAngle( x(:,ids(Loop(i-1,nIds))), x(:,ids(i)), x(:,ids(Loop(i+1,nIds))) )
            dirNext(i)  = ATAN2(x(2,ids(Loop(i+1,nIds))) - x(2,ids(i)), x(1,ids(Loop(i+1,nIds))) - x(1,ids(i)))
         END DO
         best = -HUGE(best)
         DO i = 1, nIds
            DO d = 2, MIN(nIds - 2, nIds - i)
               j   = i + d
               len = NORM2(x(:,ids(j)) - x(:,ids(i)))
               k   = MAX(0, NINT(len/h) - 1)
               IF ( MODULO(d + 1 + k, 2) /= 0 )     THEN
                  IF ( len/h - 1.0_RP > k .OR. k == 0 )     THEN
                     k = k + 1
                  ELSE
                     k = k - 1
                  END IF
               END IF
!
!              ------------------------------------------
!              Both halves must be smaller than the polygon
!              ------------------------------------------
!
               DO WHILE ( d + 1 + k >= nIds .OR. nIds - d + 1 + k >= nIds )
                  k = k - 2
               END DO
               IF ( k < 0 )     CYCLE
!
!              --------------------------------------------------------
!              Angles on either side of the chord at its ends; the best
!              chord has the largest smallest one, and near 90 degrees.
!              The chord splits the corner angles at its ends, so the
!              smallest angle is at most half of either corner and the
!              largest at least half of either: a chord whose bound is
!              no better than the best so far is skipped. Whether the
!              chord lies inside the polygon is only checked for a chord
!              that would be the best so far.
!              --------------------------------------------------------
!
               bound = 0.5_RP*MIN(interior(i), interior(j)) - 0.1_RP*MAX(interior(i), interior(j)) &
                     - 10.0_RP*ABS(len/h - (k + 1)) - valencePenalty*(nChords(ids(i)) + nChords(ids(j)))
               IF ( bound + BOUND_SLACK <= best )     CYCLE
               angI(1) = angleFromDirection( x(:,ids(j)), x(:,ids(i)), dirNext(i) )
               angI(2) = interior(i) - angI(1)
               angJ(1) = angleFromDirection( x(:,ids(i)), x(:,ids(j)), dirNext(j) )
               angJ(2) = interior(j) - angJ(1)
               score = MIN(MINVAL(angI), MINVAL(angJ)) - 0.2_RP*MAX(MAXVAL(angI), MAXVAL(angJ)) &
                     - 10.0_RP*ABS(len/h - (k + 1)) - valencePenalty*(nChords(ids(i)) + nChords(ids(j)))
               IF ( .NOT.(score > best) )     CYCLE
               IF ( .NOT.chordInside( x, ids, nIds, i, j ) )     CYCLE
               best  = score
               iBest = i
               jBest = j
               kBest = k
            END DO
         END DO
         IF ( best == -HUGE(best) )     RETURN
!
!        ----------------------------------------------
!        Nodes on the chord and the two halves, both ccw
!        ----------------------------------------------
!
         i = iBest
         j = jBest
         k = kBest
         nChords(ids(i)) = nChords(ids(i)) + 1
         nChords(ids(j)) = nChords(ids(j)) + 1
         DO c = 1, k
            nAll = nAll + 1
            x(:,nAll) = x(:,ids(i)) + c*(x(:,ids(j)) - x(:,ids(i)))/(k + 1)
         END DO
         n1 = 0
         DO c = i, j
            n1 = n1 + 1
            half1(n1) = ids(c)
         END DO
         DO c = k, 1, -1
            n1 = n1 + 1
            half1(n1) = nAll - k + c
         END DO
         n2 = 0
         DO c = 0, nIds - (j - i)
            n2 = n2 + 1
            half2(n2) = ids(Loop(j + c, nIds))
         END DO
         DO c = 1, k
            n2 = n2 + 1
            half2(n2) = nAll - k + c
         END DO
         CALL SplitFill( x, nAll, half1(1:n1), n1, h, quads, nQuads, ok, nChords )
         IF ( .NOT.ok )     RETURN
         CALL SplitFill( x, nAll, half2(1:n2), n2, h, quads, nQuads, ok, nChords )

      END SUBROUTINE SplitFill
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION cornerAngle( xPrev, x, xNext )
         IMPLICIT NONE
!
!        -------------------------------------------------------------
!        The counterclockwise angle in degrees at x from the direction
!        to xNext to the direction to xPrev, i.e. the interior angle of
!        a counterclockwise polygon
!        -------------------------------------------------------------
!
         REAL(KIND=RP) :: xPrev(2), x(2), xNext(2)
         REAL(KIND=RP), PARAMETER :: DEG = 180.0_RP/PI
         cornerAngle = MODULO((ATAN2(xPrev(2) - x(2), xPrev(1) - x(1)) - ATAN2(xNext(2) - x(2), xNext(1) - x(1)))*DEG, &
                              360.0_RP)
      END FUNCTION cornerAngle
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION angleFromDirection( xPrev, x, dirNext )
         IMPLICIT NONE
!
!        -------------------------------------------------------------
!        cornerAngle with the direction (ATAN2) from x to the next node
!        already known
!        -------------------------------------------------------------
!
         REAL(KIND=RP) :: xPrev(2), x(2), dirNext
         REAL(KIND=RP), PARAMETER :: DEG = 180.0_RP/PI
         angleFromDirection = MODULO((ATAN2(xPrev(2) - x(2), xPrev(1) - x(1)) - dirNext)*DEG, 360.0_RP)
      END FUNCTION angleFromDirection
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION chordInside( x, ids, nIds, i, j )
         IMPLICIT NONE
!
!        ------------------------------------------------------------
!        True if the segment between polygon nodes i and j starts into
!        the polygon at both ends, crosses none of its edges and passes
!        through none of its other nodes. A chord through a node would
!        pinch one half at that node, and no quads can fill it.
!        ------------------------------------------------------------
!
         REAL(KIND=RP) :: x(:,:)
         INTEGER       :: nIds, i, j, ids(nIds)
         INTEGER       :: k, k1
         REAL(KIND=RP) :: a(2), b(2), c(2), d(2)
         INTEGER, EXTERNAL :: Loop

         chordInside = .FALSE.
         IF ( cornerAngle( x(:,ids(j)), x(:,ids(i)), x(:,ids(Loop(i+1,nIds))) ) >= &
              cornerAngle( x(:,ids(Loop(i-1,nIds))), x(:,ids(i)), x(:,ids(Loop(i+1,nIds))) ) )     RETURN
         IF ( cornerAngle( x(:,ids(i)), x(:,ids(j)), x(:,ids(Loop(j+1,nIds))) ) >= &
              cornerAngle( x(:,ids(Loop(j-1,nIds))), x(:,ids(j)), x(:,ids(Loop(j+1,nIds))) ) )     RETURN
         a = x(:,ids(i))
         b = x(:,ids(j))
         DO k = 1, nIds
            k1 = Loop(k+1,nIds)
            IF ( k == i .OR. k == j .OR. k1 == i .OR. k1 == j )     CYCLE
            c = x(:,ids(k))
            d = x(:,ids(k1))
            IF ( cross2(b - a, c - a)*cross2(b - a, d - a) < 0.0_RP .AND. &
                 cross2(d - c, a - c)*cross2(d - c, b - c) < 0.0_RP )     RETURN
            IF ( vertexOnSegment(c, a, b) )     RETURN
         END DO
         chordInside = .TRUE.
      END FUNCTION chordInside
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION cross2( u, v )
         IMPLICIT NONE
         REAL(KIND=RP) :: u(2), v(2)
         cross2 = u(1)*v(2) - u(2)*v(1)
      END FUNCTION cross2
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PaveInterface( mesh, model, sizer, list, paved )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        Pave the band around an interface chain (PaveInterfaceWithBand),
!        when interfacePaving is set. The width of the band suited to a
!        chain depends on its size and its neighbors, so plan the paving
!        for the default width and two wider ones, score each by its worst
!        quads, and apply the best plan: the one that drops the fewest
!        elements of large islands, then the one with the best score. The
!        samples of the curve and the sizes along it do not depend on the
!        width, so they are found once.
!        ---------------------------------------------------------------
!
         TYPE (SMMesh)      , POINTER :: mesh
         TYPE (SMModel)     , POINTER :: model
         TYPE (MeshSizer)   , POINTER :: sizer
         CLASS(FTLinkedList), POINTER :: list
         LOGICAL                      :: paved
         INTEGER, PARAMETER           :: NUMBER_OF_WIDTHS = 3
         REAL(KIND=RP), PARAMETER     :: BAND_SCALES(NUMBER_OF_WIDTHS) = [1.0_RP, 4.0_RP/3.0_RP, 1.5_RP]
         TYPE(PavePlan)               :: plan, bestPlan
         INTEGER                      :: try
         REAL(KIND=RP), ALLOCATABLE   :: xs(:,:), hs(:), hBand(:)
         LOGICAL      , ALLOCATABLE   :: isFeature(:)

         CALL SampleInterfaceChain( mesh, model, sizer, list, xs, hs, hBand, isFeature )
         DO try = 1, NUMBER_OF_WIDTHS
            CALL PaveInterfaceWithBand( mesh, model, list, xs, hs, hBand, isFeature, BAND_SCALES(try), plan )
            IF ( .NOT.plan % ok )     CYCLE
            IF ( .NOT.bestPlan % ok )     THEN
               CALL MovePavePlan( plan, bestPlan )
            ELSE IF ( plan % nDropped < bestPlan % nDropped )     THEN
               CALL MovePavePlan( plan, bestPlan )
            ELSE IF ( plan % nDropped == bestPlan % nDropped .AND. plan % score > bestPlan % score )     THEN
               CALL MovePavePlan( plan, bestPlan )
            END IF
         END DO
         paved = bestPlan % ok
         IF ( paved )     CALL CommitPavePlan( mesh, model, bestPlan )
      END SUBROUTINE PaveInterface
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SampleInterfaceChain( mesh, model, sizer, list, xs, hs, hBand, isFeature )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        Samples xs of the interface chain whose nodes are in list, and
!        the element sizes along it (InterfaceCurveSizes): hs for the
!        nodes on the curve, hBand for the width of the band, and
!        isFeature at the joints of the chain.
!        ---------------------------------------------------------------
!
         TYPE (SMMesh)      , POINTER :: mesh
         TYPE (SMModel)     , POINTER :: model
         TYPE (MeshSizer)   , POINTER :: sizer
         CLASS(FTLinkedList), POINTER :: list
         REAL(KIND=RP), ALLOCATABLE   :: xs(:,:), hs(:), hBand(:)
         LOGICAL      , ALLOCATABLE   :: isFeature(:)
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, PARAMETER :: MAX_SAMPLES = 40000
         TYPE(SMNodePtr), DIMENSION(:), POINTER :: seq
         TYPE(SMNodePtr)   , ALLOCATABLE :: nodeForID(:)
         LOGICAL           , ALLOCATABLE :: inMesh(:)
         REAL(KIND=RP)     , ALLOCATABLE :: xOther(:,:)
         TYPE (SMNode)        , POINTER  :: node
         TYPE (SMElement)     , POINTER  :: e
         CLASS(FTObject)      , POINTER  :: obj
         CLASS(SMChainedCurve), POINTER  :: chain
         INTEGER            :: nCurves, nPer, nS, maxID, nid, i, k, nOther, chainID
         REAL(KIND=RP)      :: turnMax, x3(3)

         nPer    = paveSamplesPerCurve
         turnMax = paveMaxTurn*PI/180.0_RP

         seq => GatheredNodes(list)
         chainID = seq(1) % node % bCurveChainID
         DEALLOCATE(seq)
         chain   => model % chainWithID(chainID)
         nCurves =  chain % numberOfCurvesInChain
!
!        Samples per curve: 2000, fewer on a chain of many curves so the
!        total stays near MAX_SAMPLES
!
         IF ( nPer <= 0 )     nPer = MAX(200, MIN(2000, MAX_SAMPLES/nCurves))
         nS = nCurves*nPer
         ALLOCATE( xs(2,nS), hs(nS), hBand(nS), isFeature(nS) )
         DO i = 1, nS
            x3 = chain % positionAt( REAL(i-1,RP)/nS )
            xs(:,i) = x3(1:2)
         END DO
!
!        -------------------------------------------------------------
!        Nodes of the remaining elements on the other boundaries and
!        interfaces, whose distance limits the size
!        -------------------------------------------------------------
!
         maxID = 0
         CALL mesh % nodesIterator % setToStart()
         DO WHILE ( .NOT.mesh % nodesIterator % isAtEnd() )
            obj => mesh % nodesIterator % object()
            CALL castToSMNode(obj, node)
            maxID = MAX(maxID, node % id)
            CALL mesh % nodesIterator % moveToNext()
         END DO
         ALLOCATE( nodeForID(maxID), inMesh(maxID) )
         CALL mesh % nodesIterator % setToStart()
         DO WHILE ( .NOT.mesh % nodesIterator % isAtEnd() )
            obj => mesh % nodesIterator % object()
            CALL castToSMNode(obj, node)
            nodeForID(node % id) % node => node
            CALL mesh % nodesIterator % moveToNext()
         END DO
         inMesh = .FALSE.
         CALL mesh % elementsIterator % setToStart()
         DO WHILE ( .NOT.mesh % elementsIterator % isAtEnd() )
            obj => mesh % elementsIterator % object()
            CALL castToSMElement(obj, e)
            IF ( .NOT.e % remove )     THEN
               DO k = 1, 4
                  inMesh(e % nodes(k) % node % id) = .TRUE.
               END DO
            END IF
            CALL mesh % elementsIterator % moveToNext()
         END DO

         nOther = 0
         DO nid = 1, maxID
            IF ( .NOT.inMesh(nid) )     CYCLE
            k = nodeForID(nid) % node % bCurveChainID
            IF ( k > 0 .AND. k /= chainID )     nOther = nOther + 1
         END DO
         ALLOCATE( xOther(2,MAX(1,nOther)) )
         nOther = 0
         DO nid = 1, maxID
            IF ( .NOT.inMesh(nid) )     CYCLE
            k = nodeForID(nid) % node % bCurveChainID
            IF ( k > 0 .AND. k /= chainID )     THEN
               nOther = nOther + 1
               xOther(:,nOther) = nodeForID(nid) % node % x(1:2)
            END IF
         END DO
         CALL InterfaceCurveSizes( sizer, xs, nS, nPer, nCurves, turnMax, xOther, nOther, hs, hBand, isFeature )

      END SUBROUTINE SampleInterfaceChain
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MovePavePlan( from, to )
         IMPLICIT NONE
!
!        ------------------------------------------------
!        to = from, moving the arrays rather than copying
!        ------------------------------------------------
!
         TYPE(PavePlan) :: from, to

         to % ok         =  from % ok
         to % score      =  from % score
         to % nDropped   =  from % nDropped
         to % chain      => from % chain
         to % chainID    =  from % chainID
         to % nCurves    =  from % nCurves
         to % bCurveSide =  from % bCurveSide
         to % nc         =  from % nc
         to % nfr        =  from % nfr
         to % nAll       =  from % nAll
         to % nQ         =  from % nQ
         to % ccw        =  from % ccw
         CALL MOVE_ALLOC( from % removedElements, to % removedElements )
         CALL MOVE_ALLOC( from % seq            , to % seq )
         CALL MOVE_ALLOC( from % bandEdge       , to % bandEdge )
         CALL MOVE_ALLOC( from % cg             , to % cg )
         CALL MOVE_ALLOC( from % sx             , to % sx )
         CALL MOVE_ALLOC( from % quads          , to % quads )
         from % ok = .FALSE.
      END SUBROUTINE MovePavePlan
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PaveInterfaceWithBand( mesh, model, list, xs, hs, hBand, isFeature, bandScale, plan )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        Mesh the band around an interface curve from the curve outward,
!        as boundary-first meshers do:
!          1. Remove the elements within bandScale*paveBandWidth local
!             element sizes of the curve, on both sides. Where the band
!             cuts a large part of the mesh off, reconnect it through
!             the band (ReconnectIsland), or else pave over it and count
!             its elements in plan % nDropped.
!          2. Put new nodes on the curve, spaced by the size hs taken
!             from the curve geometry (SampleInterfaceChain), with a node
!             at each joint and corner. Both sides share them.
!          3. Pave one row of elements on each side. A curve node gets one
!             element (a wedge) if its angle is below wedgeAngle, three if
!             it is above fanAngle, two otherwise.
!          4. Fill the gap between each new front and the remaining
!             elements by cutting it into polygons and splitting those.
!        The mesh is not changed: plan holds the result, with plan % ok
!        = .FALSE. if any step fails, and the quality of the worst new
!        quads as plan % score. CommitPavePlan applies it.
!        ---------------------------------------------------------------
!
         TYPE (SMMesh)      , POINTER :: mesh
         TYPE (SMModel)     , POINTER :: model
         CLASS(FTLinkedList), POINTER :: list
         REAL(KIND=RP)                :: xs(:,:), hs(:), hBand(:)
         LOGICAL                      :: isFeature(:)
         REAL(KIND=RP)                :: bandScale
         TYPE(PavePlan), INTENT(OUT)  :: plan
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, PARAMETER :: MAX_GROUP = 16, MAX_LOOPS = 32, NUMBER_OF_PASSES = 4
!
!        An island of fewer elements than MIN_ISLAND_SIZE is paved over.
!        A larger one is reconnected through band elements as far from
!        the curve as possible: at least RECONNECT_DISTANCES(1) local
!        sizes, else at least RECONNECT_DISTANCES(2).
!
         INTEGER      , PARAMETER :: MIN_ISLAND_SIZE = 20
         REAL(KIND=RP), PARAMETER :: RECONNECT_DISTANCES(2) = [1.0_RP, 0.5_RP]
         TYPE(SMNodePtr), DIMENSION(:), POINTER :: seq
         TYPE(SMElementPtr), ALLOCATABLE :: elems(:)
         TYPE(SMNodePtr)   , ALLOCATABLE :: nodeForID(:)
         INTEGER           , ALLOCATABLE :: nodeElCount(:), nodeEl(:,:), nOut(:), cnt(:)
         LOGICAL           , ALLOCATABLE :: removed(:), used(:), attached(:)
         REAL(KIND=RP)     , ALLOCATABLE :: nodeDistance(:)
         REAL(KIND=RP)     , ALLOCATABLE :: cx(:,:), cg(:), sx(:,:,:), wSeg(:)
         INTEGER           , ALLOCATABLE :: featureIdx(:), nSeg(:)
         INTEGER           , ALLOCATABLE :: eFrom(:), eTo(:), loopNodes(:), quads(:,:,:), newFront(:,:)
         INTEGER                         :: loopStart(MAX_LOOPS+1), nLoops
         TYPE (SMNode)        , POINTER  :: node
         TYPE (SMElement)     , POINTER  :: e
         CLASS(FTObject)      , POINTER  :: obj
         CLASS(SMChainedCurve), POINTER  :: chain
         INTEGER  :: n, nCurves, nS, maxID, nE, nid, i, j, k, f, g, p, iter, nF, cur, nc, kk
         INTEGER  :: nInLoops, nOutLoops, fOut, side, nfr(2), nAll(2), nQ(2), nNew(2), kBest, total
         INTEGER  :: m, iS, iFr(2), maxN, bCurveSide, pass, nComp, nStack, keep(2)
         INTEGER           , ALLOCATABLE :: comp(:), stack(:), compSize(:)
         LOGICAL           , ALLOCATABLE :: compInside(:)
         REAL(KIND=RP)                   :: xMin(2), xMax(2)
         TYPE(PointCellGrid)             :: polylineGrid
         INTEGER                         :: nFeat, i2
         REAL(KIND=RP) :: bandFactor, bandReach, wedgeAngle, fanAngle, d, ds, area, w, target, frac, x3(3), rowScale
         LOGICAL  :: changed, ccw, ok, inside, found, reconnected
         INTEGER  :: nDropped, try
         INTEGER, EXTERNAL              :: Loop

         plan % ok  = .FALSE.
         bandFactor = bandScale*paveBandWidth
         wedgeAngle = paveWedgeAngle
         fanAngle   = paveFanAngle

         seq => GatheredNodes(list)
         n   = SIZE(seq)
         chain      => model % chainWithID(seq(1) % node % bCurveChainID)
         nCurves    =  chain % numberOfCurvesInChain
         bCurveSide =  seq(1) % node % bCurveSide
         nS         =  SIZE(hs)
!
!        ---------------------------------------------------------
!        Node and element tables, and the local element size at each
!        node: the average length of the element sides at the node
!        ---------------------------------------------------------
!
         maxID = 0
         CALL mesh % nodesIterator % setToStart()
         DO WHILE ( .NOT.mesh % nodesIterator % isAtEnd() )
            obj => mesh % nodesIterator % object()
            CALL castToSMNode(obj, node)
            maxID = MAX(maxID, node % id)
            CALL mesh % nodesIterator % moveToNext()
         END DO
         ALLOCATE( nodeForID(maxID), nodeElCount(maxID), nodeEl(MAX_GROUP, maxID), nOut(maxID) )
         CALL mesh % nodesIterator % setToStart()
         DO WHILE ( .NOT.mesh % nodesIterator % isAtEnd() )
            obj => mesh % nodesIterator % object()
            CALL castToSMNode(obj, node)
            nodeForID(node % id) % node => node
            CALL mesh % nodesIterator % moveToNext()
         END DO
         ALLOCATE( elems(mesh % elements % COUNT()) )
         nodeElCount = 0
         nE          = 0
         CALL mesh % elementsIterator % setToStart()
         DO WHILE ( .NOT.mesh % elementsIterator % isAtEnd() )
            obj => mesh % elementsIterator % object()
            CALL castToSMElement(obj, e)
            IF ( .NOT.e % remove )     THEN
               nE = nE + 1
               elems(nE) % element => e
               DO k = 1, 4
                  nid = e % nodes(k) % node % id
                  IF ( nodeElCount(nid) < MAX_GROUP )     THEN
                     nodeElCount(nid) = nodeElCount(nid) + 1
                     nodeEl(nodeElCount(nid), nid) = nE
                  END IF
               END DO
            END IF
            CALL mesh % elementsIterator % moveToNext()
         END DO

         area = 0.0_RP
         DO i = 1, nS
            j = Loop(i+1, nS)
            area = area + xs(1,i)*xs(2,j) - xs(1,j)*xs(2,i)
         END DO
         ccw = area > 0.0_RP
!
!        -----------------------------------------------------
!        1. The band: elements with a node closer to the curve
!           than bandFactor local sizes. nodeDistance is the
!           distance of a node to the curve in local sizes.
!        -----------------------------------------------------
!
         ALLOCATE( removed(nE), cnt(nE), nodeDistance(maxID) )
         removed      = .FALSE.
         nodeDistance = HUGE(1.0_RP)
         bandReach = bandFactor*MAXVAL(hBand)
         xMin = MINVAL(xs, DIM = 2) - bandReach
         xMax = MAXVAL(xs, DIM = 2) + bandReach
         CALL BuildPointCellGrid( polylineGrid, xs(:,1:nS) )
         DO nid = 1, maxID
            IF ( nodeElCount(nid) == 0 )     CYCLE
            IF ( ANY(nodeForID(nid) % node % x(1:2) < xMin) .OR. ANY(nodeForID(nid) % node % x(1:2) > xMax) )     CYCLE
            CALL NearestOnPolyline( nodeForID(nid) % node % x(1:2), xs, nS, polylineGrid, iS, d, searchRadius = bandReach )
            nodeDistance(nid) = d/hBand(iS)
            IF ( d < bandFactor*hBand(iS) )     THEN
               DO k = 1, nodeElCount(nid)
                  removed(nodeEl(k,nid)) = .TRUE.
               END DO
            END IF
         END DO
         DO i = 1, n
            nid = seq(i) % node % id
            DO k = 1, nodeElCount(nid)
               removed(nodeEl(k,nid)) = .TRUE.
            END DO
         END DO
!
!        -------------------------------------------------------
!        Keep the elements with a side on the edge of the mesh
!        (e.g. on the outer boundary)
!        -------------------------------------------------------
!
         DO f = 1, nE
            e => elems(f) % element
            DO k = 1, 4
               p = e % nodes(k) % node % id
               j = e % nodes(Loop(k+1,4)) % node % id
               IF ( elementWithDirectedEdge( j, p, elems, nodeEl, nodeElCount ) == 0 )     removed(f) = .FALSE.
            END DO
         END DO
!
         ALLOCATE( eFrom(4*nE), eTo(4*nE), comp(nE), stack(nE), compSize(1000), compInside(1000), attached(nE) )
         nDropped = 0
         DO pass = 1, NUMBER_OF_PASSES
!        ------------------------------------------------------------
!        Make the edge of the band simple: remove the elements at nodes
!        where it touches itself, and the elements that stick into it
!        ------------------------------------------------------------
!
         DO iter = 1, 100
            nF   = 0
            nOut = 0
            cnt  = 0
            DO f = 1, nE
               IF ( .NOT.removed(f) )     CYCLE
               e => elems(f) % element
               DO k = 1, 4
                  p = e % nodes(k) % node % id
                  j = e % nodes(Loop(k+1,4)) % node % id
                  g = elementWithDirectedEdge( j, p, elems, nodeEl, nodeElCount )
                  IF ( g /= 0 )     THEN
                     IF ( removed(g) )     CYCLE
                     cnt(g) = cnt(g) + 1
                  END IF
                  nF = nF + 1
                  eFrom(nF) = p
                  eTo(nF)   = j
                  nOut(p)   = nOut(p) + 1
               END DO
            END DO
            changed = .FALSE.
            DO f = 1, nE
               IF ( .NOT.removed(f) .AND. cnt(f) >= 3 )     THEN
                  removed(f) = .TRUE.
                  changed    = .TRUE.
               END IF
            END DO
            DO i = 1, nF
               IF ( nOut(eFrom(i)) > 1 )     THEN
                  DO k = 1, nodeElCount(eFrom(i))
                     IF ( .NOT.removed(nodeEl(k,eFrom(i))) )     THEN
                        removed(nodeEl(k,eFrom(i))) = .TRUE.
                        changed = .TRUE.
                     END IF
                  END DO
               END IF
            END DO
            IF ( .NOT.changed )     EXIT
         END DO
!
!        -----------------------------------------------------------
!        Loops of the band edge, with the band on the left, and on which
!        side of the curve each is
!        -----------------------------------------------------------
!
         ALLOCATE( loopNodes(nF), used(nF) )
         used   = .FALSE.
         nLoops = 0
         m      = 0
         DO i = 1, nF
            IF ( used(i) )     CYCLE
            IF ( nLoops == MAX_LOOPS )     THEN
               DEALLOCATE(seq)
               RETURN
            END IF
            nLoops = nLoops + 1
            loopStart(nLoops) = m + 1
            j = i
            DO
               used(j) = .TRUE.
               m = m + 1
               loopNodes(m) = eFrom(j)
               cur = eTo(j)
               IF ( cur == eFrom(i) )     EXIT
               DO k = 1, nF
                  IF ( eFrom(k) == cur .AND. .NOT.used(k) )     EXIT
               END DO
               IF ( k > nF )     THEN
                  DEALLOCATE(seq)
                  RETURN
               END IF
               j = k
            END DO
         END DO
         loopStart(nLoops+1) = m + 1
         nInLoops  = 0
         nOutLoops = 0
         fOut      = 0
         DO i = 1, nLoops
            inside = pointInPolygon( nodeForID(loopNodes(loopStart(i))) % node % x(1:2), xs, nS )
            IF ( inside )     THEN
               nInLoops    = nInLoops + 1
               iFr(1)      = i
            ELSE
               nOutLoops   = nOutLoops + 1
               fOut        = fOut + loopStart(i+1) - loopStart(i)
               iFr(2)      = i
            END IF
         END DO
         IF ( nOutLoops == 1 .AND. nInLoops <= 1 )     EXIT
         IF ( pass == NUMBER_OF_PASSES )     THEN
            DEALLOCATE(seq)
            RETURN
         END IF
!
!        -------------------------------------------------------------
!        Islands of elements cut off by the band: keep the largest
!        group of connected elements on each side of the curve. In the
!        first two passes, reconnect each large island to it through the
!        band. Pave over the remaining islands.
!        -------------------------------------------------------------
!
         comp = 0
         nComp = 0
         DO f = 1, nE
            IF ( removed(f) .OR. comp(f) /= 0 )     CYCLE
            nComp = nComp + 1
            compSize(nComp) = 0
            nStack = 1
            stack(1) = f
            comp(f)  = nComp
            DO WHILE ( nStack > 0 )
               g = stack(nStack)
               nStack = nStack - 1
               compSize(nComp) = compSize(nComp) + 1
               e => elems(g) % element
               DO k = 1, 4
                  p = elementWithDirectedEdge( e % nodes(Loop(k+1,4)) % node % id, e % nodes(k) % node % id, &
                                               elems, nodeEl, nodeElCount )
                  IF ( p == 0 )     CYCLE
                  IF ( removed(p) .OR. comp(p) /= 0 )     CYCLE
                  comp(p) = nComp
                  nStack = nStack + 1
                  stack(nStack) = p
               END DO
            END DO
            x3 = 0.0_RP
            DO k = 1, 4
               x3 = x3 + 0.25_RP*elems(f) % element % nodes(k) % node % x
            END DO
            compInside(nComp) = pointInPolygon( x3(1:2), xs, nS )
            IF ( nComp == SIZE(compSize) )     EXIT
         END DO
         keep = 0
         DO i = 1, nComp
            side = MERGE(1, 2, compInside(i))
            IF ( keep(side) == 0 )     THEN
               keep(side) = i
            ELSE IF ( compSize(i) > compSize(keep(side)) )     THEN
               keep(side) = i
            END IF
         END DO
         reconnected = .FALSE.
         DO i = 1, nComp
            IF ( i == keep(1) .OR. i == keep(2) )     CYCLE
            IF ( compSize(i) < MIN_ISLAND_SIZE )     CYCLE
            side  = MERGE(1, 2, compInside(i))
            found = .FALSE.
            DO try = 1, SIZE(RECONNECT_DISTANCES)
               IF ( pass > 2 .OR. found )     EXIT
               CALL ReconnectIsland( elems, nE, nodeEl, nodeElCount, removed, comp, i, keep(side), keep(3-side), &
                                     nodeDistance, RECONNECT_DISTANCES(try), found )
            END DO
            IF ( found )     THEN
               reconnected = .TRUE.
               WHERE ( comp(1:nE) == i )     comp(1:nE) = keep(side)
            ELSE
               nDropped = nDropped + compSize(i)
            END IF
         END DO
         DO f = 1, nE
            IF ( removed(f) )     CYCLE
            IF ( comp(f) /= keep(1) .AND. comp(f) /= keep(2) )     removed(f) = .TRUE.
         END DO
!
!        -------------------------------------------------------------
!        A reconnection can cut parts of the band off from the curve.
!        Those are not paved: put their elements back.
!        -------------------------------------------------------------
!
         IF ( reconnected )     THEN
            attached = .FALSE.
            nStack   = 0
            DO i = 1, n
               nid = seq(i) % node % id
               DO k = 1, nodeElCount(nid)
                  f = nodeEl(k,nid)
                  IF ( removed(f) .AND. .NOT.attached(f) )     THEN
                     attached(f)    = .TRUE.
                     nStack         = nStack + 1
                     stack(nStack)  = f
                  END IF
               END DO
            END DO
            DO WHILE ( nStack > 0 )
               g      = stack(nStack)
               nStack = nStack - 1
               e => elems(g) % element
               DO k = 1, 4
                  p = elementWithDirectedEdge( e % nodes(Loop(k+1,4)) % node % id, e % nodes(k) % node % id, &
                                               elems, nodeEl, nodeElCount )
                  IF ( p == 0 )     CYCLE
                  IF ( .NOT.removed(p) .OR. attached(p) )     CYCLE
                  attached(p)   = .TRUE.
                  nStack        = nStack + 1
                  stack(nStack) = p
               END DO
            END DO
            DO f = 1, nE
               IF ( removed(f) .AND. .NOT.attached(f) )     removed(f) = .FALSE.
            END DO
         END IF
         DEALLOCATE( loopNodes, used )
         END DO
         IF ( nInLoops == 0 )     iFr(1) = 0
!
!        ------------------------------------------------------------
!        2. Nodes on the curve: a node at each feature (joint)
!           and, between two features, as many edges as sizes fit, so
!           the total has the parity of the band edge, placed evenly in
!           the integral of 1/size
!        ------------------------------------------------------------
!
         nFeat = 0
         DO i = 1, nS
            IF ( isFeature(i) )     nFeat = nFeat + 1
         END DO
         ALLOCATE( featureIdx(nFeat+1), nSeg(nFeat), wSeg(nFeat) )
         k = 0
         DO i = 1, nS
            IF ( isFeature(i) )     THEN
               k = k + 1
               featureIdx(k) = i
            END IF
         END DO
         featureIdx(nFeat+1) = featureIdx(1) + nS
         total = 0
         DO k = 1, nFeat
            wSeg(k) = 0.0_RP
            DO i = featureIdx(k), featureIdx(k+1) - 1
               i2 = MODULO(i-1, nS) + 1
               j  = MODULO(i, nS) + 1
               wSeg(k) = wSeg(k) + 2.0_RP*NORM2(xs(:,j) - xs(:,i2))/(hs(i2) + hs(j))
            END DO
            nSeg(k) = MAX(1, NINT(wSeg(k)))
            total   = total + nSeg(k)
         END DO
         IF ( MODULO(total - fOut, 2) /= 0 )     THEN
            kBest = MAXLOC(wSeg - nSeg, DIM = 1)
            nSeg(kBest) = nSeg(kBest) + 1
            total = total + 1
         END IF
         IF ( total < 4 )     THEN
            DEALLOCATE(seq)
            RETURN
         END IF
         nc = total
         ALLOCATE( cx(2,nc), cg(nc) )
         m = 0
         DO k = 1, nFeat
            m = m + 1
            cg(m) = REAL(featureIdx(k)-1,RP)/nS
            w = 0.0_RP
            i = featureIdx(k)
            DO kk = 1, nSeg(k) - 1
               target = kk*wSeg(k)/nSeg(k)
               DO
                  i2 = MODULO(i-1, nS) + 1
                  j  = MODULO(i, nS) + 1
                  ds = 2.0_RP*NORM2(xs(:,j) - xs(:,i2))/(hs(i2) + hs(j))
                  IF ( w + ds >= target .OR. i >= featureIdx(k+1) - 1 )     EXIT
                  w = w + ds
                  i = i + 1
               END DO
               frac = MIN(1.0_RP, (target - w)/MAX(ds, TINY(ds)))
               m = m + 1
               cg(m) = MODULO((REAL(i-1,RP) + frac)/nS, 1.0_RP)
            END DO
         END DO
         DO m = 1, nc
            x3 = chain % positionAt(cg(m))
            cx(:,m) = x3(1:2)
         END DO
!
!        ------------------------------------------------------------
!        3. A paving row on each side. Local indices: 1..nc the curve
!           nodes, with the side on the left, then the band edge loop,
!           then the new nodes.
!        ------------------------------------------------------------
!
         maxN = 40*(nc + nF) + 100
         ALLOCATE( sx(2,maxN,2), quads(4,maxN,2), newFront(4*nc,2) )
         DO side = 1, 2
            DO m = 1, nc
               sx(:,m,side) = cx(:,sideCurveIndex(m, nc, side, ccw))
            END DO
            nfr(side) = 0
            IF ( iFr(side) > 0 )     THEN
               DO i = loopStart(iFr(side)), loopStart(iFr(side)+1) - 1
                  nfr(side) = nfr(side) + 1
                  sx(:,nc + nfr(side),side) = nodeForID(loopNodes(i)) % node % x(1:2)
               END DO
            END IF
            rowScale = 1.0_RP
            DO
               nAll(side) = nc + nfr(side)
               CALL PaveRow( sx(:,:,side), nc, nfr(side), wedgeAngle, fanAngle, rowScale, nAll(side), &
                             quads(:,:,side), nQ(side), newFront(:,side), nNew(side), ok )
               IF ( ok .OR. rowScale < 0.1_RP )     EXIT
               rowScale = 0.5_RP*rowScale
            END DO
            IF ( .NOT.ok )     THEN
               DEALLOCATE(seq)
               RETURN
            END IF
         END DO

!
!        ----------------------------------------------------------
!        4. Fill between each new front and the edge of the remaining
!           elements
!        ----------------------------------------------------------
!
         DO side = 1, 2
            CALL FillBetweenLoops( sx(:,:,side), nAll(side), newFront(:,side), nNew(side), nc, nfr(side), &
                                   quads(:,:,side), nQ(side), ok )
            IF ( ok )     CALL RemoveDoublets( sx(:,:,side), nc + nfr(side), nAll(side), quads(:,:,side), nQ(side) )
            IF ( .NOT.ok )     THEN
               DEALLOCATE(seq)
               RETURN
            END IF
         END DO
!
!        ------------------------------------------------------------
!        A band wider than the default must not reach another curve:
!        between curves close together the paved regions would meet in
!        the gap and squeeze the elements there
!        ------------------------------------------------------------
!
         IF ( bandScale > 1.0_RP )     THEN
            DO f = 1, nE
               IF ( .NOT.removed(f) )     CYCLE
               DO k = 1, 4
                  node => elems(f) % element % nodes(k) % node
                  IF ( node % bCurveChainID > 0 .AND. node % bCurveChainID /= seq(1) % node % bCurveChainID )     THEN
                     IF ( node % distToBoundary == 0.0_RP )     THEN
                        DEALLOCATE(seq)
                        RETURN
                     END IF
                  END IF
               END DO
            END DO
         END IF
!
!        ----
!        Plan
!        ----
!
         plan % score = MIN(worstQuadQuality( sx(:,:,1), nc, nc + nfr(1), nAll(1), quads(:,:,1), nQ(1) ), &
                            worstQuadQuality( sx(:,:,2), nc, nc + nfr(2), nAll(2), quads(:,:,2), nQ(2) ))
         plan % chain      => chain
         plan % chainID    =  seq(1) % node % bCurveChainID
         plan % nCurves    =  nCurves
         plan % bCurveSide =  bCurveSide
         plan % nc         =  nc
         plan % nfr        =  nfr
         plan % nAll       =  nAll
         plan % nQ         =  nQ
         plan % ccw        =  ccw
         plan % nDropped   =  nDropped
         plan % removedElements = PACK(elems(1:nE), removed(1:nE))
         plan % seq = seq
         ALLOCATE( plan % bandEdge(MAX(1, MAXVAL(nfr)), 2) )
         DO side = 1, 2
            DO m = 1, nfr(side)
               plan % bandEdge(m,side) % node => nodeForID(loopNodes(loopStart(iFr(side)) + m - 1)) % node
            END DO
         END DO
         plan % cg    = cg(1:nc)
         plan % sx    = sx(:,1:MAXVAL(nAll),:)
         plan % quads = quads(:,1:MAX(1, MAXVAL(nQ)),:)
         plan % ok    = .TRUE.
         DEALLOCATE(seq)

      END SUBROUTINE PaveInterfaceWithBand
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CommitPavePlan( mesh, model, plan )
         IMPLICIT NONE
!
!        --------------------------------------------------------------
!        Apply a plan of PaveInterfaceWithBand: remove the elements of
!        the band, make the curve and interior nodes, and add the quads
!        --------------------------------------------------------------
!
         TYPE (SMMesh) , POINTER :: mesh
         TYPE (SMModel), POINTER :: model
         TYPE(PavePlan)          :: plan
         TYPE(SMNodePtr)         :: curveNodes(plan % nc), sideNodes(MAXVAL(plan % nAll)), q(4)
         LOGICAL                 :: nodeUsed(MAXVAL(plan % nAll))
         REAL(KIND=RP)           :: gNode, d, x3(3)
         INTEGER                 :: i, j, k, m, side, nc

         nc = plan % nc
         DO i = 1, SIZE(plan % removedElements)
            plan % removedElements(i) % element % remove = .TRUE.
         END DO
         DO m = 1, nc
            gNode = plan % cg(m)*plan % nCurves
            NULLIFY(curveNodes(m) % node)
            IF ( ABS(gNode - NINT(gNode)) <= 1.0e-12_RP )     THEN
               DO i = 1, SIZE(plan % seq)
                  d = plan % seq(i) % node % gWhereOnBoundary*plan % nCurves
                  IF ( ABS(MODULO(d - gNode + 0.5_RP*plan % nCurves, REAL(plan % nCurves,RP)) &
                       - 0.5_RP*plan % nCurves) <= 1.0e-12_RP )     THEN
                     curveNodes(m) % node => plan % seq(i) % node
                     x3 = plan % chain % positionAt(plan % cg(m))
                     curveNodes(m) % node % x = x3
                     EXIT
                  END IF
               END DO
            END IF
            IF ( .NOT.ASSOCIATED(curveNodes(m) % node) )     THEN
               curveNodes(m) % node => newChainNodeAt( mesh, model, plan % chain, plan % cg(m), plan % bCurveSide )
            END IF
         END DO
         DO side = 1, 2
            nodeUsed = .FALSE.
            DO j = 1, plan % nQ(side)
               nodeUsed(plan % quads(:,j,side)) = .TRUE.
            END DO
            DO m = 1, plan % nAll(side)
               NULLIFY(sideNodes(m) % node)
               IF ( m <= nc )     THEN
                  sideNodes(m) % node => curveNodes(sideCurveIndex(m, nc, side, plan % ccw)) % node
               ELSE IF ( m <= nc + plan % nfr(side) )     THEN
                  sideNodes(m) % node => plan % bandEdge(m - nc, side) % node
               ELSE IF ( nodeUsed(m) )     THEN
                  sideNodes(m) % node => newInteriorNode( mesh, plan % sx(:,m,side) )
               END IF
            END DO
            DO j = 1, plan % nQ(side)
               DO k = 1, 4
                  q(k) % node => sideNodes(plan % quads(k,j,side)) % node
               END DO
               CALL AddQuad4( mesh, q, .FALSE. )
            END DO
         END DO

         k = plan % chainID
         IF ( k >= 0 .AND. k <= MAX_PAVED_CHAIN_ID )     chainWasPaved(k) = .TRUE.

      END SUBROUTINE CommitPavePlan
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PaveRow( x, nf, nfr, wedgeAngle, fanAngle, rowScale, nAll, quads, nQ, newFront, nNew, ok )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        One row of quads along the closed front x(:,1:nf), which has the
!        region to mesh on its left. x(:,nf+1:nf+nfr) is the closed edge
!        of the remaining mesh, which the row must not cross. A front node
!        with an angle below wedgeAngle is the corner of one wedge quad
!        (and the row nodes of its neighbors merge), one above fanAngle
!        gets three quads, the others two. New nodes are appended to x;
!        newFront lists the nodes of the new front, region on its left.
!        ---------------------------------------------------------------
!
         REAL(KIND=RP) :: x(:,:), wedgeAngle, fanAngle, rowScale
         INTEGER       :: nf, nfr, nAll, nQ, nNew
         INTEGER       :: quads(:,:), newFront(:)
         LOGICAL       :: ok

         INTEGER, PARAMETER       :: SIDE_NODE = 0, WEDGE = 1, FAN3 = 2, IN_WEDGE = 3
         REAL(KIND=RP), PARAMETER :: DEG = 180.0_RP/PI
         INTEGER       :: typ(nf), rowIdx(nf), s1(nf), s2(nf), jn(nf)
         INTEGER       :: j, k, jp, jq, left, right, a1, a2, b1, b2
         REAL(KIND=RP) :: theta(nf), un(2), h, sp, dmin, phi, corners(2,4)
         INTEGER, EXTERNAL :: Loop
         ok  = .FALSE.
         nQ  = 0
         nNew = 0
         DO j = 1, nf
            theta(j) = cornerAngle( x(:,Loop(j-1,nf)), x(:,j), x(:,Loop(j+1,nf)) )
         END DO
         typ = SIDE_NODE
         DO j = 1, nf
            jp = Loop(j-1,nf)
            jq = Loop(j+1,nf)
            IF ( theta(j) < wedgeAngle .AND. typ(j) == SIDE_NODE .AND. typ(jp) == SIDE_NODE .AND. typ(jq) == SIDE_NODE )     THEN
               IF ( typ(Loop(j-2,nf)) /= WEDGE .AND. typ(Loop(j+2,nf)) /= WEDGE )     THEN
                  typ(j)  = WEDGE
                  typ(jp) = IN_WEDGE
                  typ(jq) = IN_WEDGE
               END IF
            END IF
         END DO
         DO j = 1, nf
            IF ( typ(j) == SIDE_NODE .AND. theta(j) > fanAngle )     typ(j) = FAN3
         END DO
!
!        ---------
!        Row nodes
!        ---------
!
         DO j = 1, nf
            IF ( typ(j) == IN_WEDGE )     CYCLE
            jp = Loop(j-1,nf)
            jq = Loop(j+1,nf)
            sp = 0.5_RP*(NORM2(x(:,jq) - x(:,j)) + NORM2(x(:,j) - x(:,jp)))
            dmin = HUGE(dmin)
            DO k = 1, nf
               IF ( ABS(k - j) <= 1 .OR. ABS(k - j) >= nf - 1 )     CYCLE
               dmin = MIN(dmin, NORM2(x(:,k) - x(:,j)))
            END DO
            DO k = nf + 1, nf + nfr
               dmin = MIN(dmin, NORM2(x(:,k) - x(:,j)))
            END DO
            h  = rowScale*MIN(0.8_RP*sp, 0.45_RP*dmin)
            un = (x(:,jq) - x(:,j))/NORM2(x(:,jq) - x(:,j))
            SELECT CASE ( typ(j) )
               CASE ( WEDGE )
                  nAll = nAll + 1
                  x(:,nAll) = x(:,jp) + x(:,jq) - x(:,j)
                  rowIdx(jp) = nAll
                  rowIdx(j)  = nAll
                  rowIdx(jq) = nAll
               CASE ( FAN3 )
                  phi  = theta(j)/(3.0_RP*DEG)
                  nAll = nAll + 1
                  s2(j) = nAll
                  x(:,nAll) = x(:,j) + h*rotated(un, phi)
                  phi  = 2.0_RP*theta(j)/(3.0_RP*DEG)
                  nAll = nAll + 1
                  s1(j) = nAll
                  x(:,nAll) = x(:,j) + h*rotated(un, phi)
                  phi  = theta(j)/(2.0_RP*DEG)
                  nAll = nAll + 1
                  jn(j) = nAll
                  x(:,nAll) = x(:,j) + 1.4_RP*h*rotated(un, phi)
               CASE DEFAULT
                  phi  = theta(j)/(2.0_RP*DEG)
                  nAll = nAll + 1
                  rowIdx(j) = nAll
                  x(:,nAll) = x(:,j) + h/MAX(SIN(phi), 0.5_RP)*rotated(un, phi)
            END SELECT
         END DO
!
!        -----------------------------------------
!        Row quads: wedges, sides and reflex fans
!        -----------------------------------------
!
         DO j = 1, nf
            jq = Loop(j+1,nf)
            IF ( typ(jq) == WEDGE )     THEN
               nQ = nQ + 1
               quads(:,nQ) = [j, jq, Loop(j+2,nf), rowIdx(jq)]
               CYCLE
            END IF
            IF ( typ(j) == WEDGE )     CYCLE
            left  = rowIdx(j)
            right = rowIdx(jq)
            IF ( typ(j)  == FAN3 )     left  = s2(j)
            IF ( typ(jq) == FAN3 )     right = s1(jq)
            nQ = nQ + 1
            quads(:,nQ) = [j, jq, right, left]
            IF ( typ(jq) == FAN3 )     THEN
               nQ = nQ + 1
               quads(:,nQ) = [jq, s2(jq), jn(jq), s1(jq)]
            END IF
         END DO
!
!        ---------
!        New front
!        ---------
!
         DO j = 1, nf
            IF ( typ(j) == FAN3 )     THEN
               CALL AppendUnique( newFront, nNew, s1(j) )
               CALL AppendUnique( newFront, nNew, jn(j) )
               CALL AppendUnique( newFront, nNew, s2(j) )
            ELSE
               CALL AppendUnique( newFront, nNew, rowIdx(j) )
            END IF
         END DO
         IF ( nNew > 1 .AND. newFront(nNew) == newFront(1) )     nNew = nNew - 1
!
!        -------------------------------------------------------------
!        Checks: no folded quads, and the new front crosses neither
!        itself nor the edge of the remaining mesh
!        -------------------------------------------------------------
!
         IF ( nNew < 4 )     RETURN
         DO k = 1, nQ
            DO j = 1, 4
               corners(:,j) = x(:,quads(j,k))
            END DO
            IF ( smallestCornerSine(corners) < -0.5_RP )     RETURN
            IF ( polygonArea(corners, 4) <= 0.0_RP )     RETURN
         END DO
         DO j = 1, nNew
            a1 = newFront(j)
            a2 = newFront(Loop(j+1,nNew))
            DO k = j + 2, nNew
               IF ( j == 1 .AND. k == nNew )     CYCLE
               b1 = newFront(k)
               b2 = newFront(Loop(k+1,nNew))
               IF ( segmentsCross(x(:,a1), x(:,a2), x(:,b1), x(:,b2)) )     RETURN
            END DO
            DO k = 1, nfr
               b1 = nf + k
               b2 = nf + Loop(k+1,nfr)
               IF ( segmentsCross(x(:,a1), x(:,a2), x(:,b1), x(:,b2)) )     RETURN
            END DO
         END DO
         ok = .TRUE.

      END SUBROUTINE PaveRow
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION rotated( u, phi ) RESULT(v)
         IMPLICIT NONE
         REAL(KIND=RP) :: u(2), phi, v(2)
         v = [COS(phi)*u(1) - SIN(phi)*u(2), SIN(phi)*u(1) + COS(phi)*u(2)]
      END FUNCTION rotated
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION polygonArea( x, n )
         IMPLICIT NONE
         INTEGER       :: n
         REAL(KIND=RP) :: x(2,n)
         INTEGER       :: j
         INTEGER, EXTERNAL :: Loop
         polygonArea = 0.0_RP
         DO j = 1, n
            polygonArea = polygonArea + 0.5_RP*cross2(x(:,j), x(:,Loop(j+1,n)))
         END DO
      END FUNCTION polygonArea
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION segmentsCross( a, b, c, d )
         IMPLICIT NONE
!
!        -----------------------------------------------
!        True if the segments ab and cd properly intersect
!        -----------------------------------------------
!
         REAL(KIND=RP) :: a(2), b(2), c(2), d(2)
         segmentsCross = cross2(b - a, c - a)*cross2(b - a, d - a) < 0.0_RP .AND. &
                         cross2(d - c, a - c)*cross2(d - c, b - c) < 0.0_RP
      END FUNCTION segmentsCross
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION vertexOnSegment( c, a, b )
         IMPLICIT NONE
!
!        ------------------------------------------------------------
!        True if c lies on the segment ab, away from its ends, within
!        a small tolerance relative to the length of the segment
!        ------------------------------------------------------------
!
         REAL(KIND=RP) :: a(2), b(2), c(2)
         REAL(KIND=RP) :: lengthSquared, t
         REAL(KIND=RP), PARAMETER :: TOL = 1.0E-6_RP

         vertexOnSegment = .FALSE.
         lengthSquared   = DOT_PRODUCT(b - a, b - a)
         IF ( lengthSquared <= 0.0_RP )                         RETURN
         IF ( ABS(cross2(b - a, c - a)) > TOL*lengthSquared )   RETURN
         t = DOT_PRODUCT(c - a, b - a)/lengthSquared
         vertexOnSegment = t > TOL .AND. t < 1.0_RP - TOL
      END FUNCTION vertexOnSegment
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE NearestOnPolyline( x, xs, nS, grid, iNear, dist, searchRadius )
         IMPLICIT NONE
!
!        ----------------------------------------------------------
!        Distance from x to the closed polyline xs, and the vertex of
!        the polyline nearest to x, the first of equally near ones.
!        The cells of grid, the vertices sorted by BuildPointCellGrid,
!        are searched in rings around x: a vertex not yet seen is in
!        a ring further out, more than (ring - 1) cells away. One more
!        ring is searched to allow for rounding at the cell edges.
!        With a searchRadius, the search stops once every vertex not
!        yet seen is further away than that. The result is then the
!        nearest vertex if it is within searchRadius; otherwise dist is
!        only known to be larger.
!        ----------------------------------------------------------
!
         INTEGER                 :: nS, iNear
         REAL(KIND=RP)           :: x(2), xs(2,nS), dist
         TYPE(PointCellGrid)     :: grid
         REAL(KIND=RP), OPTIONAL :: searchRadius
         INTEGER             :: i, j, k, cell, ring, ix, iy, ix0, iy0
         REAL(KIND=RP)       :: d, t, u(2), best
         INTEGER, EXTERNAL   :: Loop

         best  = HUGE(best)
         iNear = 1
         ix0   = gridCellIndex( grid, x(1), 1 )
         iy0   = gridCellIndex( grid, x(2), 2 )
         DO ring = 0, MAX(grid % nCellsX, grid % nCellsY)
            IF ( best < (ring - 2)*grid % cellSize )     EXIT
            IF ( PRESENT(searchRadius) )     THEN
               IF ( searchRadius < (ring - 2)*grid % cellSize )     EXIT
            END IF
            DO iy = MAX(1, iy0 - ring), MIN(grid % nCellsY, iy0 + ring)
               DO ix = MAX(1, ix0 - ring), MIN(grid % nCellsX, ix0 + ring)
                  IF ( MAX(ABS(ix - ix0), ABS(iy - iy0)) /= ring )     CYCLE
                  cell = ix + (iy - 1)*grid % nCellsX
                  DO k = grid % cellStart(cell), grid % cellStart(cell+1) - 1
                     i = grid % cellPoints(k)
                     d = NORM2(x - xs(:,i))
                     IF ( d < best .OR. (d <= best .AND. i < iNear) )     THEN
                        best  = d
                        iNear = i
                     END IF
                  END DO
               END DO
            END DO
         END DO
         dist = best
         DO j = iNear - 1, iNear
            i = Loop(j, nS)
            u = xs(:,Loop(i+1,nS)) - xs(:,i)
            IF ( DOT_PRODUCT(u,u) == 0.0_RP )     CYCLE
            t = MAX(0.0_RP, MIN(1.0_RP, DOT_PRODUCT(x - xs(:,i), u)/DOT_PRODUCT(u,u)))
            dist = MIN(dist, NORM2(x - xs(:,i) - t*u))
         END DO
      END SUBROUTINE NearestOnPolyline
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION pointInPolygon( x, xs, nS )
         IMPLICIT NONE
!
!        ------------------------------------------
!        Crossing number test against a closed polyline
!        ------------------------------------------
!
         INTEGER       :: nS
         REAL(KIND=RP) :: x(2), xs(2,nS)
         INTEGER       :: i, j
         INTEGER, EXTERNAL :: Loop

         pointInPolygon = .FALSE.
         DO i = 1, nS
            j = Loop(i+1, nS)
            IF ( (xs(2,i) > x(2)) .NEQV. (xs(2,j) > x(2)) )     THEN
               IF ( x(1) < xs(1,i) + (x(2) - xs(2,i))*(xs(1,j) - xs(1,i))/(xs(2,j) - xs(2,i)) )     THEN
                  pointInPolygon = .NOT.pointInPolygon
               END IF
            END IF
         END DO
      END FUNCTION pointInPolygon
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION sideCurveIndex( m, nc, side, ccw )
         IMPLICIT NONE
!
!        -----------------------------------------------------------
!        Index of the m-th curve node when the curve is traversed with
!        side (1 = inside, 2 = outside) on the left
!        -----------------------------------------------------------
!
         INTEGER :: m, nc, side
         LOGICAL :: ccw
         INTEGER, EXTERNAL :: Loop
         IF ( (side == 1) .EQV. ccw )     THEN
            sideCurveIndex = m
         ELSE
            sideCurveIndex = Loop(nc + 2 - m, nc)
         END IF
      END FUNCTION sideCurveIndex
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION newChainNodeAt( mesh, model, chain, g, bCurveSide ) RESULT(node)
         IMPLICIT NONE
!
!        -------------------------------------------
!        A node on the chain at the chain parameter g
!        -------------------------------------------
!
         TYPE (SMMesh)        , POINTER :: mesh
         TYPE (SMModel)       , POINTER :: model
         CLASS(SMChainedCurve), POINTER :: chain
         REAL(KIND=RP)                  :: g
         INTEGER                        :: bCurveSide
         TYPE (SMNode)        , POINTER :: node
         CLASS(SMChainedCurve), POINTER :: owner
         CLASS(SMCurve)       , POINTER :: c
         CLASS(FTObject)      , POINTER :: obj

         c => model % curveWithID(chain % myCurveIDs(chain % curveNumberForLocation(g)), owner)
         ALLOCATE(node)
         CALL initBoundaryNode( c, chain, chain % curveTForChainT(g), bCurveSide, mesh % newNodeID(), node )
         obj => node
         CALL mesh % nodes % add(obj)
         CALL releaseSMNode(node)
      END FUNCTION newChainNodeAt
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FillBetweenLoops( x, nAll, front, nFront, b0, nB, quads, nQ, ok )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        Fill the region between the closed front x(:,front(1:nFront))
!        and the closed loop x(:,b0+1:b0+nB) (nB = 0: no loop), both with
!        the region on their left, with quads. With a loop, two chords
!        between the front and the loop cut the region into two polygons,
!        which are then split into quads. The shortest chords are tried
!        first. Whether a pair of nodes can be joined by a chord
!        (bridgeOK) is only checked when the pair is reached, and kept.
!        ---------------------------------------------------------------
!
         REAL(KIND=RP) :: x(:,:)
         INTEGER       :: nAll, nFront, b0, nB, nQ
         INTEGER       :: front(:), quads(:,:)
         LOGICAL       :: ok

         INTEGER, PARAMETER :: MAX_TRIES = 12
         INTEGER       :: i, j, nBr, t1, t2, i1, j1, i2, j2, di, dj, nAll0, nQ0, nTried, nFirst
         INTEGER       :: nShort, nSorted
         INTEGER       :: brI(nFront*nB), brJ(nFront*nB), order(nFront*nB), bridgeState(nFront*nB)
         INTEGER, ALLOCATABLE :: shortPairs(:)
         REAL(KIND=RP) :: h, shortLen, brLen(nFront*nB)
         INTEGER       :: nChords(SIZE(x,2))
         INTEGER, EXTERNAL :: Loop

         ok      = .FALSE.
         nChords = 0
         h  = 0.0_RP
         DO i = 1, nFront
            h = h + NORM2(x(:,front(Loop(i+1,nFront))) - x(:,front(i)))
         END DO
         h = h/nFront
         IF ( nB == 0 )     THEN
            CALL SplitFill( x, nAll, front(1:nFront), nFront, h, quads, nQ, ok, nChords )
            RETURN
         END IF
!
!        ----------------------------------------------------------
!        All pairs of a front and a loop node, shortest first. Pairs
!        of the same length keep their order (front node, then loop
!        node), so the chords are tried in the same order as when the
!        valid ones are found first and then sorted.
!        ----------------------------------------------------------
!
         nBr = 0
         DO i = 1, nFront
            DO j = 1, nB
               nBr = nBr + 1
               brI(nBr)   = i
               brJ(nBr)   = j
               brLen(nBr) = NORM2(x(:,b0+j) - x(:,front(i)))
            END DO
         END DO
!
!        -------------------------------------------------------------
!        The search nearly always ends among the short pairs. Sort first
!        the pairs no longer than the largest distance from a front node
!        to its nearest loop node, and all of them only if the search
!        gets past those. The short pairs come first in the full order,
!        so the chords are tried in the same order either way.
!        -------------------------------------------------------------
!
         shortLen = 0.0_RP
         DO i = 1, nFront
            shortLen = MAX(shortLen, MINVAL(brLen((i-1)*nB+1:i*nB)))
         END DO
         ALLOCATE(shortPairs(nBr))
         nShort = 0
         DO t1 = 1, nBr
            IF ( brLen(t1) <= shortLen )     THEN
               nShort = nShort + 1
               shortPairs(nShort) = t1
            END IF
         END DO
         CALL StableSortByKey( brLen(shortPairs(1:nShort)), order(1:nShort) )
         order(1:nShort) = shortPairs(order(1:nShort))
         nSorted = nShort
         bridgeState = BRIDGE_UNCHECKED

         nAll0  = nAll
         nQ0    = nQ
         nTried = 0
         nFirst = 0
         DO t1 = 1, nBr
            IF ( t1 > nSorted )     THEN
               CALL StableSortByKey( brLen(1:nBr), order(1:nBr) )
               nSorted = nBr
            END IF
            i1 = brI(order(t1))
            j1 = brJ(order(t1))
            IF ( .NOT.isBridge( bridgeState(order(t1)), x, front, nFront, b0, nB, i1, j1 ) )     CYCLE
            nFirst = nFirst + 1
            IF ( nFirst > MAX_TRIES )     EXIT
            DO t2 = 1, nBr
               IF ( t2 > nSorted )     THEN
                  CALL StableSortByKey( brLen(1:nBr), order(1:nBr) )
                  nSorted = nBr
               END IF
               i2 = brI(order(t2))
               j2 = brJ(order(t2))
               di = MODULO(i2 - i1, nFront)
               dj = MODULO(j2 - j1, nB)
               IF ( di < nFront/4 .OR. di > (3*nFront)/4 )     CYCLE
               IF ( dj < nB/4 .OR. dj > (3*nB)/4 )     CYCLE
               IF ( .NOT.isBridge( bridgeState(order(t2)), x, front, nFront, b0, nB, i2, j2 ) )     CYCLE
               IF ( segmentsCross(x(:,front(i2)), x(:,b0+j2), x(:,front(i1)), x(:,b0+j1)) )     CYCLE
               nAll    = nAll0
               nQ      = nQ0
               nChords = 0
               CALL FillWithTwoChords( x, nAll, front, nFront, b0, nB, i1, j1, i2, j2, h, quads, nQ, ok, nChords )
               IF ( ok )     RETURN
               nTried = nTried + 1
               IF ( nTried >= MAX_TRIES )     EXIT
            END DO
         END DO
         nAll = nAll0
         nQ   = nQ0

      END SUBROUTINE FillBetweenLoops
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION isBridge( state, x, front, nFront, b0, nB, i, j )
         IMPLICIT NONE
!
!        ------------------------------------------------------------
!        bridgeOK for front node i and loop node j, checked only the
!        first time: state is BRIDGE_UNCHECKED until then, and keeps
!        the answer (BRIDGE_VALID or BRIDGE_INVALID)
!        ------------------------------------------------------------
!
         INTEGER       :: state, nFront, b0, nB, i, j, front(:)
         REAL(KIND=RP) :: x(:,:)

         IF ( state == BRIDGE_UNCHECKED )     THEN
            IF ( bridgeOK( x, front, nFront, b0, nB, i, j ) )     THEN
               state = BRIDGE_VALID
            ELSE
               state = BRIDGE_INVALID
            END IF
         END IF
         isBridge = state == BRIDGE_VALID

      END FUNCTION isBridge
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE StableSortByKey( key, order )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        order = the permutation of 1..SIZE(key) that sorts key upward.
!        Equal keys keep their order. A bottom-up merge sort.
!        ---------------------------------------------------------------
!
         REAL(KIND=RP) :: key(:)
         INTEGER       :: order(:)
         INTEGER       :: n, width, lo, mid, hi, a, b, k
         INTEGER       :: merged(SIZE(key))

         n = SIZE(key)
         DO k = 1, n
            order(k) = k
         END DO
         width = 1
         DO WHILE ( width < n )
            DO lo = 1, n, 2*width
               mid = MIN(lo + width - 1, n)
               hi  = MIN(lo + 2*width - 1, n)
               a   = lo
               b   = mid + 1
               DO k = lo, hi
                  IF ( b > hi )     THEN
                     merged(k) = order(a)
                     a         = a + 1
                  ELSE IF ( a > mid )     THEN
                     merged(k) = order(b)
                     b         = b + 1
                  ELSE IF ( key(order(a)) <= key(order(b)) )     THEN
                     merged(k) = order(a)
                     a         = a + 1
                  ELSE
                     merged(k) = order(b)
                     b         = b + 1
                  END IF
               END DO
            END DO
            order(1:n) = merged(1:n)
            width      = 2*width
         END DO

      END SUBROUTINE StableSortByKey
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION bridgeOK( x, front, nFront, b0, nB, i, j )
         IMPLICIT NONE
!
!        ---------------------------------------------------------
!        True if the segment from front node i to loop node j leaves
!        both into the region, and crosses no edge of either loop and
!        passes through no other node of them
!        ---------------------------------------------------------
!
         REAL(KIND=RP) :: x(:,:)
         INTEGER       :: nFront, b0, nB, i, j, front(:)
         INTEGER       :: k
         REAL(KIND=RP) :: a(2), b(2)
         INTEGER, EXTERNAL :: Loop

         bridgeOK = .FALSE.
         a = x(:,front(i))
         b = x(:,b0+j)
         IF ( cornerAngle( b, a, x(:,front(Loop(i+1,nFront))) ) >= &
              cornerAngle( x(:,front(Loop(i-1,nFront))), a, x(:,front(Loop(i+1,nFront))) ) )     RETURN
         IF ( cornerAngle( a, b, x(:,b0+Loop(j+1,nB)) ) >= &
              cornerAngle( x(:,b0+Loop(j-1,nB)), b, x(:,b0+Loop(j+1,nB)) ) )     RETURN
         DO k = 1, nFront
            IF ( k /= i )     THEN
               IF ( vertexOnSegment(x(:,front(k)), a, b) )     RETURN
            END IF
            IF ( k == i .OR. Loop(k+1,nFront) == i )     CYCLE
            IF ( segmentsCross(a, b, x(:,front(k)), x(:,front(Loop(k+1,nFront)))) )     RETURN
         END DO
         DO k = 1, nB
            IF ( k /= j )     THEN
               IF ( vertexOnSegment(x(:,b0+k), a, b) )     RETURN
            END IF
            IF ( k == j .OR. Loop(k+1,nB) == j )     CYCLE
            IF ( segmentsCross(a, b, x(:,b0+k), x(:,b0+Loop(k+1,nB))) )     RETURN
         END DO
         bridgeOK = .TRUE.
      END FUNCTION bridgeOK
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FillWithTwoChords( x, nAll, front, nFront, b0, nB, i1, j1, i2, j2, h, quads, nQ, ok, nChords )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        Cut the region between the front and the loop along the chords
!        front(i1)-loop(j1) and front(i2)-loop(j2), with nodes about h
!        apart and the parity that makes both polygons even, and split
!        both polygons into quads.
!        Polygon 1: front i1 -> i2, chord 2, loop j2 -> j1, chord 1.
!        Polygon 2: front i2 -> i1, chord 1 back, loop j1 -> j2, chord 2
!        back.
!        ---------------------------------------------------------------
!
         REAL(KIND=RP) :: x(:,:), h
         INTEGER       :: nAll, nFront, b0, nB, i1, j1, i2, j2, nQ
         INTEGER       :: front(:), quads(:,:), nChords(:)
         LOGICAL       :: ok

         INTEGER       :: k1, k2, c, n1, n2, kChord1, kChord2
         INTEGER       :: poly1(2*(nFront + nB) + 200), poly2(2*(nFront + nB) + 200)
         REAL(KIND=RP) :: len1, len2
         INTEGER, EXTERNAL :: Loop

         len1 = NORM2(x(:,b0+j1) - x(:,front(i1)))
         len2 = NORM2(x(:,b0+j2) - x(:,front(i2)))
         k1 = MAX(0, NINT(len1/h) - 1)
         k2 = MAX(0, NINT(len2/h) - 1)
         n1 = MODULO(i2 - i1, nFront) + 1 + MODULO(j1 - j2, nB) + 1 + k1 + k2
         IF ( MODULO(n1, 2) /= 0 )     THEN
            IF ( len2/h - 1.0_RP > k2 .OR. k2 == 0 )     THEN
               k2 = k2 + 1
            ELSE
               k2 = k2 - 1
            END IF
         END IF
         kChord1 = nAll
         DO c = 1, k1
            nAll = nAll + 1
            x(:,nAll) = x(:,b0+j1) + c*(x(:,front(i1)) - x(:,b0+j1))/(k1 + 1)
         END DO
         kChord2 = nAll
         DO c = 1, k2
            nAll = nAll + 1
            x(:,nAll) = x(:,front(i2)) + c*(x(:,b0+j2) - x(:,front(i2)))/(k2 + 1)
         END DO

         n1 = 0
         DO c = 0, MODULO(i2 - i1, nFront)
            n1 = n1 + 1
            poly1(n1) = front(Loop(i1 + c, nFront))
         END DO
         DO c = 1, k2
            n1 = n1 + 1
            poly1(n1) = kChord2 + c
         END DO
         DO c = 0, MODULO(j1 - j2, nB)
            n1 = n1 + 1
            poly1(n1) = b0 + Loop(j2 + c, nB)
         END DO
         DO c = 1, k1
            n1 = n1 + 1
            poly1(n1) = kChord1 + c
         END DO

         n2 = 0
         DO c = 0, MODULO(i1 - i2, nFront)
            n2 = n2 + 1
            poly2(n2) = front(Loop(i2 + c, nFront))
         END DO
         DO c = k1, 1, -1
            n2 = n2 + 1
            poly2(n2) = kChord1 + c
         END DO
         DO c = 0, MODULO(j2 - j1, nB)
            n2 = n2 + 1
            poly2(n2) = b0 + Loop(j1 + c, nB)
         END DO
         DO c = k2, 1, -1
            n2 = n2 + 1
            poly2(n2) = kChord2 + c
         END DO

         CALL SplitFill( x, nAll, poly1(1:n1), n1, h, quads, nQ, ok, nChords )
         IF ( .NOT.ok )     RETURN
         CALL SplitFill( x, nAll, poly2(1:n2), n2, h, quads, nQ, ok, nChords )

      END SUBROUTINE FillWithTwoChords
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE InterfaceCurveSizes( sizer, xs, nS, nPer, nCurves, turnMax, xOther, nOther, hs, hBand, isFeature )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        Element sizes along a closed interface curve sampled at xs, taken
!        from the curve geometry rather than from the background mesh.
!        The features are the joints of the chain. Each curve of the chain
!        is smooth, so a tight bend inside a curve is not a corner: its
!        turning sets the size like any other. The size is the smallest of
!          - the sizer's control size (base size, centers and lines),
!            limited so that minNumberOfElementsInsideArea fit across,
!          - the size with turnMax turning per element, where the turning
!            is measured between features so a corner does not shrink it,
!          - the distance, over closeCurveFactor, to a part of the curve
!            that is near in space but far along the curve, with no
!            feature between, or to a node on another curve, xOther.
!        hBand is that size, graded. hs is also at most the length of the
!        piece between two features, so that each piece gets an edge,
!        then graded.
!        ---------------------------------------------------------------
!
         TYPE(MeshSizer), POINTER :: sizer
         INTEGER       :: nS, nPer, nCurves, nOther
         REAL(KIND=RP) :: xs(2,nS), turnMax, xOther(2,*), hs(nS), hBand(nS)
         LOGICAL       :: isFeature(nS)

         REAL(KIND=RP), PARAMETER :: GROWTH = 0.5_RP, NEAR_RATIO = 3.0_RP
         INTEGER, PARAMETER       :: MAX_COARSE = 2000, CURVATURE_ITERATIONS = 6, CORNER_SAMPLES = 2
         REAL(KIND=RP) :: arc(nS+1), absTurn(nS), hBase(nS), hCurv(nS), hProx(nS), hNew(nS)
         REAL(KIND=RP) :: reach, xMin(2), xMax(2), perimeter, aSize, x3(3), u(2), v(2), d, dArc, turnSum, covered, dFwd, dBack
         INTEGER       :: i, j, k, iter, stride, nFeatures, kFwd, kBack
         INTEGER       :: featureCount(0:nS), stepsForward(nS), stepsBack(nS)
         REAL(KIND=RP) :: turnBefore(0:nS)
         INTEGER, EXTERNAL :: Loop

         arc(1) = 0.0_RP
         DO i = 1, nS
            arc(i+1) = arc(i) + NORM2(xs(:,MODULO(i,nS)+1) - xs(:,i))
         END DO
         perimeter = arc(nS+1)
         DO i = 1, nS
            u = xs(:,i) - xs(:,Loop(i-1,nS))
            v = xs(:,Loop(i+1,nS)) - xs(:,i)
            absTurn(i) = ABS(ATAN2(cross2(u,v), DOT_PRODUCT(u,v)))
         END DO
!
!        ----------------------------------
!        Control size and the area limit
!        ----------------------------------
!
         aSize = MINVAL(MAXVAL(xs, DIM = 2) - MINVAL(xs, DIM = 2))/minNumberOfElementsInsideArea
         DO i = 1, nS
            x3 = [xs(1,i), xs(2,i), 0.0_RP]
            hBase(i) = MIN(controlSize(sizer, x3), aSize)
         END DO
!
!        ---------------------------------
!        Features: the joints of the chain
!        ---------------------------------
!
         isFeature = .FALSE.
         DO k = 1, nCurves
            isFeature((k-1)*nPer + 1) = .TRUE.
         END DO
!
!        ------------------------------------------------------------
!        The turning at the samples next to a joint belongs to the
!        joint, not to the curvature
!        ------------------------------------------------------------
!
         DO i = 1, nS
            IF ( .NOT.isFeature(i) )     CYCLE
            DO k = -CORNER_SAMPLES, CORNER_SAMPLES
               absTurn(MODULO(i - 1 + k, nS) + 1) = 0.0_RP
            END DO
         END DO
!
!        ---------------------------------------------------------------
!        Curvature: the turning over a window one size long, not crossing
!        a feature, sets the size to turnMax per element. The window ends
!        are found by bisection and the turning in it from its sums.
!        ---------------------------------------------------------------
!
         turnBefore(0) = 0.0_RP
         DO i = 1, nS
            turnBefore(i) = turnBefore(i-1) + absTurn(i)
         END DO
         CALL StepsToFeatures( isFeature, nS, stepsForward, stepsBack )

         hCurv = hBase
         DO iter = 1, CURVATURE_ITERATIONS
            DO i = 1, nS
               turnSum = 0.0_RP
               IF ( .NOT.isFeature(i) )     turnSum = absTurn(i)
               kFwd  = windowSteps( arc, nS, perimeter, i,  1, 0.5_RP*hCurv(i), MIN(nS/2, stepsForward(i) - 1) )
               kBack = windowSteps( arc, nS, perimeter, i, -1, 0.5_RP*hCurv(i), MIN(nS/2, stepsBack(i) - 1) )
               dFwd  = 0.0_RP
               IF ( kFwd > 0 )     THEN
                  dFwd    = MODULO(arc(MODULO(i - 1 + kFwd, nS) + 1) - arc(i), perimeter)
                  turnSum = turnSum + cyclicSum( turnBefore, nS, MODULO(i, nS) + 1, kFwd )
               END IF
               dBack = 0.0_RP
               IF ( kBack > 0 )     THEN
                  dBack   = MODULO(arc(i) - arc(MODULO(i - 1 - kBack, nS) + 1), perimeter)
                  turnSum = turnSum + cyclicSum( turnBefore, nS, MODULO(i - 1 - kBack, nS) + 1, kBack )
               END IF
               covered = dFwd + dBack
               IF ( turnSum > 0.0_RP .AND. covered > 0.0_RP )     THEN
                  hNew(i) = MIN(hBase(i), turnMax*covered/turnSum)
               ELSE
                  hNew(i) = hBase(i)
               END IF
            END DO
            hCurv = hNew
         END DO
!
!        -------------------------------------------------------------
!        Proximity, on a coarser set of samples: parts of the curve near
!        in space but far along it, not separated by a feature
!        -------------------------------------------------------------
!
         featureCount(0) = 0
         DO i = 1, nS
            featureCount(i) = featureCount(i-1)
            IF ( isFeature(i) )     featureCount(i) = featureCount(i) + 1
         END DO
         nFeatures = featureCount(nS)
         stride = MAX(1, nS/MAX_COARSE)
         hProx  = HUGE(1.0_RP)
         DO i = 1, nS, stride
            DO j = i + stride, nS, stride
               d    = NORM2(xs(:,j) - xs(:,i))
               dArc = arc(j) - arc(i)
               IF ( dArc <= 0.5_RP*perimeter )     THEN
                  k = featureCount(j-1) - featureCount(i)
               ELSE
                  dArc = perimeter - dArc
                  k = nFeatures - featureCount(j) + featureCount(i-1)
               END IF
               IF ( dArc > NEAR_RATIO*d .AND. k == 0 )     THEN
                  hProx(i) = MIN(hProx(i), d/closeCurveFactor)
                  hProx(j) = MIN(hProx(j), d/closeCurveFactor)
               END IF
            END DO
         END DO
         reach = closeCurveFactor*MAXVAL(hBase)
         xMin  = MINVAL(xs, DIM = 2) - reach
         xMax  = MAXVAL(xs, DIM = 2) + reach
         DO j = 1, nOther
            IF ( ANY(xOther(:,j) < xMin) .OR. ANY(xOther(:,j) > xMax) )     CYCLE
            DO i = 1, nS, stride
               hProx(i) = MIN(hProx(i), NORM2(xOther(:,j) - xs(:,i))/closeCurveFactor)
            END DO
         END DO
         DO i = 1, nS
            hProx(i) = hProx(((i-1)/stride)*stride + 1)
         END DO

         hBand = MIN(hBase, hCurv, hProx)
         hs    = hBand
         CALL GradeSizes( hBand, xs, nS, GROWTH )
!
!        ---------------------------------------------------
!        At most the length of the piece between two features
!        ---------------------------------------------------
!
         DO i = 1, nS
            IF ( .NOT.isFeature(i) )     CYCLE
            j = i
            DO k = 1, nS
               j = MODULO(j, nS) + 1
               IF ( isFeature(j) )     EXIT
            END DO
            d = MODULO(arc(j) - arc(i), perimeter)
            IF ( d == 0.0_RP )     d = perimeter
            k = i
            DO
               hs(k) = MIN(hs(k), d)
               k = MODULO(k, nS) + 1
               IF ( k == j )     EXIT
            END DO
         END DO
         CALL GradeSizes( hs, xs, nS, GROWTH )

      END SUBROUTINE InterfaceCurveSizes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE StepsToFeatures( isFeature, nS, stepsForward, stepsBack )
         IMPLICIT NONE
!
!        ------------------------------------------------------------
!        The number of steps from each sample of a closed curve to the
!        next feature forward and back, nS if there is no other one
!        ------------------------------------------------------------
!
         INTEGER :: nS, stepsForward(nS), stepsBack(nS)
         LOGICAL :: isFeature(nS)
         INTEGER :: i, k, pass

         stepsForward = nS
         stepsBack    = nS
         k = nS
         DO pass = 1, 2
            DO i = nS, 1, -1
               k = MIN(k + 1, nS)
               stepsForward(i) = MIN(stepsForward(i), k)
               IF ( isFeature(i) )     k = 0
            END DO
         END DO
         k = nS
         DO pass = 1, 2
            DO i = 1, nS
               k = MIN(k + 1, nS)
               stepsBack(i) = MIN(stepsBack(i), k)
               IF ( isFeature(i) )     k = 0
            END DO
         END DO
      END SUBROUTINE StepsToFeatures
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION windowSteps( arc, nS, perimeter, i, direction, reach, kMax )
         IMPLICIT NONE
!
!        ------------------------------------------------------------
!        The largest k <= kMax such that the sample k steps from i in
!        the direction (1 forward, -1 back) is at most reach away from
!        it along the curve. The distance grows with k, so bisect.
!        ------------------------------------------------------------
!
         INTEGER       :: nS, i, direction, kMax
         REAL(KIND=RP) :: arc(nS+1), perimeter, reach
         INTEGER       :: lo, hi, mid

         lo = 0
         hi = kMax
         DO WHILE ( lo < hi )
            mid = (lo + hi + 1)/2
            IF ( arcSteps(arc, nS, perimeter, i, direction, mid) > reach )     THEN
               hi = mid - 1
            ELSE
               lo = mid
            END IF
         END DO
         windowSteps = lo
      END FUNCTION windowSteps
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION arcSteps( arc, nS, perimeter, i, direction, k )
         IMPLICIT NONE
!
!        ----------------------------------------------------------
!        Distance along a closed curve from sample i to the sample k
!        steps from it in the direction (1 forward, -1 back)
!        ----------------------------------------------------------
!
         INTEGER       :: nS, i, direction, k
         REAL(KIND=RP) :: arc(nS+1), perimeter
         INTEGER       :: j

         j = MODULO(i - 1 + direction*k, nS) + 1
         IF ( direction > 0 )     THEN
            arcSteps = MODULO(arc(j) - arc(i), perimeter)
         ELSE
            arcSteps = MODULO(arc(i) - arc(j), perimeter)
         END IF
      END FUNCTION arcSteps
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION cyclicSum( sumBefore, nS, first, length )
         IMPLICIT NONE
!
!        ------------------------------------------------------------
!        Sum of length consecutive values of a closed sequence from
!        first on, from the running sums sumBefore(m) of values 1..m
!        ------------------------------------------------------------
!
         INTEGER       :: nS, first, length
         REAL(KIND=RP) :: sumBefore(0:nS)
         INTEGER       :: last

         last = first + length - 1
         IF ( last <= nS )     THEN
            cyclicSum = sumBefore(last) - sumBefore(first - 1)
         ELSE
            cyclicSum = sumBefore(nS) - sumBefore(first - 1) + sumBefore(last - nS)
         END IF
      END FUNCTION cyclicSum
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GradeSizes( h, xs, nS, growth )
         IMPLICIT NONE
!
!        ------------------------------------------------------------
!        Limit the sizes h along the closed polyline xs so that they
!        grow by at most growth times the distance along it
!        ------------------------------------------------------------
!
         INTEGER       :: nS
         REAL(KIND=RP) :: h(nS), xs(2,nS), growth
         INTEGER       :: i, j, k, iter

         DO iter = 1, 2
            DO i = 1, 2*nS
               j = MODULO(i-1, nS) + 1
               k = MODULO(i-2, nS) + 1
               h(j) = MIN(h(j), h(k) + growth*NORM2(xs(:,j) - xs(:,k)))
            END DO
            DO i = 2*nS, 1, -1
               j = MODULO(i-1, nS) + 1
               k = MODULO(i, nS) + 1
               h(j) = MIN(h(j), h(k) + growth*NORM2(xs(:,j) - xs(:,k)))
            END DO
         END DO
      END SUBROUTINE GradeSizes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RemoveDoublets( x, nFixed, nAll, quads, nQ )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        A new node (index above nFixed) in only two quads, such as a node
!        on a fill chord that neither side used again, has an angle near
!        180 degrees in both, which smoothing cannot fix. Merge the two
!        quads, (v,a,p,b) and (v,b,q,a), into (a,p,b,q) when that is a
!        valid quad, which leaves v unused.
!        ---------------------------------------------------------------
!
         REAL(KIND=RP) :: x(:,:)
         INTEGER       :: nFixed, nAll, nQ
         INTEGER       :: quads(:,:)

         INTEGER       :: v, j, k, c, nAt, at(2), pos(2), merged(4)
         LOGICAL       :: changed
         REAL(KIND=RP) :: corners(2,4)
         INTEGER, EXTERNAL :: Loop

         changed = .TRUE.
         DO WHILE ( changed )
            changed = .FALSE.
            DO v = nFixed + 1, nAll
               nAt = 0
               DO j = 1, nQ
                  DO k = 1, 4
                     IF ( quads(k,j) == v )     THEN
                        nAt = nAt + 1
                        IF ( nAt <= 2 )     THEN
                           at(nAt)  = j
                           pos(nAt) = k
                        END IF
                     END IF
                  END DO
               END DO
               IF ( nAt /= 2 )     CYCLE
               merged(1) = quads(Loop(pos(1)+1,4), at(1))
               merged(2) = quads(Loop(pos(1)+2,4), at(1))
               merged(3) = quads(Loop(pos(1)+3,4), at(1))
               merged(4) = quads(Loop(pos(2)+2,4), at(2))
               IF ( quads(Loop(pos(2)+1,4), at(2)) /= merged(3) .OR. &
                    quads(Loop(pos(2)+3,4), at(2)) /= merged(1) )     CYCLE
               DO c = 1, 4
                  corners(:,c) = x(:,merged(c))
               END DO
               IF ( polygonArea(corners, 4) <= 0.0_RP )     CYCLE
               IF ( minQuadSine(corners) <= 0.0_RP )     CYCLE
               quads(:,at(1)) = merged
               quads(:,at(2)) = quads(:,nQ)
               nQ      = nQ - 1
               changed = .TRUE.
            END DO
         END DO
      END SUBROUTINE RemoveDoublets
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION worstQuadQuality( x, nCurve, nFixed, nAll, quads, nQ )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        The mean quality of the worst tenth of the quads, after a few
!        Laplace smoothing passes of a copy of the nodes above nFixed, as
!        the smoother would move them. Nodes 1..nCurve are on the curve,
!        nCurve+1..nFixed on the edge of the remaining mesh. The quality
!        of a quad is the smallest sine of its angles times its shortest
!        over its longest side, so both a flat and a stretched quad score
!        low. The angles at the edge of the remaining mesh are left out:
!        the smoother moves those nodes later, and an edge that runs
!        straight through a node gives a 180 degree angle there.
!        ---------------------------------------------------------------
!
         REAL(KIND=RP) :: x(:,:)
         INTEGER       :: nCurve, nFixed, nAll, nQ, quads(:,:)
         INTEGER, PARAMETER :: SMOOTHING_PASSES = 20
         REAL(KIND=RP) :: y(2,nAll), sumX(2,nAll), side(4), quality(nQ), u(2), v(2), sine, q
         INTEGER       :: nNeighbors(nAll), iter, j, k, a, b, nWorst
         INTEGER, EXTERNAL :: Loop

         y = x(:,1:nAll)
         DO iter = 1, SMOOTHING_PASSES
            sumX       = 0.0_RP
            nNeighbors = 0
            DO j = 1, nQ
               DO k = 1, 4
                  a = quads(k,j)
                  b = quads(Loop(k+1,4),j)
                  sumX(:,a) = sumX(:,a) + y(:,b)
                  sumX(:,b) = sumX(:,b) + y(:,a)
                  nNeighbors(a) = nNeighbors(a) + 1
                  nNeighbors(b) = nNeighbors(b) + 1
               END DO
            END DO
            DO a = nFixed + 1, nAll
               IF ( nNeighbors(a) > 0 )     y(:,a) = sumX(:,a)/nNeighbors(a)
            END DO
         END DO

         DO j = 1, nQ
            sine = 1.0_RP
            DO k = 1, 4
               side(k) = NORM2(y(:,quads(Loop(k+1,4),j)) - y(:,quads(k,j)))
               a = quads(k,j)
               IF ( a > nCurve .AND. a <= nFixed )     CYCLE
               u = y(:,quads(Loop(k+1,4),j)) - y(:,a)
               v = y(:,quads(Loop(k-1,4),j)) - y(:,a)
               sine = MIN(sine, cross2(u,v)/MAX(NORM2(u)*NORM2(v), TINY(1.0_RP)))
            END DO
            quality(j) = sine*MINVAL(side)/MAX(MAXVAL(side), TINY(1.0_RP))
         END DO
!
!        Mean of the nWorst lowest: a partial selection sort
!
         nWorst = MAX(1, nQ/10)
         DO j = 1, nWorst
            k = j - 1 + MINLOC(quality(j:nQ), DIM = 1)
            q          = quality(j)
            quality(j) = quality(k)
            quality(k) = q
         END DO
         worstQuadQuality = SUM(quality(1:nWorst))/nWorst
      END FUNCTION worstQuadQuality
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION minQuadSine( corners )
         IMPLICIT NONE
!
!        ---------------------------------------------------------
!        The smallest sine of the corner angles of a quad, negative
!        where a corner is reflex
!        ---------------------------------------------------------
!
         REAL(KIND=RP) :: corners(2,4)
         REAL(KIND=RP) :: u(2), v(2)
         INTEGER       :: k
         INTEGER, EXTERNAL :: Loop

         minQuadSine = 1.0_RP
         DO k = 1, 4
            u = corners(:,Loop(k+1,4)) - corners(:,k)
            v = corners(:,Loop(k-1,4)) - corners(:,k)
            minQuadSine = MIN(minQuadSine, cross2(u,v)/(NORM2(u)*NORM2(v)))
         END DO
      END FUNCTION minQuadSine

   END MODULE MeshGenerationMethods

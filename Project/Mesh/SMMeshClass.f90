!
!////////////////////////////////////////////////////////////////////////
!
!      SMMeshClass.f90
!      Created: July 26, 2013 3:51 PM 
!      By: NocturnalAviationSoftware  
!
!////////////////////////////////////////////////////////////////////////
!
      Module SMMeshClass
      
      USE FTObjectClass
      USE FTLinkedListClass
      USE FTLinkedListIteratorClass
      USE SMMeshObjectsModule
      USE SegmentedCurveArrayClass
      USE FTMutableObjectArrayClass
      
      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      INTEGER, PARAMETER :: BOUNDARY_EDGES = 0, INTERFACE_EDGES = 1
      INTEGER, PARAMETER :: NODES = 0, EDGES = 1, ELEMENTS = 2
!
!     ------------------
!     Class derived type
!     ------------------
!
      TYPE, EXTENDS(FTObject) :: SMMesh
!
!        ----
!        Data
!        ----
!
         CLASS(FTLinkedList), POINTER :: nodes    => NULL()
         CLASS(FTLinkedList), POINTER :: elements => NULL()
         CLASS(FTLinkedList), POINTER :: edges    => NULL()
!
!        ---------
!        Iterators
!        ---------
!
         CLASS(FTLinkedListIterator), POINTER :: nodesIterator => NULL()
         CLASS(FTLinkedListIterator), POINTER :: edgesIterator => NULL()
         CLASS(FTLinkedListIterator), POINTER :: elementsIterator => NULL()
         
         INTEGER, PRIVATE :: elementID = 0, nodeID = 0, edgeID = 0
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: init             => initSMMesh        
         PROCEDURE :: destruct         => destructSMMesh
         PROCEDURE :: printDescription => printMeshDescription
         PROCEDURE :: buildEdgeList
         PROCEDURE :: newNodeID
         PROCEDURE :: newElementID
         PROCEDURE :: newEdgeID
         PROCEDURE :: renumberAllLists
         PROCEDURE :: renumberObjects
         PROCEDURE :: destroyEdgeArrays
         PROCEDURE :: syncEdges
         PROCEDURE :: permuteMeshDirection
      END TYPE SMMesh
!
!      ---------------------------------------------------------------------
!      sourceNodeLocalID gives the local ID of the nodes attached to a node.
!      For example, element local node 1 connects by edges to
!      element local nodes 4 and 2.
!      ---------------------------------------------------------------------
!
       INTEGER  :: sourceNodeLocalID(2,4) = RESHAPE([4,2,1,3,2,4,1,3],[2,4])
!
!      ------------------------------------------------------
!      Discrete versions of the boundary and interface curves
!      ------------------------------------------------------
!
      TYPE CurveArrayPtr
         CLASS(SegmentedCurveArray), POINTER :: curveArray => NULL()
      END TYPE CurveArrayPtr
      
      CLASS(SegmentedCurveArray)       , POINTER :: outerBoundaryCurve => NULL()
      TYPE(CurveArrayPtr), DIMENSION(:), POINTER :: interiorCurves => NULL()
      TYPE(CurveArrayPtr), DIMENSION(:), POINTER :: interfaceCurves => NULL()
!
!     ------------------------------------------
!     Convenience arrays for dealing with curves
!     ------------------------------------------
!
      REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE :: aPointInsideTheCurve
      INTEGER      , DIMENSION(:)  , ALLOCATABLE :: curveTypeForID
!
!     ---------------------------------
!     Arrays for keeping track of edges
!     ---------------------------------
!
      TYPE(FTMutableObjectArray), POINTER     :: boundaryEdgesArray => NULL()
      INTEGER                   , ALLOCATABLE :: boundaryEdgesType(:)
!
!     ========     
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////  
!  
      SUBROUTINE initSMMesh(self)  
         IMPLICIT NONE
         CLASS(SMMesh) :: self
         
         CALL self % FTObject % init()
         
         ALLOCATE( self % nodes )
         ALLOCATE( self % elements )
         ALLOCATE( self % edges )
         
         CALL self % nodes    % init()
         CALL self % elements % init()
         CALL self % edges    % init()
         
         ALLOCATE( self % nodesIterator )
         ALLOCATE( self % edgesIterator )
         ALLOCATE( self % elementsIterator )
         
         CALL self % nodesIterator    % initWithFTLinkedList( self % nodes )
         CALL self % edgesIterator    % initWithFTLinkedList( self % edges )
         CALL self % elementsIterator % initWithFTLinkedList( self % elements )
         
         self % elementID = 0
         self % nodeID    = 0
         self % edgeID    = 0
         
         outerBoundaryCurve => NULL()
         interiorCurves     => NULL()
         interfaceCurves    => NULL()
      
      END SUBROUTINE initSMMesh
!
!////////////////////////////////////////////////////////////////////////  
!  
      SUBROUTINE destructSMMesh(self)  
         IMPLICIT NONE  
         CLASS(SMMesh) :: self
         
         CALL self % nodesIterator    % release()
         CALL self % edgesIterator    % release()
         CALL self % elementsIterator % release()
         
         IF ( self % nodesIterator % isUnreferenced() )     THEN
            DEALLOCATE(self % nodesIterator) 
         END IF 
         IF ( self % edgesIterator % isUnreferenced() )     THEN
            DEALLOCATE(self % edgesIterator) 
         END IF 
         IF ( self % elementsIterator % isUnreferenced() )     THEN
            DEALLOCATE(self % elementsIterator) 
         END IF 
         
         CALL self % nodes % release()
         IF ( self % nodes % isUnreferenced() )     THEN
            DEALLOCATE(self % nodes) 
         END IF 
         
         CALL self % elements % release()
         IF ( self % elements % isUnreferenced() )     THEN
            DEALLOCATE(self % elements) 
         END IF 
         
         CALL self % edges % release()
         IF ( self % edges % isUnreferenced() )     THEN
            DEALLOCATE(self % edges) 
         END IF 
         
      END SUBROUTINE destructSMMesh
!
!//////////////////////////////////////////////////////////////////////// 
! 
      INTEGER FUNCTION newNodeID(self)  
         IMPLICIT NONE  
         CLASS(SMMesh) :: self
         self % nodeID = self % nodeID + 1
         newNodeID     = self % nodeID
      END FUNCTION newNodeID
!
!//////////////////////////////////////////////////////////////////////// 
! 
      INTEGER FUNCTION newElementID(self)  
         IMPLICIT NONE  
         CLASS(SMMesh) :: self
         self % elementID = self % elementID + 1
         newElementID     = self % elementID
      END FUNCTION newElementID
!
!//////////////////////////////////////////////////////////////////////// 
! 
      INTEGER FUNCTION newEdgeID(self)  
         IMPLICIT NONE  
         CLASS(SMMesh) :: self
         self % edgeID = self % edgeID + 1
         newEdgeID     = self % edgeID
      END FUNCTION newEdgeID
!
!////////////////////////////////////////////////////////////////////////  
!  
      SUBROUTINE printMeshDescription(self,iUnit)  
         IMPLICIT NONE  
         CLASS(SMMesh) :: self
         INTEGER       :: iUnit
         
         WRITE(iUnit,*) "Mesh Nodes..."
         WRITE(iUnit,*)
         CALL self % nodes % printDescription(iUnit)
          
         WRITE(iUnit,*) "Mesh Edges..."
         WRITE(iUnit,*)
         CALL self % edges % printDescription(iUnit)
         
         WRITE(iUnit,*) "Mesh Elements..."
         WRITE(iUnit,*)
         CALL self % elements % printDescription(iUnit)
        
      END SUBROUTINE printMeshDescription
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE buildEdgeList(self)  
         USE FTHashTableClass
         USE FTLinkedListIteratorClass
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMMesh) :: self
!
!        ---------------
!        Local Variables
!        ---------------
!
         TYPE(FTHashTable)          :: hashTable
         TYPE(FTLinkedListIterator) :: elementIterator
         CLASS(SMEdge)   , POINTER  :: edge => NULL()
         CLASS(FTObject) , POINTER  :: obj => NULL()
         CLASS(SMElement), POINTER  :: element => NULL()
         CLASS(SMNode)   , POINTER  :: node => NULL(), startNode => NULL(), endNode => NULL()
         
         INTEGER                    :: nNodes
         INTEGER                    :: nodeIDs(4), endNodes(2)
         INTEGER                    :: k         , edgeID
         INTEGER                    :: key1,key2
         
         nNodes = self % nodes % COUNT()
         CALL hashTable % initWithSize(nNodes)
         
         CALL elementIterator % initWithFTLinkedList( self % elements )
         CALL elementIterator % setToStart()
         
         DO WHILE (.NOT.elementIterator % isAtEnd())
            obj => elementIterator % object()
            CALL cast(obj,element)
!
!           -------------------
!           Gather the node ids
!           -------------------
!
            DO k = 1, 4
               obj => element % nodes % objectAtIndex(k)
               CALL cast(obj,node)
               nodeIDs(k) =  node % id
            END DO  
!
!           ---------------------------------------
!           Loop over the four edges of the element
!           ---------------------------------------
!
            DO k = 1, 4
               endNodes = [ nodeIDs(edgeMap(1,k)), nodeIDs(edgeMap(2,k)) ]
               key1     = Hash1(endNodes)
               key2     = Hash2(endNodes)
               IF ( hashTable % containsKeys(key1,key2) )     THEN
!
!                 ---------------------------------------------------
!                 This edge already exists, so add this element to it
!                 ---------------------------------------------------
!
                  obj => hashTable % objectForKeys(key1,key2) 
                  CALL cast(obj,edge)
                  edge % elements(2) % element => element
                  CALL element % retain()
                  edge % elementSide(2) = k
               ELSE 
!
!                 ------------------------------------------------
!                 This is a new edge, so add it to the mesh's list
!                 of edges and to the hash table
!                 ------------------------------------------------
!
                  obj => element % nodes % objectAtIndex(edgeMap(1,k))
                  CALL cast(obj,startNode)
                  obj => element % nodes % objectAtIndex(edgeMap(2,k))
                  CALL cast(obj,endNode)
                  
                  ALLOCATE(edge)
                  edgeID = self % newEdgeID()
                  CALL edge % initWithNodesAndID(startNode,endNode,edgeID)
                  edge % elements(1) % element => element
                  CALL element % retain()
                  edge % elementSide(1) = k
!
!                 -----------------------------
!                 Add the edge to the edge list
!                 -----------------------------
!
                  obj => edge
                  CALL self % edges % add(obj)
                  CALL edge % release()
                  
                  CALL hashTable % addObjectForKeys(obj,key1,key2)
               END IF 
               
            END DO 
            
            CALL elementIterator % moveToNext()
         END DO
!
!        --------
!        Clean up
!        --------
!
         CALL elementIterator % release()
         CALL hashTable % release()
         
      END SUBROUTINE buildEdgeList
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE permuteMeshDirection(self,pmutation)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMMesh) :: self
         INTEGER       :: pmutation
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject), POINTER  :: obj
         CLASS(SMNode)  , POINTER  :: node
         REAL(KIND=RP)             :: x(3)
         
         CALL self % nodesIterator % setToStart()
         DO WHILE( .NOT. self % nodesIterator % isAtEnd() )
         
            obj => self % nodesIterator % object()
            CALL castToSMNode(obj,node)
            
            x  = CSHIFT(node % x, SHIFT = -pmutation)
            node % x = x
            
            CALL self % nodesIterator % moveToNext()
         END DO 
      END SUBROUTINE permuteMeshDirection
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DoLazyDelete( mesh )
!
!     ----------------------------------------------------------------
!     Remove items that are marked for removal. Renumber as necessary.
!     ----------------------------------------------------------------
!
         IMPLICIT NONE
         CLASS(SMMesh) :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMElement), POINTER   :: e    => NULL()
         CLASS(SMNode)   , POINTER   :: node => NULL()
         CLASS(SMEdge)   , POINTER   :: edge => NULL()
         
         CLASS(FTLinkedList)        , POINTER :: list => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()
         
         LOGICAL                              :: takeStep
         INTEGER                              :: k
!
!        ---------------------------------------------------
!        Mark all nodes for removal and unmark them for
!        all elements and edges that are not removed. This
!        Way we can delete unused nodes from the nodes list.
!        ---------------------------------------------------
!
         iterator => mesh % nodesIterator
         CALL iterator % setToStart()
         DO WHILE( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL cast(obj,node)
            node % activeStatus = REMOVE 
            CALL iterator % movetoNext()
         END DO 
!
!        ------------------------------------------
!        Remove elements and their associated nodes
!        ------------------------------------------
!
         list     => mesh % elements
         iterator => mesh % elementsIterator

         CALL iterator % setToStart()
         DO WHILE( .NOT.iterator % isAtEnd())
            obj       => iterator % object()
            
            CALL cast(obj,e)
            IF ( e % remove )     THEN
               takeStep = .FALSE.
               CALL iterator % removeCurrentRecord()
            ELSE
               DO k = 1, 4
                  obj => e % nodes % objectAtIndex(k)
                  CALL cast(obj,node)
                  node % activeStatus = NONE 
               END DO
               takeStep  =  .TRUE.
            END IF
            
            IF(takeStep) CALL iterator % moveToNext()
         END DO  
!
!        ------------
!        Remove edges
!        ------------
!
         IF( ASSOCIATED( mesh % edges ) )     THEN
            list     => mesh % edges
            iterator => mesh % edgesIterator
            
            CALL iterator % setToStart()
            DO WHILE( .NOT.iterator % isAtEnd())
               takeStep  =  .TRUE.
               obj       => iterator % object()
               
               CALL cast(obj,edge)
               IF ( edge % remove )     THEN
                  takeStep = .FALSE.
                  CALL iterator % removeCurrentRecord()
               ELSE
                  edge % nodes(1) % node % activeStatus = NONE
                  edge % nodes(2) % node % activeStatus = NONE
               END IF
               
               IF(takeStep) CALL iterator % moveToNext()
            END DO
         END IF
!
!        ----------------------------------------------
!        Remove nodes that have been marked for removal
!        ----------------------------------------------
!
         list     => mesh % nodes
         iterator => mesh % nodesIterator
         
         CALL iterator % setToStart()
         DO WHILE( .NOT.iterator % isAtEnd())
            takeStep  =  .TRUE.
            obj       => iterator % object()
            
            CALL cast(obj,node)
            IF ( node % activeStatus == REMOVE )     THEN
               takeStep = .FALSE.
               CALL iterator % removeCurrentRecord()
            END IF
            
            IF(takeStep) CALL iterator % moveToNext()
         END DO  
!
!        --------------
!        Renumber items
!        --------------
!
         CALL mesh % renumberAllLists()
         
      END SUBROUTINE DoLazyDelete
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE renumberObjects(self,whichIterator)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMMesh) :: self
         INTEGER       :: whichIterator
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)            , POINTER :: obj => NULL()
         CLASS(SMMeshObject)        , POINTER :: mo => NULL()
         CLASS(FTLinkedListIterator), POINTER  :: iterator => NULL()
         
         INTEGER                              :: j
         
         SELECT CASE ( whichIterator )
            CASE( NODES ) 
               iterator => self % nodesIterator
            CASE( EDGES )
               iterator => self % edgesIterator
            CASE( ELEMENTS )
               iterator => self % elementsIterator 
            CASE DEFAULT 
         END SELECT 
         
         IF ( ASSOCIATED(iterator) )     THEN
            CALL iterator % setToStart()
            j = 1
            DO WHILE(.NOT.iterator % isAtEnd())
               obj     => iterator % object()
               CALL castToSMMeshObject(obj,mo)
               IF ( ASSOCIATED(mo) )     THEN
                  mo % id =  j
                  j       =  j + 1
               ELSE
                  PRINT *, "unassociated pointer in list"!DEBUG
               END IF 
               CALL iterator % moveToNext()
            END DO  
         END IF
         
         SELECT CASE ( whichIterator )
            CASE( NODES ) 
               self % nodeID = j-1
            CASE( EDGES )
               self % edgeID = j-1
            CASE( ELEMENTS )
               self % elementID = j-1
            CASE DEFAULT 
         END SELECT 
         
      END SUBROUTINE renumberObjects
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE renumberAllLists(self)  
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMMesh) :: self
         
         CALL renumberObjects( self , ELEMENTS )
         CALL renumberObjects( self , NODES )
         CALL renumberObjects( self , EDGES )
         
      END SUBROUTINE renumberAllLists
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destroyEdgeArrays(self)  
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMMesh) :: self
!
!        ----------------
!        Local varaiables
!        ----------------
!
         
         CALL boundaryEdgesArray % release()
         IF ( boundaryEdgesArray % isUnreferenced() )     THEN
            DEALLOCATE(boundaryEdgesArray)
            IF(ALLOCATED(boundaryEdgesType)) DEALLOCATE(boundaryEdgesType)
            boundaryEdgesArray => NULL()
         END IF 
      END SUBROUTINE destroyEdgeArrays
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE syncEdges(self)
         IMPLICIT NONE  
         CLASS(SMMesh) :: self
!
!        ------------------------
!        Ensure edges are in sync
!        ------------------------
!
         CALL self % edgesIterator % release()
         IF ( self % edgesIterator % isUnreferenced() )     THEN
            DEALLOCATE(self % edgesIterator) 
         END IF 
         
         CALL self % edges % release()
         IF ( self % edges % isUnreferenced() )     THEN
            DEALLOCATE(self % edges) 
         END IF
         ALLOCATE(self % edges)
         ALLOCATE(self % edgesIterator)
         
         CALL self % edges % init()
         CALL self % buildEdgeList()
         CALL self % edgesIterator % initWithFTLinkedList( self % edges )

      END SUBROUTINE syncEdges
      
      END Module SMMeshClass

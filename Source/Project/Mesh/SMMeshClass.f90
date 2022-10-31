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
! HOHQMesh contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend, 
!    https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
! * `fmin`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
! * `spline`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
! * `seval`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
!
! --- End License
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
         INTEGER                                     :: polynomialOrder
         CLASS(FTLinkedList), POINTER                :: nodes    => NULL()
         CLASS(FTLinkedList), POINTER                :: elements => NULL()
         CLASS(FTLinkedList), POINTER                :: edges    => NULL()
         INTEGER, DIMENSION(:)         , ALLOCATABLE :: curveTypeForID
         CHARACTER(LEN=:), DIMENSION(:), ALLOCATABLE :: materialNameForID
!
!        ---------
!        Iterators
!        ---------
!
         TYPE (FTLinkedListIterator), POINTER :: nodesIterator => NULL()
         TYPE (FTLinkedListIterator), POINTER :: edgesIterator => NULL()
         TYPE (FTLinkedListIterator), POINTER :: elementsIterator => NULL()
         
         INTEGER, PRIVATE :: elementID = 0, nodeID = 0, edgeID = 0
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: init             => initSMMesh        
         FINAL     :: destructSMMesh
         PROCEDURE :: printDescription => printMeshDescription
         PROCEDURE :: buildEdgeList
         PROCEDURE :: newNodeID
         PROCEDURE :: newElementID
         PROCEDURE :: newEdgeID
         PROCEDURE :: renumberAllLists
         PROCEDURE :: renumberObjects
         PROCEDURE :: destroyEdgeArrays
         PROCEDURE :: syncEdges
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
         boundaryEdgesArray => NULL()
      
      END SUBROUTINE initSMMesh
!
!////////////////////////////////////////////////////////////////////////  
!  
      SUBROUTINE destructSMMesh(self)  
         IMPLICIT NONE  
         TYPE (SMMesh) :: self
         
         CALL releaseFTLinkedListIterator(self % nodesIterator)
         CALL releaseFTLinkedListIterator(self % edgesIterator)
         CALL releaseFTLinkedListIterator(self % elementsIterator)
         
         CALL releaseFTLinkedList(self % nodes)
         CALL releaseFTLinkedList(self % elements)
         CALL releaseFTLinkedList(self % edges)
         
         IF(ALLOCATED(self % curveTypeForID))    DEALLOCATE(self % curveTypeForID)
         IF(ALLOCATED(aPointInsideTheCurve))     DEALLOCATE(aPointInsideTheCurve)
         IF(ALLOCATED(self % materialNameForID)) DEALLOCATE(self % materialNameForID)
         
         CALL destroyEdgeArrays(self)
         
      END SUBROUTINE destructSMMesh
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseMesh(self)  
         IMPLICIT NONE
         TYPE(SMMesh)  , POINTER :: self
         CLASS(FTObject), POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseMesh
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
         USE FTSparseMatrixClass
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
         TYPE(FTSparseMatrix)       :: hashTable
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
                  CALL releaseSMEdge(self = edge)
                  
                  CALL hashTable % addObjectForKeys(obj,key1,key2)
               END IF 
               
            END DO 
            
            CALL elementIterator % moveToNext()
         END DO
         
      END SUBROUTINE buildEdgeList
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
            IF ( node % activeStatus == REMOVE .OR. node % refCount() == 1 )     THEN
            ! Note that if refCount = 1 then this is the only place a node is referenced is in the 
            ! node list. It should be removed since it is not used elsewhere.
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
         CLASS(SMMesh)  :: self
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
                  PRINT *, "Unassociated pointer in list in renumberObjects"
               END IF 
               CALL iterator % moveToNext()
            END DO  
         
            SELECT CASE ( whichIterator )
               CASE( NODES ) 
                  self % nodeID = j-1
               CASE( EDGES )
                  self % edgeID = j-1
               CASE( ELEMENTS )
                  self % elementID = j-1
               CASE DEFAULT 
            END SELECT 
         END IF
         
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
         INTEGER :: k, numBoundaries
         
         IF(ASSOCIATED(boundaryEdgesArray)) THEN 
!
!           --------------------------------------------------------------------
!           I'm manually releasing the linked lists in the array, which
!           seems to avoid a memory problem that happened when they are
!           (or should be) automatically deleted in the destructor for 
!           the MutableObjectArray class. The operation is essentially identical
!           (though a bit less efficient) than the destructor, so it's not clear
!           where the problem is/was.
!           --------------------------------------------------------------------
!
            numBoundaries = boundaryEdgesArray % COUNT()
            DO k = 1, numBoundaries
               CALL boundaryEdgesArray % removeObjectAtIndex(indx = k)
            END DO
            CALL releaseFTMutableObjectArray(boundaryEdgesArray)
         END IF 
         
         IF ( .NOT. ASSOCIATED(boundaryEdgesArray) )     THEN
            IF(ALLOCATED(boundaryEdgesType)) DEALLOCATE(boundaryEdgesType)
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
         CALL releaseFTLinkedListIterator(self % edgesIterator)
         CALL releaseFTLinkedList(self = self % edges)

         ALLOCATE(self % edges)
         ALLOCATE(self % edgesIterator)
         
         CALL self % edges % init()
         CALL self % buildEdgeList()
         CALL self % edgesIterator % initWithFTLinkedList( self % edges )

      END SUBROUTINE syncEdges
      
      END Module SMMeshClass

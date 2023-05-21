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
!      SMMeshObjects.f90
!      Created: Aug. 5, 2013 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module SMMeshObjectsModule
      USE SMConstants
      USE ProgramGlobals
      USE FTObjectClass 
      USE FTMutableObjectArrayClass
      USE FTLinkedListIteratorClass
      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      INTEGER, PARAMETER :: INSIDE = 1, OUTSIDE = 2, ON_BOUNDARY = 3, ON_INTERFACE = 4
      INTEGER, PARAMETER :: LENGTH_OF_BC_STRING = 32
      
      INTEGER, PARAMETER :: edgeMap(2,4) = RESHAPE( (/1,2,2,3,4,3,1,4/), (/2,4/) )
!
!     ---------------------------
!     Base class for mesh objects
!     ---------------------------
!
      TYPE, EXTENDS(FTObject) :: SMMeshObject
         INTEGER :: id ! Global ID of the object
      END TYPE SMMeshObject
!
!     -----------------------------
!     Class type definitions - Node
!     -----------------------------
!
      TYPE, EXTENDS(SMMeshObject) :: SMNode
!
!        ----
!        Data
!        ----
!
         INTEGER       :: level            ! Used to apply subdivision templates
         INTEGER       :: bCurveID         ! Physical boundary curve ID for this node, if this is a boundary node
         INTEGER       :: bCurveChainID    ! Saves the chain id that the bCurveID is in. Not recursive.
         INTEGER       :: bCurveSide       ! Either INSIDE or OUTSIDE the curve
         INTEGER       :: activeStatus     ! = ACTIVE or INACTIVE or REMOVE. Used by 2-refinement for templates
         INTEGER       :: nodeType         ! used to distinguish between corner nodes and others.
         INTEGER       :: materialID       ! Inherited from the element (not precise, since nodes are shared)
         REAL(KIND=RP) :: x(3)             ! The node position
         REAL(KIND=RP) :: whereOnBoundary  ! Parametrization value of curve when near a physical boundary
         REAL(KIND=RP) :: gWhereOnBoundary ! Global parametrization value of curve on a chain
         REAL(KIND=RP) :: distToBoundary   ! Distance to a physical boundary, if this is a nearby node
!
!        ----------
!        Procedures
!        ----------
!
!        ========
         CONTAINS
!        ========
!         
         PROCEDURE :: initWithLocationAndID => initNodeWithLocationAndID
         PROCEDURE :: printDescription      => printNodeDescription
         PROCEDURE :: typeCopy              => copyOfNodeClass
      END TYPE SMNode
      
      TYPE SMNodePtr
         CLASS(SMNode), POINTER :: node => NULL()
      END TYPE SMNodePtr
!
!     ----------------------------
!     Element boundary information
!     ----------------------------
!
      TYPE ElementBoundaryInfo
         INTEGER                        :: nodeIDs(4)
         INTEGER                        :: bCurveFlag(4)
         CHARACTER(LEN=32)              :: bCurveName(4)
         REAL(KIND=RP)    , ALLOCATABLE :: x (:,:,:)
      END TYPE ElementBoundaryInfo
!
!     ------------------
!     Element Definition
!     ------------------
!
      TYPE, EXTENDS(SMMeshObject) :: SMElement
!
!        ----
!        Data
!        ----
!
         INTEGER                      :: eType   ! = QUAD or HEX
         LOGICAL                      :: remove
         INTEGER                      :: N
         INTEGER                      :: materialID
         TYPE(SMNodePtr), ALLOCATABLE :: nodes(:)
         TYPE(ElementBoundaryInfo)    :: boundaryInfo
         REAL(KIND=RP), ALLOCATABLE   :: xPatch(:,:,:)
!
!        ========
         CONTAINS
!        ========
!         
!        ----------
!        Procedures
!        ----------
!
         PROCEDURE :: initWithNodesIDAndType => initElementWithNodesIDAndType
         FINAL     :: destructElement
         PROCEDURE :: printDescription       => printElementDescription
      END TYPE SMElement
      
      TYPE SMElementPtr
         CLASS(SMElement), POINTER :: element => NULL()
      END TYPE SMElementPtr
!
!     ---------------
!     Edge definition
!     ---------------
!
      TYPE, EXTENDS(SMMeshObject) :: SMEdge
         INTEGER                          :: edgeType
         LOGICAL                          :: remove
         TYPE(SMNodePtr)   , DIMENSION(2) :: nodes
         TYPE(SMElementPtr), DIMENSION(2) :: elements
         INTEGER                          :: elementSide(2)
         CLASS(SMNode)    , POINTER       :: auxiliaryNode => NULL() ! used for splitting interface elements
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithNodesAndID => initEdgeWithNodesAndID
         FINAL     :: destructEdge
         PROCEDURE :: printDescription   => printEdgeDescription
      END TYPE SMEdge
      
      TYPE SMEdgePtr
         CLASS(SMEdge), POINTER :: edge => NULL()
      END TYPE SMEdgePtr
!
!     ---------------
!     Quad definition
!     ---------------
!
      TYPE, EXTENDS(FTobject) :: SMQuad
         INTEGER                       :: domainMark ! = INSIDE, OUTSIDE OR ON_BOUNDARY
         TYPE(SMNodePtr), DIMENSION(4) :: nodes
         CONTAINS 
         PROCEDURE :: initWithNodes => initQuadWithNodes
         PROCEDURE :: init          => initQuad
         FINAL     :: destructQuad
         PROCEDURE :: printDescription => printQuadDescription
      END TYPE SMQuad
      
      TYPE SMQuadPtr
         CLASS(SMQuad), POINTER :: quad => NULL()
      END TYPE SMQuadPtr
!
!     -----
!     Casts
!     -----
!
      INTERFACE cast
         MODULE PROCEDURE castToSMElement
      END INTERFACE cast

      INTERFACE cast
         MODULE PROCEDURE castToSMNode
      END INTERFACE cast

      INTERFACE cast
         MODULE PROCEDURE castToSMEdge
      END INTERFACE cast
!
!     ========      
      CONTAINS
!     ========
! 
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToSMMeshObject(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the SMNode class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)    , POINTER :: obj
         CLASS(SMMeshObject), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            CLASS is (SMMeshObject)
               cast => e
            CLASS DEFAULT
         END SELECT
         
      END SUBROUTINE castToSMMeshObject
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION nodeFromSMMeshobject(obj) RESULT(cast)  
      IMPLICIT NONE  
         CLASS(FTObject), POINTER :: obj
         CLASS(SMNode)  , POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            CLASS is (SMNode)
               cast => e
            CLASS DEFAULT
         END SELECT
         
      END FUNCTION nodeFromSMMeshobject
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initNodeWithLocationAndID(self,x,id)  
         IMPLICIT NONE  
         CLASS(SMNode) :: self
         REAL(KIND=RP) :: x(3)
         INTEGER       :: id
         
         CALL self % FTObject % init()
         
         self % x               = x
         self % id              = id
         self % level           = UNDEFINED
         self % bCurveID        = UNDEFINED
         self % activeStatus    = NONE
         self % whereOnBoundary = -1.0_RP
         self % distToBoundary  = HUGE(0.0_RP)
         self % bCurveChainID   = UNDEFINED
         self % bCurveSide      = UNDEFINED
         self % materialID      = BACKGROUND_MATERIAL_ID
         self % nodeType        = UNDEFINED
      END SUBROUTINE initNodeWithLocationAndID
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION copyOfNodeClass(self) RESULT(copy)  
         IMPLICIT NONE  
         CLASS(SMNode) :: self
         TYPE (SMNode) :: copy
         
         CALL copy % FTObject % init()
         
         copy % x               = self % x              
         copy % id              = self % id             
         copy % level           = self % level          
         copy % bCurveID        = self % bCurveID       
         copy % activeStatus    = self % activeStatus   
         copy % whereOnBoundary = self % whereOnBoundary
         copy % distToBoundary  = self % distToBoundary 
         copy % bCurveChainID   = self % bCurveChainID  
         copy % bCurveSide      = self % bCurveSide     
         copy % materialID      = self % materialID     
         copy % nodeType        = self % nodeType       
         
      END FUNCTION copyOfNodeClass
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE copyOfNodeType(source, copy)  
         IMPLICIT NONE  
         CLASS(SMNode), POINTER :: copy
         TYPE (SMNode)          :: source
         
         copy % x               = source % x              
         copy % id              = source % id             
         copy % level           = source % level          
         copy % bCurveID        = source % bCurveID       
         copy % activeStatus    = source % activeStatus   
         copy % whereOnBoundary = source % whereOnBoundary
         copy % distToBoundary  = source % distToBoundary 
         copy % bCurveChainID   = source % bCurveChainID  
         copy % bCurveSide      = source % bCurveSide     
         copy % materialID      = source % materialID     
         copy % nodeType        = source % nodeType       
         
      END SUBROUTINE copyOfNodeType
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printNodeDescription( self, iUnit )
         USE, INTRINSIC :: iso_fortran_env, only : stderr => ERROR_UNIT 
         IMPLICIT NONE
         CLASS(SMNode) :: self
         INTEGER       :: iUnit
         WRITE(iUnit,*) self % id, self % refCount(), self % x, self % bCurveChainID, self % activeStatus
         IF(self % refCount() == 0) WRITE(stderr,*) "%%%% Unreferenced Node %%% "
      END SUBROUTINE printNodeDescription
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PointNodePtr_To_( p, q )
         USE, INTRINSIC :: iso_fortran_env, only : stderr => ERROR_UNIT 
         IMPLICIT NONE 
         TYPE(SMNodePtr)          :: p, q
         CLASS(FTObject), POINTER :: obj
         
         IF( .NOT.ASSOCIATED(q % node) ) THEN
            CALL p % node % printDescription(stderr)
            ERROR STOP "Unassociated target node pointer"
         END IF
!
         obj => p % node
         CALL release(obj)
!
!        ----------------------
!        End of offending block
!        ----------------------
!
         p % node => q % node
         CALL q % node % retain()
         
      END SUBROUTINE PointNodePtr_To_
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseSMNode(self)  
         IMPLICIT NONE
         CLASS(SMNode)  , POINTER :: self
         CLASS(FTObject), POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseSMNode
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructNodePtr_ForLocation_(self, x)
         TYPE(SMNodePtr) :: self
         REAL(KIND=RP)   :: x(3)
         ALLOCATE(self % node)
         CALL self % node % initWithLocationAndID(x,UNDEFINED)
      END SUBROUTINE ConstructNodePtr_ForLocation_
!
!////////////////////////////////////////////////////////////////////////
!
      PURE LOGICAL FUNCTION isOnBoundaryCurve( node ) 
         IMPLICIT NONE 
         CLASS(SMNode), POINTER, INTENT(IN) :: node
         IF( node % bCurveChainID > UNDEFINED )     THEN
            IsOnBoundaryCurve = .true.
         ELSE
            IsOnBoundaryCurve = .false.
         END IF
      END FUNCTION isOnBoundaryCurve
!
!////////////////////////////////////////////////////////////////////////
!
      PURE LOGICAL FUNCTION IsOnOuterBox( node ) 
         IMPLICIT NONE 
         CLASS(SMNode), POINTER, INTENT(IN) :: node
         IF( node % bCurveChainID < UNDEFINED )     THEN
            IsOnOuterBox = .true.
         ELSE
            IsOnOuterBox = .false.
         END IF
      END FUNCTION IsOnOuterBox
!
!////////////////////////////////////////////////////////////////////////
!
      PURE LOGICAL FUNCTION hasFixedPosition( node ) 
         IMPLICIT NONE 
         CLASS(SMNode), POINTER, INTENT(IN) :: node
         
          IF( node%distToBoundary < EPSILON(1.0_RP) )     THEN
            HasFixedPosition = .true.
          ELSE
            HasFixedPosition = .false.
          END IF
          
      END FUNCTION hasFixedPosition
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToSMNode(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the SMNode class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject), POINTER :: obj
         CLASS(SMNode), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (SMNode)
               cast => e
            CLASS DEFAULT
         END SELECT
         
      END SUBROUTINE castToSMNode
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initElementWithNodesIDAndType(self, nodes, id, eType ) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMElement)              :: self
         TYPE(SMNodePtr), DIMENSION(:) :: nodes
         INTEGER                       :: id
         INTEGER                       :: eType
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                  :: j

         CALL self % FTObject % init()
         ALLOCATE( self % nodes(eType))
         
         self % id    = id
         self % eType = eType

         DO j = 1, eType 
            self % nodes(j) % node => nodes(j) % node
            CALL nodes(j) % node % retain()
         END DO
         
         self % remove       = .FALSE.
         self % materialID   = BACKGROUND_MATERIAL_ID
         self % N            = 1
        
      END SUBROUTINE initElementWithNodesIDAndType
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseSMElement(self)  
         IMPLICIT NONE
         CLASS(SMElement), POINTER :: self
         CLASS(FTObject) , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseSMElement
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructElement(self) 
         IMPLICIT NONE  
         TYPE(SMElement) :: self
         INTEGER         :: j
         
         DO j = 1, self % eType 
            CALL releaseSMNode(self = self % nodes(j) % node) 
         END DO 
         DEALLOCATE( self % nodes)
          
      END SUBROUTINE destructElement
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printElementDescription(self,iUnit)  
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMElement) :: self
         INTEGER          :: iUnit
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER :: j
         
         WRITE(iUnit,*) "Element with ID = ", self % id
         DO j = 1, self % eType 
            CALL printNodeDescription(self = self % nodes(j) % node,iUnit = iUnit)
         END DO 
         IF(self % refCount() == 0) WRITE(iUnit,*) "%%%% Unreferenced Element %%% "
         
      END SUBROUTINE printElementDescription
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToSMElement(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the SMNode class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject) , POINTER :: obj
         CLASS(SMElement), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (SMElement)
               cast => e
            CLASS DEFAULT
         END SELECT
         
      END SUBROUTINE castToSMElement
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initEdgeWithNodesAndID(self, startNode, endNode, id)
         IMPLICIT NONE
         CLASS(SMEdge)          :: self
         CLASS(SMNode), POINTER :: startNode, endNode
         INTEGER                :: id
         
         INTEGER                :: k
         
         CALL self % FTObject % init()
         
         self % id = id
         
         self % nodes(1) % node => startNode
         self % nodes(2) % node => endNode
         CALL startNode % retain()
         CALL endNode   % retain()
         
         self % remove      = .FALSE.
         self % elementSide = 0
         self%edgeType      = INSIDE
         
         DO k = 1, 2 
            self % elements(k) % element => NULL()
         END DO
         self % auxiliaryNode => NULL()

      END SUBROUTINE initEdgeWithNodesAndID
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructEdge(self)  
         IMPLICIT NONE  
         TYPE(SMEdge)             :: self
         INTEGER                   :: k
         CLASS(FTObject), POINTER  :: obj
         
         DO k = 1,2
!
!           -------------
!           Release nodes
!           -------------
!
            obj => self % nodes(k) % node
            CALL release(obj)
!
!           ----------------
!           Release elements
!           ----------------
!
            obj => self % elements(k) % element
            CALL release(obj)
         END DO  

         IF(ASSOCIATED(self % auxiliaryNode))     THEN
            obj => self % auxiliaryNode
            CALL release(obj)
         END IF
         
      END SUBROUTINE destructEdge
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseSMEdge(self)  
         IMPLICIT NONE
         CLASS(SMEdge)   , POINTER :: self
         CLASS(FTObject) , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseSMEdge
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printEdgeDescription(self,iUnit)  
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMEdge) :: self
         INTEGER          :: iUnit
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                  :: k
         
         WRITE(iUnit,*) "Edge with ID = ", self % id, self % refCount(), self % remove
         IF(self % refCount() == 0) WRITE(iUnit,*) "%%%% Unreferenced Edge %%% "
         
         DO k = 1, 2
            CALL self % nodes(k) % node % printDescription(iUnit) 
         END DO 
         
      END SUBROUTINE printEdgeDescription
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToSMEdge(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the SMNode class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject) , POINTER :: obj
         CLASS(SMEdge)   , POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (SMEdge)
               cast => e
            CLASS DEFAULT
         END SELECT
         
      END SUBROUTINE castToSMEdge
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE SetAuxiliaryNode( self, node )  
         IMPLICIT NONE  
         CLASS(SMEdge), POINTER    :: self
         CLASS(SMNode), POINTER    :: node
         self % auxiliaryNode => node
         CALL node % retain()
      END SUBROUTINE SetAuxiliaryNode
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initQuadWithNodes(self,nodes)  
         IMPLICIT NONE
         CLASS(SMQuad)   :: self
         TYPE(SMNodePtr) :: nodes(4)
         INTEGER         :: k
         
         CALL self % FTObject % init()
         
         DO k = 1, 4
            self % nodes(k) % node => nodes(k) % node
            CALL self % nodes(k) % node % retain()
         END DO
         
      END SUBROUTINE initQuadWithNodes
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initQuad(self)  
         IMPLICIT NONE
         CLASS(SMQuad)   :: self
         
         CALL self % FTObject % init()
         
         self % domainMark = INSIDE
         
      END SUBROUTINE initQuad

!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructQuad(self)  
         IMPLICIT NONE  
         TYPE(SMQuad)              :: self
         INTEGER                   :: k
         CLASS(FTObject), POINTER  :: obj
         
         DO k = 1, 4
            obj => self % nodes(k) % node
            CALL release(obj)
         END DO  
      END SUBROUTINE destructQuad
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseSMQuad(self)  
         IMPLICIT NONE
         CLASS(SMQuad)   , POINTER :: self
         CLASS(FTObject) , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseSMQuad
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printQuadDescription(self,iUnit)  
         IMPLICIT NONE  
         CLASS(SMQuad)   :: self
         INTEGER         :: iUnit
         WRITE(iUnit,*) "SMQuad object"
      END SUBROUTINE printQuadDescription
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ElementBoundaryInfoInit( self, N ) 
         IMPLICIT NONE
         TYPE(ElementBoundaryInfo) :: self
         INTEGER                 :: N ! = polynomial order
         
         ALLOCATE( self%x(3,0:N,4) )
         
         self % x          = 0.0_RP
         self % bCurveFlag = 0
         self % bCurveName = '---'
      END SUBROUTINE ElementBoundaryInfoInit
      
      END Module SMMeshObjectsModule

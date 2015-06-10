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
         INTEGER                     :: eType   ! = QUAD or HEX
         LOGICAL                     :: remove
         INTEGER                     :: N
         INTEGER                     :: materialID
         CHARACTER(LEN=32)           :: materialName
         TYPE (FTMutableObjectArray) :: nodes
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
         PROCEDURE :: destruct               => destructElement
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
         PROCEDURE :: destruct           => destructEdge
         PROCEDURE :: printDescription   => printEdgeDescription
         PROCEDURE :: addAuxiliaryNode
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
         PROCEDURE :: destruct      => destructQuad
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
         self % materialID      = 1
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
         IMPLICIT NONE
         CLASS(SMNode) :: self
         INTEGER       :: iUnit
         WRITE(iUnit,*) self % id, self % refCount(), self % x, self % bCurveChainID, self % activeStatus
         IF(self % refCount() == 0) WRITE(iUnit,*) "%%%% Unreferenced Node %%% "
      END SUBROUTINE printNodeDescription
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PointNodePtr_To_( p, q )
         IMPLICIT NONE 
         TYPE(SMNodePtr) :: p, q
         
         IF( .NOT.ASSOCIATED(q % node) ) THEN
            PRINT *, "Unassociated target node pointer"
            CALL p % node % printDescription(6)
            STOP
         END IF
!
         IF ( ASSOCIATED(p % node) )     THEN
            CALL p % node % release()
            IF ( p % node % isUnreferenced() )     THEN
               DEALLOCATE(p % node)
               p % node => NULL() 
            END IF 
         END IF 
!
!        ----------------------
!        End of offending block
!        ----------------------
!
         p % node => q % node
         CALL p % node % retain()
         
      END SUBROUTINE PointNodePtr_To_
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
      LOGICAL FUNCTION isOnBoundaryCurve( node ) 
         IMPLICIT NONE 
         CLASS(SMNode), POINTER :: node
         IF( node % bCurveChainID > UNDEFINED )     THEN
            IsOnBoundaryCurve = .true.
         ELSE
            IsOnBoundaryCurve = .false.
         END IF
      END FUNCTION isOnBoundaryCurve
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION IsOnOuterBox( node ) 
         IMPLICIT NONE 
         CLASS(SMNode), POINTER :: node
         IF( node % bCurveChainID < UNDEFINED )     THEN
            IsOnOuterBox = .true.
         ELSE
            IsOnOuterBox = .false.
         END IF
      END FUNCTION IsOnOuterBox
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION hasFixedPosition( node ) 
         IMPLICIT NONE 
         CLASS(SMNode), POINTER :: node
         
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
         CLASS(fTObject), POINTER :: obj => NULL()

         CALL self % FTObject % init()
         CALL self % nodes    % initWithSize(eType)
         
         self % id    = id
         self % eType = eType

         DO j = 1, eType 
            obj => nodes(j) % node
            CALL   self  % nodes % addObject(obj)
         END DO
         
         self % remove       = .FALSE.
         self % materialID   = 1
         self % materialName = "base"
         self % N            = 1
        
      END SUBROUTINE initElementWithNodesIDAndType
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructelement(self) 
         IMPLICIT NONE  
         CLASS(SMElement) :: self
         
         CALL self % nodes % release()

      END SUBROUTINE destructelement
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
         
         WRITE(iUnit,*) "Element with ID = ", self % id
         CALL self % nodes % printDescription(iUnit)
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
      SUBROUTINE addAuxiliaryNode(self,node)  
         IMPLICIT NONE
         CLASS(SMEdge)          :: self
         CLASS(SMNode), POINTER :: node
         self % auxiliaryNode => node
         CALL node % retain()
      END SUBROUTINE addAuxiliaryNode
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructEdge(self)  
         IMPLICIT NONE  
         CLASS(SMEdge)             :: self
         CLASS(SMNode)   , POINTER :: np => NULL()
         CLASS(SMElement), POINTER :: ep => NULL()
         INTEGER                   :: k
         
         DO k = 1,2
!
!           -------------
!           Release nodes
!           -------------
!
            np => self % nodes(k) % node
            IF ( ASSOCIATED(np) )     THEN
               CALL np % release()
               IF ( np % isUnReferenced() )     THEN
                  DEALLOCATE(self % nodes(k) % node)
                  self % nodes(k) % node => NULL() 
               END IF
            END IF
!
!           ----------------
!           Release elements
!           ----------------
!
            ep => self % elements(k) % element
            IF ( ASSOCIATED(ep) )     THEN
               CALL ep % release()
               IF ( ep % isUnReferenced() )     THEN
                  DEALLOCATE(self % elements(k) % element)
                  self % elements(k) % element => NULL() 
               END IF
            END IF
         END DO  

         IF(ASSOCIATED(self % auxiliaryNode))     THEN
            CALL self % auxiliaryNode % release()
            IF ( self % auxiliaryNode % isUnreferenced() )     THEN
               DEALLOCATE(self % auxiliaryNode)
               self % auxiliaryNode => NULL() 
            END IF 
         END IF
         
         CALL self % FTObject % destruct()
         
      END SUBROUTINE destructEdge
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
         CLASS(SMQuad)   :: self
         INTEGER         :: k
         
         DO k = 1, 4
            CALL self % nodes(k) % node % release()
            IF ( self % nodes(k) % node % isUnreferenced() )     THEN
               DEALLOCATE(self % nodes(k) % node)
               self % nodes(k) % node => NULL()
            END IF  
         END DO  
      END SUBROUTINE destructQuad
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printQuadDescription(self,iUnit)  
         IMPLICIT NONE  
         CLASS(SMQuad)   :: self
         INTEGER         :: iUnit
         WRITE(iUnit,*) "SMQuad object"
      END SUBROUTINE printQuadDescription
      
      END Module SMMeshObjectsModule

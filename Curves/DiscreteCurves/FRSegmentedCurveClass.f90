!
!////////////////////////////////////////////////////////////////////////
!
!      FRSegmentedCurveClass
!!      Created: 2010-08-18 16:58:26 -0400 
!!      By: David Kopriva  
!!
!!
!!      ------------------------------------------------------------------
!!      This defines a fixed resolution segmented curve class that
!!      automatically chooses the nodes so that the curve is well-resolved
!!      ------------------------------------------------------------------
!!
!!     Usage:
!!
!!        *Creation*
!!
!!           ALLOCATE(curve)
!!           CALL curve % initWithCurve(ContinuousCurve, h, id)
!!           CALL curve % initWithCurveFunction(curveExternalFunction, h, curveName, id)
!!
!!        *Reversing direction*
!!
!!           CALL ReverseFRSegmentedCurve( curve )
!!
!!        *Destruction*
!!
!!           CALL ReleaseFRSegmentedCurve( curve )
!!
!!        *Getting location*
!!
!!           x    = curve % positionAtIndex( j )
!!           t    = curve % argumentAtIndex( j )
!!           c    = curve % invScaleAtIndex( j )
!!           nHat = curve % normalAtIndex( j )
!!
!!        *Setters*
!!
!!           CALL curve % setCurveInvScaleForIndex( s,j )
!!
!!        *Printing*
!!
!!           CALL curve % printDescription( iUnit )
!!
!!        *Number of nodes in the curve*
!!
!!           N = curve % count()
!
!////////////////////////////////////////////////////////////////////////
!
      Module FRSegmentedCurveClass
      USE SMCurveClass
      USE SMConstants
      USE FTLinkedListClass
      USE FTLinkedListIteratorClass
      USE ObjectArrayAdditionsModule
      USE FTMutableObjectArrayClass
      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      INTEGER, PARAMETER :: FRSEGMENTED_CURVE_NAME_LENGTH = 32
!
!     ------------------------
!     Linked list node records
!     ------------------------
!
      TYPE, EXTENDS(FTObject) :: SMSegmentedCurveNode
         REAL(KIND=RP) :: x(3), nHat(3), invScale, t
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initSMSegmentedCurveNode
         PROCEDURE :: destruct => destructSMSegmentedCurveNode
         PROCEDURE :: printDescription => printNodeDescription
         
      END TYPE SMSegmentedCurveNode
!
!        --------------
!        Segment record
!        --------------
!
      TYPE, EXTENDS(FTObject) :: SMSegment
         CLASS(SMSegmentedCurveNode), POINTER :: leftNode , rightNode
         CLASS(SMSegment)           , POINTER :: segmentLeft, segmentRight
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initSMSegment
         PROCEDURE :: destruct => destructSMSegment
         
      END TYPE SMSegment
!
!     ----------
!     Class type
!     ----------
!
      TYPE, EXTENDS(FTObject) :: FRSegmentedCurve
         INTEGER                                      :: id
         INTEGER                                      :: direction
         LOGICAL                                      :: isCircular
         CHARACTER(LEN=FRSEGMENTED_CURVE_NAME_LENGTH) :: curveName
         REAL(KIND=RP)                                :: h
         CLASS(FTMutableObjectArray), POINTER         :: nodeArray => NULL()
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithCurve    => initFRSegmentedCurve
         PROCEDURE :: destruct         => DestructFRSegmentedCurve
         PROCEDURE :: printDescription => printFRSegmentedCurve;
         PROCEDURE :: reverse          => reverseFRSegmentedCurve
         PROCEDURE :: COUNT            => FRSegmentedCurveCount
         PROCEDURE :: setCurveInvScaleForIndex
         PROCEDURE :: positionAtIndex
         PROCEDURE :: argumentAtIndex
         PROCEDURE :: invScaleAtIndex
         PROCEDURE :: normalAtIndex
         
      END TYPE FRSegmentedCurve
      
      INTERFACE release
         MODULE PROCEDURE releaseFRSegmentedCurve 
         MODULE PROCEDURE releaseSMSegmentedCurveNode
         MODULE PROCEDURE releaseSMSegment
      END INTERFACE  

      INTERFACE cast
         MODULE PROCEDURE castToSMSegmentedCurveNode
      END INTERFACE cast
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
!      SegmentedCurveNode
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initSMSegmentedCurveNode( self, x, t )
         CLASS( SMSegmentedCurveNode ) :: self
         REAL(KIND=RP)               :: x(3), t
         
         CALL self  %  FTObject  %  init()
         
         self % x        = x
         self % t        = t
         self % invScale = HUGE(t)
         
      END SUBROUTINE initSMSegmentedCurveNode
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructSMSegmentedCurveNode(self)  
         IMPLICIT NONE
         CLASS(SMSegmentedCurveNode) :: self
         
         CALL self % FTObject % destruct()
         
      END SUBROUTINE destructSMSegmentedCurveNode
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseSMSegmentedCurveNode(self)  
         IMPLICIT NONE
         CLASS(SMSegmentedCurveNode) , POINTER :: self
         CLASS(FTObject)             , POINTER :: obj
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseSMSegmentedCurveNode
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE setInvScale(self,s)  
         IMPLICIT NONE  
         CLASS( SMSegmentedCurveNode ), POINTER :: self
         REAL(KIND=RP)                          :: s
         self % invScale = s
      END SUBROUTINE  
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToSMSegmentedCurveNode(obj,cast) 
!
!     --------------------------------------------------------------
!     Cast the base class FTObject to the SMSegmentedCurveNode class
!     --------------------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)            , POINTER :: obj
         CLASS(SMSegmentedCurveNode), POINTER :: cast

         cast => NULL()

         SELECT TYPE (e => obj)
            TYPE is (SMSegmentedCurveNode)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castToSMSegmentedCurveNode
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printNodeDescription(self,iUnit)  
         IMPLICIT NONE 
         CLASS(SMSegmentedCurveNode) :: self
         INTEGER                     :: iUnit
         
         WRITE(iUnit,*) self % t, self % x

      END SUBROUTINE printNodeDescription    
!
!////////////////////////////////////////////////////////////////////////
!
!      SMSegment Class
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initSMSegment( self, nodeLeft, nodeRight )
         CLASS(SMSegment)                     :: self
         CLASS(SMSegmentedCurveNode), POINTER :: nodeLeft, nodeRight
         
         CALL self  %  FTObject  %  init()
         
         self % leftNode     => nodeLeft
         self % RightNode    => nodeRight
         self % segmentLeft  => NULL()
         self % segmentRight => NULL()
         
         CALL self % leftNode  % retain()
         CALL self % rightNode % retain()
                  
      END SUBROUTINE initSMSegment
!
!//////////////////////////////////////////////////////////////////////// 
! 
      RECURSIVE SUBROUTINE DestructSMSegment(self)  
         IMPLICIT NONE
         CLASS(SMSegment)           :: self
         CLASS(FTObject), POINTER :: obj
                  
         CALL release(self % leftNode)
         CALL release(self % rightNode)
         
         IF(ASSOCIATED(self % segmentRight))    CALL release(self % segmentRight)
         IF(ASSOCIATED(self % segmentLeft))     CALL release(self % segmentLeft)
         
         CALL self % FTObject % destruct()

      END SUBROUTINE DestructSMSegment
!
!//////////////////////////////////////////////////////////////////////// 
! 
      RECURSIVE SUBROUTINE releaseSMSegment(self)  
         IMPLICIT NONE
         CLASS(SMSegment) , POINTER :: self
         CLASS(FTObject)  , POINTER :: obj
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseSMSegment
!
!//////////////////////////////////////////////////////////////////////// 
! 
      RECURSIVE SUBROUTINE subdivideSMSegment(self, atT, h, alongCurve, controls, savingNodesTo)  
         USE SizerControls
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMSegment)   , POINTER :: self
         CLASS(SMCurve)               :: alongCurve
         CLASS(FTLinkedList)          :: savingNodesTo
         CLASS(FTLinkedList), POINTER :: controls
         REAL(KIND=RP)                :: h, atT, hLoc, cSize
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP)                        :: xMid(3)
         LOGICAL                              :: doSubdivision
         REAL(KIND=RP)                        :: tL, tR
         CLASS(SMSegmentedCurveNode), POINTER :: nodeMid, targetNode
         CLASS(FTObject)            , POINTER :: objPtr    
!
!        -----------------------------------------------
!        Use the two endpoints and midpoint to determine
!        if this segment needs to be subdivided
!        -----------------------------------------------
!
         xMid = alongCurve % positionAt(t = atT)
         ALLOCATE(nodeMid)
         CALL nodeMid % initSMSegmentedCurveNode(x = xMid,t = atT)
         
         hLoc = h
         IF(ASSOCIATED(controls))     THEN
            cSize = controlsSize(controlsList = controls,x = xMid)
            hLoc = MIN(hLoc, cSize)
         END IF  
         
         CALL TestForSubdivision(self            = self,          &
                                 shouldSubdivide = doSubdivision, &
                                 h               = hLoc,          &
                                 midNode         = nodeMid)
!
!        -------------------------------------------------------
!        If subdivision is needed, split into two child segments
!        -------------------------------------------------------
!
         IF ( doSubdivision )     THEN
  
            tL = self % leftNode % t
            tR = self % rightNode % t
                    
            ALLOCATE( self % segmentLeft, self % segmentRight)
            CALL self % segmentLeft  % initSMSegment( self % leftNode, nodeMid )
            CALL self % segmentRight % initSMSegment(nodeLeft = nodeMid,nodeRight = self % rightNode)
            
            CALL subdivideSMSegment(self          = self % segmentLeft ,  &
                                    atT           = tL + 0.5_RP*(atT-TL), &
                                    h             = h,                    &
                                    alongCurve    = alongCurve,           &
                                    controls      = controls,             &
                                    savingNodesTo = savingNodesTo)
            CALL subdivideSMSegment(self          = self % segmentRight, &
                                    atT           = atT + 0.5_RP*(tR-atT), &
                                    h             = h,                    &
                                    alongCurve    = alongCurve,           &
                                    controls      = controls,             &
                                    savingNodesTo = savingNodesTo)
            
         END IF
         
         CALL release(nodeMid)
!
!        ---------------------------------------------------------------
!        Add the right node to the list, as long as it is not duplicated
!        ---------------------------------------------------------------
!         
         objPtr => savingNodesTo % tail % recordObject
         CALL cast(obj = objPtr,cast = targetNode)
         IF ( ASSOCIATED(targetNode) )     THEN
            IF ( ABS(targetNode % t - self % rightNode % t) &
                 > 2.0*EPSILON(targetNode % t))                 THEN
               objPtr => self % rightNode
               CALL savingNodesTo % add(obj = objPtr)
            END IF  
         END IF 
         
      END SUBROUTINE subdivideSMSegment
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE TestForSubdivision(self, shouldSubdivide, h, midNode)
         USE ProgramGlobals
         USE Geometry, ONLY: Curvature
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMSegment), POINTER   :: self
         CLASS(SMSegmentedCurveNode) :: midNode
         LOGICAL                     :: shouldSubdivide
         REAL(KIND=RP)               :: h
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP)  :: xl(3), xR(3), xMid(3)
         REAL(KIND=RP)  :: xPrime(3), xDoublePrime(3)
         REAL(KIND=RP)  :: tL, tR, atT, dt, c, d, s
         shouldSubdivide = .FALSE.
         
         xL   = self % leftNode % x
         xR   = self % rightNode % x
         xMid = midNode % x
         
         tL  = self % leftNode % t
         tR  = self % rightNode % t
         atT = midNode % t
         dt  = 0.5_RP*(tR - tL)
         
         xPrime       = 0.5_RP*(xR - xL)/dt             
         xDoublePrime = (xR - 2*xMid + xL)/dt**2
  
         c = Curvature( xPrime, xDoublePrime )
         
         s = 1.0_RP/MAX( 1.0_RP/h, c ) !Choose the smaller of h or radius of curvature          
         d = SQRT( (xR(1)-xL(1))**2 + (xR(2)-xL(2))**2 + (xR(3)-xL(3))**2)
       
         IF( d > s .OR. dt > 1.0_RP/numCurvePoints ) shouldSubdivide = .TRUE.
         
      END SUBROUTINE TestForSubdivision
!
!////////////////////////////////////////////////////////////////////////
!
!      FRSegmentedCurve
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initFRSegmentedCurve( self, theCurve, h, controls, id )
         USE Geometry, ONLY:Curvature
         USE ProgramGlobals, ONLY:curvatureFactor, INNER, ROW_SIDE
!
!     ------------------------------------------------
!     Construct the curve given a Curve class instance
!     ------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve)      :: self
         CLASS(SMCurve)     , POINTER :: theCurve
         CLASS(FTLinkedList), POINTER :: controls
         REAL(KIND=RP)                :: h
         INTEGER                      :: id
!
!        ---------------
!        Local Variables
!        ---------------
!
         CLASS(SMSegmentedCurveNode), POINTER   :: left => NULL(), right => NULL(), middle => NULL(), p => NULL()
         REAL(KIND=RP)                          :: t, x(3)
         REAL(KIND=RP)                          :: xL(3), xM(3), xR(3), tL, tM, tR, c, s
         REAL(KIND=RP)                          :: xPrimeL(3), xPrimeR(3), xPrime(3), xDoublePrime(3)
         REAL(KIND=RP)                          :: norm, hLocal
         INTEGER                                :: j, N, jointType
         LOGICAL                                :: isCircular
         CLASS(SMSegment)   , POINTER           :: rootSegment
         CLASS(FTLinkedList), POINTER           :: nodes
         CLASS(FTObject)    , POINTER           :: objPtr, obj
!
!        ----
!        Self
!        ----
!
         CALL self % FTobject % init()
         ALLOCATE(self % nodeArray)
         
         self % curveName  = theCurve % curveName()
         self % id         = id
         self % isCircular = .FALSE.
         ALLOCATE(left, right)
!
!        ------------------------------------------------
!        Create root segment, which spans the whole range
!        ------------------------------------------------
!
         t = 0.0_RP
         x = theCurve % positionAt( t )
         CALL left % initSMSegmentedCurveNode( x, t )
                  
         t = 1.0_RP
         x = theCurve % positionAt( t )
         CALL right % initSMSegmentedCurveNode( x, t )
         
         ALLOCATE(rootSegment)
         CALL rootSegment % initSMSegment(nodeLeft = left, nodeRight = right)
         CALL release(left)
         CALL release(right)
!
         self % h = h
!
!        ---------------------------------------------
!        Collect the nodes at the ends of the segments
!        Start off with the left value, and collect
!        the rest along the way.
!        ---------------------------------------------
!
         ALLOCATE(nodes)
         CALL nodes % init()
         objPtr => left
         CALL nodes % add(obj = objPtr)
!
!        ----------------------
!        Subdivide if necessary
!        ----------------------
!
         CALL subdivideSMSegment(self          = rootSegment, &
                                 atT           = 0.5_RP,      &
                                 alongCurve    = theCurve,    &
                                 h             = self % h,    &
                                 controls      = controls,    &
                                 savingNodesTo = nodes)
!
!        ------------------------------------
!        Create an array from the linked list
!        ------------------------------------
!
         CALL initArrayWithLinkedList( self % nodeArray, nodes )
         N = self % nodeArray % COUNT()

         CALL release(rootSegment)
         CALL nodes % destruct()
!         CALL release(self = nodes) !TODO why isn't release called right???
!
!        ----------------------------
!        See if the curve is circular
!        ----------------------------
!
         isCircular = .FALSE.
         obj => self % nodeArray % objectAtIndex(1)
         CALL cast(obj,left)
         obj => self % nodeArray % objectAtIndex(N)
         CALL cast(obj,right)
         x = right % x - left % x
         t = MAXVAL(ABS(x))
         IF ( t < 100*EPSILON(1.0_RP) )     THEN
            self % isCircular = .TRUE. 
         END IF
         
         IF ( self % isCircular )     THEN
            jointType = JointClassification(theCurve,theCurve,INNER)
         END IF 
!
!        ----------------------
!        Compute the curvatures
!        ----------------------
!
         DO j = 1, N
            obj => self % nodeArray % objectAtIndex(j)
            CALL cast(obj,p)
            
            IF ( j == 1 )     THEN
               obj => self % nodeArray % objectAtIndex(1)
               CALL cast(obj,left)
               
               obj => self % nodeArray % objectAtIndex(2)
               CALL cast(obj,right)
               xL           = left  % x
               xR           = right % x
               tL           = left  % t
               tR           = right % t
               xPrime       = (xR - xL)/(tR - tL)
               xDoublePrime = 0.0_RP
            ELSE IF (j == N)     THEN 
               obj => self % nodeArray % objectAtIndex(N-1)
               CALL cast(obj,left)
               obj => self % nodeArray % objectAtIndex(N)
               CALL cast(obj,right)
               xL           = left  % x
               xR           = right % x
               tL           = left  % t
               tR           = right % t
               xPrime       = (xR - xL)/(tR - tL)
               xDoublePrime = 0.0_RP
            ELSE
               xM = p % x
               tM = p % t
               obj => self % nodeArray % objectAtIndex(j-1)
               CALL cast(obj,left)
               obj => self % nodeArray % objectAtIndex(j+1)
               CALL cast(obj,right)
               xL           = left  % x
               xR           = right % x
               tL           = left  % t
               tR           = right % t
               xPrimeR      = (xR - xM)/(tR - tM)
               xPrimeL      = (xM - xL)/(tM - tL)
               xPrime       = 0.5*(xPrimeR + xPrimeL)
               xDoublePrime = 2.0_RP*(xPrimeR - xPrimeL)/(tR - tL)
            END IF 
            
            c = Curvature( xPrime, xDoublePrime )
            s = curvatureFactor/MAX( 1.0_RP/h, c ) !Choose the smaller of h or radius of curvature          
            p % invScale = 1.0_RP/s
            norm         = SQRT(xPrime(1)**2 + xPrime(2)**2)
            p % nHat(1)  =  xPrime(2)/norm
            p % nHat(2)  = -xPrime(1)/norm
            p % nHat(3)  = 0.0_RP
         END DO
!
!        -------------------------------------------
!        Match ends if the curve is a closed loop
!        and if there is no sharp corner. Otherwise,
!        extend the curvature from the second and
!        next to last points.
!        -------------------------------------------
!
         IF ( self % isCircular .AND. jointType == ROW_SIDE )     THEN
         
            obj => self % nodeArray % objectAtIndex(1)
            CALL cast(obj,p)
            
            xM = p % x
            tM = p % t

            obj => self % nodeArray % objectAtIndex(N-1)
            CALL cast(obj,left)
            obj => self % nodeArray % objectAtIndex(2)
            CALL cast(obj,right)
            
            xL           = left  % x
            xR           = right % x
            tL           = left  % t - 1.0_RP
            tR           = right % t
            xPrimeR      = (xR - xM)/(tR - tM)
            xPrimeL      = (xM - xL)/(tM - tL)
            xPrime       = 0.5*(xPrimeR + xPrimeL)
            xDoublePrime = 2.0_RP*(xPrimeR - xPrimeL)/(tR - tL)

            c = Curvature( xPrime, xDoublePrime )
            s = curvatureFactor/MAX( 1.0_RP/h, c ) !Choose the smaller of h or radius of curvature          
            p % invScale = 1.0_RP/s
            norm         = SQRT(xPrime(1)**2 + xPrime(2)**2)
            p % nHat(1)  =  xPrime(2)/norm
            p % nHat(2)  = -xPrime(1)/norm
            p % nHat(3)  = 0.0_RP

            obj => self % nodeArray % objectAtIndex(N)
            CALL cast(obj,left)
            
            left % x        = p % x
            left % nHat     = p % nHat
            left % invScale = p % invScale
         ELSE
            obj => self % nodeArray % objectAtIndex(1)
            CALL cast(obj,p)

            obj => self % nodeArray % objectAtIndex(2)
            CALL cast(obj,right)
            
            p % nHat     = right % nHat
            p % invScale = right % invScale
            
            obj => self % nodeArray % objectAtIndex(N)
            CALL cast(obj,p)

            obj => self % nodeArray % objectAtIndex(N-1)
            CALL cast(obj,left)
            
            p % nHat     = left % nHat
            p % invScale = left % invScale

         END IF 
         
      END SUBROUTINE initFRSegmentedCurve
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DestructFRSegmentedCurve( self )
         CLASS( FRSegmentedCurve )      :: self

         CALL release(self % nodeArray)
               
      END SUBROUTINE DestructFRSegmentedCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseFRSegmentedCurve(self)  
         IMPLICIT NONE
         CLASS(FRSegmentedCurve), POINTER :: self
         CLASS(FTObject)        , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseFRSegmentedCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION FRSegmentedCurveCount(self)  RESULT(n)
         IMPLICIT NONE  
         CLASS( FRSegmentedCurve )  :: self
         INTEGER                    :: n
         n = self % nodeArray % COUNT() 
      END FUNCTION FRSegmentedCurveCount
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReverseFRSegmentedCurve( self )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve)               :: self
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTMutableObjectArray), POINTER  :: newNodes  => NULL()
         CLASS(FTObject)            , POINTER  :: obj       => NULL()
         INTEGER                               :: N, j
!         
         N = self % nodeArray % COUNT()
         ALLOCATE(newNodes)
         CALL newNodes % initWithSize(N)
         
         DO j = 1, N
            obj => self % nodeArray % objectAtIndex(N-j+1)
            CALL newNodes % addObject(obj)
         END DO
         
         CALL release(self % nodeArray)
         
         self % nodeArray => newNodes
         
      END SUBROUTINE ReverseFRSegmentedCurve
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PrintFRSegmentedCurve( self, iUnit )
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve) :: self
         INTEGER                 :: iUnit
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)            , POINTER :: objectPtr => NULL()
         CLASS(SMSegmentedCurveNode)  , POINTER :: node      => NULL()
         INTEGER                              :: N, j
         
         N = self % nodeArray % COUNT()
         
         DO j = 1, N
            objectPtr => self % nodeArray % objectAtIndex(j)
            CALL cast(objectPtr,node)
            WRITE(iUnit,*) node % t, node % x, node % nHat, node % invScale
         END DO  
         WRITE(iUnit,*) "---------------------------------"

      END SUBROUTINE PrintFRSegmentedCurve
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE setCurveInvScaleForIndex( self, s, j )
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve) :: self
         REAL(KIND=RP)           :: s
         INTEGER                 :: j
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)            , POINTER :: objectPtr => NULL()
         CLASS(SMSegmentedCurveNode)  , POINTER :: node      => NULL()
         
         objectPtr => self % nodeArray % objectAtIndex(j)
         CALL cast(objectPtr,node)
         
         CALL setInvScale(node,s) !Gets around an optimizer bug in gfortran 4.8.0
!         node % invScale = s ! Yes, it took a long time to find it.

      END SUBROUTINE setCurveInvScaleForIndex
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION positionAtIndex(self,j)  RESULT(x)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve) :: self
         INTEGER                 :: j
         REAL(KIND=RP)           :: x(3)
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)            , POINTER :: objectPtr => NULL()
         CLASS(SMSegmentedCurveNode)  , POINTER :: node      => NULL()

         objectPtr => self % nodeArray % objectAtIndex(j)
         CALL castToSegmentedCurveNode(objectPtr,node)
         
         x = node % x
      END FUNCTION positionAtIndex
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION normalAtIndex(self,j)  RESULT(x)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve) :: self
         INTEGER                 :: j
         REAL(KIND=RP)           :: x(3)
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)            , POINTER :: objectPtr => NULL()
         CLASS(SMSegmentedCurveNode)  , POINTER :: node      => NULL()

         objectPtr => self % nodeArray % objectAtIndex(j)
         CALL castToSegmentedCurveNode(objectPtr,node)
         
         x = node % nHat
      END FUNCTION normalAtIndex
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION invScaleAtIndex(self,j)  RESULT(x)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve) :: self
         INTEGER                 :: j
         REAL(KIND=RP)           :: x
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)            , POINTER :: objectPtr  => NULL()
         CLASS(SMSegmentedCurveNode)  , POINTER :: node       => NULL()

         objectPtr => self % nodeArray % objectAtIndex(j)
         CALL castToSegmentedCurveNode(objectPtr,node)
         
         x = node % invScale
      END FUNCTION invScaleAtIndex
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION argumentAtIndex(self,j)  RESULT(x)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve) :: self
         INTEGER                 :: j
         REAL(KIND=RP)           :: x
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)            , POINTER :: objectPtr => NULL()
         CLASS(SMSegmentedCurveNode)  , POINTER :: node      => NULL()

         objectPtr => self % nodeArray % objectAtIndex(j)
         CALL castToSegmentedCurveNode(objectPtr,node)
         
         x = node % t
      END FUNCTION argumentAtIndex
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToSegmentedCurveNode(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)          , POINTER :: obj
         CLASS(SMSegmentedCurveNode), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            CLASS IS(SMSegmentedCurveNode)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castToSegmentedCurveNode
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToSegmentedCurve(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)        , POINTER :: obj
         CLASS(FRSegmentedCurve), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            CLASS IS(FRSegmentedCurve)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castToSegmentedCurve
      
      
      END Module FRSegmentedCurveClass
      
!
!////////////////////////////////////////////////////////////////////////
!
!      SMChainedCurveClass.f90
!      Created: July 31, 2013 2:59 PM 
!      By: David Kopriva  
!
!      Connects any number of SMCurves into a *closed* chain of curves
!!      Usage:
!!
!!        *Creation*
!!
!!           CALL chain % initChainNameAndID(name,id)
!!
!!        *Destruction*
!!
!!           CALL chain % release()
!!
!!        *Adding Curves*
!!
!!           CALL chain % addCurve(curve)
!!
!!        *Finished adding curves*
!!
!!           CALL chain % complete()
!!
!!        *Getting curves*
!!           curve => chain % curveWithID(id)
!!           curve => chain % curveWithLocation(t)
!!           n     =  chain % curveNumberForLocation( t )
!!
!!        *Number of curves in the chain*
!!
!!           N = chain % count()
!!
!!        *Evaluating at parameteric location*
!!
!!           x = chain % positionAt(t)
!!
!!        *Conversion of local to global parametrization & vise-versa*
!!
!!           local_t   = chain % curveTForChainT( global_t )
!!           global_t  = chain % ChainTForCurveTInCurve( local_t )
!!
!      Throws a CURVES_DONT_JOIN_EXCEPTION exception if the curves in the chain do not
!!     join up. The user dictionary of the exception contains the following objects
!!     and keys:
!!
!!     (FTValue, "chainName")
!!     (SMCurve, "curve")
!!     (SMCurve, "nextCurve")
!!     (FTValue, "message")
!

!////////////////////////////////////////////////////////////////////////
!
      Module SMChainedCurveClass
      USE FTObjectClass
      USE SMCurveClass
      USE FTLinkedListClass
      USE FTLinkedListRecordClass
      USE FTLinkedListIteratorClass
      USE FTMutableObjectArrayClass
      USE ProgramGlobals
      USE FTExceptionClass
      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      CHARACTER(LEN=16) :: CURVES_DONT_JOIN_EXCEPTION = "Curves dont join"
!
!     ---------------------
!     Class type definition
!     ---------------------
!
      TYPE, EXTENDS(SMCurve) :: SMChainedCurve
         CLASS(FTMutableObjectArray), POINTER     :: curvesArray  => NULL()
         
         INTEGER      , DIMENSION(:), ALLOCATABLE :: myCurveIDs
         INTEGER      , DIMENSION(:), ALLOCATABLE :: jointClassification
         LOGICAL      , DIMENSION(:), ALLOCATABLE :: swapDirection
         INTEGER                                  :: numberOfCurvesInChain
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initChainWithNameAndID  => initChainedCurveWithNameAndID
         PROCEDURE :: destruct                => destructChainedCurve
         PROCEDURE :: addCurve                => addCurveToChain
         PROCEDURE :: COUNT                   => chainedCurveCount
         PROCEDURE :: positionAt              => positionOnChainedCurveAt
         PROCEDURE :: tangentAt               => tangentOnChainedCurveAt
         PROCEDURE :: complete                => finalizeChainedCurve
         PROCEDURE :: curveAtIndex
         PROCEDURE :: curveWithLocation
         PROCEDURE :: CurveNumberForLocation
         PROCEDURE :: curveWithID
         PROCEDURE :: indexOfCurveWithID
         PROCEDURE :: curveTForChainT
         PROCEDURE :: ChainTForCurveTInCurve

      END TYPE SMChainedCurve
!
!     ========
      CONTAINS
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initChainedCurveWithNameAndID( self, chainName, id )
         IMPLICIT NONE
         CLASS(SMChainedCurve) :: self
         CHARACTER(LEN=*)      :: chainName
         INTEGER               :: id
         
         CALL self % SMCurve % initWithNameAndID(chainName,id)

         ALLOCATE( self % curvesArray )
         CALL self % curvesArray  % initWithSize(10)
         
         self % numberOfCurvesInChain = 0
        
      END SUBROUTINE initChainedCurveWithNameAndID
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructChainedCurve(self)  
         IMPLICIT NONE  
         CLASS(SMChainedCurve) :: self
         
         IF ( ASSOCIATED(self % curvesArray) )     THEN
            CALL self % curvesArray  % release()
            IF ( self % curvesArray % isUnreferenced() )     THEN
               DEALLOCATE(self % curvesArray) 
               self % curvesArray => NULL()
            END IF 
         END IF 
         
         CALL self % SMCurve % destruct()
         
      END SUBROUTINE destructChainedCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addCurveToChain( self, curve )  
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMChainedCurve)          :: self
         CLASS(SMCurve)       , POINTER :: curve
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject), POINTER :: obj => NULL()
!
!        --------------------------
!        Add the curve to the chain
!        --------------------------
!
         obj => curve
         CALL self % curvesArray % addObject(obj)
         
      END SUBROUTINE addCurveToChain
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE finalizeChainedCurve(self, inOutFlag)  
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMChainedCurve) :: self
         INTEGER, INTENT(IN)   :: inOutFlag
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMCurve)             , POINTER :: curve => NULL(), nextCurve => NULL(), previousCurve => NULL()
         CLASS(FTObject)            , POINTER :: objectPtr => NULL()
         
         REAL(KIND=RP)            :: xEnd(3), x0(3), xN(3)
         INTEGER                  :: nCurves, k, kp1, km1
         INTEGER, EXTERNAL        :: loop
         
         nCurves = self % curvesArray % COUNT()
         IF( nCurves == 0 ) RETURN
!
!        --------------------------------------
!        Allocate and initialize chain's memory
!        --------------------------------------
!
         self % numberOfCurvesInChain = nCurves
         
         ALLOCATE( self % myCurveIDS(nCurves))
         ALLOCATE( self % jointClassification(0:nCurves-1))
         ALLOCATE( self % swapDirection(nCurves))
         
         self % myCurveIDs          = UNDEFINED
         self % swapDirection       = .FALSE.
         self % jointClassification = ROW_SIDE
!
!        ----------------------------
!        Check integrity of the chain
!        ----------------------------
!
         DO k = 1, nCurves
!
!           ------------------------
!           Get the adjoining curves
!           ------------------------
!
            objectPtr => self  % curvesArray % objectAtIndex(k)
            CALL cast(objectPtr,curve)
            self % myCurveIDs(k) =  curve % id()
            
            kp1       =  loop(k+1,nCurves)
            objectPtr => self % curvesArray % objectAtIndex(kp1)
            CALL cast(objectPtr,nextCurve)
!
!           -----------------
!           See if they match
!           -----------------
!
            IF ( self % swapDirection(k) )     THEN
               xEnd = curve % positionAt(0.0_RP) 
            ELSE
               xEnd = curve % positionAt(1.0_RP) 
            END IF
            
            x0   = nextCurve % positionAt(0.0_RP)
            xN   = nextCurve % positionAt(1.0_RP)
            
            IF( MaxVal(ABS(x0-xEnd)) <= 100*EPSILON(1.0_RP) )     THEN
               self % swapDirection(k) = .FALSE.
            ELSE IF ( MaxVal(ABS(xN-xEnd)) <= 100*EPSILON(1.0_RP) )     THEN
               self % swapDirection(k) = .TRUE.
            ELSE
               IF ( kp1 < k )     THEN
                  CALL ThrowCurvesDontJoinException(self,curve,nextCurve,"Chain does not close")
               ELSE 
                  CALL ThrowCurvesDontJoinException(self,curve,nextCurve,"Curves do not adjoin")
               END IF 
               RETURN
            END IF
         END DO  
!
!        -------------------
!        Classify the joints
!        -------------------
!
         DO k = 1, nCurves
            objectPtr => self  % curvesArray % objectAtIndex(k)
            CALL cast(objectPtr,curve)
            self % myCurveIDs(k) =  curve % id()
            
            km1       =  loop(k-1,nCurves)
            objectPtr => self % curvesArray % objectAtIndex(km1)
            CALL cast(objectPtr,previousCurve)
            
            self % jointClassification(k-1) = JointClassification(previousCurve,curve,inOutFlag)
         END DO  
        
      END SUBROUTINE finalizeChainedCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToSMChainedCurve(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)       , POINTER :: obj
         CLASS(SMchainedCurve) , POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            CLASS IS(SMchainedCurve)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castToSMChainedCurve
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION curveNumberForLocation( self, t ) RESULT(curveNumber)
!
!        ----------------------------------------------------
!        Find which curve (by order) the global parameter t
!        corresponds to
!        ----------------------------------------------------
!
         IMPLICIT NONE 
         CLASS(SMChainedCurve) :: self
         REAL(KIND=RP)         :: t
         INTEGER               :: curveNumber
         
         curveNumber = INT(t*self%numberOfCurvesInChain) + 1
         curveNumber = MIN(curveNumber,self%numberOfCurvesInChain)
         
      END FUNCTION curveNumberForLocation
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION curveWithLocation( self, t ) RESULT(c)
!
!     ------------------------------------
!     Returns the curve with this location
!     ------------------------------------
!
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMChainedCurve) :: self
         CLASS(SMCurve)       , POINTER :: c
         REAL(KIND=RP)                  :: t
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject), POINTER :: obj => NULL()
         INTEGER                  :: l
         
         l   =  self % CurveNumberForLocation(t)
         obj => self % curvesArray % objectAtIndex(l)
         CALL cast(obj,c)
         
      END FUNCTION curveWithLocation
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION curveWithID( self, id ) RESULT(c)
!
!     ------------------------------
!     Returns the curve with this id
!     ------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMChainedCurve)          :: self
         CLASS(SMCurve)       , POINTER :: c
         INTEGER                        :: id
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                  :: k
         CLASS(FTObject), POINTER :: obj
         
         DO k = 1, self % curvesArray % COUNT()
            obj => self % curvesArray % objectAtIndex(k)
            CALL cast(obj,c)
            IF ( c % id() == id )     RETURN
         END DO  
      END FUNCTION curveWithID
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION curveAtIndex( self, k ) RESULT(c)
!
!     ------------------------------
!     Returns the curve with this id
!     ------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMChainedCurve)          :: self
         CLASS(SMCurve)       , POINTER :: c
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                  :: k
         CLASS(FTObject), POINTER :: obj => NULL()
         
         obj => self % curvesArray % objectAtIndex(k)
         CALL cast(obj,c)
      END FUNCTION curveAtIndex
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION chainedCurveCount( self ) RESULT(N) 
         IMPLICIT NONE 
         CLASS(SMChainedCurve) :: self
         INTEGER               :: N
         N = self % curvesArray % COUNT()
      END FUNCTION chainedCurveCount
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION indexOfCurveWithID(self,id) RESULT(k)
         IMPLICIT NONE  
         CLASS(SMChainedCurve) :: self
         INTEGER               :: id
         INTEGER               :: k
         
         CLASS(SMCurve) , POINTER :: c => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
         
         DO k = 1, self % numberOfCurvesInChain
            obj => self % curvesArray % objectAtIndex(k)
            CALL cast(obj,c)
            IF ( c % id() == id )     RETURN
         END DO  
      END FUNCTION  
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION curveTForChainT( self, t ) RESULT(s)
!
!     -------------------------------------------------
!     Given the parametriztion along the chain, convert
!     to the local parametrization of the curve
!     at which the point is located
!     -------------------------------------------------
!
         IMPLICIT NONE 
         CLASS(SMChainedCurve) :: self
         REAL(KIND=RP)         :: t, s
         
         INTEGER               :: k
         
         k = self % curveNumberForLocation(t)
         s = t*self % numberOfCurvesInChain - k + 1

         IF( self % swapDirection(k) )     THEN
            s = 1.0_RP - s
         END IF
         
      END FUNCTION curveTForChainT
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION ChainTForCurveTInCurve( self, s, curve ) RESULT(t)
         IMPLICIT NONE 
         CLASS(SMChainedCurve) :: self
         CLASS(SMCurve)        :: curve
         REAL(KIND=RP)         :: t, s
         INTEGER               :: k, id
         
         id = curve % id()
         k  = self % indexOfCurveWithID(id)
         t  = DBLE(s + k - 1)/DBLE(self % numberOfCurvesInChain)

         IF( self % swapDirection(k) )     THEN
            t = 1.0_RP - t
         END IF
         
      END FUNCTION ChainTForCurveTInCurve
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION positionOnChainedCurveAt( self, t ) RESULT(x)
!
!        ----------------------------------------------------------
!        Given the fractional position along the chain, find the
!        curve in the chain that this point corresponds to, and the
!        location in the curve and return the (x,y) position.
!        ----------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMChainedCurve)       :: self
         REAL(KIND=RP)               :: t
         REAL(KIND=RP), DIMENSION(3) :: x
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                  :: curveNumber
         REAL(KIND=RP)            :: s
         CLASS(SMCurve) , POINTER :: c => NULL()
         CLASS(FTobject), POINTER :: obj => NULL()
!
!        ----------------------------------------------------
!        Find which curve (by order) the point corresponds to
!        ----------------------------------------------------
!
         curveNumber = self % curveNumberForLocation(t)
!
!        ---------------------------------
!        Move to that curve (if necessary)
!        ---------------------------------
!
         obj => self % curvesArray % objectAtIndex(curveNumber)
         CALL cast(obj,c)
!
!        --------------------------------------------
!        Convert coordinate to local curve coordinate
!        --------------------------------------------
!
         s = self % curveTForChainT(t)
!
!        --------
!        Evaluate
!        --------
!
         x = c % positionAt(s)
         
      END FUNCTION positionOnChainedCurveAt
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION tangentOnChainedCurveAt( self, t ) RESULT(x)
!
!        ----------------------------------------------------------
!        Given the fractional position along the chain, find the
!        curve in the chain that this point corresponds to, and the
!        location in the curve and return the tangent vector.
!        ----------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMChainedCurve)       :: self
         REAL(KIND=RP)               :: t
         REAL(KIND=RP), DIMENSION(3) :: x
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                  :: curveNumber
         REAL(KIND=RP)            :: s
         CLASS(SMCurve) , POINTER :: c => NULL()
         CLASS(FTobject), POINTER :: obj => NULL()
!
!        ----------------------------------------------------
!        Find which curve (by order) the point corresponds to
!        ----------------------------------------------------
!
         curveNumber = self % curveNumberForLocation(t)
!
!        ---------------------------------
!        Move to that curve (if necessary)
!        ---------------------------------
!
         obj => self % curvesArray % objectAtIndex(curveNumber)
         CALL cast(obj,c)
!
!        --------------------------------------------
!        Convert coordinate to local curve coordinate
!        --------------------------------------------
!
         s = self % curveTForChainT(t)
!
!        --------
!        Evaluate
!        --------
!
         x = c % tangentAt(s)
         
      END FUNCTION tangentOnChainedCurveAt
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE printChainForPlotting( self )
!
!     ----------------------
!     For debugging purposes
!     ----------------------
!
         IMPLICIT NONE
         CLASS(SMChainedCurve) :: self
         REAL(KIND=RP)         :: t, dt, x(3)
         INTEGER               :: N, j
         
         N = 200
         dt = 1.0_RP/N
         DO j = 0, N 
            t = j*dt
            x = self % positionAt(t)
            PRINT *, t, x
         END DO

      END SUBROUTINE printChainForPlotting
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ThrowCurvesDontJoinException(self,curve,nextCurve,msg)  
         USE FTValueClass
         USE SharedExceptionManagerModule
         IMPLICIT NONE  
         CLASS(SMChainedCurve)          :: self
         CLASS(SMCurve)       , POINTER :: curve, nextCurve
         CHARACTER(LEN=*)               :: msg
         CLASS(FTException)   , POINTER :: exception => NULL()
         CLASS(FTDictionary)  , POINTER :: userDictionary => NULL()
         CLASS(FTObject)      , POINTER :: obj => NULL()
         CLASS(FTValue)       , POINTER :: v => NULL()
!
!        -----------------------------------------------------
!        The userDictionary for this exception contains the
!        chain itself and the two curves within the chain that
!        are supposed to match ends but dont. The keys are
!        "chainName", "curve" and "nextCurve"
!        -----------------------------------------------------
!
         ALLOCATE(userDictionary)
         CALL userDictionary % initWithSize(4)
         
         ALLOCATE(v)
         CALL v % initWithValue(self % curveName())
         obj => v
         CALL userDictionary % addObjectForKey(obj,"chainName")
         CALL v % release()
         
         obj => curve
         CALL userDictionary % addObjectForKey(obj,"curve")
         obj => nextCurve
         CALL userDictionary % addObjectForKey(obj,"nextCurve")
         
         ALLOCATE(v)
         CALL v % initWithValue(msg)
         obj => v
         CALL userDictionary % addObjectForKey(obj,"message")
         CALL v % release()
!
!        --------------------
!        Create the exception
!        --------------------
!
         ALLOCATE(exception)
         
         CALL exception % initFTException(FT_ERROR_FATAL, &
                              exceptionName   = CURVES_DONT_JOIN_EXCEPTION, &
                              infoDictionary  = userDictionary)
         CALL userDictionary % release()
!
!        -------------------
!        Throw the exception
!        -------------------
!
         CALL throw(exception)
         CALL exception % release()
         
      END SUBROUTINE ThrowCurvesDontJoinException

      END Module SMChainedCurveClass

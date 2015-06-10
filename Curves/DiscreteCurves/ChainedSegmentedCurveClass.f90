!
!////////////////////////////////////////////////////////////////////////
!
!      ChainedSegmentedCurveClass.f90
!      Created: August 13, 2013 12:00 PM 
!      By: David Kopriva 
!
!      Defines a chain with numberOfCurvesInChain of FRSegementedCurve s
!
!////////////////////////////////////////////////////////////////////////
!
      Module ChainedSegmentedCurveClass
      USE SMConstants
      USE FTMutableObjectArrayClass
      USE FRSegmentedCurveClass
      USE FTExceptionClass
      USE SharedExceptionManagerModule
      USE ProgramGlobals, ONLY: BBOX_TOP, BBOX_BOTTOM, BBOX_LEFT, BBOX_RIGHT
      IMPLICIT NONE 
!
!     ---------
!     Constants
!     ---------
!
      CHARACTER(LEN=26)            :: SEGMENTED_CURVES_DONT_JOIN_EXCEPTION = "Segmented Curves dont join"
      INTEGER          , PARAMETER :: CHAINED_SEGMENTED_CURVE_NAME_LENGTH = 32
      REAL(KIND=RP)    , PARAMETER :: chainTol = 1.0d-10
!
!     ----------------
!     Class definition
!     ----------------
!
      TYPE, EXTENDS(FTObject) :: ChainedSegmentedCurve
         CHARACTER(LEN=CHAINED_SEGMENTED_CURVE_NAME_LENGTH) :: curveName
         INTEGER                                            :: id
         INTEGER                                            :: numberOfCurvesInChain
         LOGICAL                                            :: isCircular
         REAL(KIND=RP)                                      :: boundingBox(6)
         CLASS(FTMutableobjectArray), POINTER               :: chain  => NULL()
         INTEGER, DIMENSION(:), ALLOCATABLE                 :: curveStartIndices
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initwithNameAndID => initChain
         PROCEDURE :: destruct          => destructChain
         PROCEDURE :: add               => addSegmentedCurveToChain
         PROCEDURE :: printDescription  => printChainedCurveDescription
         PROCEDURE :: curveCount        => numberOfCurvesInChainFunction
         PROCEDURE :: nodeCount         => numberOfNodesInChain
         PROCEDURE :: complete          => completeChainedSegmentedCurve
         PROCEDURE :: positionAtIndex   => positionAtChainIndex
         PROCEDURE :: segmentedCurveAtIndex
         PROCEDURE :: chainNumberForIndex
         PROCEDURE :: localIndexForChainIndexInCurveNumber
         PROCEDURE :: curveIDAtChainIndex
         
      END TYPE ChainedSegmentedCurve
!
!     ========      
      CONTAINS 
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initChain(self, curveName, id)  
         IMPLICIT NONE
         CLASS(ChainedSegmentedCurve) :: self
         CHARACTER(LEN=*)             :: curveName
         INTEGER                      :: id
         
         CALL self % ftObject % init()
         
         self % curveName             = curveName
         self % id                    = id
         self % numberOfCurvesInChain = 0
         self % isCircular            = .FALSE.
         self % boundingBox           = 0.0_RP
         
         ALLOCATE(self % chain)
         CALL self % chain % initWithSize(5)
         
      END SUBROUTINE initChain
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructChain(self)  
         IMPLICIT NONE  
         CLASS(ChainedSegmentedCurve) :: self
         
         IF ( ASSOCIATED(self % chain) )     THEN
            CALL self % chain % release()
            IF ( self % chain % isUnreferenced() )     THEN
               DEALLOCATE(self % chain) 
            END IF  
         END IF 
         
      END SUBROUTINE destructChain
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addSegmentedCurveToChain(self,c)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(ChainedSegmentedCurve)          :: self
         CLASS(FRSegmentedCurve)     , POINTER :: c
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)             , POINTER :: obj => NULL()
         CLASS(FRSegmentedCurve)     , POINTER :: prevCurve => NULL()
         REAL(KIND=RP)                         :: xStart(3), xEnd(3), d
         INTEGER                               :: nChain
         
         nChain = self % numberOfCurvesInChain
         
         IF ( nChain == 0 )     THEN
!
!           -----------------
!           Start a new chain
!           -----------------
!
            obj => c
            CALL self % chain % addObject(obj) 
         ELSE 
!
!           ------------------------
!           Add to the current chain
!           ------------------------
!
            obj    => self % chain % objectAtIndex(nChain) 
            CALL castToSegmentedCurve(obj,prevCurve)
            xEnd   = prevCurve % positionAtIndex(prevCurve % COUNT())
            xStart = c % positionAtIndex(1)
            d      = MAXVAL(ABS(xEnd - xStart))
            
            IF ( d < chainTol )     THEN
               obj => c
               CALL self % chain % addObject(obj)
               
            ELSE ! Check first if the new curve is reversed
            
               xStart = c % positionAtIndex(c % COUNT())
               d      = MAXVAL(ABS(xEnd - xStart))
               
               IF ( d < chaintol )     THEN
                  CALL c % reverse()
                  obj => c
                  CALL self % chain % addObject(obj) 
               ELSE ! Trouble - they don't match up
                  CALL throwCurveDoesntFollowException(self,c,xStart,xEnd)
                  RETURN
               END IF 
               
            END IF 
!
!           ------------------------------------------------
!           The chain is circular if the end of the last one
!           matches the start of the first
!           ------------------------------------------------
!
            obj    => self % chain % objectAtIndex(1) 
            CALL castToSegmentedCurve(obj,prevCurve)
            xStart = prevCurve % positionAtIndex(1)
                  
            xEnd = c % positionAtIndex(c % COUNT())
            
            d    = MAXVAL(ABS(xEnd - xStart))
            
            IF ( d < chainTol )     self % isCircular = .TRUE.

         END IF 
         
         self % numberOfCurvesInChain =  nChain + 1
         
      END SUBROUTINE addSegmentedCurveToChain
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE completeChainedSegmentedCurve(self)  
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(ChainedSegmentedCurve) :: self
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                          :: j, N, m
         CLASS(FTObject)        , POINTER :: obj   => NULL()
         CLASS(FRSegmentedCurve), POINTER :: curve => NULL()
!
!        ----------------------------------------
!        Save indices where curvest start and end
!        ----------------------------------------
!
         ALLOCATE(self % curveStartIndices(self % numberOfCurvesInChain))
         self % curveStartIndices(1) = 1
         
         N = self % numberOfCurvesInChain
         m = 1
         DO j = 2, N
            obj => self % chain % objectAtIndex(j-1) 
            CALL castToSegmentedCurve(obj,curve)
            m = m + curve % COUNT()
            self % curveStartIndices(j) = m
         END DO
         CALL ComputeBoundingBox(self)
         
      END SUBROUTINE completeChainedSegmentedCurve
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION segmentedCurveAtIndex(self,indx)  RESULT(c)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(ChainedSegmentedCurve)          :: self
         CLASS(FRSegmentedCurve)     , POINTER :: c
         INTEGER                               :: indx
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)             , POINTER :: obj => NULL()
         
         obj => self % chain % objectAtIndex(indx)
         CALL castToSegmentedCurve(obj,c)
         
      END FUNCTION segmentedCurveAtIndex
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION chainNumberForIndex(self,j)  RESULT(n)
         IMPLICIT NONE
!  
!        ---------
!        Arguments
!        ---------
!
         CLASS(ChainedSegmentedCurve)          :: self
         INTEGER                               :: j, n
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER :: k, nCurves
         
         nCurves = SIZE(self % curveStartIndices)
         n       =  nCurves
         
         DO k = 1, nCurves-1
            IF ( j < self % curveStartIndices(k+1) )     THEN
               n = k
               RETURN  
            END IF  
         END DO
         
      END FUNCTION  
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION numberOfNodesInChain(self)  RESULT(m)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(ChainedSegmentedCurve) :: self
         INTEGER                      :: m
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                          :: j, N
         CLASS(FTObject)        , POINTER :: obj => NULL()
         CLASS(FRSegmentedCurve), POINTER :: curve => NULL()
         
         
         
         N = self % numberOfCurvesInChain
         m = 0
         DO j = 1, N
            obj => self % chain % objectAtIndex(j) 
            CALL castToSegmentedCurve(obj,curve)
            m = m + curve % COUNT()
         END DO
      END FUNCTION numberOfNodesInChain
!
!//////////////////////////////////////////////////////////////////////// 
! 
      INTEGER FUNCTION numberOfCurvesInChainFunction(self)  
         IMPLICIT NONE  
         CLASS(ChainedSegmentedCurve) :: self
         numberOfCurvesInChainFunction = self % numberOfCurvesInChain
      END FUNCTION numberOfCurvesInChainFunction
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printChainedCurveDescription(self,iUnit)  
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(ChainedSegmentedCurve) :: self
         INTEGER                      :: iUnit
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                          :: j, N
         CLASS(FTobject), POINTER         :: obj   => NULL()
         CLASS(FRSegmentedCurve), POINTER :: curve => NULL()
         
         N = self % numberOfCurvesInChain
         WRITE(iUnit,*) "Chain ",TRIM(self % curveName)
         DO j = 1, N
            obj => self % chain % objectAtIndex(j)
            CALL castToSegmentedCurve(obj,curve)
            WRITE(iUnit,*) "Curve ",j, " = ",TRIM(curve % curveName) 
            CALL curve % printDescription(6)
         END DO  
      END SUBROUTINE printChainedCurveDescription
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION positionAtChainIndex(self,indx)  RESULT(x)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(ChainedSegmentedCurve) :: self
         INTEGER                      :: indx
         REAL(KIND=RP)                :: x(3)
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                          :: curveNumber, j
         CLASS(FRSegmentedCurve), POINTER :: c => NULL()
         
         
         curveNumber =  self % chainNumberForIndex(indx)
         c           => self % segmentedCurveAtIndex(curveNumber)
         j           =  self % localIndexForChainIndexInCurveNumber(indx,curveNumber)
         x = c % positionAtIndex(j)
         
      END FUNCTION positionAtChainIndex
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION normalAtChainIndex(self,indx)  RESULT(x)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(ChainedSegmentedCurve) :: self
         INTEGER                      :: indx
         REAL(KIND=RP)                :: x(3)
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                          :: curveNumber, j
         CLASS(FRSegmentedCurve), POINTER :: c => NULL()
         
         
         curveNumber =  self % chainNumberForIndex(indx)
         c           => self % segmentedCurveAtIndex(curveNumber)
         j           =  self % localIndexForChainIndexInCurveNumber(indx,curveNumber)
         x = c % normalAtIndex(j)
         
      END FUNCTION normalAtChainIndex
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION argumentAtChainIndex(self,indx)  RESULT(t)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(ChainedSegmentedCurve) :: self
         INTEGER                      :: indx
         REAL(KIND=RP)                :: t
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                          :: curveNumber, j
         CLASS(FRSegmentedCurve), POINTER :: c => NULL()
         
         
         curveNumber =  self % chainNumberForIndex(indx)
         c           => self % segmentedCurveAtIndex(curveNumber)
         j           =  self % localIndexForChainIndexInCurveNumber(indx,curveNumber)
         
         t = c % argumentAtIndex(j)
         
      END FUNCTION argumentAtChainIndex
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION curveIDAtChainIndex(self,indx)  RESULT(id)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(ChainedSegmentedCurve) :: self
         INTEGER                      :: indx
         INTEGER                      :: id
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                          :: curveNumber, j
         CLASS(FRSegmentedCurve), POINTER :: c => NULL()
         
         
         curveNumber =  self % chainNumberForIndex(indx)
         c           => self % segmentedCurveAtIndex(curveNumber)
         j           =  self % localIndexForChainIndexInCurveNumber(indx,curveNumber)
         
         id = c % id
         
      END FUNCTION curveIDAtChainIndex
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION localIndexForChainIndexInCurveNumber(self,chainIndex,curveNumber)  RESULT(j)
      IMPLICIT NONE  
         CLASS(ChainedSegmentedCurve) :: self
         INTEGER                      :: chainIndex, curveNumber
         INTEGER                      :: j
         j =  chainIndex - self % curveStartIndices(curveNumber) + 1
      END FUNCTION localIndexForChainIndexInCurveNumber
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeBoundingBox( self )
         IMPLICIT NONE
         
         CLASS(ChainedSegmentedCurve)     :: self
         
         CLASS(FRSegmentedCurve), POINTER :: c => NULL()
         REAL(KIND=RP)                    :: xMin, xMax, yMin, yMax, x(3)
         INTEGER                          :: j, k
         
         xMin =  HUGE(xMin)
         xMax = -HUGE(xMax)
         yMin =  HUGE(xMin)
         yMax = -HUGE(xMax)
         
         DO k = 1, self % numberOfCurvesInChain
            c => self % segmentedCurveAtIndex(k)
            DO j = 1, c % COUNT()
               x = c % positionAtIndex(j) 
               xMax = MAX(x(1),xMax)
               xMin = MIN(x(1),xMin)
               yMax = MAX(x(2),yMax)
               yMin = MIN(x(2),yMin)
            END DO  
         END DO  
         
         self % boundingBox = 0.0_RP
         self % boundingBox(BBOX_TOP)    = yMax
         self % boundingBox(BBOX_BOTTOM) = yMin
         self % boundingBox(BBOX_LEFT)   = xMin
         self % boundingBox(BBOX_RIGHT)  = xMax
         
      END SUBROUTINE ComputeBoundingBox 
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION Height( self ) 
         IMPLICIT NONE 
         CLASS(ChainedSegmentedCurve) :: self
         Height = self % boundingBox(BBOX_TOP) - self % boundingBox(BBOX_BOTTOM)
      END FUNCTION Height 
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION Width( self ) 
         IMPLICIT NONE 
         CLASS(ChainedSegmentedCurve) :: self
         Width = self % boundingBox(BBOX_RIGHT) - self % boundingBox(BBOX_LEFT)
      END FUNCTION Width 

!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToChainedSegmentedCurve(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)             , POINTER :: obj
         CLASS(ChainedSegmentedCurve), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            CLASS IS(ChainedSegmentedCurve)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castToChainedSegmentedCurve
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE throwCurvedoesntFollowException(self,c,xStart,xEnd)  
         USE FTDataClass
         USE FTValueClass
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(ChainedSegmentedCurve)          :: self
         CLASS(FRSegmentedCurve)     , POINTER :: c
         REAL(KIND=RP), DIMENSION(3)           :: xStart, xEnd
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTException)   , POINTER     :: exception      => NULL()
         CLASS(FTDictionary)  , POINTER     :: userDictionary => NULL()
         CLASS(FTObject)      , POINTER     :: obj            => NULL()
         CLASS(FTValue)       , POINTER     :: v              => NULL()
         CLASS(FTData)        , POINTER     :: d              => NULL()
         CHARACTER(LEN=1)     , ALLOCATABLE :: dat(:)
         INTEGER                            :: dataLength
!
!        -----------------------------------------------------
!           Dictionary keys:
!              chainName
!              curveName
!              endPosition       "REAL,DIMENSION(3)"
!              startPosition     "REAL,DIMENSION(3)"
!        -----------------------------------------------------
!
         ALLOCATE(userDictionary)
         CALL userDictionary % initWithSize(6)
          
         ALLOCATE(v)
         CALL v % initWithValue(self % curveName)
         obj => v
         CALL userDictionary % addObjectForKey(obj,"chainName")
         CALL v % release()
           
         ALLOCATE(v)
         CALL v % initWithValue( c % curveName)
         obj => v
         CALL userDictionary % addObjectForKey(obj,"curveName")
         CALL v % release()
         
         dataLength = SIZE(TRANSFER(xStart,dat))
         ALLOCATE(dat(dataLength))
         dat = TRANSFER(xStart,dat)
         ALLOCATE(d)
         CALL d % initWithDataOfType(dat,"REAL,DIMENSION(3)")
         obj => d
         CALL userDictionary % addObjectForKey(obj,"startPosition")
         CALL d % release()
         
         dat = TRANSFER(xEnd,dat)
         ALLOCATE(d)
         CALL d % initWithDataOfType(dat,"REAL,DIMENSION(3)")
         obj => d
         CALL userDictionary % addObjectForKey(obj,"endPosition")
         CALL d % release()
!
!        --------------------
!        Create the exception
!        --------------------
!
         ALLOCATE(exception)
         
         CALL exception % initFTException(FT_ERROR_FATAL, &
                              exceptionName   = SEGMENTED_CURVES_DONT_JOIN_EXCEPTION, &
                              infoDictionary  = userDictionary)
         CALL userDictionary % release()
!
!        -------------------
!        Throw the exception
!        -------------------
!
         CALL throw(exception)
         CALL exception % release()
         
      END SUBROUTINE throwCurvedoesntFollowException

      END Module ChainedSegmentedCurveClass

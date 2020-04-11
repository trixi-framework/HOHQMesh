!
!////////////////////////////////////////////////////////////////////////
!
!      SMModel.f90
!      Created: August 5, 2013 2:11 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module SMModelClass
      USE ProgramGlobals
      USE FTLinkedListClass
      USE FTExceptionClass
      USE SMChainedCurveClass
      USE SharedExceptionManagerModule
      USE SMParametricEquationCurveClass
      USE SMSplineCurveClass
      USE SMLineClass
      USE CurveSweepClass
      USE FTValueDictionaryClass
      USE ErrorTypesModule
      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      INTEGER, PARAMETER, PRIVATE :: INNER_BOUNDARY_BLOCK = 0, INTERFACE_BOUNDARY_BLOCK = 1
      INTEGER, PARAMETER          :: BOUNDARY_CURVE       = 0, INTERFACE_CURVE = 1
      INTEGER, PARAMETER          :: BLOCK_NAME_STRING_LENGTH = 32
      CHARACTER(LEN=16)           :: MODEL_READ_EXCEPTION = "Model read error"
      
      CHARACTER(LEN=LINE_LENGTH), PARAMETER, PRIVATE  :: OUTER_BOUNDARY_BLOCK_KEY       = "OUTER_BOUNDARY"
      CHARACTER(LEN=LINE_LENGTH), PARAMETER, PRIVATE  :: INNER_BOUNDARIES_BLOCK_KEY     = "INNER_BOUNDARIES"      
      CHARACTER(LEN=LINE_LENGTH), PARAMETER, PRIVATE  :: INTERFACE_BOUNDARIES_BLOCK_KEY = "INTERFACE_BOUNDARIES"      
!
!     ---------------------
!     Class type definition
!     ---------------------
!
      TYPE, EXTENDS(FTObject) ::  SMModel
         INTEGER                                  :: curveCount
         INTEGER                                  :: numberOfOuterCurves, numberOfSweepCurves
         INTEGER                                  :: numberOfInnerCurves
         INTEGER                                  :: numberOfInterfaceCurves
         CHARACTER(LEN=32)                        :: modelName
         CLASS(SMChainedCurve)      , POINTER     :: outerBoundary       => NULL()
         CLASS(SMChainedCurve)      , POINTER     :: sweepCurve          => NULL()
         CLASS(FTLinkedList)        , POINTER     :: innerBoundaries     => NULL()
         CLASS(FTLinkedList)        , POINTER     :: interfaceBoundaries => NULL()
         CLASS(FTLinkedListIterator), POINTER     :: innerBoundariesIterator => NULL()
         CLASS(FTLinkedListIterator), POINTER     :: interfaceBoundariesIterator => NULL()
         INTEGER                    , ALLOCATABLE :: boundaryCurveMap(:) ! Tells which chain a curve is in
         INTEGER, DIMENSION(:)      , ALLOCATABLE :: curveType           ! Either a boundary or an interface
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithContentsOfDictionary         
         FINAL     :: destructModel
         PROCEDURE :: chainWithID
         PROCEDURE :: curveWithID => curveInModelWithID
      END TYPE SMModel
!
!     ========
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initWithContentsOfDictionary( self, modelDict )  
         IMPLICIT NONE
         CLASS(SMModel)                    :: self
         CLASS(FTValueDictionary), POINTER :: modelDict
         
         CALL self % FTObject % init()
         
         self % outerBoundary               => NULL()
         self % innerBoundaries             => NULL()
         self % interfaceBoundaries         => NULL()
         self % innerBoundariesIterator     => NULL()
         self % interfaceBoundariesIterator => NULL()
         self % curveCount                  =  0
         self % numberOfOuterCurves         =  0
         self % numberOfSweepCurves         =  0
         self % numberOfInnerCurves         =  0
         self % numberOfInterfaceCurves     =  0
         
         IF( .NOT.ASSOCIATED(modelDict)) RETURN 
         CALL constructModelFromDictionary( self, modelDict )
         
      END SUBROUTINE initWithContentsOfDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructModel(self)  
         IMPLICIT NONE  
         TYPE(SMModel)            :: self
         CLASS(FTObject), POINTER :: obj
         
         obj => self % innerBoundariesIterator
         CALL release(self = obj)
         obj => self % interfaceBoundariesIterator
         CALL release(self = obj)
         obj => self % innerBoundaries
         CALL release(self = obj)
         obj => self % interfaceBoundaries
         CALL release(self = obj)
         obj => self % outerBoundary
         CALL release(obj)
         
         IF ( ASSOCIATED(self % sweepCurve) )     THEN
            obj => self % sweepCurve
            CALL release(obj)
            self % numberOfSweepCurves = 0
         END IF 
         
         IF ( ALLOCATED(self % boundaryCurveMap) )     THEN
            DEALLOCATE(self % boundaryCurveMap)
         END IF
         
         IF ( ALLOCATED(self % curveType) )     THEN
            DEALLOCATE(self % curveType)
         END IF
         
      END SUBROUTINE destructModel
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseModel(self)  
         IMPLICIT NONE
         TYPE (SMModel)  , POINTER :: self
         CLASS(FTObject), POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseModel
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE constructModelFromDictionary( self, modelDict )  
         IMPLICIT NONE
!
!        -----------
!        Arguments  
!        -----------
!
         CLASS(SMModel)                    :: self
         CLASS(FTValueDictionary), POINTER :: modelDict
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, EXTERNAL                 :: UnusedUnit
         CLASS(FTValueDictionary), POINTER :: outerBoundaryDict, innerBoundariesDict, sweepCurveDict
         CLASS(FTLinkedList)     , POINTER :: innerBoundariesList
         CLASS(FTObject)         , POINTER :: obj
!
!        ----------
!        Interfaces
!        ----------
!
         LOGICAL, EXTERNAL :: ReturnOnFatalError
!
!        --------------------------------
!        Construct outer boundary, if any
!        --------------------------------
!
         IF ( modelDict % containsKey(key = OUTER_BOUNDARY_BLOCK_KEY) )     THEN
         
            ALLOCATE( self % outerBoundary )
            CALL self % outerBoundary % initChainWithNameAndID("Outer Boundary",1)
         
            obj               => modelDict % objectForKey(key = OUTER_BOUNDARY_BLOCK_KEY)
            outerBoundaryDict => valueDictionaryFromObject(obj)
            
            CALL ConstructOuterBoundary( self, outerBoundaryDict )
            IF(ReturnOnFatalError())     RETURN 
            self % numberOfOuterCurves = 1
            
         END IF 
!
!        ----------------------------------
!        Construct inner boundaries, if any
!        ----------------------------------
!
         IF ( modelDict% containsKey(key = INNER_BOUNDARIES_BLOCK_KEY) )     THEN
         
            ALLOCATE( self % innerBoundaries )
            CALL self % innerBoundaries % init()
            
            obj                 => modelDict % objectForKey(key = INNER_BOUNDARIES_BLOCK_KEY)
            innerBoundariesDict => valueDictionaryFromObject(obj)
            obj                 => innerBoundariesDict % objectForKey(key = "LIST")
            innerBoundariesList => linkedListFromObject(obj)

            CALL ConstructInnerBoundaries( self, INNER_BOUNDARY_BLOCK, innerBoundariesList )
            IF(ReturnOnFatalError())     RETURN 
            
         END IF 

         IF ( ASSOCIATED(self % innerBoundaries) )     THEN
            ALLOCATE(self % innerBoundariesIterator)
            CALL  self % innerBoundariesIterator % initWithFTLinkedList(self % innerboundaries)
         END IF 
!
!        ----------------------------------
!        Import iterface boundaries, if any
!        ----------------------------------
!
         IF ( modelDict% containsKey(key = INTERFACE_BOUNDARIES_BLOCK_KEY) )     THEN
            ALLOCATE( self % interfaceBoundaries )
            CALL self % interfaceBoundaries % init()
            
            obj                 => modelDict % objectForKey(key = INTERFACE_BOUNDARIES_BLOCK_KEY)
            innerBoundariesDict => valueDictionaryFromObject(obj)
            obj                 => innerBoundariesDict % objectForKey(key = "LIST")
            innerBoundariesList => linkedListFromObject(obj)
            
            CALL ConstructInnerBoundaries( self, INTERFACE_BOUNDARY_BLOCK,innerBoundariesList )
            IF(ReturnOnFatalError())     RETURN 
            
         END IF 

         IF ( ASSOCIATED(self % interfaceBoundaries) )     THEN
            ALLOCATE(self % interfaceBoundariesIterator)
            CALL  self % interfaceBoundariesIterator % initWithFTLinkedList(self % interfaceBoundaries)
         END IF 
!
!        --------------------------------------
!        Construct sweep curve boundary, if any
!        --------------------------------------
!
         IF ( modelDict % containsKey(key = SWEEP_CURVE_BLOCK_KEY) )     THEN
         
            ALLOCATE( self % sweepCurve )
            CALL self % sweepCurve % initChainWithNameAndID("Sweep curve",1)
         
            obj            => modelDict % objectForKey(key = SWEEP_CURVE_BLOCK_KEY)
            sweepCurveDict => valueDictionaryFromObject(obj)
            
            CALL AssembleChainCurve(self           = self,             &
                                    curveDict      = sweepCurveDict,   &
                                    curveChain     = self % sweepCurve,&
                                    innerOrOuter   = NOT_APPLICABLE,   &
                                    chainMustClose = .FALSE.)
            IF(ReturnOnFatalError())     RETURN 
            
         END IF 
!
!        ---------
!        Finish up
!        ---------
!
         CALL MakeCurveToChainConnections(self)
                 
      END SUBROUTINE constructModelFromDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ConstructOuterBoundary( self, outerBoundaryDict ) 
         IMPLICIT NONE  
!
!        -----------
!        Arguments  
!        -----------
!
         CLASS(SMModel)                    :: self
         CLASS(FTValueDictionary), POINTER :: outerBoundaryDict
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedList)       , POINTER :: outerBoundaryList
         CLASS(FTObject)           , POINTER :: obj
         CLASS(FTValueDictionary)  , POINTER :: blockDict
         TYPE(FTLinkedListIterator)          :: iterator
                  
         CALL AssembleChainCurve(self           = self,                &
                                 curveDict      = outerBoundaryDict,   &
                                 curveChain     = self % outerBoundary,&
                                 innerOrOuter   = OUTER,               &
                                 chainMustClose = .TRUE.)
         
      END SUBROUTINE ConstructOuterBoundary
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE AssembleChainCurve( self, curveDict, curveChain, innerOrOuter, chainMustClose ) 
         IMPLICIT NONE  
!
!        -----------
!        Arguments  
!        -----------
!
         CLASS(SMModel)                    :: self
         CLASS(FTValueDictionary), POINTER :: curveDict
         CLASS(SMChainedCurve)   , POINTER :: curveChain
         INTEGER                           :: innerOrOuter
         LOGICAL                           :: chainMustClose
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedList)       , POINTER :: curveList
         CLASS(FTObject)           , POINTER :: obj
         CLASS(FTValueDictionary)  , POINTER :: blockDict
         TYPE(FTLinkedListIterator)          :: iterator
                  
         obj       => curveDict % objectForKey(key = "LIST")
         curveList => linkedListFromObject(obj)
         CALL iterator % initWithFTLinkedList(list = curveList)
         
         DO WHILE (.NOT. iterator % isAtEnd())
            obj       => iterator % object()
            blockDict => valueDictionaryFromObject(obj)
            
            CALL ConstructCurve( self, curveChain, blockDict )
            
            CALL iterator % moveToNext()
         END DO 
!
!        ------------------
!        Finalize the chain
!        ------------------
!
         CALL curveChain % complete(innerOrOuterCurve = innerOrOuter,chainMustClose = chainMustClose)
         
      END SUBROUTINE AssembleChainCurve
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructInnerBoundaries( self, blockType, boundariesList ) 
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMModel)               :: self
         INTEGER                      :: blockType
         CLASS(FTLinkedList), POINTER :: boundariesList
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedListIterator), POINTER    :: innerBoundariesIterator, listOfCurvesIterator
         CLASS(FTValueDictionary)   , POINTER    :: chainDict, curveDict
         CLASS(FTObject)            , POINTER    :: obj
         CLASS(FTLinkedList)        , POINTER    :: listOfCurves
         
         CLASS(SMChainedCurve), POINTER          :: chain => NULL()
         CHARACTER(LEN=BLOCK_NAME_STRING_LENGTH) :: chainName

         ALLOCATE(innerBoundariesIterator)
         CALL innerBoundariesIterator % initWithFTLinkedList(list = boundariesList)
         ALLOCATE(listOfCurvesIterator)
         CALL listOfCurvesIterator % init()
         
         CALL innerBoundariesIterator % setToStart()
         DO WHILE(.NOT. innerBoundariesIterator % isAtEnd())
!
!           ------------------------------------
!           Inner boundaries is a list of chains
!           ------------------------------------
!
            obj       => innerBoundariesIterator % object()
            chainDict => valueDictionaryFromObject(obj)
            chainName = chainDict % stringValueForKey(key = "name", &
                                                      requestedLength = BLOCK_NAME_STRING_LENGTH)
               
            ALLOCATE(chain)
            CALL chain % initChainWithNameAndID(chainName,0)
!
!           -------------------------------
!           Chains contain a list of curves
!           -------------------------------
!
            obj          => chainDict % objectForKey(key = "LIST")
            listOfCurves => linkedListFromObject(obj)
            CALL listOfCurvesIterator % setLinkedList(list = listOfCurves)
            CALL listOfCurvesIterator % setToStart()
            
            DO WHILE( .NOT. listOfCurvesIterator % isAtEnd())
               obj       => listOfCurvesIterator % object()
               curveDict => valueDictionaryFromObject(obj)
               
               CALL ConstructCurve(self, chain, curveDict )
               
               CALL listOfCurvesIterator % moveToNext()
            END DO 
               
            IF ( blockType == INNER_BOUNDARY_BLOCK )     THEN
              obj => chain
              CALL self % innerBoundaries % add(obj)
            ELSE
              obj => chain
              CALL self % interfaceBoundaries % add(obj)
            END IF
!
!           ------------------
!           Finalize the chain
!           ------------------
!
            CALL chain % complete(innerOrOuterCurve = INNER,chainMustClose = .TRUE.)
            obj => chain
            CALL release(obj)
!
!           --------------------------------------------------------
!           The chain has been created, clean up and then start over
!           --------------------------------------------------------
!
            IF ( blockType == INNER_BOUNDARY_BLOCK )     THEN
               self % numberOfInnerCurves     = self % numberOfInnerCurves + 1
            ELSE
               self % numberOfInterfaceCurves = self % numberOfInterfaceCurves + 1
            END IF
         
            CALL innerBoundariesIterator % moveToNext()
         END DO 
         
         obj => innerBoundariesIterator
         CALL release(self = obj)
         obj => listOfCurvesIterator
         CALL release(self = obj)
         
      END SUBROUTINE ConstructInnerBoundaries
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructCurve( self, chain, curveDict )
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMModel)                    :: self
         CLASS(SMChainedCurve)   , POINTER :: chain
         CLASS(FTValueDictionary), POINTER :: curveDict
!
!        ----------
!        Interfaces
!        ----------
!
         LOGICAL, EXTERNAL :: ReturnOnFatalError

         SELECT CASE ( curveDict % stringValueForKey(key = "TYPE", &
                       requestedLength = BLOCK_NAME_STRING_LENGTH) )
         
            CASE("PARAMETRIC_EQUATION_CURVE")
            
               CALL ConstructParametricEquationFromDict( self, chain, curveDict )
               IF(ReturnOnFatalError())     RETURN 
               
            CASE ("SPLINE_CURVE" )
            
               CALL ImportSplineBlock( self, chain, curveDict )
               
            CASE ("END_POINTS_LINE" )
            
               CALL ImportLineEquationBlock( self, chain, curveDict )
               
            CASE DEFAULT

               RETURN
         END SELECT
         
         self % curveCount = self % curveCount + 1
         
      END SUBROUTINE ConstructCurve
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructParametricEquationFromDict( self, chain, curveDict ) 
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMModel)                    :: self
         CLASS(SMChainedCurve)   , POINTER :: chain
         CLASS(FTValueDictionary), POINTER :: curveDict
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=SM_CURVE_NAME_LENGTH)       :: curveName
         CHARACTER(LEN=EQUATION_STRING_LENGTH)     :: eqnX, eqnY, eqnZ
         CLASS(SMParametricEquationCurve), POINTER :: cCurve => NULL()
         CLASS(SMCurve)                  , POINTER :: curvePtr => NULL()
         CLASS(FTObject)                 , POINTER :: obj
!
!        ----------
!        Interfaces
!        ----------
!
         LOGICAL :: returnOnFatalError
!
!        ------------
!        Get the data
!        ------------
!
         IF ( curveDict % containsKey(key = "name") )     THEN
            curveName = curveDict % stringValueForKey(key = "name", &
                                                      requestedLength = SM_CURVE_NAME_LENGTH) 
         ELSE
            curveName = "curve"
            CALL ThrowErrorExceptionOfType(poster = "ImportParametricEquationBlock",&
                                           msg = "PARAMETRIC_EQUATION_CURVE has no name. Use default 'curve'", &
                                           typ = FT_ERROR_WARNING)
         END IF 
         
         IF ( curveDict % containsKey(key = "xEqn") )     THEN
            eqnX = curveDict % stringValueForKey(key = "xEqn", &
                                                      requestedLength = DEFAULT_CHARACTER_LENGTH) 
         ELSE
            CALL ThrowErrorExceptionOfType(poster = "ImportParametricEquationBlock",&
                                           msg = "PARAMETRIC_EQUATION_CURVE has no xEqn.", &
                                           typ = FT_ERROR_FATAL)
            RETURN 
         END IF 
         
         IF ( curveDict % containsKey(key = "yEqn") )     THEN
            eqnY = curveDict % stringValueForKey(key = "yEqn", &
                                                      requestedLength = DEFAULT_CHARACTER_LENGTH) 
         ELSE
            CALL ThrowErrorExceptionOfType(poster = "ImportParametricEquationBlock",&
                                           msg = "PARAMETRIC_EQUATION_CURVE has no yEqn.", &
                                           typ = FT_ERROR_FATAL)
            RETURN 
         END IF 
         
         IF ( curveDict % containsKey(key = "zEqn") )     THEN
            eqnZ = curveDict % stringValueForKey(key = "zEqn", &
                                                      requestedLength = DEFAULT_CHARACTER_LENGTH) 
         ELSE
            CALL ThrowErrorExceptionOfType(poster = "ImportParametricEquationBlock",&
                                           msg = "PARAMETRIC_EQUATION_CURVE has no zEqn. Default is z = 0", &
                                           typ = FT_ERROR_WARNING)
            eqnZ = "z(t) = 0.0"
         END IF 
!
!        ----------------
!        Create the curve
!        ----------------
!
         ALLOCATE(cCurve)
         CALL cCurve % initWithEquationsNameAndID(eqnX, eqnY, eqnZ, curveName, self % curveCount + 1)
         IF(ReturnOnFatalError())     RETURN 
         
         curvePtr => cCurve
         CALL chain  % addCurve(curvePtr)
         obj => cCurve
         CALL release(obj)
         
      END SUBROUTINE ConstructParametricEquationFromDict
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ImportSplineBlock( self, chain, splineDict ) 
         USE ValueSettingModule
         USE FTDataClass
         USE EncoderModule
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMModel)                    :: self
         CLASS(FTValueDictionary), POINTER :: splineDict
         CLASS(SMChainedCurve)   , POINTER :: chain
!
!        ----------
!        Interfaces
!        ----------
!
         LOGICAL, EXTERNAL :: ReturnOnFatalError
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject) , POINTER                  :: obj
         CLASS(FTData)   , POINTER                  :: splineData
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH)    :: curveName
         CHARACTER(LEN=1), POINTER                  :: encodedData(:)
         REAL(KIND=RP), ALLOCATABLE                 :: decodedArray(:,:)
         CLASS(SMSplineCurve)             , POINTER :: cCurve      => NULL()
         CLASS(SMCurve)                   , POINTER :: curvePtr    => NULL()
         INTEGER                                    :: numKnots
         
         INTEGER, EXTERNAL :: GetIntValue
!
!        ------------
!        Get the data
!        ------------
!
         curveName = "spline"
         CALL SetStringValueFromDictionary(valueToSet = curveName,                 &
                                           sourceDict = splineDict,                &
                                           key = "name",                           &
                                           errorLevel = FT_ERROR_WARNING,          &
                                           message = "spline block has no name. Using default name spline", &
                                           poster = "ImportSplineBlock")

         CALL SetIntegerValueFromDictionary(valueToSet = numKnots,             &
                                            sourceDict = splineDict,           &
                                            key = "nKnots",                    &
                                            errorLevel = FT_ERROR_FATAL,       &
                                            message = "nKnots keyword not found in spline definition", &
                                            poster = "ImportSplineBlock")
         IF(ReturnOnFatalError()) RETURN 
!
!        ---------------------
!        Get the spline points
!        ---------------------
!
         obj         => splineDict % objectForKey(key = "data")
         splineData  => dataFromObject(obj)
         encodedData => splineData % storedData()

         IF(.NOT. ASSOCIATED(encodedData))     THEN
            CALL ThrowErrorExceptionOfType(poster = "ImportSplineBlock", &
                                           msg    = "Spline does not appear to contain any data",&
                                           typ    = FT_ERROR_FATAL)
            RETURN 
         END IF 
         
         CALL DECODE(enc = encodedData, N = 4, M = numKnots, arrayOut = decodedArray)
!
!        ----------------
!        Create the curve
!        ----------------
!
         ALLOCATE(cCurve)
         CALL cCurve % initWithPointsNameAndID(decodedArray(1,:), decodedArray(2,:), &
                                               decodedArray(3,:), decodedArray(4,:), &
                                               curveName, self % curveCount + 1 )
         !Spline curves have no exceptions thrown
         curvePtr => cCurve
         CALL chain  % addCurve(curvePtr)
         obj => cCurve
         CALL release(obj)
         
      END SUBROUTINE ImportSplineBlock
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ImportLineEquationBlock( self, chain, lineBlockDict) 
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMModel)                    :: self
         CLASS(SMChainedCurve)   , POINTER :: chain
         CLASS(FTValueDictionary), POINTER :: lineBlockDict
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=LINE_LENGTH)            :: inputLine = " "
         CHARACTER(LEN=SM_CURVE_NAME_LENGTH)   :: curveName
         REAL(KIND=RP), DIMENSION(3)           :: xStart, xEnd
         CLASS(SMLine)          , POINTER      :: cCurve => NULL()
         CLASS(SMCurve)         , POINTER      :: curvePtr => NULL()
         CLASS(FTObject)        , POINTER      :: obj
!
!        ------------------------------------------------         
         INTERFACE
            FUNCTION GetRealArray( inputLine ) RESULT(x)
               USE SMConstants
               IMPLICIT NONE
               REAL(KIND=RP), DIMENSION(3) :: x
               CHARACTER ( LEN = * ) :: inputLine
            END FUNCTION GetRealArray
         END INTERFACE
!        ________________________________________________
!
!        ------------
!        Get the data
!        ------------
!
         IF( lineBlockDict % containsKey(key = 'name') )     THEN
            curveName = lineBlockDict % stringValueForKey(key             = "name", &
                                                          requestedLength = SM_CURVE_NAME_LENGTH)
         ELSE
            curveName = "line"
            CALL ThrowErrorExceptionOfType(poster = "ImportLineEquationBlock",&
                                           msg = "No name found in line curve definition. Using 'line' as default", &
                                           typ = FT_ERROR_WARNING)
            
         END IF 
         
         IF( lineBlockDict % containsKey(key = 'xStart') )     THEN
            inputLine = lineBlockDict % stringValueForKey(key             = "xStart", &
                                                          requestedLength = LINE_LENGTH)
            xStart = GetRealArray(inputLine)
         ELSE
            CALL ThrowErrorExceptionOfType(poster = "ImportLineEquationBlock",&
                                           msg = "No xStart in line curve definition.", &
                                           typ = FT_ERROR_FATAL)
            RETURN 
         END IF 
         
         IF( lineBlockDict % containsKey(key = 'xEnd') )     THEN
            inputLine = lineBlockDict % stringValueForKey(key             = "xEnd", &
                                                          requestedLength = LINE_LENGTH)
            xEnd = GetRealArray(inputLine)
         ELSE
            CALL ThrowErrorExceptionOfType(poster = "ImportLineEquationBlock",&
                                           msg = "No xEnd in line curve definition.", &
                                           typ = FT_ERROR_FATAL)
            RETURN 
         END IF 
!
!        ----------------
!        Create the curve
!        ----------------
!
         ALLOCATE(cCurve)
         CALL cCurve % initWithStartEndNameAndID( xStart, xEnd, curveName, self % curveCount + 1 )
         !SMLine does not throw exceptions on init
         
         curvePtr => cCurve
         CALL chain  % addCurve(curvePtr)
         obj => cCurve
         CALL release(obj)
         
      END SUBROUTINE ImportLineEquationBlock
!
!///////////////////////////////////////////////////////////////////////
!
!     ----------------------------------------------------------------
!!    Extracts the string within the parentheses  in an input file
!     ----------------------------------------------------------------
!
      CHARACTER( LEN=LINE_LENGTH ) FUNCTION GetStringValue( inputLine )
!
      CHARACTER ( LEN = * ) :: inputLine
      INTEGER               :: cStart, cEnd
!
      cStart = INDEX(inputLine,'"')
      cEnd   = INDEX(inputLine, '"', .true. )
      GetStringValue = inputLine( cStart+1: cEnd-1 )
!
      END FUNCTION GetStringValue
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE MakeCurveToChainConnections(self)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMModel) :: self
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMChainedCurve), POINTER :: chain => NULL()
         CLASS(SMCurve)       , POINTER :: currentCurve => NULL()
         CLASS(FTObject)      , POINTER :: obj => NULL()
         CLASS(FTLinkedListIterator), POINTER    :: iterator => NULL()
         
         INTEGER                        :: chainCount
         INTEGER                        :: j
         
         IF( self%curveCount == 0 )     RETURN 
         
         ALLOCATE( self%boundaryCurveMap(self%curveCount) )
         ALLOCATE( self%curveType(self%curveCount) ) ! Number of chains is always <= number of curves
         chainCount = 0
!
!        -----------
!        Outer chain
!        -----------
!
         IF( ASSOCIATED(self%outerBoundary) )     THEN 
            chain                          => self % outerBoundary
            chainCount                     =  chainCount + 1
            self % curveType(chainCount)   =  BOUNDARY_CURVE
            
            CALL self % outerBoundary % setID(chainCount)
            
            DO j = 1, chain % COUNT()
                obj => chain % curvesArray % objectAtIndex(j)
                CALL cast(obj,currentCurve)
                self % boundaryCurveMap( currentCurve % id() ) = self % outerBoundary % id()
            END DO  
         END IF
!
!        ----------------
!        Inner boundaries
!        ----------------
!
         IF( ASSOCIATED( self % innerBoundaries ) )     THEN
            ALLOCATE(iterator)
            CALL iterator % initWithFTLinkedList(self % innerBoundaries)
            CALL iterator % setToStart()
            
            DO WHILE (.NOT.iterator % isAtEnd())
!
!              --------------------
!              Set chain properties
!              --------------------
!
               obj => iterator % object()
               CALL castToSMChainedCurve(obj,chain)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
               chainCount                   =  chainCount + 1
               self % curveType(chainCount) =  BOUNDARY_CURVE
               
               CALL chain % setID(chainCount)
!
!              ------------------------
!              Component curve settings
!              ------------------------
!
            
               DO j = 1, chain % COUNT()
                   obj => chain % curvesArray % objectAtIndex(j)
                   CALL cast(obj,currentCurve)
                   self % boundaryCurveMap( currentCurve % id() ) = chain % id()
               END DO 
                
               CALL iterator % moveToNext()
            END DO
            obj => iterator
           CALL release(obj)
        END IF
!!
!!        --------------------
!!        Interface boundaries
!!        --------------------
!!
         IF( ASSOCIATED( self % interfaceBoundaries ) )     THEN
            ALLOCATE(iterator)
            CALL iterator % initWithFTLinkedList(self % interfaceBoundaries)
            CALL iterator % setToStart()
            
            DO WHILE (.NOT.iterator % isAtEnd())
!
!              --------------------
!              Set chain properties
!              --------------------
!
               obj => iterator % object()
               CALL castToSMChainedCurve(obj,chain)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
               chainCount                   =  chainCount + 1
               self % curveType(chainCount) =  INTERFACE_CURVE
               
               CALL chain % setID(chainCount)
!
!              ------------------------
!              Component curve settings
!              ------------------------
!
            
               DO j = 1, chain % COUNT()
                   obj => chain % curvesArray % objectAtIndex(j)
                   CALL cast(obj,currentCurve)
                   self % boundaryCurveMap( currentCurve % id() ) = chain % id()
               END DO 
                
               CALL iterator % moveToNext()
            END DO
            obj => iterator
           CALL release(obj)
        END IF

      END SUBROUTINE MakeCurveToChainConnections
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ThrowModelReadException(objectName,msg)  
         USE FTValueClass
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=*)  :: msg
         CHARACTER(LEN=*)  :: objectName
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (FTException)   , POINTER :: exception => NULL()
         CLASS(FTDictionary)  , POINTER :: userDictionary => NULL()
         CLASS(FTObject)      , POINTER :: obj => NULL()
         CLASS(FTValue)       , POINTER :: v => NULL()
!
!        -----------------------------------------------------
!        The userDictionary for this exception contains the
!        message to be delivered under the name "message"
!        and the object being created by the name "objectName"
!        -----------------------------------------------------
!
         ALLOCATE(userDictionary)
         CALL userDictionary % initWithSize(4)
         
         ALLOCATE(v)
         CALL v % initWithValue(objectName)
         obj => v
         CALL userDictionary % addObjectForKey(obj,"objectName")
         CALL release(obj)
         
         ALLOCATE(v)
         CALL v % initWithValue(msg)
         obj => v
         CALL userDictionary % addObjectForKey(obj,"message")
         CALL release(obj)
!
!        --------------------
!        Create the exception
!        --------------------
!
         ALLOCATE(exception)
         
         CALL exception % initFTException(FT_ERROR_FATAL, &
                              exceptionName   = MODEL_READ_EXCEPTION, &
                              infoDictionary  = userDictionary)
         obj => userDictionary
         CALL release(obj)
!
!        -------------------
!        Throw the exception
!        -------------------
!
         CALL throw(exception)
         obj => exception
         CALL release(obj)
         
      END SUBROUTINE ThrowModelReadException
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION chainWithID( self, chainID ) RESULT(chain)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                        :: chainID
         CLASS(SMModel)                 :: self
         CLASS(SMChainedCurve), POINTER :: chain
!
!        ---------------
!        Local Variables
!        ---------------
!         
         CLASS(FTObject)            , POINTER :: obj => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         
         chain => NULL()

         IF( ASSOCIATED(self % outerBoundary) )     THEN
            IF( chainID ==  self % outerBoundary % id() )     THEN
               chain => self % outerBoundary
               RETURN
            END IF
         END IF
         
         IF( ASSOCIATED(self % innerBoundaries) )     THEN
            iterator => self % innerBoundariesIterator
            CALL iterator % setToStart()
            DO WHILE( .NOT.iterator % isAtEnd() )
               obj => iterator % object()
               CALL castToSMChainedCurve(obj,chain)
               
               IF( chainID == chain % id()) RETURN
               
               CALL iterator % moveToNext()
            END DO 
         END IF
         
         IF( ASSOCIATED(self % interfaceBoundaries) )     THEN
            iterator => self % interfaceBoundariesIterator
            CALL iterator % setToStart()
            DO WHILE( .NOT.iterator % isAtEnd() )
               obj => iterator % object()
               CALL castToSMChainedCurve(obj,chain)
               
               IF( chainID == chain % id()) RETURN
               
               CALL iterator % moveToNext()
            END DO 
         END IF

      END FUNCTION chainWithID
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION curveInModelWithID( self, curveID, chain ) RESULT(curve)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                        :: curveID
         CLASS(SMModel)                 :: self
         CLASS(SMChainedCurve), POINTER :: chain
         CLASS(SMCurve)       , POINTER :: curve
!
!        ---------------
!        Local Variables
!        ---------------
!         
         INTEGER                              :: chainID
         CLASS(FTObject)            , POINTER :: obj => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         
         chain   => NULL()
         curve   => NULL()
         chainID = self % boundaryCurveMap(curveID)

         IF( ASSOCIATED(self % outerBoundary) )     THEN
            IF( chainID ==  self % outerBoundary % id() )     THEN
               chain => self % outerBoundary
               curve => chain % curveWithID(curveID)
               RETURN
            END IF
         END IF
         
         IF( ASSOCIATED(self % innerBoundaries) )     THEN
            iterator => self % innerBoundariesIterator
            CALL iterator % setToStart()
            DO WHILE( .NOT.iterator % isAtEnd() )
               obj => iterator % object()
               CALL castToSMChainedCurve(obj,chain)
               
               IF( chainID == chain % id()) THEN
                  curve => chain % curveWithID(curveID)
                  RETURN 
               END IF 
               
               CALL iterator % moveToNext()
            END DO 
         END IF
         
         IF( ASSOCIATED(self % interfaceBoundaries) )     THEN
            iterator => self % interfaceBoundariesIterator
            CALL iterator % setToStart()
            DO WHILE( .NOT.iterator % isAtEnd() )
               obj => iterator % object()
               CALL castToSMChainedCurve(obj,chain)
               
               IF( chainID == chain % id()) THEN
                  curve => chain % curveWithID(curveID)
                  RETURN 
               END IF 
               
               CALL iterator % moveToNext()
            END DO 
         END IF

      END FUNCTION curveInModelWithID

      END Module SMModelClass
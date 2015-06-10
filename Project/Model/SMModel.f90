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
!
!     ---------------------
!     Class type definition
!     ---------------------
!
      TYPE, EXTENDS(FTObject) ::  SMModel
         INTEGER                                  :: curveCount
         INTEGER                                  :: numberOfOuterCurves
         INTEGER                                  :: numberOfInnerCurves
         INTEGER                                  :: numberOfInterfaceCurves
         CHARACTER(LEN=32)                        :: modelName
         CLASS(SMChainedCurve)      , POINTER     :: outerBoundary => NULL()
         CLASS(FTLinkedList)        , POINTER     :: innerBoundaries => NULL()
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
         PROCEDURE :: initWithContentsOfFile         
         PROCEDURE :: destruct    => destructModel
         PROCEDURE :: chainWithID
         PROCEDURE :: curveWithID => curveInModelWithID
      END TYPE SMModel
      
      PRIVATE :: destroyIterator, destroyList
!
!     ========
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initWithContentsOfFile( self, fUnit )  
         IMPLICIT NONE
         CLASS(SMModel)  :: self
         INTEGER         :: fUnit
         
         CALL self % FTObject % init()
         
         self % outerBoundary               => NULL()
         self % innerBoundaries             => NULL()
         self % interfaceBoundaries         => NULL()
         self % innerBoundariesIterator     => NULL()
         self % interfaceBoundariesIterator => NULL()
         self % curveCount                  =  0
         self % numberOfOuterCurves         =  0
         self % numberOfInnerCurves         =  0
         self % numberOfInterfaceCurves     =  0
         
         CALL constructModelFromFile( self, fUnit )
         
      END SUBROUTINE initWithContentsOfFile
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructModel(self)  
         IMPLICIT NONE  
         CLASS(SMModel)   :: self
         
         CALL destroyIterator(self % innerBoundariesIterator)
         CALL destroyIterator(self % interfaceBoundariesIterator)
         CALL destroyList    (self % innerBoundaries)
         CALL destroyList    (self % interfaceBoundaries)
         
         IF(ASSOCIATED(self % outerBoundary))     THEN
            CALL self % outerBoundary % release()
            IF ( self % outerBoundary % isUnreferenced() )     THEN
               DEALLOCATE(self % outerBoundary)
               NULLIFY(self % outerBoundary) 
            END IF 
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
      SUBROUTINE destroyIterator(obj)  
         IMPLICIT NONE  
         CLASS(FTLinkedListIterator), POINTER :: obj
         
         IF ( ASSOCIATED(obj) )     THEN
            CALL obj % release()
            IF ( obj % isUnreferenced() )     THEN
               DEALLOCATE(obj)
               NULLIFY(obj) 
            END IF 
         END IF 
         
      END SUBROUTINE destroyIterator
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destroyList(obj)  
         IMPLICIT NONE  
         CLASS(FTLinkedList), POINTER :: obj
         
         IF ( ASSOCIATED(obj) )     THEN
            CALL obj % release()
            IF ( obj % isUnreferenced() )     THEN
               DEALLOCATE(obj)
               NULLIFY(obj) 
            END IF 
         END IF 
         
      END SUBROUTINE destroyList
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE constructModelFromFile( self, fUnit )  
         IMPLICIT NONE
!
!        -----------
!        Arguments  
!        -----------
!
         CLASS(SMModel)  :: self
         INTEGER         :: fUnit
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                        :: ios
         INTEGER, EXTERNAL              :: UnusedUnit
         CLASS(FTException)   , POINTER :: exception => NULL()
!
!        -----------------------------
!        Import outer boundary, if any
!        -----------------------------
!
         rewind( fUnit )
         CALL MoveToBlock( "\begin{OuterBoundary}" , fUnit, ios )
        
         IF( ios == 0 )     THEN
            ALLOCATE( self % outerBoundary )
            CALL self % outerBoundary % initChainWithNameAndID("Outer Boundary",1)
            CALL ReadOuterBoundaryBlock( self, fUnit )
            
            IF ( catch(MODEL_READ_EXCEPTION) )     THEN  ! Pass the error up the chain
               CALL self % outerBoundary % release()
               DEALLOCATE( self % outerBoundary )
               exception => errorObject()
               CALL throw(exception)
               RETURN
            END IF 
            
            self % numberOfOuterCurves = 1
            
         END IF
!
!        -------------------------------
!        Import inner boundaries, if any
!        -------------------------------
!
         rewind( fUnit )
         CALL MoveToBlock( "\begin{InnerBoundaries}" , fUnit, ios )
         
         IF( ios == 0 )     THEN
         
            ALLOCATE( self % innerBoundaries )
            CALL self % innerBoundaries % init()
            
            CALL ReadInnerBoundaryBlock( self, INNER_BOUNDARY_BLOCK, fUnit )
            
            IF ( catch(MODEL_READ_EXCEPTION) )     THEN  ! Pass the error up the chain
               CALL self % innerBoundaries % release()
               DEALLOCATE( self % innerBoundaries )
               exception => errorObject()
               CALL throw(exception)
               RETURN
            END IF 
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
         rewind( fUnit )
         CALL MoveToBlock( "\begin{InterfaceBoundaries}" , fUnit, ios )
         
         IF( ios == 0 )     THEN
         
            ALLOCATE( self % interfaceBoundaries )
            CALL self % interfaceBoundaries % init()
            
            CALL ReadInnerBoundaryBlock( self, INTERFACE_BOUNDARY_BLOCK, fUnit )
            
            IF ( catch(MODEL_READ_EXCEPTION) )     THEN  ! Pass the error up the chain
               CALL self % interfaceBoundaries % release()
               DEALLOCATE( self % interfaceBoundaries )
               exception => errorObject()
               CALL throw(exception)
               RETURN
            END IF 
         END IF
         IF ( ASSOCIATED(self % interfaceBoundaries) )     THEN
            ALLOCATE(self % interfaceBoundariesIterator)
            CALL  self % interfaceBoundariesIterator % initWithFTLinkedList(self % interfaceBoundaries)
         END IF 
!
!        ---------
!        Finish up
!        ---------
!
         CALL MakeCurveToChainConnections(self)
                 
      END SUBROUTINE constructModelFromFile
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ReadOuterBoundaryBlock( self, fUnit ) 
         IMPLICIT NONE  
!
!        -----------
!        Arguments  
!        -----------
!
         CLASS(SMModel)  :: self
         INTEGER         :: fUnit
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                                 :: ios
         INTEGER                                 :: cStart, cEnd
         CHARACTER(LEN=BLOCK_NAME_STRING_LENGTH) :: blockName
         CHARACTER(LEN=LINE_LENGTH)              :: inputLine = " "
         CLASS(FTException), POINTER             :: exception => NULL()
         
         DO WHILE( INDEX(inputLine,"\end{OuterBoundary}") == 0 )
         
            READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
            IF( ios /= 0 )     THEN
               CALL ThrowModelReadException("OuterBoundary","\end{OuterBoundary} not found")
               RETURN
            END IF
            
           IF( INDEX(inputLine, "\begin{") /= 0 )   THEN
           
              cStart    = INDEX(inputLine,"{")
              cEnd      = INDEX(inputLine,"}")
              blockName = inputLine(cStart+1:cEnd-1)

              CALL ImportCurve( self, fUnit, blockName, self % outerBoundary )
            
              IF ( catch(MODEL_READ_EXCEPTION) )     THEN  ! Pass the error up the chain
                 exception => errorObject()
                 CALL throw(exception)
                 RETURN
              END IF 
              
           END IF
           
         END DO
!
!        ------------------
!        Finalize the chain
!        ------------------
!
         CALL self % outerBoundary % complete(OUTER)
         
      END SUBROUTINE ReadOuterBoundaryBlock
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReadInnerBoundaryBlock( self, blockType, fUnit ) 
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMModel) :: self
         INTEGER        :: fUnit
         INTEGER        :: blockType
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMChainedCurve), POINTER          :: chain => NULL()
         CLASS(FTObject)      , POINTER          :: obj => NULL()
         INTEGER                                 :: ios
         INTEGER                                 :: cStart, cEnd
         CHARACTER(LEN=BLOCK_NAME_STRING_LENGTH) :: blockName, chainName
         CHARACTER(LEN=LINE_LENGTH)              :: inputLine = " "
         CHARACTER(LEN=LINE_LENGTH)              :: bBlockEndName
         
         IF ( blockType == INNER_BOUNDARY_BLOCK )     THEN
            bBlockEndName = "\end{InnerBoundaries}"
         ELSE
            bBlockEndName = "\end{InterfaceBoundaries}"
         END IF
!
!        -------------------------------
!        Do until end of bondaries block
!        -------------------------------
!
         DO WHILE( INDEX(inputLine,TRIM(bBlockEndName)) == 0 )        
!
!           --------------------------------------------------------
!           Read a line. If end of file is hit, something is wrong -
!           the block is not closed.
!           --------------------------------------------------------
!
            READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
            IF( ios /= 0 )     THEN
               IF ( blockType == INNER_BOUNDARY_BLOCK )     THEN
                  CALL ThrowModelReadException("Inner Boundaries","Inner Boundary block syntax error")
               ELSE
                  CALL ThrowModelReadException("Interface Boundaries","Interface Boundary block syntax error")
               END IF
               RETURN
            END IF
!
!           ---------------------------------------------------------
!           If the line is a \begin block then construct the chains
!           otherwise go back to the do-while and read the next line.
!           ---------------------------------------------------------
!
           IF( INDEX(inputLine, "\begin{") /= 0 )   THEN
              cStart = INDEX(inputLine,"{")
              cEnd   = INDEX(inputLine,"}")
              blockName = inputLine(cStart+1:cEnd-1)
!
!             -----------------------------------------------------
!             If the block is a chain, so read all of the curves in 
!             the chain.
!             -----------------------------------------------------
!
              IF( blockName == "Chain" )     THEN
                 READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
                 chainName = GetStringValue( inputLine )
                 ALLOCATE(chain)
                 CALL chain % initChainWithNameAndID(chainName,0)
!
!                ---------------------------------
!                Read all the curves in this chain
!                ---------------------------------
!
                 DO WHILE( INDEX(inputLine,"\end{Chain}") == 0 )
                     READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
                     IF( ios /= 0 )     THEN
                        CALL ThrowModelReadException(chainName,"Chain block syntax error")
                        RETURN
                     END IF
                     
                     IF( INDEX(inputLine, "\begin{") /= 0 )   THEN
                        cStart    = INDEX(inputLine,"{")
                        cEnd      = INDEX(inputLine,"}")
                        blockName = inputLine(cStart+1:cEnd-1)
                        CALL ImportCurve( self, fUnit, blockName, chain )
                     END IF
                     
                 END DO ! Read chain
!
!                --------------------------------------------
!                All the curves in this chain have been read. 
!                --------------------------------------------
!
               IF ( blockType == INNER_BOUNDARY_BLOCK )     THEN
                 obj => chain
                 CALL self % innerBoundaries % add(obj)
               ELSE
                 obj => chain
                 CALL self % interfaceBoundaries % add(obj)
               END IF
                 
              ELSE
!
!                -----------------------------------------------------
!                The user didn't specify a chain, so do it for him/her
!                -----------------------------------------------------
!
                 ALLOCATE(chain)
                 CALL chain % initChainWithNameAndID("Untitled",0)

                 CALL ImportCurve( self, fUnit, blockName, chain )
                 
                 IF ( blockType == INNER_BOUNDARY_BLOCK )     THEN
                    obj => chain
                    CALL self % innerBoundaries % add(obj)
                 ELSE
                    obj => chain
                    CALL self % interfaceBoundaries % add(obj)
                 END IF
              END IF
!
!             ------------------
!             Finalize the chain
!             ------------------
!
              CALL chain % complete(INNER)
              CALL chain % release()
!
!             --------------------------------------------------------
!             The chain has been created, clean up and then start over
!             --------------------------------------------------------
!
               IF ( blockType == INNER_BOUNDARY_BLOCK )     THEN
                  self % numberOfInnerCurves     = self % numberOfInnerCurves + 1
               ELSE
                  self % numberOfInterfaceCurves = self % numberOfInterfaceCurves + 1
               END IF
           END IF
           
         END DO !Read inner boundaries
         
      END SUBROUTINE ReadInnerBoundaryBlock
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ImportCurve( self, fUnit, equationType, chain )
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMModel)                 :: self
         INTEGER                        :: fUnit
         CLASS(SMChainedCurve), POINTER :: chain
         CHARACTER(LEN=*)               :: equationType
         CLASS(FTException)   , POINTER :: exception => NULL()

         SELECT CASE ( equationType )
            CASE("ParametricEquationCurve")
            
               CALL ImportParametricEquationBlock( self, fUnit, chain )
         
               IF ( catch(EQUATION_FORMAT_EXCEPTION) )     THEN  ! Pass the error up the chain
                  exception => errorObject()
                  CALL throw(exception)
                  CALL ThrowModelReadException(equationType,"Error constructing parametric equation")
                  RETURN
               END IF 
               
            CASE ("SplineCurve" )
            
               CALL ImportSplineBlock( self, fUnit, chain )
               
            CASE ("EndPointsCurve" )
            
               CALL ImportLineEquationBlock( self, fUnit, chain )
               
            CASE DEFAULT
               CALL ThrowModelReadException(equationType,"Unknown equation type")
               RETURN
         END SELECT
         
         self % curveCount = self % curveCount + 1
         
      END SUBROUTINE ImportCurve
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ImportParametricEquationBlock( self, fUnit, chain ) 
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMModel)                 :: self
         INTEGER                        :: fUnit
         CLASS(SMChainedCurve), POINTER :: chain
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=LINE_LENGTH)                :: inputLine = " "
         CHARACTER(LEN=SM_CURVE_NAME_LENGTH)       :: curveName
         CHARACTER(LEN=EQUATION_STRING_LENGTH)     :: eqnX, eqnY, eqnZ
         INTEGER                                   :: ios
         CLASS(SMParametricEquationCurve), POINTER :: cCurve => NULL()
         CLASS(FTException)              , POINTER :: exception => NULL()
         CLASS(SMCurve)                  , POINTER :: curvePtr => NULL()
!
!        ------------
!        Get the data
!        ------------
!
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         curveName = GetStringValue(inputLine)
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         eqnX = GetStringValue(inputLine)
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         eqnY = GetStringValue(inputLine)
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         eqnZ = GetStringValue(inputLine)
!
!        ----------------
!        Create the curve
!        ----------------
!
         ALLOCATE(cCurve)
         CALL cCurve % initWithEquationsNameAndID(eqnX, eqnY, eqnZ, curveName, self % curveCount + 1)

         IF ( catch(EQUATION_FORMAT_EXCEPTION) )     THEN  ! Pass the error up the chain
            CALL cCurve % release()
            DEALLOCATE(cCurve)
            exception => errorObject()
            CALL throw(exception)
            CALL ThrowModelReadException(curveName,"Equation format error")
            RETURN
         END IF
         
         curvePtr => cCurve
         CALL chain  % addCurve(curvePtr)
         CALL cCurve % release()
!
!        ----------------------------
!        Make sure the block is ended
!        ----------------------------
!
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         IF( INDEX(inputLine,"\end{ParametricEquationCurve}") == 0 )     THEN
            CALL ThrowModelReadException(chain % curveName(),"\end{ParametricEquationCurve} not found")
         END IF
         
      END SUBROUTINE ImportParametricEquationBlock
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ImportSplineBlock( self, fUnit, chain ) 
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMModel)                 :: self
         INTEGER                        :: fUnit
         CLASS(SMChainedCurve), POINTER :: chain
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=LINE_LENGTH)                :: inputLine = " "
         CHARACTER(LEN=SM_CURVE_NAME_LENGTH)       :: curveName
         INTEGER                                   :: ios
         CLASS(SMSplineCurve)         , POINTER    :: cCurve => NULL()
         CLASS(SMCurve)               , POINTER    :: curvePtr => NULL()
         REAL(KIND=RP) , DIMENSION(:), ALLOCATABLE :: t, x, y, z
         INTEGER                                   :: numKnots, j
         
         INTEGER, EXTERNAL :: GetIntValue
!
!        ------------
!        Get the data
!        ------------
!
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         curveName = GetStringValue(inputLine)
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         numKnots = GetIntValue(inputLine)
         
         ALLOCATE( x(numKnots), y(numKnots), z(numKnots), t(numKnots) )
         DO j = 1, numKnots 
            READ( fUnit, * ) t(j), x(j), y(j), z(j)
         END DO
!
!        ----------------
!        Create the curve
!        ----------------
!
         ALLOCATE(cCurve)
         CALL cCurve % initWithPointsNameAndID(t, x, y, z, curveName, self % curveCount + 1 )
         !Spline curves have no exceptions thrown
         
         curvePtr => cCurve
         CALL chain  % addCurve(curvePtr)
         CALL cCurve % release()
!
!        ----------------------------
!        Make sure the block is ended
!        ----------------------------
!
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         IF( INDEX(inputLine,"\end{SplineCurve}") == 0 )     THEN
            CALL ThrowModelReadException(chain % curveName(),"\end{SplineCurve} not found")
         END IF
         
      END SUBROUTINE ImportSplineBlock
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ImportLineEquationBlock( self, fUnit, chain ) 
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMModel)                 :: self
         INTEGER                        :: fUnit
         CLASS(SMChainedCurve), POINTER :: chain
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=LINE_LENGTH)            :: inputLine = " "
         CHARACTER(LEN=SM_CURVE_NAME_LENGTH)   :: curveName
         REAL(KIND=RP), DIMENSION(3)           :: xStart, xEnd
         INTEGER                               :: ios
         CLASS(SMLine)          , POINTER      :: cCurve => NULL()
         CLASS(SMCurve)         , POINTER      :: curvePtr => NULL()
         
         INTERFACE
            FUNCTION GetRealArray( inputLine ) RESULT(x)
               USE SMConstants
               IMPLICIT NONE
               REAL(KIND=RP), DIMENSION(3) :: x
               CHARACTER ( LEN = * ) :: inputLine
            END FUNCTION GetRealArray
         END INTERFACE
!
!        ------------
!        Get the data
!        ------------
!
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         curveName = GetStringValue(inputLine)
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         xStart = GetRealArray(inputLine)
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         xEnd = GetRealArray(inputLine)
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
         CALL cCurve % release()
!
!        ----------------------------
!        Make sure the block is ended
!        ----------------------------
!
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         IF( INDEX(inputLine,"\end{EndPointsCurve}") == 0 )     THEN
            CALL ThrowModelReadException(chain % curveName(),"\end{EndPointsCurve} not found")
         END IF
         
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
           CALL iterator % release()
           DEALLOCATE(iterator)
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
           CALL iterator % release()
           DEALLOCATE(iterator)
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
         CLASS(FTException)   , POINTER :: exception => NULL()
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
         CALL v % release()
         
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
                              exceptionName   = MODEL_READ_EXCEPTION, &
                              infoDictionary  = userDictionary)
         CALL userDictionary % release()
!
!        -------------------
!        Throw the exception
!        -------------------
!
         CALL throw(exception)
         CALL exception % release()
         
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

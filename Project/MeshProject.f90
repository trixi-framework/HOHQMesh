!
!////////////////////////////////////////////////////////////////////////
!
!      MeshProject.f90
!      Created: August 19, 2013 11:19 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   Module MeshProjectClass
      USE SMConstants
      USE SMModelClass
      USE MeshSizerClass
      USE SMMeshClass
      USE SharedExceptionManagerModule
      USE QuadTreeGridClass
      USE MeshSmootherClass
      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      CHARACTER(LEN=18) :: PROJECT_READ_EXCEPTION = "Project read error"
!
!     ------------------
!     Private data types
!     ------------------
!
      TYPE RunParameters
         CHARACTER(LEN=LINE_LENGTH) :: MeshFileName
         CHARACTER(LEN=LINE_LENGTH) :: plotFileName
         CHARACTER(LEN=LINE_LENGTH) :: statsFileName
         INTEGER                    :: meshFileFormat
         INTEGER                    :: polynomialOrder
      END TYPE RunParameters
      PRIVATE :: RunParameters
      
      TYPE MeshParameters
         INTEGER       :: meshType
         REAL(KIND=RP) :: backgroundGridSize(3)
      END TYPE MeshParameters
      PRIVATE :: MeshParameters
      
      TYPE BackgroundGridParameters
         INTEGER       :: N(3)
         REAL(KIND=RP) :: dx(3)
         REAL(KIND=RP) :: x0(3)
      END TYPE backgroundGridParameters
      
      TYPE CentersParameters
         REAL(KIND=RP) :: x0(3)
         REAL(KIND=RP) :: centerMeshSize
         REAL(KIND=RP) :: centerExtent
         INTEGER       :: centerType
      END TYPE CentersParameters
      PRIVATE :: CentersParameters
      
      TYPE lineParameters
         REAL(KIND=RP) :: x0(3), x1(3)
         REAL(KIND=RP) :: lineMeshSize
         REAL(KIND=RP) :: lineExtent
         INTEGER       :: lineControlType
      END TYPE lineParameters
      PRIVATE :: lineParameters
!
!     ------------------------
!     Project class definition
!     ------------------------
!
      TYPE, EXTENDS(FTObject) ::  MeshProject
         CLASS(SMModel)           , POINTER :: model    => NULL()
         CLASS(SMMesh)            , POINTER :: mesh     => NULL()
         CLASS(MeshSizer)         , POINTER :: sizer    => NULL()
         CLASS(QuadTreeGrid)      , POINTER :: grid     => NULL()
         CLASS(MeshSmoother)      , POINTER :: smoother => NULL()
         TYPE(RunParameters)                :: runParams
         TYPE(MeshParameters)               :: meshParams
         TYPE(BackgroundGridParameters)     :: backgroundParams
         CHARACTER(LEN=32)                  :: backgroundMaterialName
!         
!        ========         
         CONTAINS
!        ========         
!
         PROCEDURE :: initWithContentsOfFileUnit         
      END TYPE MeshProject
!
!     ========
      CONTAINS
!     ========
!
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initWithContentsOfFileUnit(self, fUnit )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER            :: fUnit
         CLASS(MeshProject) :: self
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                     :: iOS
         CHARACTER(LEN=LINE_LENGTH)  :: inputLine = " ", msg
         CLASS(FTException), POINTER :: exception => NULL()
!
!        ---------------------------
!        Call superclass initializer
!        ---------------------------
!
         CALL self % FTObject % init()
!
!        ------------------
!        Get run parameters
!        ------------------
!
         REWIND(fUnit)
         CALL MoveToBlock("\begin{RunParameters}", fUnit, iOS )
         IF( ios /= 0 )     THEN
            CALL ThrowProjectReadException(FT_ERROR_FATAL, "initWithContentsOfFileUnit",&
                                           "Run parameters block not found in control file")
            RETURN
         END IF
         CALL ReadRunParametersBlock( fUnit, self % runParams )
!
!        -----------------------------------------------------------------
!        If this is a multiple material mesh, as given by the ism-mm flag,
!        read the background material name.
!        -----------------------------------------------------------------
!
         IF ( self % runParams % meshFileFormat == ISM_MM )     THEN
            REWIND(fUnit)
            CALL MoveToBlock("\begin{Materials}", fUnit, iOS )
            IF( ios /= 0 )     THEN
               msg = "Background material block not found in control file. Using default name = 'base'"
               CALL ThrowProjectReadException(FT_ERROR_WARNING, "initWithContentsOfFileUnit",msg)
               self % backgroundMaterialName = "base"
            ELSE
               READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
               self % backgroundMaterialName = GetStringValue( inputLine )
            END IF
         END IF
!
!        --------------------------
!        Read mesh parameters block
!        --------------------------
!
         REWIND(fUnit)
         CALL MoveToBlock("\begin{MeshParameters}", fUnit, iOS )
         IF( ios /= 0 )     THEN
            msg = "Mesh parameters block not found in control file"
            CALL ThrowProjectReadException(FT_ERROR_FATAL, "initWithContentsOfFileUnit",msg)
            RETURN
         END IF
         CALL ReadMeshParametersBlock( fUnit, self % meshParams )
!
!        -------------------
!        Read the model file
!        -------------------
!
         ALLOCATE(self % model)
         CALL self % model % initWithContentsOfFile( fUnit )

         IF ( catch(MODEL_READ_EXCEPTION) )     THEN  ! Pass the error up the chain
            exception => errorObject()
            CALL throw(exception)
            RETURN
         END IF 
!
!        -----------------
!        Build the project
!        -----------------
!
         CALL BuildProjectWithContentsOfFile( self, fUnit )

      END SUBROUTINE initWithContentsOfFileUnit
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE DestructMeshProject(self)  
         IMPLICIT NONE  
         CLASS(MeshProject) :: self
         
         IF ( ASSOCIATED(self % model) )     THEN
            CALL self % model % release()
            IF ( self % model % isUnreferenced() )     THEN
               DEALLOCATE(self % model) 
            END IF
         END IF 
         
         IF ( ASSOCIATED(self % mesh) )     THEN
            CALL self % mesh % release()
            IF ( self % mesh % isUnreferenced() )     THEN
               DEALLOCATE(self % mesh) 
            END IF
         END IF 
         
         IF ( ASSOCIATED(self % sizer) )     THEN
            CALL self % sizer % release()
            IF ( self % sizer % isUnreferenced() )     THEN
               DEALLOCATE(self % sizer) 
            END IF
         END IF 
         
         IF ( ASSOCIATED(self % grid) )     THEN
            CALL self % grid % release()
            IF ( self % grid % isUnreferenced() )     THEN
               DEALLOCATE(self % grid) 
            END IF
         END IF 
         
         IF ( ASSOCIATED(self % smoother) )     THEN
            CALL self % smoother % destruct()
            DEALLOCATE(self % smoother) 
         END IF 
         
      END SUBROUTINE DestructMeshProject
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE BuildProjectWithContentsOfFile( self, fUnit )
         USE ChainedSegmentedCurveClass
         USE SizerControls
         USE SMChainedCurveClass
         USE CurveConversionsModule
         USE SpringMeshSmootherClass
         USE LaplaceMeshSmootherClass
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER           :: fUnit
         TYPE(MeshProject) :: self
!
!        ---------------
!        Local Variables
!        ---------------
!
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedOuterBoundary => NULL()
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedInnerBoundary => NULL()
         CLASS(SMChainedCurve)       , POINTER :: chain => NULL()
         CLASS(FTException)          , POINTER :: exception => NULL()
         CLASS(FTLinkedListIterator) , POINTER :: iterator => NULL()
         CLASS(FTObject)             , POINTER :: obj => NULL()
         CLASS(QuadTreeGrid)         , POINTER :: parent => NULL()
         CLASS(SpringMeshSmoother)   , POINTER :: springSmoother => NULL()
         CLASS(LaplaceMeshSmoother)  , POINTER :: laplaceSmoother => NULL()
         
         CLASS(SizerCentercontrol), POINTER :: c => NULL()
         CLASS(SizerLineControl)  , POINTER :: L => NULL()
         
         INTEGER                        :: iOS
         TYPE(BackgroundGridParameters) :: backgroundGrid
         TYPE(CentersParameters)        :: centerParams
         TYPE(SpringSmootherParameters) :: smootherParams
         TYPE(LaplaceSmootherParameters):: laplaceParameters
         TYPE(LineParameters)           :: lineParams
         
         REAL(KIND=RP)                  :: h, xMax(3)
         INTEGER                        :: curveID
         LOGICAL                        :: smootherWasRead = .FALSE.
         
         NULLIFY( self % grid )
         NULLIFY( self % sizer )
!
!-------------------------------------------------------------------------------
!             Construct the Background Grid and initialize Sizer
!-------------------------------------------------------------------------------
!
         REWIND(fUnit)
         CALL MoveToBlock("\begin{BackgroundGrid}", fUnit, iOS )
!
!        -----------------------------------------------------------
!        The backgound grid size can eigher be specified or inferred
!        from the model depending on if the background grid block is
!        present or not.
!        -----------------------------------------------------------
!
         IF( ios == 0 )     THEN
         
            CALL ReadbackgroundGridBlock( backgroundGrid, fUnit )
            self % meshParams % backgroundGridSize = 2*backgroundGrid % dx
            self % backgroundParams                = backgroundGrid
            
            xMax = backgroundGrid % x0 + backgroundGrid % N*backgroundGrid % dx
            
            ALLOCATE(self % sizer)
            CALL self % sizer % initWithProperties(backgroundGrid % dx, backgroundGrid % x0, xMax)
         ELSE
         
            CALL BuildbackgroundGridFromModel( backgroundGrid, self % model, self % meshParams % backgroundGridSize )
         
            IF ( catch(PROJECT_READ_EXCEPTION) )     THEN  ! Pass the error up the chain
               exception => errorObject()
               CALL throw(exception)
               RETURN
            END IF 
            
            xMax = backgroundGrid % x0 + backgroundGrid % N*backgroundGrid % dx
            ALLOCATE(self % sizer)
            CALL self % sizer % initWithProperties( self % meshParams % backgroundGridSize, backgroundGrid % x0, xMax )
         END IF
         
         NULLIFY(parent)
         ALLOCATE(self % grid)
         CALL self % grid % initGridWithParameters( backgroundGrid % dx, backgroundGrid % x0, backgroundGrid % N,&
                                   parent, (/0,0,0/), 0)
!
!-------------------------------------------------------------------------------
!                               Construct Sizer
!-------------------------------------------------------------------------------
!
!
!        -------------------------------------------
!        Discretize boundary curves and add to sizer
!        -------------------------------------------
!
         curveID = 0
         h       = MINVAL(self % meshParams % backgroundGridSize(1:2))
         IF( ASSOCIATED( self % model % outerBoundary ) )     THEN
            curveID                =  curveID + 1
            segmentedOuterBoundary => allocAndInitSegmentedChainFromChain( self % model % outerBoundary, h, curveID )
            CALL self % sizer % addBoundaryCurve(segmentedOuterBoundary,OUTER)
            CALL segmentedOuterBoundary % release()
         END IF
!
!        --------------------------------------
!        Step through each inner boundary curve
!        and construct chains.
!        --------------------------------------
!
         IF( ASSOCIATED( self % model % innerBoundaries ) )     THEN
            iterator => self % model % innerBoundariesIterator
            CALL iterator % setToStart
            DO WHILE (.NOT.iterator % isAtEnd())
               curveID =  curveID + 1
               obj     => iterator % object()
               CALL castToSMChainedCurve(obj,chain)
               segmentedInnerBoundary => allocAndInitSegmentedChainFromChain( chain, h, curveID )
                
               CALL self % sizer % addBoundaryCurve(segmentedInnerBoundary,INNER)
               CALL segmentedInnerBoundary % release()
                
               CALL iterator % moveToNext()           
            END DO
         END IF
!
!        ------------------------------------------
!        Step through each interface boundary curve
!        and construct chains.
!        ------------------------------------------
!
         IF( ASSOCIATED( self % model % interfaceBoundaries ) )     THEN
            iterator => self % model % interfaceBoundariesIterator
            CALL iterator % setToStart
             DO WHILE (.NOT.iterator % isAtEnd())
                curveID =  curveID + 1
                obj     => iterator % object()
                CALL castToSMChainedCurve(obj,chain)
                segmentedInnerBoundary => allocAndInitSegmentedChainFromChain( chain, h, curveID )
                
                CALL self % sizer % addBoundaryCurve(segmentedInnerBoundary,INTERIOR_INTERFACE)
                CALL segmentedInnerBoundary % release()
                
                CALL iterator % moveToNext()           
             END DO  
         END IF
!
!        -------------------------------------------------------
!        Make sure that there are enough elements between curves
!        -------------------------------------------------------
!
         IF ( ASSOCIATED( self % model % interfaceBoundaries ) .OR. &
              ASSOCIATED( self % model % innerBoundaries ) )     THEN
            CALL ComputeCurveDistanceScales( self % sizer )
         END IF
         
         IF ( ASSOCIATED( self % model % interfaceBoundaries ) ) THEN
            CALL ComputeInterfaceCurveScales( self % sizer )
         END IF
!
!        ------------------------------------------------------
!        Construct any refinement centers as instructed by the 
!        control file
!        ------------------------------------------------------
!
         REWIND(fUnit)
         ios = 0
         DO WHILE (iOS == 0 )
            CALL MoveToBlock("\begin{RefinementCenter}", fUnit, iOS )
            IF( iOS /= 0 )   EXIT
            
            CALL ReadCenterMeshSizerBlock( centerParams, fUnit )
            
            ALLOCATE(c)
            CALL c % initWithProperties( centerParams % x0, centerParams % centerExtent, &
                                       centerParams % centerMeshSize, centerParams % centerType )
            CALL self % sizer % addSizerCenterControl(c)
            CALL c % release()
         END DO
!
!        --------------------------------
!        Do the same with RefinementLines
!        --------------------------------
!
         REWIND(fUnit)
         ios = 0
         DO WHILE (iOS == 0 )
            CALL MoveToBlock("\begin{RefinementLine}", fUnit, iOS )
            IF( iOS /= 0 )   EXIT
            
            CALL ReadLineMeshSizerBlock( lineParams, fUnit )
            
            ALLOCATE(L)
            CALL L    % initWithProperties( lineParams % x0, lineParams % x1, lineParams % lineExtent, &
                                            lineParams % lineMeshSize, lineParams % lineControlType )
            CALL self % sizer % addSizerLineControl(L)
            CALL L    % release()
         END DO
!
!-------------------------------------------------------------------------------
!                          Construct Smoother, if requested
!-------------------------------------------------------------------------------
!
         smootherWasRead = .FALSE.
         NULLIFY(self % smoother)
!
!        --------------------
!        Spring-Mass smoother
!        --------------------
!
         rewind(fUnit)
         CALL MoveToBlock("\begin{SpringSmoother}", fUnit, iOS )
         IF( ios == 0 )     THEN
            CALL ReadSpringSmootherBlock( fUnit, smootherParams )
            IF( smootherParams % smoothingOn )     THEN
               ALLOCATE(springSmoother)
               CALL springSmoother % init(  smootherParams % springConstant, &
                                            smootherParams % mass, &
                                            smootherParams % restLength, &
                                            smootherParams % dampingCoefficient, &
                                            smootherParams % springType, &
                                            smootherParams % deltaT, &
                                            smootherParams % numSteps )
               self % smoother => springSmoother
               smootherWasRead = .TRUE.
            END IF
         END IF
         IF(smootherWasRead) RETURN 
!
!        ------------------
!        Laplacian Smoother
!        ------------------
!
         rewind(fUnit)
         CALL MoveToBlock("\begin{LaplaceSmoother}", fUnit, iOS )
         IF ( ios == 0 )     THEN
            CALL ReadLaplaceSmootherBlock( fUnit, laplaceParameters )
            IF ( laplaceParameters % smoothingOn )     THEN
               ALLOCATE(laplaceSmoother)
               CALL laplaceSmoother % init(laplaceParameters)
               self % smoother => laplaceSmoother
               smootherWasRead = .TRUE.
            END IF 
         END IF 
         IF(smootherWasRead) RETURN 
         
      END SUBROUTINE BuildProjectWithContentsOfFile
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE BuildbackgroundGridFromModel( backgroundGrid, model, backgroundGridSize )
!
!     -----------------------------------------------------------------------
!     This routine will take the background grid size and the model to 
!     generate a background mesh that bounds the outer boundary of the model.
!     -----------------------------------------------------------------------
!
         USE CurveConversionsModule
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(BackgroundGridParameters) :: backgroundGrid
         CLASS(SMModel)                 :: model
         REAL(KIND=RP)                  :: backgroundGridSize(3)
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedOuterBoundary => NULL()
         INTEGER                               :: curveID
         CHARACTER(LEN=128)                    :: msg
         
         INTEGER       :: nX, nY
         REAL(KIND=RP) :: heightB, widthB
         REAL(KIND=RP) :: leftB, rightB, topB, bottomB, h
         
         leftB   = HUGE(leftB)
         rightB  = -leftB
         topB    =  rightB
         bottomB =  leftB
!
!        ----------------------------------------------------
!        Create a discrete curve to determine the extents of 
!        the background mesh
!        ----------------------------------------------------
!
         h = MAXVAL( backgroundGridSize(1:2) )
         IF( ASSOCIATED( model % outerBoundary ) )     THEN
            curveID = 1
            segmentedOuterBoundary => allocAndInitSegmentedChainFromChain( model % outerBoundary, h, curveID )
!
!           ----------------------------------
!           Find the bounds on the outer curve
!           ----------------------------------
!
            leftB   = segmentedOuterBoundary % boundingBox(BBOX_LEFT)
            rightB  = segmentedOuterBoundary % boundingBox(BBOX_RIGHT)
            topB    = segmentedOuterBoundary % boundingBox(BBOX_TOP)
            bottomB = segmentedOuterBoundary % boundingBox(BBOX_BOTTOM)
         ELSE
            msg = "To automatically create background grid, the model needs an outer boundary curve"
            CALL ThrowProjectReadException( FT_ERROR_FATAL, "BuildbackgroundGridFromModel", msg )
            RETURN
         END IF
!
!        ---------------------------
!        Compute the background grid
!        ---------------------------
!
         widthB    = rightB  - leftB
         heightB   = topB    - bottomB
         leftB     = leftB   - backgroundGridSize(1)
         bottomB   = bottomB - backgroundGridSize(2)
         nX        = INT(widthB/backgroundGridSize(1)) + 2
         nY        = INT(heightB/backgroundGridSize(2)) + 2
         
         backgroundGrid % N  = (/nX, nY, 0/)
         backgroundGrid % dx = backgroundGridSize
         backgroundGrid % x0 = (/leftB, bottomB, 0.0_RP /)
         
      END SUBROUTINE BuildbackgroundGridFromModel
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ThrowProjectReadException( level, objectName, msg )  
         USE FTValueClass
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=*)  :: msg
         CHARACTER(LEN=*)  :: objectName
         INTEGER           :: level
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
!        and the object being read by the name "objectName"
!        -----------------------------------------------------
!
         ALLOCATE(userDictionary)
         CALL userDictionary  %  initWithSize(4)
         
         ALLOCATE(v)
         CALL v  %  initWithValue(objectName)
         obj => v
         CALL userDictionary  %  addObjectForKey(obj,"objectName")
         CALL v  %  release()
         
         ALLOCATE(v)
         CALL v  %  initWithValue(msg)
         obj => v
         CALL userDictionary  %  addObjectForKey(obj,"message")
         CALL v  %  release()
!
!        --------------------
!        Create the exception
!        --------------------
!
         ALLOCATE(exception)
         
         CALL exception  %  initFTException(level, &
                              exceptionName   = PROJECT_READ_EXCEPTION, &
                              infoDictionary  = userDictionary)
         CALL userDictionary  %  release()
!
!        -------------------
!        Throw the exception
!        -------------------
!
         CALL throw(exception)
         CALL exception  %  release()
         
      END SUBROUTINE ThrowProjectReadException
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReadRunParametersBlock( fUnit, params ) 
!
!        Example block is:
!
!         \begin{RunParameters}
!            model file name = "model.gm"
!            mesh file name = "fname.mesh"
!            stats file name = "fname.txt" (Optional)
!            mesh file format = "Basic", ...
!            tecplot file name = "tName.tec"  (Optional)
!         \end{RunParameters}
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER             :: fUnit
         TYPE(RunParameters) :: params
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                    :: ios
         CHARACTER(LEN=32)          :: fileFormat
         CHARACTER(LEN=LINE_LENGTH) :: inputLine = " "
         INTEGER, EXTERNAL          :: GetIntValue
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         params % meshFileName = GetStringValue( inputLine )
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         params % plotFileName = GetStringValue( inputLine )
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         params % statsFileName = GetStringValue( inputLine )
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         fileFormat = GetStringValue( inputLine )
         
         IF( fileFormat == "Basic" )     THEN
            params % meshFileFormat = BASIC_MESH_FORMAT
         ELSE IF ( fileFormat == "BasicWithEdges" )     THEN
            params % meshFileFormat = BASIC_PLUS_EDGES_FORMAT
         ELSE IF( fileFormat == "ISM" .OR. fileFormat == "ISM-V1" )     THEN
            params % meshFileFormat = ISM
         ELSE IF( fileFormat == "ISM-v2" .OR. fileFormat == "ISM-V2" )     THEN
            params % meshFileFormat = ISM2
         ELSE IF( fileFormat == "ISM-MM" )     THEN
            params % meshFileFormat = ISM_MM
         ELSE
            CALL ThrowProjectReadException(FT_ERROR_FATAL, "ReadRunParametersBlock",&
                                           "Unknown mesh file format")
         END IF
   
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         params % polynomialOrder = GetIntValue( inputLine )

      END SUBROUTINE ReadRunParametersBlock
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReadMeshParametersBlock( fUnit, params ) 
!
!        Example block is:
!
!         \begin{MeshParameters}
!            mesh type = "conforming"
!            background grid size = 1.0
!         \end{MeshParameters}
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER              :: fUnit
         TYPE(MeshParameters) :: params
!
!        ---------------
!        Local variables
!        ---------------
!
         
         INTERFACE
            FUNCTION GetRealArray( inputLine ) RESULT(x)
               USE SMConstants
               IMPLICIT NONE
               REAL(KIND=RP), DIMENSION(3) :: x
               CHARACTER ( LEN = * ) :: inputLine
            END FUNCTION GetRealArray
         END INTERFACE
         
         INTEGER                    :: ios
         CHARACTER(LEN=32)          :: typeName
         CHARACTER(LEN=LINE_LENGTH) :: inputLine = " "
         REAL(KIND=RP), EXTERNAL    :: GetRealValue
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         typeName = GetStringValue( inputLine )
         
         IF( typeName == "conforming" )     THEN
            params % meshType = 0! CONFORMING
         ELSE
            params % meshType = 1 !NON_CONFORMING
         END IF
   
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         params % backgroundGridSize = GetRealArray( inputLine )

      END SUBROUTINE ReadMeshParametersBlock
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReadBackgroundGridBlock( backgroundGrid, fUnit )
!
!        Example block is:
!
!         \begin{BackgroundMesh}
!            x0 = [-15.0, -15.0, 0.0]
!            dx = [1.0, 1.0, 0.0]
!            N  = [10,10, 0]
!         \end{BackgroundMesh}
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                        :: fUnit
         TYPE(BackgroundGridParameters) :: backgroundGrid
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                    :: ios
         CHARACTER(LEN=LINE_LENGTH) :: inputLine = " "
         
         INTERFACE
            FUNCTION GetRealArray( inputLine ) RESULT(x)
               USE SMConstants
               IMPLICIT NONE
               REAL(KIND=RP), DIMENSION(3) :: x
               CHARACTER ( LEN = * ) :: inputLine
            END FUNCTION GetRealArray
            FUNCTION GetIntArray( inputLine ) RESULT(x)
               IMPLICIT NONE
               INTEGER, DIMENSION(3) :: x
               CHARACTER ( LEN = * ) :: inputLine
            END FUNCTION GetIntArray
         END INTERFACE
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         backgroundGrid % x0 = GetRealArray( inputLine )
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         backgroundGrid % dx = GetRealArray( inputLine )
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         backgroundGrid % N = GetIntArray( inputLine )
         
      END SUBROUTINE ReadBackgroundGridBlock
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReadCenterMeshSizerBlock( centerParams, fUnit ) 
!
!        Example block is:
!
!         \begin{RefinementCenter}
!            type = "smooth"
!            x0 = [0.0,0.0, 0.0]
!            h  = 0.1
!            w  = 0.5
!         \end{RefinementCenter}
!
      IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                 :: fUnit
         TYPE(CentersParameters) :: centerParams
         REAL(KIND=RP), EXTERNAL :: GetRealValue
         INTERFACE
            FUNCTION GetRealArray( inputLine ) RESULT(x)
               USE SMConstants
               IMPLICIT NONE
               REAL(KIND=RP), DIMENSION(3) :: x
               CHARACTER ( LEN = * ) :: inputLine
            END FUNCTION GetRealArray
         END INTERFACE
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                    :: ios
         CHARACTER(LEN=LINE_LENGTH) :: inputLine = " "
         CHARACTER(LEN=32)          :: str
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         str = GetStringValue( inputLine )
         
         IF( str == "smooth" )     THEN
            centerParams % centerType = CENTER_SMOOTH
         ELSE
            centerParams % centerType = CENTER_SHARP
         END IF
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         centerParams % x0 = GetRealArray( inputLine )
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         centerParams % centerMeshSize = GetRealValue( inputLine )
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         centerParams % centerExtent = GetRealValue( inputLine )
      END SUBROUTINE ReadCenterMeshSizerBlock
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReadLineMeshSizerBlock( lineParams, fUnit ) 
!
!        Example block is:
!
!         \begin{Refinementline}
!            type = "smooth"
!            x0 = [0.0,0.0, 0.0]
!            x1 = [1.0,1.0, 0.0]
!            h  = 0.1
!            w  = 0.5
!         \end{Refinementline}
!
      IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                 :: fUnit
         TYPE(lineParameters)    :: lineParams
         REAL(KIND=RP), EXTERNAL :: GetRealValue
         INTERFACE
            FUNCTION GetRealArray( inputLine ) RESULT(x)
               USE SMConstants
               IMPLICIT NONE
               REAL(KIND=RP), DIMENSION(3) :: x
               CHARACTER ( LEN = * ) :: inputLine
            END FUNCTION GetRealArray
         END INTERFACE
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                    :: ios
         CHARACTER(LEN=LINE_LENGTH) :: inputLine = " "
         CHARACTER(LEN=32)          :: str
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         str = GetStringValue( inputLine )
         
         IF( str == "smooth" )     THEN
            lineParams % lineControlType = CENTER_SMOOTH
         ELSE
            lineParams % lineControlType = CENTER_SHARP
         END IF
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         lineParams % x0 = GetRealArray( inputLine )
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         lineParams % x1 = GetRealArray( inputLine )
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         lineParams % lineMeshSize = GetRealValue( inputLine )
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         lineParams % lineExtent = GetRealValue( inputLine )
         
      END SUBROUTINE ReadLineMeshSizerBlock
      

   END MODULE MeshProjectClass



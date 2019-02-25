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
      USE FTValueDictionaryClass
      USE ErrorTypesModule
      USE ValueSettingModule
      USE HexMeshObjectsModule
      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      CHARACTER(LEN=18)         , PARAMETER :: PROJECT_READ_EXCEPTION     = "Project read error"
      
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: RUN_PARAMETERS_KEY         = "RUN_PARAMETERS"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: MESH_FILE_NAME_KEY         = "mesh file name"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: PLOT_FILE_NAME_KEY         = "plot file name"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: STATS_FILE_NAME_KEY        = "stats file name"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: MESH_FILE_FORMAT_NAME_KEY  = "mesh file format"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: POLYNOMIAL_ORDER_KEY       = "polynomial order"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: PLOT_FORMAT_KEY            = "plot file format"
      
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: MESH_PARAMETERS_KEY         = "MESH_PARAMETERS"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: MESH_TYPE_KEY               = "mesh type"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: GRID_SIZE_KEY               = "background grid size"
      
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: MATERIAL_BLOCK_KEY          = "MATERIALS"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: BACKGROUND_MATERIAL_KEY     = "material"
      
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: X_START_NAME_KEY            = "x0"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: X_END_NAME_KEY              = "x1"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: DX_NAME_KEY                 = "dx"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: SPACING_NAME_KEY            = "h"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: EXTENT_NAME_KEY             = "w"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: NUM_INTERVALS_NAME_KEY      = "N"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: TYPE_NAME_KEY               = "type"

      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: REFINEMENT_REGIONS_KEY      = "REFINEMENT_REGIONS"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: REFINEMENT_CENTER_KEY       = "REFINEMENT_CENTER"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: REFINEMENT_LINE_KEY         = "REFINEMENT_LINE"

      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: ELEMENT_TYPE_KEY            = "element type"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: SIMPLE_EXTRUSION_BLOCK_KEY  = "SIMPLE_EXTRUSION"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: SIMPLE_ROTATION_BLOCK_KEY   = "SIMPLE_ROTATION"
      
!
!     ------------------
!     Private data types
!     ------------------
!
      TYPE RunParameters
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: MeshFileName
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: plotFileName
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: statsFileName
         INTEGER                    :: meshFileFormat
         INTEGER                    :: polynomialOrder
         INTEGER                    :: plotFileFormat ! = SKELETON_FORMAT OR = SEM_FORMAT
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
      
      INTEGER, PARAMETER :: SKELETON_FORMAT = 0, SEM_FORMAT = 1
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
         TYPE(StructuredHexMesh)  , POINTER :: hexMesh  => NULL()
         TYPE(RunParameters)                :: runParams
         TYPE(MeshParameters)               :: meshParams
         TYPE(BackgroundGridParameters)     :: backgroundParams
         CHARACTER(LEN=32)                  :: backgroundMaterialName
!         
!        ========         
         CONTAINS
!        ========         
!
         PROCEDURE :: initWithDictionary         
      END TYPE MeshProject
      
      INTERFACE release
         MODULE PROCEDURE :: releaseMeshProject 
      END INTERFACE  
!
!     ========
      CONTAINS
!     ========
!
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initWithDictionary(self, masterControlDictionary )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MeshProject)       :: self
         CLASS(FTValueDictionary) :: masterControlDictionary
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH)          :: msg
         CLASS(FTValueDictionary)  , POINTER :: controlDict, modelDict, matBlockdict
         CLASS(FTObject)           , POINTER :: obj
!
!        ----------
!        Interfaces
!        ----------
!
         LOGICAL, EXTERNAL :: ReturnOnFatalError
!
!        ---------------------------
!        Call superclass initializer
!        ---------------------------
!
         CALL self % FTObject % init()
!
!        ------------------------------------
!        Get run and model parameters
!        there must be a CONTROL_INPUT block, 
!        but a model is optional 
!        ------------------------------------
!
         obj         => masterControlDictionary % objectForKey(key = "CONTROL_INPUT")
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            CALL ThrowErrorExceptionOfType(poster = "initWithDictionary",             &
                                           msg = "CONTROL_INPUT block not available", &
                                           typ = FT_ERROR_FATAL) 
            RETURN 
         END IF 
         controlDict => valueDictionaryFromObject(obj)
         
         obj         => masterControlDictionary % objectForKey(key = "MODEL")
         modelDict   => valueDictionaryFromObject(obj)
         
         CALL SetRunParametersBlock( self % runParams, controlDict )
         IF(ReturnOnFatalError())     RETURN 
         CALL SetMeshParametersBlock( self % meshParams, controlDict )
         IF(ReturnOnFatalError())     RETURN 
!
!        -------------------
!        Read the model file
!        -------------------
!
         ALLOCATE(self % model)
         CALL self % model % initWithContentsOfDictionary( modelDict )
         IF(ReturnOnFatalError())     RETURN 
!
!        -----------------------------------------------------------------
!        If this is a multiple material mesh, as given by the ISM-MM flag,
!        read the background material name.
!        -----------------------------------------------------------------
!
         IF ( self % runParams % meshFileFormat == ISM_MM )     THEN
            IF ( modelDict % containsKey(key = MATERIAL_BLOCK_KEY) )     THEN
               obj => modelDict % objectForKey(key = MATERIAL_BLOCK_KEY)
               matBlockdict => valueDictionaryFromObject(obj)
               self % backgroundMaterialName = matBlockdict % stringValueForKey(key = BACKGROUND_MATERIAL_KEY,&
                                                                                requestedLength = DEFAULT_CHARACTER_LENGTH)
            ELSE 
               msg = "Background material block not found in control file. Using default name = 'base'"
               CALL ThrowErrorExceptionOfType(poster = "initWithDictionary", &
                                              msg    = msg, &
                                              typ    = FT_ERROR_WARNING)
               self % backgroundMaterialName = "base"
            END IF 
         END IF
!
!        -----------------
!        Build the project
!        -----------------
!
         CALL BuildProject( self, controlDict )

      END SUBROUTINE initWithDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE DestructMeshProject(self)  
         IMPLICIT NONE  
         CLASS(MeshProject) :: self
         
         IF ( ASSOCIATED(self % model) )     THEN
            CALL release(self % model)
         END IF 
         
         IF ( ASSOCIATED(self % mesh) )     THEN
            CALL release(self % mesh)
         END IF 
         
         IF ( ASSOCIATED(self % sizer) )     THEN
            CALL release(self % sizer)
         END IF 
         
         IF ( ASSOCIATED(self % grid) )     THEN
            CALL release(self % grid)
         END IF 
         
         IF ( ASSOCIATED(self % smoother) )     THEN
            CALL self % smoother % destruct()
            DEALLOCATE(self % smoother) 
         END IF 
         
         IF (  ASSOCIATED(self % hexMesh) )     THEN
            CALL DestructStructuredHexMesh(hexMesh  = self % hexMesh) 
         END IF  
         
      END SUBROUTINE DestructMeshProject
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseMeshProject(self)  
         IMPLICIT NONE
         CLASS(MeshProject), POINTER :: self
         CLASS(FTObject)   , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF     
          
      END SUBROUTINE releaseMeshProject
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE resetProject(self)  
         IMPLICIT NONE  
         CLASS(MeshProject), POINTER :: self
         
         IF ( ASSOCIATED(self % grid) )     THEN
            CALL release(self = self % grid) 
            self % grid => NULL()
         END IF

         CALL BuildQuadtreeGrid(self)
         
         IF ( ASSOCIATED( self % mesh) )     THEN
            CALL release(self = self % mesh) 
            self % mesh => NULL()
         END IF 

      END SUBROUTINE resetProject
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE BuildProject( self, controlDict )
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
         CLASS(FTValueDictionary) :: controlDict
         TYPE(MeshProject)        :: self
!
!        ----------
!        Interfaces
!        ----------
!
         LOGICAL, EXTERNAL :: ReturnOnFatalError
!
!        ---------------
!        Local Variables
!        ---------------
!
         CLASS(FTValueDictionary)    , POINTER :: smootherDict, backgroundGridDict, refinementsDict, refinementObjectDict
         CLASS(FTLinkedList)         , POINTER :: refinementsList
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedOuterBoundary => NULL()
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedInnerBoundary => NULL()
         CLASS(SMChainedCurve)       , POINTER :: chain => NULL()
         CLASS(FTLinkedListIterator) , POINTER :: iterator => NULL(), refinementIterator => NULL()
         CLASS(FTObject)             , POINTER :: obj => NULL()
         CLASS(QuadTreeGrid)         , POINTER :: parent => NULL()
         CLASS(SpringMeshSmoother)   , POINTER :: springSmoother => NULL()
!         CLASS(LaplaceMeshSmoother)  , POINTER :: laplaceSmoother => NULL()
         
         CLASS(SizerCentercontrol), POINTER :: c => NULL()
         CLASS(SizerLineControl)  , POINTER :: L => NULL()
         
         TYPE(BackgroundGridParameters) :: backgroundGrid
         TYPE(CentersParameters)        :: centerParams
         TYPE(SpringSmootherParameters) :: smootherParams
!         TYPE(LaplaceSmootherParameters):: laplaceParameters
         TYPE(LineParameters)           :: lineParams
         
         REAL(KIND=RP)                  :: h, xMax(3)
         INTEGER                        :: curveID
         LOGICAL                        :: smootherWasRead = .FALSE.
         
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: str
         
         NULLIFY( self % grid )
         NULLIFY( self % sizer )
!
!-------------------------------------------------------------------------------
!        Construct the Background Grid and initialize Sizer
!        The backgound grid size can eigher be specified or inferred
!        from the model depending on if the background grid block is
!        present or not.
!-------------------------------------------------------------------------------
!
         IF ( controlDict % containsKey(key = "BACKGROUND_GRID") )     THEN
         
            obj => controlDict % objectForKey(key = "BACKGROUND_GRID")
            backgroundGridDict => valueDictionaryFromObject(obj)
            
            CALL SetBackgroundGridBlock( backgroundGrid, backgroundGridDict )
            IF(ReturnOnFatalError())     RETURN 
            
            self % meshParams % backgroundGridSize = 2*backgroundGrid % dx
            
            xMax = backgroundGrid % x0 + backgroundGrid % N*backgroundGrid % dx
            
            ALLOCATE(self % sizer)
            CALL self % sizer % initWithProperties(backgroundGrid % dx, backgroundGrid % x0, xMax)
            
         ELSE 
         
            CALL BuildbackgroundGridFromModel( backgroundGrid, self % model, self % meshParams % backgroundGridSize )
            IF(ReturnOnFatalError())     RETURN 
            
            xMax = backgroundGrid % x0 + backgroundGrid % N*backgroundGrid % dx
            ALLOCATE(self % sizer)
            CALL self % sizer % initWithProperties( self % meshParams % backgroundGridSize, backgroundGrid % x0, xMax )
            IF(ReturnOnFatalError())     RETURN 
            
         END IF 
         self % backgroundParams = backgroundGrid
!
!        -----------------------
!        Build the quadtree grid
!        -----------------------
!
         CALL BuildQuadtreeGrid(self)
!
!        -------------------------
!        Build up Sizer properties
!        -------------------------
!
         IF ( controlDict % containsKey(key = REFINEMENT_REGIONS_KEY) )     THEN
         
            ALLOCATE(refinementIterator)
            obj             => controlDict % objectForKey(key = REFINEMENT_REGIONS_KEY)
            refinementsDict => valueDictionaryFromObject(obj)
            obj             => refinementsDict % objectForKey(key = "LIST")
            refinementsList => linkedListFromObject(obj)
            
            CALL refinementIterator % initWithFTLinkedList(list = refinementsList)
            CALL refinementIterator % setToStart()
            DO WHILE (.NOT. refinementIterator % isAtEnd()) 
               
               obj                  => refinementIterator % object()
               refinementObjectDict => valueDictionaryFromObject(obj)
               str = refinementObjectDict % stringValueForKey(key = "TYPE", &
                                                              requestedLength = DEFAULT_CHARACTER_LENGTH)
               SELECT CASE ( str )
               
                  CASE( REFINEMENT_CENTER_KEY ) 
                  
                     CALL SetCenterMeshSizerBlock(centerParams = centerParams, centerDict = refinementObjectDict) 
                     ALLOCATE(c)
                     CALL c % initWithProperties( centerParams % x0, centerParams % centerExtent, &
                                                centerParams % centerMeshSize, centerParams % centerType )
                     CALL self % sizer % addSizerCenterControl(c)
                     CALL release(c)
                     
                  CASE ( REFINEMENT_LINE_KEY)
                  
                     CALL SetLineMeshSizerBlock(lineParams = lineParams, lineSizerDict = refinementObjectDict)
                     
                     ALLOCATE(L)
                     CALL L    % initWithProperties( lineParams % x0, lineParams % x1, lineParams % lineExtent, &
                                                     lineParams % lineMeshSize, lineParams % lineControlType )
                     CALL self % sizer % addSizerLineControl(L)
                     CALL release(L)
                  CASE DEFAULT 
                  PRINT *, "Funny object"
                  STOP 
               END SELECT 
               
               CALL refinementIterator % moveToNext() 
            END DO 
            
            CALL release(refinementIterator)
         END IF 
!
!        ------------------------------------------------
!        Discretize boundary curves and add to sizer.
!        Use current sizer attributes to subdivide the
!        boundary curves. There is a relationship between
!        computing the discretization of the boundary
!        curves and the size to be associated with the
!        sizer. 
!        ------------------------------------------------
!
         curveID = 0
         h       = MINVAL(self % meshParams % backgroundGridSize(1:2))
         
         IF( ASSOCIATED( self % model % outerBoundary ) )     THEN
            curveID                =  curveID + 1
            segmentedOuterBoundary => allocAndInitSegmentedChainFromChain( self % model % outerBoundary, &
                                                                           h, self % sizer % controlsList, curveID )
            CALL self % sizer % addBoundaryCurve(segmentedOuterBoundary,OUTER)
            CALL release(segmentedOuterBoundary)
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
               segmentedInnerBoundary => allocAndInitSegmentedChainFromChain( chain, h, self % sizer % controlsList, curveID )
                
               CALL self % sizer % addBoundaryCurve(segmentedInnerBoundary,INNER)
               CALL release(segmentedInnerBoundary)
                
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
                segmentedInnerBoundary => allocAndInitSegmentedChainFromChain( chain, h, self % sizer % controlsList, curveID )
                
                CALL self % sizer % addBoundaryCurve(segmentedInnerBoundary,INTERIOR_INTERFACE)
                CALL release(segmentedInnerBoundary)
                
                CALL iterator % moveToNext()           
             END DO  
         END IF
!
!        -------------------------------------------------------
!        Make sure that there are enough elements between curves
!        Adjust the sizer to account for the curves.
!        -------------------------------------------------------
!
         IF ( ASSOCIATED( self % model % interfaceBoundaries ) .OR. &
              ASSOCIATED( self % model % innerBoundaries )     .OR. &
              ASSOCIATED(self % model % outerBoundary))        THEN
            CALL ComputeCurveDistanceScales( self % sizer )
         END IF
         
         IF ( ASSOCIATED( self % model % interfaceBoundaries ) ) THEN
            CALL ComputeInterfaceCurveScales( self % sizer )
         END IF
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
         IF ( controlDict % containsKey(key = "SPRING_SMOOTHER") )     THEN
         
            obj          => controlDict % objectForKey(key = "SPRING_SMOOTHER")
            smootherDict => valueDictionaryFromObject(obj)
            
            CALL SetSpringSmootherBlock( smootherDict, smootherParams )
            IF(ReturnOnFatalError())     RETURN 
            
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
!!
!TODO: Implement laplacian smoother? It hasn't worked so well.
!!        ------------------
!!        Laplacian Smoother
!!        ------------------
!!
!         rewind(fUnit)
!         CALL MoveToBlock("\begin{LaplaceSmoother}", fUnit, iOS )
!         IF ( ios == 0 )     THEN
!            CALL ReadLaplaceSmootherBlock( fUnit, laplaceParameters )
!            IF ( laplaceParameters % smoothingOn )     THEN
!               ALLOCATE(laplaceSmoother)
!               CALL laplaceSmoother % init(laplaceParameters)
!               self % smoother => laplaceSmoother
!               smootherWasRead = .TRUE.
!            END IF 
!         END IF 
!         IF(smootherWasRead) RETURN 
         
      END SUBROUTINE BuildProject
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE BuildQuadtreeGrid(self)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(MeshProject)        :: self
!
!        ---------------
!        Local Variables
!        ---------------
!
         CLASS(QuadTreeGrid), POINTER :: parent => NULL()
         NULLIFY(parent)
         
         IF(ASSOCIATED(self % grid))      THEN
            CALL release(self % grid)
            self % grid => NULL()
         END IF 
         
         ALLOCATE(self % grid)
         CALL self % grid % initGridWithParameters( self % backgroundParams % dx, &
                                                    self % backgroundParams % x0, &
                                                    self % backgroundParams % N,  &
                                                     parent, (/0,0,0/), 0)
      END SUBROUTINE BuildQuadtreeGrid
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
         CLASS(FTLinkedList)         , POINTER :: controlsList => NULL()
         
         INTEGER       :: nX, nY
         REAL(KIND=RP) :: heightB, widthB
         REAL(KIND=RP) :: leftB, rightB, topB, bottomB, h
         
         leftB   = HUGE(leftB)
         rightB  = -leftB
         topB    =  rightB
         bottomB =  leftB
         controlsList => NULL()
!
!        ----------------------------------------------------
!        Create a discrete curve to determine the extents of 
!        the background mesh
!        ----------------------------------------------------
!
         h = MAXVAL( backgroundGridSize(1:2) )
         IF( ASSOCIATED( model % outerBoundary ) )     THEN
            curveID = 1
            segmentedOuterBoundary => allocAndInitSegmentedChainFromChain( model % outerBoundary, h, controlsList, curveID )
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
            CALL ThrowErrorExceptionOfType(poster = "BuildbackgroundGridFromModel",msg = msg,typ = FT_ERROR_FATAL)
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
      SUBROUTINE SetRunParametersBlock( params, controlDict ) 
!
!        Example block is:
!
!         \begin{RunParameters}
!            model file name = "model.gm"
!            mesh file name = "fname.mesh"
!            stats file name = "fname.txt" (Optional)
!            mesh file format = "Basic", ...
!            plot file name = "tName.tec"  (Optional)
!            plot file format = "skeleton" OR "sem"
!         \end{RunParameters}
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(RunParameters)               :: params
         CLASS(FTValueDictionary), POINTER :: controlDict
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: fileFormat
         CLASS(FTValueDictionary), POINTER       :: paramsDict
         CLASS(FTObject)         , POINTER       :: obj
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: msg
         
         obj        => controlDict % objectForKey(key = RUN_PARAMETERS_KEY)
         paramsDict => valueDictionaryFromObject(obj)
         
         params % MeshFileName = "MeshFile.mesh"
         msg = "Control file is missing the Mesh file name. Using default name, MeshFile.mesh."
         CALL SetStringValueFromDictionary(valueToSet = params % MeshFileName,  &
                                           sourceDict = paramsDict,             &
                                           key        = MESH_FILE_NAME_KEY,     &
                                           errorLevel = FT_ERROR_WARNING,       &
                                           message    = msg,                    &
                                           poster     = "SetRunParametersBlock")
         
         
         params % plotFileName = "PlotFile.tec"
         msg = "Control file is missing the plot file name. Using default name, PlotFile.tec."
         CALL SetStringValueFromDictionary(valueToSet = params % plotFileName,  &
                                           sourceDict = paramsDict,             &
                                           key        = PLOT_FILE_NAME_KEY,     &
                                           errorLevel = FT_ERROR_WARNING,       &
                                           message    = msg,                    &
                                           poster     = "SetRunParametersBlock")
         
         params % statsFileName = "None"
         msg = "Control file is missing the stats file name. Stats not written."
         CALL SetStringValueFromDictionary(valueToSet = params % statsFileName, &
                                           sourceDict = paramsDict,             &
                                           key        = STATS_FILE_NAME_KEY,    &
                                           errorLevel = FT_ERROR_WARNING,       &
                                           message    = msg,                    &
                                           poster     = "SetRunParametersBlock")
                                           
         msg = "Unknown mesh file format or mesh file format not set. Set to ISM"
         CALL SetStringValueFromDictionary(valueToSet = fileFormat,                &
                                           sourceDict = paramsDict,                &
                                           key        = MESH_FILE_FORMAT_NAME_KEY, &
                                           errorLevel = FT_ERROR_WARNING,          &
                                           message    = msg,                       &
                                           poster     = "SetRunParametersBlock")

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
            params % meshFileFormat = ISM
         END IF

         msg = "Control file is missing the polynomial order. Using default N = 5."
         params % polynomialOrder = 5
         CALL SetIntegerValueFromDictionary(valueToSet = params % polynomialOrder, &
                                           sourceDict = paramsDict,               &
                                           key        = POLYNOMIAL_ORDER_KEY,     &
                                           errorLevel = FT_ERROR_WARNING,         &
                                           message    = msg,                      &
                                           poster     = "SetRunParametersBlock")
                                           
         msg = "Unknown plot file format or plot file format not set. Set to skeleton"
         CALL SetStringValueFromDictionary(valueToSet = fileFormat,                &
                                           sourceDict = paramsDict,                &
                                           key        = PLOT_FORMAT_KEY, &
                                           errorLevel = FT_ERROR_WARNING,          &
                                           message    = msg,                       &
                                           poster     = "SetRunParametersBlock")
         IF ( fileFormat == "skeleton" )     THEN
            params % plotFileFormat = SKELETON_FORMAT 
         ELSE IF(fileFormat == "sem")     THEN 
            params % plotFileFormat = SEM_FORMAT 
         ELSE
            params % plotFileFormat = SEM_FORMAT
         END IF 
          
      END SUBROUTINE SetRunParametersBlock
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetMeshParametersBlock( params, controlDict ) 
!
!        Example block is:
!
!         \begin{MeshParameters}
!            background grid size = [1.0, 1.0, 1.0]
!         \end{MeshParameters}
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(MeshParameters)              :: params
         CLASS(FTValueDictionary), POINTER :: controlDict
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTValueDictionary), POINTER       :: paramsDict
         CLASS(FTObject)         , POINTER       :: obj
         
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: typeName
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: msg
         
         obj        => controlDict % objectForKey(key = MESH_PARAMETERS_KEY)
         paramsDict => valueDictionaryFromObject(obj)
!
!        -----------------------------------------
!        Make sure mesh parameters are in the file
!        -----------------------------------------
!
         IF(.NOT.ASSOCIATED(paramsDict)) THEN
            CALL ThrowErrorExceptionOfType(poster = "SetMeshParametersBlock", &
                                           msg    = "Control file is missing the mesh parameters block.", &
                                           typ    = FT_ERROR_FATAL)
            RETURN 
         END IF
!
!        ---------
!        Mesh type
!        ---------
!
         typeName = "conforming"
!         msg      = "Control file is missing the mesh type. Using default 'conforming'."
!         CALL SetStringValueFromDictionary(valueToSet = typeName,               &
!                                           sourceDict = paramsDict,             &
!                                           key        = MESH_TYPE_KEY,          &
!                                           errorLevel = FT_ERROR_WARNING,       &
!                                           message    = msg,                    &
!                                           poster     = "SetMeshParametersBlock")         
         IF( typeName == "conforming" )     THEN
            params % meshType = 0! CONFORMING
         ELSE
            params % meshType = 1 !NON_CONFORMING
         END IF
!
!        ---------
!        Mesh size
!        ---------
!
         msg      = "Control file is missing the mesh size."
         CALL SetRealArrayValueFromDictionary(arrayToSet = params % backgroundGridSize, &
                                              sourceDict = paramsDict,                  &
                                              key = GRID_SIZE_KEY,                      &
                                              errorLevel = FT_ERROR_FATAL,              &
                                              message = msg,                            &
                                              poster = "SetMeshParametersBlock")
   
      END SUBROUTINE SetMeshParametersBlock
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetBackgroundGridBlock( backgroundGrid, backgroundGridDict )
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
         CLASS(FTValueDictionary), POINTER :: backgroundGridDict
         TYPE(BackgroundGridParameters)    :: backgroundGrid
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: msg
!
!
!        ----------
!        Interfaces
!        ----------
!
         LOGICAL, EXTERNAL :: ReturnOnFatalError
!
!        ------------------------------------------------
!
         msg = "Background grid block missing parameter " // TRIM(X_START_NAME_KEY)
         CALL SetRealArrayValueFromDictionary(arrayToSet =  backgroundGrid % x0,      &
                                              sourceDict = backgroundGridDict,        &
                                              key        = X_START_NAME_KEY,          &
                                              errorLevel = FT_ERROR_FATAL,            &
                                              message    = msg,                       &
                                              poster     = "SetBackgroundGridBlock")
         IF(ReturnOnFatalError()) RETURN

         msg = "Background grid block missing parameter " // TRIM(DX_NAME_KEY)
         CALL SetRealArrayValueFromDictionary(arrayToSet = backgroundGrid % dx,       &
                                              sourceDict = backgroundGridDict,        &
                                              key        = DX_NAME_KEY,               &
                                              errorLevel = FT_ERROR_FATAL,            &
                                              message    = msg,                       &
                                              poster     = "SetBackgroundGridBlock")
         IF(ReturnOnFatalError()) RETURN
          
         msg = "Background grid block missing parameter " // TRIM(NUM_INTERVALS_NAME_KEY)
         CALL SetIntegerArrayValueFromDictionary(arrayToSet = backgroundGrid % N,        &
                                                 sourceDict = backgroundGridDict,        &
                                                 key        = NUM_INTERVALS_NAME_KEY,    &
                                                 errorLevel = FT_ERROR_FATAL,            &
                                                 message    = msg,                       &
                                                 poster     = "SetBackgroundGridBlock")
         IF(ReturnOnFatalError()) RETURN

         
      END SUBROUTINE SetBackgroundGridBlock
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetCenterMeshSizerBlock( centerParams, centerDict ) 
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
         CLASS(FTValueDictionary), POINTER :: centerDict
         TYPE(CentersParameters)           :: centerParams
!
!        ----------
!        Interfaces
!        ----------
!
         logical, EXTERNAL :: ReturnOnFatalError
!
!        ----------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: str, msg
!
!        ---------------------------------
!        Smooth OR sharp center variation 
!        ---------------------------------
!
         str = "smooth"
         msg = "Refinement center block missing smooth parameter. Using default smooth"
         CALL SetStringValueFromDictionary(valueToSet = str,              &
                                           sourceDict = centerDict,       &
                                           key = TYPE_NAME_KEY,           &
                                           errorLevel = FT_ERROR_WARNING, &
                                           message = msg,                 &
                                           poster = "SetCenterMeshSizerBlock")
         
         IF( str == "smooth" )     THEN
            centerParams % centerType = CENTER_SMOOTH
         ELSE
            centerParams % centerType = CENTER_SHARP
         END IF
!
!        --------
!        Location
!        --------
!
         msg = "Refinement center block missing parameter " // TRIM(X_START_NAME_KEY)
         CALL SetRealArrayValueFromDictionary(arrayToSet = centerParams % x0,         &
                                              sourceDict = centerDict,                &
                                              key        = X_START_NAME_KEY,          &
                                              errorLevel = FT_ERROR_FATAL,            &
                                              message    = msg,                       &
                                              poster     = "SetCenterMeshSizerBlock")
         IF(ReturnOnFatalError()) RETURN
!
!        -----
!        Size 
!        -----
!
         msg = "Refinement center block missing parameter " // TRIM(SPACING_NAME_KEY)
         CALL SetRealValueFromDictionary(valueToSet = centerParams % centerMeshSize, &
                                         sourceDict = centerDict,                    &
                                         key        = SPACING_NAME_KEY,              &
                                         errorLevel = FT_ERROR_FATAL,                &
                                         message    = msg,                           &
                                         poster     = "SetCenterMeshSizerBlock")
         IF(ReturnOnFatalError()) RETURN
!
!        ------
!        Extent
!        ------
!
         msg = "Refinement center block missing parameter " // TRIM(EXTENT_NAME_KEY)
         CALL SetRealValueFromDictionary(valueToSet = centerParams % centerExtent,   &
                                         sourceDict = centerDict,                    &
                                         key        = EXTENT_NAME_KEY,               &
                                         errorLevel = FT_ERROR_FATAL,                &
                                         message    = msg,                           &
                                         poster     = "SetCenterMeshSizerBlock")
         IF(ReturnOnFatalError()) RETURN
         
      END SUBROUTINE SetCenterMeshSizerBlock
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetLineMeshSizerBlock( lineParams, lineSizerDict ) 
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
         CLASS(FTValueDictionary), POINTER :: lineSizerDict
         TYPE(lineParameters)              :: lineParams
!
!        ----------
!        Interfaces
!        ----------
!
         logical, EXTERNAL :: ReturnOnFatalError
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: str, msg
!
!        --------------------
!        Smooth or sharp type
!        --------------------
!
         str = "smooth"
         msg = "Refinement line block missing smooth type parameter. Using default smooth"
         CALL SetStringValueFromDictionary(valueToSet = str,                 &
                                           sourceDict = lineSizerDict,       &
                                           key        = TYPE_NAME_KEY,       &
                                           errorLevel = FT_ERROR_WARNING,    &
                                           message    = msg,                 &
                                           poster = "SetLineMeshSizerBlock")
         
         IF( str == "smooth" )     THEN
            lineParams % lineControlType = CENTER_SMOOTH
         ELSE
            lineParams % lineControlType = CENTER_SHARP
         END IF
!
!        --------------
!        Start location
!        --------------
!
         msg = "Refinement line block missing parameter " // TRIM(X_START_NAME_KEY)
         CALL SetRealArrayValueFromDictionary(arrayToSet = lineParams % x0,           &
                                              sourceDict = lineSizerDict,             &
                                              key        = X_START_NAME_KEY,          &
                                              errorLevel = FT_ERROR_FATAL,            &
                                              message    = msg,                       &
                                              poster     = "SetLineMeshSizerBlock")
         IF(ReturnOnFatalError()) RETURN
!
!        ------------
!        End location
!        ------------
!
         msg = "Refinement line block missing parameter " // TRIM(X_END_NAME_KEY)
         CALL SetRealArrayValueFromDictionary(arrayToSet = lineParams % x1,           &
                                              sourceDict = lineSizerDict,             &
                                              key        = X_END_NAME_KEY,            &
                                              errorLevel = FT_ERROR_FATAL,            &
                                              message    = msg,                       &
                                              poster     = "SetLineMeshSizerBlock")
         IF(ReturnOnFatalError()) RETURN
         
!
!        ---------
!        Mesh size
!        ---------
!
         msg = "Refinement line block missing parameter " // TRIM(SPACING_NAME_KEY)
         CALL SetRealValueFromDictionary(valueToSet = lineParams % lineMeshSize  ,   &
                                         sourceDict = lineSizerDict,                 &
                                         key        = SPACING_NAME_KEY,              &
                                         errorLevel = FT_ERROR_FATAL,                &
                                         message    = msg,                           &
                                         poster     = "SetLineMeshSizerBlock")
         IF(ReturnOnFatalError()) RETURN
!
!        ------
!        Extent
!        ------
!
         msg = "Refinement line block missing parameter " // TRIM(EXTENT_NAME_KEY)
         CALL SetRealValueFromDictionary(valueToSet = lineParams % lineExtent,       &
                                         sourceDict = lineSizerDict,                 &
                                         key        = EXTENT_NAME_KEY,               &
                                         errorLevel = FT_ERROR_FATAL,                &
                                         message    = msg,                           &
                                         poster     = "SetLineMeshSizerBlock")
         IF(ReturnOnFatalError()) RETURN
         
      END SUBROUTINE SetLineMeshSizerBlock
      

   END MODULE MeshProjectClass



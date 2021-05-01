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
      USE Geometry3DModule
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
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: BACKGROUND_GRID_KEY         = "BACKGROUND_GRID"
      
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
      
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: SCALE_TRANSFORM_BLOCK_KEY   = "SCALE_TRANSFORM"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: SCALE_TRANSFORM_SCALE_KEY   = "scale factor"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: SCALE_TRANSFORM_ORIGIN_KEY  = "origin"
      
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: ROTATION_TRANSFORM_BLOCK_KEY       = "ROTATION_TRANSFORM"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: ROTATION_TRANSFORM_TRANSLATION_KEY = "translation"
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: ROTATION_TRANSFORM_DIRECTION_KEY   = "direction"
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
      END TYPE MeshParameters
      PRIVATE :: MeshParameters
      
      TYPE BackgroundGridParameters
         REAL(KIND=RP) :: backgroundGridSize(3)
         INTEGER       :: N(3)
         REAL(KIND=RP) :: dx(3)
         REAL(KIND=RP) :: x0(3)
         REAL(KIND=RP) :: xMax(3)
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
         TYPE (SMModel)           , POINTER :: model    => NULL()
         TYPE (SMMesh)            , POINTER :: mesh     => NULL()
         TYPE (MeshSizer)         , POINTER :: sizer    => NULL()
         CLASS(QuadTreeGrid)      , POINTER :: grid     => NULL()
         CLASS(MeshSmoother)      , POINTER :: smoother => NULL()
         TYPE(StructuredHexMesh)  , POINTER :: hexMesh  => NULL()
         TYPE(RunParameters)                :: runParams
         TYPE(MeshParameters)               :: meshParams
         TYPE(BackgroundGridParameters)     :: backgroundParams
         TYPE(RotationTransform)             :: rotationTransformer
         TYPE(ScaleTransform)               :: scaleTransformer
         CHARACTER(LEN=32)                  :: backgroundMaterialName
!         
!        ========         
         CONTAINS
!        ========         
!
         PROCEDURE :: initWithDictionary         
      END TYPE MeshProject
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
                                           msg = "CONTROL_INPUT block is missing from control file", &
                                           typ = FT_ERROR_FATAL) 
            RETURN 
         END IF 
         controlDict => valueDictionaryFromObject(obj)
         
         modelDict => NULL()
         obj       => masterControlDictionary % objectForKey(key = "MODEL")
         IF ( ASSOCIATED(obj) )     THEN
            modelDict   => valueDictionaryFromObject(obj)
         END IF 
!
!        --------------------------------------------------------------------
!        The MESH_PARAMETERS block is optional, but the BACKGROUND_GRID block
!        must be present
!        --------------------------------------------------------------------
!
         IF ( .NOT. controlDict % containsKey(key =  BACKGROUND_GRID_KEY) )     THEN
            CALL ThrowErrorExceptionOfType(poster = "initWithDictionary",             &
                                           msg    = "Control file needs a BACKGROUND_GRID block", &
                                           typ    = FT_ERROR_FATAL) 
            RETURN 
         END IF 
         
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
            CALL releaseModel(self % model)
         END IF 
         
         IF ( ASSOCIATED(self % mesh) )     THEN
            CALL releaseMesh(self % mesh)
         END IF 
         
         IF ( ASSOCIATED(self % sizer) )     THEN
            CALL releaseSizer(self % sizer)
         END IF 
         
         IF ( ASSOCIATED(self % grid) )     THEN
            CALL releaseGrid(self % grid)
         END IF 
         
         IF ( ASSOCIATED(self % smoother) )     THEN
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
      SUBROUTINE ResetProject(self)  
         IMPLICIT NONE  
         CLASS(MeshProject), POINTER :: self
         
         IF ( ASSOCIATED(self % grid) )     THEN
            CALL releaseGrid(self % grid)
         END IF

         CALL BuildQuadtreeGrid(self)
         
         IF ( ASSOCIATED( self % mesh) )     THEN
            CALL releaseMesh(self % mesh)
         END IF 

      END SUBROUTINE ResetProject
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
         CLASS(FTValueDictionary)    , POINTER :: smootherDict, refinementsDict
         CLASS(FTValueDictionary)    , POINTER :: scaleTransformDict, rotationTransformDict
         CLASS(FTLinkedList)         , POINTER :: refinementsList
         CLASS(FTLinkedListIterator) , POINTER :: refinementIterator => NULL()
         CLASS(FTObject)             , POINTER :: obj => NULL()
         CLASS(SpringMeshSmoother)   , POINTER :: springSmoother => NULL()
                  
         TYPE(SpringSmootherParameters) :: smootherParams
                           
         NULLIFY( self % grid )
         NULLIFY( self % sizer )
         CALL ConstructIdentityScaleTransform(self = self % scaleTransformer)
         CALL ConstructIdentityRotationTransform(self = self % rotationTransformer)
!
         CALL BuildBackgroundGrid(self, controlDict )
         CALL BuildQuadtreeGrid(self)
!
!        -------------------------
!        Build up Sizer properties
!        -------------------------
!
         ALLOCATE(self % sizer)
         CALL self % sizer % initWithProperties( self % backgroundParams % dx, &
                                                 self % backgroundParams % x0, &
                                                 self % backgroundParams % xMax )
         IF(ReturnOnFatalError())     RETURN 
         
         IF ( controlDict % containsKey(key = REFINEMENT_REGIONS_KEY) )     THEN
         
            ALLOCATE(refinementIterator)
            obj             => controlDict % objectForKey(key = REFINEMENT_REGIONS_KEY)
            refinementsDict => valueDictionaryFromObject(obj)
            obj             => refinementsDict % objectForKey(key = "LIST")
            refinementsList => linkedListFromObject(obj)
            
            CALL AddRefinementRegionsToSizer(refinementsList, sizer = self % sizer)
            
         END IF 
         CALL BuildSizerBoundaryCurves(self)
!
!        -------------------------------------------------------
!        Check integrity of boundary curves: stop if any overlap
!        -------------------------------------------------------
!
!         CALL CheckForBoundaryIntersections(self % sizer) !Development in progress
         IF(catch())     RETURN 
!
!        ------------------
!        Construct smoother
!        ------------------
!
         NULLIFY(self % smoother)
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
            END IF
         ELSE
            ! For other possibilities added later
         END IF 
!
!        --------------------------
!        Construct Rotation transform
!        --------------------------
!
         IF ( controlDict % containsKey(key = ROTATION_TRANSFORM_BLOCK_KEY) )     THEN
            obj                => controlDict % objectForKey(key = ROTATION_TRANSFORM_BLOCK_KEY)
            rotationTransformDict => valueDictionaryFromObject(obj)
            CALL SetRotationTransformBlock(rotationBlockDict   = rotationTransformDict,     &
                                           rotationTransformer = self % rotationTransformer)
         END IF 
!
!        ---------------------------
!        Construct Scaling transform
!        ---------------------------
!
         IF ( controlDict % containsKey(key = SCALE_TRANSFORM_BLOCK_KEY) )     THEN
            obj                => controlDict % objectForKey(key = SCALE_TRANSFORM_BLOCK_KEY)
            scaleTransformDict => valueDictionaryFromObject(obj)
            CALL SetScaleTransformBlock(scaleBlockDict   = scaleTransformDict, &
                                        scaleTransformer = self % scaleTransformer)
         END IF 
         
      END SUBROUTINE BuildProject
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE BuildBackgroundGrid(self, controlDict)  
!
!-------------------------------------------------------------------------------
!     Construct the Background Grid and initialize Sizer
!     The backgound grid size can eigher be specified or inferred
!     from the model depending on if the background grid block is
!     present or not.
!-------------------------------------------------------------------------------
!
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTValueDictionary) :: controlDict
         TYPE(MeshProject)        :: self
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP)                  ::xMax(3)
         TYPE(BackgroundGridParameters) :: backgroundGrid
         CLASS(FTObject)             , POINTER :: obj => NULL()
         CLASS(FTValueDictionary)    , POINTER :: backgroundGridDict
!
!        ----------
!        Interfaces
!        ----------
!
         LOGICAL, EXTERNAL :: ReturnOnFatalError
         
         obj => controlDict % objectForKey(key = BACKGROUND_GRID_KEY)
         backgroundGridDict => valueDictionaryFromObject(obj)
         CALL SetBackgroundGridBlock( backgroundGrid, backgroundGridDict )
         IF(ReturnOnFatalError())     RETURN 
         
         IF( .NOT. backgroundGridDict % containsKey(key = GRID_SIZE_KEY))     THEN 
            
            backgroundGrid % backgroundGridSize = 2*backgroundGrid % dx
            
            xMax                  = backgroundGrid % x0 + backgroundGrid % N*backgroundGrid % dx
            backgroundGrid % xMax = xMax
            
         ELSE 
         
            CALL BuildbackgroundGridFromModel( backgroundGrid, self % model, &
                                               backgroundGrid % backgroundGridSize )
            IF(ReturnOnFatalError())     RETURN 
            
            xMax = backgroundGrid % x0 + backgroundGrid % N*backgroundGrid % dx
            backgroundGrid % xMax = xMax
            
         END IF 
         self % backgroundParams = backgroundGrid
         
      END SUBROUTINE BuildBackgroundGrid
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
            CALL releaseGrid(self % grid)
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
      SUBROUTINE AddRefinementRegionsToSizer( refinementsList, sizer)
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList) , POINTER :: refinementsList
         TYPE (MeshSizer)    , POINTER :: sizer
!
!        ---------------
!        Local Variables
!        ---------------
!
         TYPE (FTLinkedListIterator) , POINTER   :: refinementIterator => NULL()
         CLASS(FTObject)             , POINTER   :: obj => NULL()
         CLASS(FTValueDictionary)    , POINTER   :: refinementObjectDict
         
         CLASS(SizerCentercontrol), POINTER      :: c => NULL()
         CLASS(SizerLineControl)  , POINTER      :: L => NULL()
         
         TYPE(CentersParameters)                 :: centerParams
         TYPE(LineParameters)                    :: lineParams
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: str
!
!        --------------------------------------------------------------
!        Refinement regions are stored in a linked list of dictionaries
!        --------------------------------------------------------------
!
         ALLOCATE(refinementIterator)
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
                  CALL sizer % addSizerCenterControl(c)
                  obj => c
                  CALL release(obj)
                  
               CASE ( REFINEMENT_LINE_KEY)
               
                  CALL SetLineMeshSizerBlock(lineParams = lineParams, lineSizerDict = refinementObjectDict)
                  
                  ALLOCATE(L)
                  CALL L    % initWithProperties( lineParams % x0, lineParams % x1, lineParams % lineExtent, &
                                                  lineParams % lineMeshSize, lineParams % lineControlType )
                  CALL sizer % addSizerLineControl(L)
                  obj => L
                  CALL release(obj)
                  
               CASE DEFAULT 
                  CALL ThrowErrorExceptionOfType(poster = "AddRefinementRegionsToSizer", &
                                                 msg    = "Unknown refinement region is ignored: "// TRIM(str), &
                                                 typ    = FT_ERROR_WARNING)
            END SELECT 
            
            CALL refinementIterator % moveToNext() 
         END DO 
         
         CALL releaseFTLinkedListIterator(refinementIterator)
 
      END SUBROUTINE AddRefinementRegionsToSizer
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE BuildSizerBoundaryCurves(self)  
         USE ChainedSegmentedCurveClass
         USE SMChainedCurveClass
         USE CurveConversionsModule
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
         INTEGER                               :: curveID
         REAL(KIND=RP)                         :: h
         CLASS(FTLinkedListIterator) , POINTER :: iterator => NULL()
         CLASS(FTObject)             , POINTER :: obj => NULL()
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedOuterBoundary => NULL()
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedInnerBoundary => NULL()
         CLASS(SMChainedCurve)       , POINTER :: chain => NULL()
         LOGICAL, EXTERNAL :: ReturnOnFatalError
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
         h       = MINVAL(self % backgroundParams % backgroundGridSize(1:2))
         
         IF( ASSOCIATED( self % model % outerBoundary ) )     THEN
            curveID                =  curveID + 1
            segmentedOuterBoundary => allocAndInitSegmentedChainFromChain( self % model % outerBoundary, &
                                                                           h, self % sizer % controlsList, curveID )
            CALL self % sizer % addBoundaryCurve(segmentedOuterBoundary,OUTER)
            
            CALL releaseChainChainedSegmentedCurve(segmentedOuterBoundary)
        END IF
!
!        --------------------------------------
!        Step through each inner boundary curve
!        and construct chains.
!        --------------------------------------
!
         IF( ASSOCIATED( self % model % innerBoundaries ) )     THEN
            iterator => self % model % innerBoundariesIterator
            CALL iterator % setToStart()
            DO WHILE (.NOT.iterator % isAtEnd())
               curveID =  curveID + 1
               obj     => iterator % object()
               CALL castToSMChainedCurve(obj,chain)
               segmentedInnerBoundary => allocAndInitSegmentedChainFromChain( chain, h, self % sizer % controlsList, curveID )
                
               CALL self % sizer % addBoundaryCurve(segmentedInnerBoundary,INNER)
               CALL releaseChainChainedSegmentedCurve(segmentedInnerBoundary)
                
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
                CALL releaseChainChainedSegmentedCurve(segmentedInnerBoundary)
                
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
         
      END SUBROUTINE BuildSizerBoundaryCurves
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
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            msg = "Control file is missing the block: " // TRIM(RUN_PARAMETERS_KEY)
            CALL ThrowErrorExceptionOfType(poster = "SetRunParametersBlock", &
                                           msg    = msg,                     &
                                           typ    = FT_ERROR_FATAL)
            RETURN 
         END IF 
          
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
                                           
         msg        = "Unknown plot file format or plot file format not set. Set to skeleton"
         fileFormat = "skeleton"
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
!            element type = quad OR hex
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
         
         obj        => controlDict % objectForKey(key = MESH_PARAMETERS_KEY)
         paramsDict => valueDictionaryFromObject(obj)
!
!        ---------------------------
!        Mesh parameters is optional
!        ---------------------------
!
         params % meshType = 0! CONFORMING
         IF(.NOT.ASSOCIATED(paramsDict)) THEN 
            params % meshType = 0! CONFORMING
         END IF 
   
      END SUBROUTINE SetMeshParametersBlock
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetBackgroundGridBlock( backgroundGrid, backgroundGridDict )
!
!        Example block is:
!
!         \begin{BACKROUND_GRID}
!            x0 = [-15.0, -15.0, 0.0]
!            dx = [1.0, 1.0, 0.0]
!            N  = [10,10, 0]
!         \end{BACKROUND_GRID}
!
!         OR
!
!         \begin{BACKROUND_GRID}
!            background grid size = [1.0, 1.0, 1.0]
!         \end{BACKROUND_GRID}
!
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
!
!        ---------
!        Mesh size
!        ---------
!
         IF ( backgroundGridDict % containsKey(key = GRID_SIZE_KEY) )     THEN
            msg      = "Control file is missing the mesh size."
            CALL SetRealArrayValueFromDictionary(arrayToSet = backgroundGrid % backgroundGridSize, &
                                                 sourceDict = backgroundGridDict,                  &
                                                 key        = GRID_SIZE_KEY,                       &
                                                 errorLevel = FT_ERROR_NONE,                       &
                                                 message    = msg,                                 &
                                                 poster     = "SetBackgroundGridBlock")
         ELSE 
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
         END IF 
         
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
         
      END SUBROUTINE SetLineMeshSizerBlock
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE SetScaleTransformBlock(scaleBlockDict, scaleTransformer)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTValueDictionary), POINTER :: scaleBlockDict
         TYPE(ScaleTransform)              :: scaleTransformer
!
!        ---------------
!        Local Variables
!        ---------------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: msg
         REAL(KIND=RP)                           :: s, c(3)
!
!        ----------
!        Interfaces
!        ----------
!
         logical, EXTERNAL :: ReturnOnFatalError
!
!        ------
!        Origin
!        ------
!
         msg = "Scale transform block missing parameter " // TRIM(SCALE_TRANSFORM_ORIGIN_KEY)
        
         CALL SetRealArrayValueFromDictionary(arrayToSet = c, &
                                              sourceDict = scaleBlockDict,                  &
                                              key        = SCALE_TRANSFORM_ORIGIN_KEY,      &
                                              errorLevel = FT_ERROR_FATAL,                  &
                                              message    = msg,                             &
                                              poster     = "SetScaleTransformBlock")
!
!        ------------
!        Scale factor
!        ------------
!
         msg = "Scale transform block missing parameter " // TRIM(SCALE_TRANSFORM_SCALE_KEY)
         CALL SetRealValueFromDictionary(valueToSet = s,                             &
                                         sourceDict = scaleBlockDict,                &
                                         key        = SCALE_TRANSFORM_SCALE_KEY,     &
                                         errorLevel = FT_ERROR_FATAL,                &
                                         message    = msg,                           &
                                         poster     = "SetScaleTransformBlock")
         IF(ReturnOnFatalError()) RETURN
         
         CALL ConstructScaleTransform(self = scaleTransformer,         &
                                      origin = c,                      &
                                      factor = s,                      &
                                      normal = [0.0_RP,0.0_RP,1.0_RP])
         
      END SUBROUTINE SetScaleTransformBlock
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE SetRotationTransformBlock(rotationBlockDict, rotationTransformer)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTValueDictionary), POINTER :: rotationBlockDict
         TYPE(RotationTransform)             :: rotationTransformer
!
!        ---------------
!        Local Variables
!        ---------------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: msg
         REAL(KIND=RP)                           :: t(3), d(3)
!
!        ----------
!        Interfaces
!        ----------
!
         logical, EXTERNAL :: ReturnOnFatalError
!
!        ------
!        Origin
!        ------
!
         msg = "Rotation transform block missing parameter " // TRIM(ROTATION_TRANSFORM_TRANSLATION_KEY)
        
         CALL SetRealArrayValueFromDictionary(arrayToSet = t, &
                                              sourceDict = rotationBlockDict,                 &
                                              key        = ROTATION_TRANSFORM_TRANSLATION_KEY,&
                                              errorLevel = FT_ERROR_FATAL,                  &
                                              message    = msg,                             &
                                              poster     = "SetRotationTransformBlock")
!
!        ------------
!        Direction
!        ------------
!
         msg = "Rotation transform block missing parameter " // TRIM(ROTATION_TRANSFORM_DIRECTION_KEY)
         CALL SetRealArrayValueFromDictionary(arrayToSet = d, &
                                              sourceDict = rotationBlockDict,                 &
                                              key        = ROTATION_TRANSFORM_DIRECTION_KEY,  &
                                              errorLevel = FT_ERROR_FATAL,                  &
                                              message    = msg,                             &
                                              poster     = "SetRotationTransformBlock")
         IF(ReturnOnFatalError()) RETURN
         
         CALL ConstructRotationTransform(self           = rotationTransformer,        &
                                         rotationPoint  = t,                          &
                                         startDirection = [0.0_RP,0.0_RP,1.0_RP],     &
                                         newDirection   = d)

      END SUBROUTINE SetRotationTransformBlock
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE CheckForBoundaryIntersections(sizer)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(MeshSizer), POINTER  :: sizer
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER                               :: k, j
         CLASS(SegmentedCurveArray) , POINTER  :: curveArrayA, curveArrayB
         LOGICAL                               :: intersectionFound, isNested
         CHARACTER(LEN=STRING_CONSTANT_LENGTH) :: msg
         REAL(KIND=RP)                         :: bbox(6)
         
         CALL generateTemporaryBoundaryArrays( sizer )
!
!        -------------------------------------------------
!        See if exterior and any interior curves intersect
!        -------------------------------------------------
!
         IF ( ASSOCIATED( outerBoundaryCurve ) )     THEN
            bbox = outerBoundaryCurve % boundingBox
            
            IF( ASSOCIATED( interiorCurves ) )    THEN         
               DO k = 1, SIZE(interiorCurves)
               
                  curveArrayB => interiorCurves(k) % curveArray
                  isNested = BBoxIsNested(BoxA = bbox, &
                                                   BoxB = curveArrayB % boundingBox)   ! Easy Full curve BBox check
                  IF ( .NOT.isNested )     THEN ! Check further
!
!                    -----------------------
!                    Failure, post exception
!                    -----------------------
!
                     WRITE(msg,*) "Interior curve ", k," overlaps with exterior curve"
                     CALL ThrowErrorExceptionOfType(poster = "CheckForBoundaryIntersections", &
                                                    msg    = msg,                       &
                                                    typ    = FT_ERROR_FATAL) 
                     CALL destroyTemporaryBoundaryArrays
                     RETURN 
                  END IF 
                  
               END DO 
             END IF 
         END IF 
!
!        --------------------------------
!        See if interior curves intersect
!        --------------------------------
!
         IF( ASSOCIATED( interiorCurves ) )    THEN         
            DO k = 1, SIZE(interiorCurves)
               curveArrayA => interiorCurves(k) % curveArray
               
               DO j = k+1, SIZE(interiorCurves) 
               
                  curveArrayB => interiorCurves(j) % curveArray
                  intersectionFound = BBoxIntersects(BoxA = curveArrayA % boundingBox, &
                                                     BoxB = curveArrayB % boundingBox)   ! Easy Full curve BBox check
                  IF ( intersectionFound )     THEN ! Check further

!
!                    -----------------------
!                    Failure, post exception
!                    -----------------------
!
                     WRITE(msg,*) "Interior curves ", k," and ", j, "overlap"
                     CALL ThrowErrorExceptionOfType(poster = "CheckForBoundaryIntersections", &
                                                    msg    = msg,                       &
                                                    typ    = FT_ERROR_FATAL) 
                     CALL destroyTemporaryBoundaryArrays
                     RETURN 
                  END IF 
               END DO 
            END DO 
         END IF 
         
         CALL destroyTemporaryBoundaryArrays
         
      END SUBROUTINE CheckForBoundaryIntersections

   END MODULE MeshProjectClass



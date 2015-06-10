!
!////////////////////////////////////////////////////////////////////////
!
!      ProgramGlobals.f90
!      Created: 2010-09-01 09:06:30 -0400 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module ProgramGlobals
         USE SMConstants
         IMPLICIT NONE
!
!        -----------------
!        Generic constants
!        -----------------
!
         INTEGER, PARAMETER :: NONE      = 0
         LOGICAL, PARAMETER :: FAIL      = .false.
         INTEGER, PARAMETER :: UNDEFINED = 0
         INTEGER, PARAMETER :: ON        = 1, OFF = 0
!
!        -----------------------------
!        Constants indicating location
!        -----------------------------
!
         INTEGER, PARAMETER :: INNER    =  2 , OUTER     =  1, INTERIOR_INTERFACE =  3
         INTEGER, PARAMETER :: TOP      = -1,  BOTTOM    = -2, LEFT               = -3, RIGHT      = -4, OUTER_BOX = -5
         INTEGER, PARAMETER :: BBOX_TOP =  1 , BBOX_LEFT =  2, BBOX_BOTTOM        =  3, BBOX_RIGHT =  4
!
!        -----------------------------
!        Node classification constants
!        -----------------------------
!
         INTEGER, PARAMETER :: ROW_END = 1, ROW_SIDE = 2, ROW_REVERSAL = 3, CORNER_NODE = 4, ROW_CORNER = 5
!
!        ---------------------------
!        Meshing operation constants
!        ---------------------------
!
         INTEGER, PARAMETER :: REFINEMENT_3 = 3, REFINEMENT_2 = 2
         INTEGER, PARAMETER :: REMOVE_HANGING_NODES_OPERATION = 0, FLATTEN_NODE_LEVELS_OPERATION = 1
         INTEGER, PARAMETER :: INACTIVE = 0, ACTIVE = 1, REMOVE = 2
         INTEGER, PARAMETER :: SIMPLE_EXTRUSION_ALGORITHM = 0, SIMPLE_ROTATION_ALGORITHM = 1
!
!        -------------------------
!        Mesh formatting constants
!        -------------------------
!
         INTEGER, PARAMETER :: BASIC_MESH_FORMAT = 1, BASIC_PLUS_EDGES_FORMAT = 2, ISM = 3, ISM2 = 4, ISM_MM = 5
!
!        ----------------------
!        Element type constants
!        ----------------------
!
         INTEGER, PARAMETER :: QUAD = 4, HEX = 8
!
!        ----------------
!        String constants
!        ----------------
!
         INTEGER         , PARAMETER :: LINE_LENGTH            = 256
         INTEGER         , PARAMETER :: STRING_CONSTANT_LENGTH = 64
         CHARACTER(LEN=3), PARAMETER :: NO_BC_STRING           = "---"
!
!        -----------
!        Preferences
!        -----------
!
         INTEGER, PARAMETER :: MAX_VALENCE    = 8
         INTEGER, PARAMETER :: numCurvePoints = 100
         
         INTEGER       :: minNumberOfElementsInsideArea = 6
         REAL(KIND=RP) :: curvatureFactor         = 2.0_RP
         REAL(KIND=RP) :: curveSubdivisionFactor  = 15.0_RP
         REAL(KIND=RP) :: minimizationTolerance   = 1.0d-5
         REAL(KIND=RP) :: edgeLengthFactor        = 0.3_RP
         REAL(KIND=RP) :: maxParameterChange      = 0.5_RP 
         REAL(KIND=RP) :: parameterDelta          = 0.2_RP
         REAL(KIND=RP) :: subdivisionRelTol       = 0.05_RP
         REAL(KIND=RP) :: directionPenalty        = 1.d-4
!
!        --------------------
!        For printing history
!        --------------------
!
         LOGICAL :: printMessage = .false.
!
!        ----------------------
!        For close curve sizing
!        ----------------------
!
         REAL(KIND=RP) :: closeCurveFactor          = 3.25_RP  ! Should be around 3
         REAL(KIND=RP) :: closeCurveNormalAlignment = 0.8_RP   ! Dot product near 1
         
         INTEGER       :: refinementType          = 2
         INTEGER       :: boundarySmoothingPasses = 1

!        ========
         CONTAINS
!        ========
!         
!
!////////////////////////////////////////////////////////////////////////
!
         SUBROUTINE LoadPreferences( fUnit ) 
            IMPLICIT NONE
            INTEGER :: fUnit
            READ(fUnit,*) curvatureFactor      , curveSubdivisionFactor
            READ(fUnit,*) minimizationTolerance, edgeLengthFactor
            READ(fUnit,*) maxParameterChange   , parameterDelta, directionPenalty
            READ(fUnit,*) closeCurveFactor     , closeCurveNormalAlignment
            READ(fUnit,*) refinementType       , boundarySmoothingPasses
            READ(fUnit,*) printMessage         , minNumberOfElementsInsideArea
         END SUBROUTINE LoadPreferences
!
!////////////////////////////////////////////////////////////////////////
!
         SUBROUTINE WriteDefaultPreferences( fUnit ) 
            IMPLICIT NONE
            INTEGER :: fUnit
            WRITE(fUnit,*) curvatureFactor      , curveSubdivisionFactor
            WRITE(fUnit,*) minimizationTolerance, edgeLengthFactor
            WRITE(fUnit,*) maxParameterChange   , parameterDelta, directionPenalty
            WRITE(fUnit,*) closeCurveFactor     , closeCurveNormalAlignment
            WRITE(fUnit,*) refinementType       , boundarySmoothingPasses
            WRITE(fUnit,*) printMessage         , minNumberOfElementsInsideArea
         END SUBROUTINE WriteDefaultPreferences
         
      END Module ProgramGlobals
      
! MIT License
!
! Copyright (c) 2010-present David A. Kopriva and other contributors: AUTHORS.md
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
! HOHQMesh contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend,
!    https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
! * `fmin`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler,
!    Computer Methods for Mathematical Computations, 1977
! * `spline`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler,
!    Computer Methods for Mathematical Computations, 1977
! * `seval`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler,
!    Computer Methods for Mathematical Computations, 1977
!
! --- End License
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
         INTEGER, PARAMETER :: ON        = 1, OFF = 0, NOT_APPLICABLE = -1
!
!        -----------------------------
!        Constants indicating location
!        -----------------------------
!
         INTEGER, PARAMETER :: INNER    =  2 , OUTER     =  1, INTERIOR_INTERFACE =  3, SWEEP_CURVE = -1
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
         INTEGER, PARAMETER :: SIMPLE_EXTRUSION_ALGORITHM = 0, SIMPLE_ROTATION_ALGORITHM = 1, SWEEP_ALGORITHM = 2
!
!        -------------------------
!        Mesh formatting constants
!        -------------------------
!
         INTEGER, PARAMETER :: BASIC_MESH_FORMAT = 1, BASIC_PLUS_EDGES_FORMAT = 2, ISM = 3, ISM2 = 4, ISM_MM = 5, ABAQUS = 6
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
         INTEGER, PARAMETER :: numCurvePoints = 500

         INTEGER       :: minNumberOfElementsInsideArea = 4
         REAL(KIND=RP) :: curvatureFactor         = 2.0_RP
         REAL(KIND=RP) :: curveSubdivisionFactor  = 15.0_RP
         REAL(KIND=RP) :: minimizationTolerance   = 1.0d-7
         REAL(KIND=RP) :: edgeLengthFactor        = 0.15_RP
         REAL(KIND=RP) :: maxParameterChange      = 0.5_RP
         REAL(KIND=RP) :: parameterDelta          = 0.2_RP
         REAL(KIND=RP) :: subdivisionRelTol       = 0.05_RP
         REAL(KIND=RP) :: directionPenalty        = 1.0d-4
         REAL(KIND=RP) :: boundingBoxOverlapTol   = 1.0d-5
         LOGICAL       :: boundarySlipping        = .FALSE.
         INTEGER       :: maxLevelLimit           = 8      ! allow at most maxLevelLimit levels of subdivision
!
!        --------------------
!        For printing history
!        --------------------
!
         LOGICAL :: printMessage = .FALSE.
!
!        ----------------------
!        For close curve sizing
!        ----------------------
!
         REAL(KIND=RP) :: closeCurveFactor          = 3.25_RP  ! Should be around 3
         REAL(KIND=RP) :: closeCurveNormalAlignment = 0.7_RP   ! Dot product near 1. This used to be 0.8
         REAL(KIND=RP) :: normalTangentMin          = 0.01_RP  ! Dot of normal and tangent should be zero. Use this instead.

         INTEGER       :: refinementType          = 2
         INTEGER       :: boundarySmoothingPasses = 1
!
!        -----------
!        Error Codes
!        -----------
!
         INTEGER, PARAMETER :: A_OK_ERROR_CODE                  = 0
         INTEGER, PARAMETER :: VALENCE_TOO_HIGH_ERROR_CODE      = 1
         INTEGER, PARAMETER :: CURVE_NOT_FOUND_ERROR_CODE       = 2
         INTEGER, PARAMETER :: UNASSOCIATED_POINTER_ERROR_CODE  = 3

      END Module ProgramGlobals

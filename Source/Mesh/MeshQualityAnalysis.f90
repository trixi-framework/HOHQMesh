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
!      MeshQualityAnalysis.f90
!      Created: 2011-05-19 08:58:04 -0400
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
      Module MeshQualityAnalysisClass
      USE SMMeshClass
      USE HexMeshObjectsModule
      IMPLICIT NONE
!
!-------------------------------------------------------------------
!> Compute standard mesh quality measures. Optionally write them out
!! to a file for graphical analysis. Mesh Quality meqasures are
!! taken from "The Verdict Library Reference Manual" by Stimpson et al.
!-------------------------------------------------------------------
!
!     Mesh quality is measured for Quad and Hex meshes
!
      INTEGER, PARAMETER :: QUAD_STATISTICS = 1, HEX_STATISTICS = 2
!
!     ------------------------------------------------------------------
!     Statistics storage
!        statsType indicates whether the measures are for Quads or Hexes
!     ------------------------------------------------------------------
!
      TYPE MeshStatistics
         INTEGER                                      :: statsType
         REAL(KIND=RP)    , DIMENSION(:), ALLOCATABLE :: maxValues
         REAL(KIND=RP)    , DIMENSION(:), ALLOCATABLE :: minValues
         REAL(KIND=RP)    , DIMENSION(:), ALLOCATABLE :: avgValues
      END TYPE MeshStatistics

      INTERFACE ComputeMeshQualityStatistics
         MODULE PROCEDURE :: ComputeMeshQualityStatistics2D
         MODULE PROCEDURE :: ComputeMeshQualityStatistics3D
      END INTERFACE ComputeMeshQualityStatistics

      INTERFACE OutputMeshQualityMeasures
         MODULE PROCEDURE :: OutputMeshQualityMeasures2D
      END INTERFACE OutputMeshQualityMeasures

      PRIVATE :: Cross3D, Dot3D, Norm3D, Normalize3D
!
!////////////////////////////////////////////////////////////////////////
!
!                 2D Shape Measures
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER, PARAMETER :: SIGNED_AREA_INDEX = 1, ASPECT_RATIO_INDEX = 2, &
                            CONDITION_INDEX   = 3, EDGE_RATIO_INDEX   = 4, &
                            JACOBIAN_INDEX    = 5, MIN_ANGLE_INDEX    = 6, &
                            MAX_ANGLE_INDEX   = 7, AREA_SIGN          = 8

      REAL(KIND=RP), PARAMETER, PRIVATE :: FUDGE_FACTOR_HIGH = 1.1_RP, FUDGE_FACTOR_LOW = 0.9
!
!     ----------------------
!     Quality measure arrays
!     ----------------------
!
      INTEGER, PARAMETER :: NUMBER_OF_2D_SHAPE_MEASURES = 8

      CHARACTER(LEN=16), DIMENSION(NUMBER_OF_2D_SHAPE_MEASURES) :: &
              measureNames = (/  "Signed Area     ", &
                                 "Aspect Ratio    ", &
                                 "Condition       ", &
                                 "Edge Ratio      ", &
                                 "Jacobian        ", &
                                 "Minimum Angle   ", &
                                 "Maximum Angle   ", &
                                 "Area Sign       "  &
                             /)

      REAL(KIND=RP)    , DIMENSION(NUMBER_OF_2D_SHAPE_MEASURES) :: &
              refValues     = (/  1.0_RP , &
                                  1.0_RP , &
                                  1.0_RP , &
                                  1.0_RP , &
                                  1.0_RP , &
                                  90.0_RP, &
                                  90.0_RP, &
                                   1.0_RP  &
                             /)

      REAL(KIND=RP)    , DIMENSION(NUMBER_OF_2D_SHAPE_MEASURES) :: &
              acceptableLow = (/  0.0_RP , &
                                  1.0_RP , &
                                  1.0_RP , &
                                  1.0_RP , &
                                  0.0_RP , &
                                  40.0_RP, &
                                  90.0_RP,  &
                                   1.0_RP  &
                             /)

      REAL(KIND=RP)    , DIMENSION(NUMBER_OF_2D_SHAPE_MEASURES) :: &
              acceptableHigh = (/  999.999_RP , &
                                   999.999_RP , &
                                   4.0_RP     , &
                                   4.0_RP     , &
                                   999.999_RP , &
                                   90.0_RP    , &
                                   135.0_RP   , &
                                     1.0_RP     &
                             /)
!
!
!////////////////////////////////////////////////////////////////////////
!
!                 3D Shape Measures
!
!////////////////////////////////////////////////////////////////////////
!
   INTEGER, PARAMETER :: NUMBER_OF_3D_SHAPE_MEASURES = 6

   INTEGER, PARAMETER :: DIAGONAL3D_INDEX = 1, EDGE_RATIO3D_INDEX = 2, &
                         JACOBIAN3D_INDEX = 3, SHAPE3D_INDEX      = 4, &
                         SKEW3D_INDEX     = 5, VOLUME3D_INDEX     = 6

   CHARACTER(LEN=16), DIMENSION(NUMBER_OF_3D_SHAPE_MEASURES) :: &
           shapeMeasureNames3D = [ "Diagonal        ", &
                                   "Edge Ratio      ", &
                                   "Jacobian        ", &
                                   "Shape           ", &
                                   "Skew            ", &
                                   "Volume          "  &
                                 ]

   REAL(KIND=RP)    , DIMENSION(NUMBER_OF_3D_SHAPE_MEASURES) :: &
           refValues3D = [  1.0_RP , &
                            1.0_RP , &
                            1.0_RP , &
                            1.0_RP , &
                            0.0_RP , &
                            1.0_RP   &
                          ]

      REAL(KIND=RP)    , DIMENSION(NUMBER_OF_3D_SHAPE_MEASURES) :: &
              acceptableLow3D = [ 0.65_RP , &
                                  1.0_RP , &
                                  0.0_RP , &
                                  0.3_RP , &
                                  0.0_RP , &
                                  0.0_RP   &
                                ]

      REAL(KIND=RP)    , DIMENSION(NUMBER_OF_3D_SHAPE_MEASURES) :: &
              acceptableHigh3D = (/  1.0_RP     , &
                                   999.999_RP , &
                                   999.999_RP , &
                                   1.0_RP     , &
                                   0.5_RP     , &
                                   999.999_RP   &
                             /)


      INTERFACE OPERATOR (.cross.)
         MODULE PROCEDURE CrossProduct
      END INTERFACE
      PRIVATE :: CrossProduct
!
! note1: will have to change for nonplanar geometries
!
!     ========
      CONTAINS
!     ========
!
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DestructMeshStatistics(stats)
         IMPLICIT NONE
         TYPE(MeshStatistics) :: stats
         DEALLOCATE( STATs % avgValues)
         DEALLOCATE( stats % maxValues)
         DEALLOCATE( stats % minValues)
         stats % statsType = UNDEFINED
      END SUBROUTINE DestructMeshStatistics
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructMeshStatistics( stats, statsType )
         IMPLICIT NONE
         TYPE(MeshStatistics) :: stats
         INTEGER              :: statsType
         INTEGER              :: numStats

         IF ( ALLOCATED(STATs % avgValues) )     THEN
            CALL DestructMeshStatistics(stats)
         END IF

         stats % statsType = statsType
         IF ( statsType == QUAD_STATISTICS )     THEN
            numStats =  NUMBER_OF_2D_SHAPE_MEASURES
         ELSE
            numStats = NUMBER_OF_3D_SHAPE_MEASURES
         END IF

         ALLOCATE(stats % avgValues(numStats))
         ALLOCATE(stats % minValues(numStats))
         ALLOCATE(stats % maxValues(numStats))

         stats % avgValues  = 0.0_RP
         stats % maxValues  = 0.0_RP
         stats % minValues  = HUGE(1.0_RP)

      END SUBROUTINE ConstructMeshStatistics
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeMeshQualityStatistics2D( stats, mesh )
!
!     ------------------------------------------------------------------
!     Compute mesh quality for each element, print the elemental values
!     and accumulate the statistics.
!     ------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(MeshStatistics)          :: stats
         TYPE (SMMesh)       , POINTER :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedListIterator), POINTER :: elementIterator => NULL()
         CLASS(SMElement)           , POINTER :: e               => NULL()
         CLASS(FTObject)            , POINTER :: obj             => NULL()
         REAL(KIND=RP)                        :: shapeMeasures(NUMBER_OF_2D_SHAPE_MEASURES)
         INTEGER                              :: k, nValues

         CALL ConstructMeshStatistics(stats, statsType = QUAD_STATISTICS)

         elementIterator => mesh % elementsIterator
         CALL elementIterator % setToStart()

         nValues = 0
         DO WHILE ( .NOT.elementIterator % isAtEnd() )
            obj => elementIterator % object()
            CALL cast(obj,e)
            CALL ComputeElementShapeMeasures2D( e, shapeMeasures )

            DO k = 1, NUMBER_OF_2D_SHAPE_MEASURES
               stats % avgValues(k) = stats % avgValues(k) + shapeMeasures(k)
               stats % maxValues(k) = MAX( stats % maxValues(k), shapeMeasures(k) )
               stats % minValues(k) = MIN( stats % minValues(k), shapeMeasures(k) )
            END DO

            CALL elementIterator % moveToNext()
            NValues = nValues + 1
         END DO
         stats % avgValues = stats % avgValues/NValues

      END SUBROUTINE ComputeMeshQualityStatistics2D
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeMeshQualityStatistics3D( stats, mesh )
!
!     ------------------------------------------------------------------
!     Compute mesh quality for each element, print the elemental values
!     and accumulate the statistics.
!     ------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(MeshStatistics)              :: stats
         TYPE (StructuredHexMesh), POINTER :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP) :: shapeMeasures(NUMBER_OF_3D_SHAPE_MEASURES)
         INTEGER       :: k, nValues, lev, j, globalNodeID
         INTEGER       :: nodeLoc,nodeLev
         REAL(KIND=RP) :: P(3,0:7)

         CALL ConstructMeshStatistics(stats, statsType = HEX_STATISTICS)

         nValues = 0
         DO lev = 1, mesh % numberofLayers
            DO j = 1, mesh % numberOfQuadElements
               DO k = 1, 8
                  globalNodeID = mesh % elements(j,lev) % nodeIDs(k)
                  nodeLoc      = mesh % locAndLevelForNodeID(1,globalNodeID)
                  nodeLev      = mesh % locAndLevelForNodeID(2,globalNodeID)
                  P(:,k-1) = mesh % nodes(nodeLoc,nodeLev) % x
               END DO
               CALL Compute3DShapeMeasures(P,shapeMeasures)

               DO k = 1, NUMBER_OF_3D_SHAPE_MEASURES
                  stats % avgValues(k) = stats % avgValues(k) + shapeMeasures(k)
                  stats % maxValues(k) = MAX( stats % maxValues(k), shapeMeasures(k) )
                  stats % minValues(k) = MIN( stats % minValues(k), shapeMeasures(k) )
               END DO

               NValues = nValues + 1
            END DO
         END DO
         stats % avgValues = stats % avgValues/NValues

      END SUBROUTINE ComputeMeshQualityStatistics3D
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE OutputMeshQualityMeasures2D( mesh, fUnit )
!
!     ------------------------------------------------------------------
!     Compute mesh quality for each element, print the elemental values
!     and accumulate the statistics.
!     ------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh), POINTER :: mesh
         INTEGER               :: fUnit
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedListIterator), POINTER :: elementIterator => NULL()
         CLASS(SMElement)           , POINTER :: e => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()
         REAL(KIND=RP)                        :: shapeMeasures(NUMBER_OF_2D_SHAPE_MEASURES)
         CHARACTER(LEN=16)                    :: namesFmt = "(   8A16 )", valuesFmt = '(  8F16.3)', numb = "10"

         WRITE(numb,FMT='(I3)') NUMBER_OF_2D_SHAPE_MEASURES
         namesFmt  = "(" // TRIM(numb) // "A16)"
         valuesFmt = "(" // TRIM(numb) // "(1PE16.4))"

         WRITE(fUnit,namesFmt) measureNames

         elementIterator => mesh % elementsIterator
         CALL elementIterator % setToStart()

         DO WHILE ( .NOT.elementIterator % isAtEnd() )
            obj => elementIterator % object()
            CALL cast(obj,e)
            CALL ComputeElementShapeMeasures2D( e, shapeMeasures )
            WRITE( fUnit,valuesFmt) shapeMeasures

            CALL elementIterator % moveToNext()
         END DO

      END SUBROUTINE OutputMeshQualityMeasures2D
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeElementShapeMeasures2D( e, shapeMeasures )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMElement)  , POINTER :: e
         REAL(KIND=RP)               :: shapeMeasures(NUMBER_OF_2D_SHAPE_MEASURES)
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP) :: P(3,4)         ! Location of the four corners
         REAL(KIND=RP) :: L(3,4)         ! Sequential edge vectors
         REAL(KIND=RP) :: LNorm(4)       ! lengths of the edge vectors
         REAL(KIND=RP) :: LMin, LMax     ! largest and smallest edge lengths
         REAL(KIND=RP) :: D1(3), D2(3)   ! Diagonals
         REAL(KIND=RP) :: DMax, DMin     ! Largest and smallest diagonal lengths
         REAL(KIND=RP) :: X1(3), X2(3)   ! Principle Axes
         REAL(KIND=RP) :: N(4)           ! Corner normal directions (not normalized).  See note1
         REAL(KIND=RP) :: nHat(4)        ! Corner normals. See note1
         REAL(KIND=RP) :: Nc, nHatC      ! center normals. See note1
         REAL(KIND=RP) :: alpha(4)       ! vertex areas
         REAL(KIND=RP) :: alphaMin
         REAL(KIND=RP) :: area           ! signed area of the quad
         REAL(KIND=RP) :: condition      ! condition NUMBER
         REAL(KIND=RP) :: angles(4)      ! The 4 angles in the quad
         REAL(KIND=RP) :: s

         INTEGER       :: loop(0:5) = (/4,1,2,3,4,1/)
         INTEGER       :: k
         LOGICAL       :: rightHanded
!
!        **************************************************************
!                                 Setup
!        **************************************************************
!
!        ------------------
!        Grab the locations
!        ------------------
!
         DO k = 1, 4
            P(:,k) = e % nodes(k) % node % x
         END DO
!
!        ----------------------------
!        Compute lengths of the sides
!        and the diagonals
!        ----------------------------
!
         DO k = 1,4
            L(:,k)   = P(:,loop(k+1)) - P(:,k)
            LNorm(k) = Norm2(L(:,k))
         END DO
         LMax = MAXVAL(LNorm)
         LMin = MINVAL(LNorm)

         D1   = P(:,3) - P(:,1)
         D2   = P(:,4) - P(:,2)
         DMax = MAX( Norm2(D1), Norm2(D2) )
         DMin = MIN( Norm2(D1), Norm2(D2) )
!
!        --------------
!        Principle Axes
!        --------------
!
         X1 = (P(:,2) - P(:,1)) + (P(:,3) - P(:,4))
         X2 = (P(:,3) - P(:,2)) + (P(:,4) - P(:,1))
!
!        -------
!        Normals
!        -------
!
         DO k = 1,4
            N(k)    = L(:,loop(k-1)).cross.L(:,k)
            NHat(k) = 1.0_RP !  See note1
         END DO
         Nc    = X1.cross.X2
         nHatC = 1.0_RP      !  See note1
!
!        -----
!        Areas
!        -----
!
         DO k = 1, 4
            alpha(k) = N(k)  !  See note1
         END DO
!
!        **************************************************************
!                         Compute shape measures now
!        **************************************************************
!
!        -----------
!        Signed Area
!        -----------
!
         area                             = 0.25_RP*SUM(alpha)
         shapeMeasures(SIGNED_AREA_INDEX) = area
!
!        ------------
!        Aspect Ratio
!        ------------
!
         shapeMeasures(ASPECT_RATIO_INDEX) = LMax*SUM(LNorm)/(4.0_RP*area)
!
!        ----------------
!        Condition number
!        ----------------
!
         alphaMin      = MINVAL(alpha)
         IF( alphaMin <= TINY(alphaMin) )     THEN
            condition = HUGE(condition)
         ELSE
            condition = 0.5_RP*MAX( (LNorm(1)**2 + LNorm(4)**2)/alpha(1), &
                                    (LNorm(2)**2 + LNorm(1)**2)/alpha(2), &
                                    (LNorm(3)**2 + LNorm(2)**2)/alpha(3), &
                                    (LNorm(4)**2 + LNorm(3)**2)/alpha(4) )
         END IF
         shapeMeasures(CONDITION_INDEX) = condition
!
!        ----------
!        Edge Ratio
!        ----------
!
         shapeMeasures(EDGE_RATIO_INDEX) = LMax/LMin
!
!        --------
!        Jacobian
!        --------
!
         shapeMeasures(JACOBIAN_INDEX) = MINVAL(alpha)
!
!        -----------------------------------
!        Sign of the area (Gives handedness)
!        -----------------------------------
!
         s = 0.0_RP
         DO k = 1, 4
            s = s + P(1,k)*P(2,Loop(k+1)) - P(1,Loop(k+1))*P(2,k)
         END DO
         shapeMeasures(AREA_SIGN) = SIGN(1.0_RP,s)
         IF ( shapeMeasures(AREA_SIGN) > 0.0_RP )     THEN
            rightHanded = .true.
         ELSE
            rightHanded = .false.
         END IF
!
!        ---------------------------------------------------
!        Maximum/Minimum Angle - Assume positive orientation
!        ---------------------------------------------------
!
         CALL ElementAngles(e, angles, rightHanded)
         shapeMeasures(MIN_ANGLE_INDEX) = MINVAL(angles)
         shapeMeasures(MAX_ANGLE_INDEX) = MAXVAL(angles)

      END SUBROUTINE ComputeElementShapeMeasures2D
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ElementAngles( e, angles, rightHanded )
!
!     ------------------------------------------------------------
!     Compute the interior angles of each of the 4 element corners
!     ------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMElement), POINTER :: e
         REAL(KIND=RP)             :: angles(4)
         LOGICAL                   :: rightHanded
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP) :: P(3,4)         ! Location of the four corners
         REAL(KIND=RP) :: L(3,4)         ! Sequential edge vectors
         REAL(KIND=RP) :: LNorm(4)       ! lengths of the edge vectors
         REAL(KIND=RP) :: theta
         REAL(KIND=RP) :: r              ! positive for right handed element
         REAL(KIND=RP) :: c              ! Cross product result

         INTEGER       :: k
         INTEGER       :: loop(0:5) = (/4,1,2,3,4,1/)

         IF ( rightHanded )     THEN
            r = 1.0_RP
         ELSE
            r = -1.0_RP
         END IF

!
!        ------------------
!        Grab the locations
!        ------------------
!
         DO k = 1, 4
            P(:,k) = e % nodes(k) % node % x
         END DO
!
!        ----------------------------
!        Compute lengths of the sides
!        ----------------------------
!
         DO k = 1,4
            L(:,k)   = P(:,loop(k+1)) - P(:,k)
            LNorm(k) = Norm2(L(:,k))
         END DO
!
!        ------------------
!        Compute the angles
!        ------------------
!
         DO k = 1, 4
            theta = ACOS( -(L(1,k)*L(1,loop(k+1)) + L(2,k)*L(2,loop(k+1)))/&
                           (LNorm(k)*LNorm(loop(k+1))))*180.0_RP/PI
!
!           --------------------------
!           Check if angle is external
!           --------------------------
!
            c = L(:,k).cross.L(:,loop(k+1))
            IF( c*r < 0.0_RP )     THEN
               theta = 360.0_RP - c
            END IF

            angles(loop(k+1)) = theta !angle between edges k & k+1 <=> node k.

         END DO

      END SUBROUTINE ElementAngles
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION BadElementsInMesh( mesh ) RESULT(array)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SMMesh)              , POINTER :: mesh
         CLASS(FTMutableObjectArray), POINTER :: array
!
!        ---------------
!        Local Variables
!        ---------------
!
         CLASS(SMElement)           , POINTER :: e => NULL()
         CLASS(FTLinkedListIterator), POINTER :: elementIterator => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()

         elementIterator => mesh % elementsIterator
         ALLOCATE(array)
         CALL array % initWithSize(100)

         CALL elementIterator % setToStart()

         DO WHILE ( .NOT.elementIterator % isAtEnd() )
            obj => elementIterator % object()
            CALL cast(obj,e)

            IF( elementIsBad(e) )  CALL array % addObject(obj)

            CALL elementIterator % moveToNext()
         END DO

         IF ( array % COUNT() == 0 )     THEN
            DEALLOCATE(array)
            array => NULL()
         END IF

      END FUNCTION BadElementsInMesh
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION elementIsBad(e)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMElement), POINTER :: e
!
!        ---------------
!        Local Variables
!        ---------------
!
         REAL(KIND=RP) :: shapeMeasures(NUMBER_OF_2D_SHAPE_MEASURES)
         INTEGER       :: k

         CALL ComputeElementShapeMeasures2D( e, shapeMeasures )

         elementIsBad = .false.
         DO k = 1, NUMBER_OF_2D_SHAPE_MEASURES
            IF( shapeMeasures(k) < FUDGE_FACTOR_LOW*acceptableLow(k) .OR. &
                shapeMeasures(k) > FUDGE_FACTOR_HIGH*acceptableHigh(k) )     THEN
               elementIsBad = .true.
               EXIT
            END IF
         END DO

      END FUNCTION elementIsBad
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ExtractBadElementInfo( shapeMeasures, info )
!
!     -----------------------------------------
!     Flag which criteria are bad on an element
!     -----------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         LOGICAL       :: info(NUMBER_OF_2D_SHAPE_MEASURES)
         REAL(KIND=RP) :: shapeMeasures(NUMBER_OF_2D_SHAPE_MEASURES)
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER       :: k

         info = .false.
         DO k = 1, NUMBER_OF_2D_SHAPE_MEASURES
            IF( shapeMeasures(k) < FUDGE_FACTOR_LOW*acceptableLow(k) .OR. &
                shapeMeasures(k) > FUDGE_FACTOR_HIGH*acceptableHigh(k) )     THEN
                info(k) = .true.
            END IF
         END DO

      END SUBROUTINE ExtractBadElementInfo
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PrintBadElementInfo( e, fUnit )
         USE SMMeshObjectsModule
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMElement), POINTER :: e
         INTEGER                   :: fUnit
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER                  :: k
         REAL(KIND=RP)            :: shapeMeasures(NUMBER_OF_2D_SHAPE_MEASURES)
         LOGICAL                  :: info(NUMBER_OF_2D_SHAPE_MEASURES)

         CALL ComputeElementShapeMeasures2D( e, shapeMeasures )
         CALL ExtractBadElementInfo( shapeMeasures, info )

         WRITE( fUnit, *) "Element ", e % id

         DO k = 1,4
            WRITE( fUnit, *) "      ", e % nodes(k) % node % x
         END DO

         WRITE( fUnit, *) "Problems:"
         DO k = 1, NUMBER_OF_2D_SHAPE_MEASURES
            IF( info(k) ) WRITE( fUnit, *) "      ", measureNames(k), shapeMeasures(k)
         END DO
         WRITE( fUnit, *) " "
         WRITE( funit, * ) "//////////////////////////////////////////////////"

      END SUBROUTINE PrintBadElementInfo
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION CrossProduct( u, v ) RESULT(c)
      IMPLICIT NONE
      REAL(KIND=RP), INTENT(IN)  :: u(3), v(3)
      REAL(KIND=RP)              :: c

      c = u(1)*v(2) - v(1)*u(2)

      END FUNCTION CrossProduct
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE Compute3DShapeMeasures(P, shapeMeasures)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP) :: P(3,0:7)
         REAL(KIND=RP) :: shapeMeasures(NUMBER_OF_3D_SHAPE_MEASURES)
!
!        ---------------
!        Local Variables
!        ---------------
!
         REAL(KIND=RP) :: L(3,0:11)
         REAL(KIND=RP) :: X(3,3)
         REAL(KIND=RP) :: A(3,3,0:8)
         REAL(KIND=RP) :: alpha(0:8)
!
!        -----
!        Setup
!        -----
!
         CALL ComputeLVectors(P,L)
         CALL ComputeXVectors(P,X)
         CALL ComputeHexShapeMatrices(P,L,X,A)
         CALL ComputeAlphaVector(A,alpha)
!
!        -----
!        Tests
!        -----
!
         shapeMeasures(DIAGONAL3D_INDEX)   = DiagonalMeasure(P)
         shapeMeasures(EDGE_RATIO3D_INDEX) = EdgeRatio3D(L)
         shapeMeasures(JACOBIAN3D_INDEX)   = Jacobian3D(alpha)
         shapeMeasures(SHAPE3D_INDEX)      = Shape3D(alpha,A)
         shapeMeasures(SKEW3D_INDEX)       = Skew3D(X)
         shapeMeasures(VOLUME3D_INDEX)     = Volume3D(alpha)


      END SUBROUTINE Compute3DShapeMeasures
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeLVectors(P,L)
         IMPLICIT NONE
         REAL(KIND=RP) :: P(3,0:7)
         REAL(KIND=RP) :: L(3,0:11)

         L(:,0)  = P(:,1) - P(:,0)
         L(:,1)  = P(:,2) - P(:,1)
         L(:,2)  = P(:,3) - P(:,2)
         L(:,3)  = P(:,3) - P(:,0)
         L(:,4)  = P(:,4) - P(:,0)
         L(:,5)  = P(:,5) - P(:,1)
         L(:,6)  = P(:,6) - P(:,2)
         L(:,7)  = P(:,7) - P(:,3)
         L(:,8)  = P(:,5) - P(:,4)
         L(:,9)  = P(:,6) - P(:,5)
         L(:,10) = P(:,7) - P(:,6)
         L(:,11) = P(:,7) - P(:,4)

      END SUBROUTINE ComputeLVectors
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION LMax3D(L)
         IMPLICIT NONE
         REAL(KIND=RP) :: L(3,0:11)
         INTEGER       :: i
         REAL(KIND=RP) :: d

         LMax3D = 0.0_RP
         DO i = 0,11
            CALL Norm3D(u = L(:,i),norm = d)
            LMax3D = MAX(LMax3D, d)
         END DO

      END FUNCTION LMax3D
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION LMin3D(L)
         IMPLICIT NONE
         REAL(KIND=RP) :: L(3,0:11)
         INTEGER       :: i
         REAL(KIND=RP) :: d

         LMin3D = HUGE(LMin3D)
         DO i = 0,11
            CALL Norm3D(u = L(:,i),norm = d)
            LMin3D = MIN(LMin3D, d)
         END DO

      END FUNCTION LMin3D
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeXVectors(P,X)
         IMPLICIT NONE
         REAL(KIND=RP) :: P(3,0:7)
         REAL(KIND=RP) :: X(3,3)

         X(:,1) = (P(:,1) - P(:,0)) + (P(:,2) - P(:,3)) + &
                  (P(:,5) - P(:,4)) + (P(:,6) - P(:,7))
         X(:,2) = (P(:,3) - P(:,0)) + (P(:,2) - P(:,1)) + &
                  (P(:,7) - P(:,4)) + (P(:,6) - P(:,5))
         X(:,3) = (P(:,4) - P(:,0)) + (P(:,5) - P(:,1)) + &
                  (P(:,6) - P(:,2)) + (P(:,7) - P(:,3))

      END SUBROUTINE ComputeXVectors
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeHexShapeMatrices(P, L, X, A)
      IMPLICIT NONE
!
!        ----------
!        Arguments
!        ----------
!
         REAL(KIND=RP) :: P(3,0:7)
         REAL(KIND=RP) :: L(3,0:11)
         REAL(KIND=RP) :: X(3,3)
         REAL(KIND=RP) :: A(3,3,0:8)
!
!        -------------------
!        Fill the A matrices
!        -------------------
!
         A(:,1,0) = L(:,0)
         A(:,2,0) = L(:,3)
         A(:,3,0) = L(:,4)

         A(:,1,1) =  L(:,1)
         A(:,2,1) = -L(:,0)
         A(:,3,1) =  L(:,5)

         A(:,1,2) =  L(:,2)
         A(:,2,2) = -L(:,1)
         A(:,3,2) =  L(:,6)

         A(:,1,3) = -L(:,3)
         A(:,2,3) = -L(:,2)
         A(:,3,3) =  L(:,7)

         A(:,1,4) =  L(:,11)
         A(:,2,4) =  L(:,8)
         A(:,3,4) = -L(:,4)

         A(:,1,5) = -L(:,8)
         A(:,2,5) =  L(:,9)
         A(:,3,5) = -L(:,5)

         A(:,1,6) = -L(:,9)
         A(:,2,6) =  L(:,10)
         A(:,3,6) = -L(:,6)

         A(:,1,7) = -L(:,10)
         A(:,2,7) = -L(:,11)
         A(:,3,7) = -L(:,7)

         A(:,1,8) =  X(:,1)
         A(:,2,8) =  X(:,2)
         A(:,3,8) =  X(:,3)

      END SUBROUTINE ComputeHexShapeMatrices
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeAlphaVector(A,alpha)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP) :: alpha(0:8)
         REAL(KIND=RP) :: A(3,3,0:8)
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER :: i

         DO i = 0, 8
            alpha(i) = determinant(A(:,:,i))
         END DO

      END SUBROUTINE ComputeAlphaVector
!
!////////////////////////////////////////////////////////////////////////
!
!                 Shape Measures
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION DiagonalMeasure(P)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP) :: P(3,0:7)
!
!        ---------------
!        Local Variables
!        ---------------
!
         REAL(KIND=RP) :: D(3,0:3), DMin, DMax, DNorm
         INTEGER       :: i

         DMin = HUGE(DMin)
         DMax = TINY(DMax)

         D(:,0) = P(:,6) - P(:,0)
         D(:,1) = P(:,7) - P(:,1)
         D(:,2) = P(:,4) - P(:,2)
         D(:,3) = P(:,5) - P(:,3)

         DO i = 0, 3
            CALL Norm3D(D(:,i),norm = DNorm)
            DMin = MIN(DMin, DNorm)
            DMax = MAX(DMax, DNorm)
         END DO

         DiagonalMeasure = DMin/DMax

      END FUNCTION DiagonalMeasure
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION EdgeRatio3D(L)
         IMPLICIT NONE
         REAL(KIND=RP) :: L(3,0:11)

         EdgeRatio3D = LMax3D(L)/LMin3D(L)

      END FUNCTION EdgeRatio3D
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION Jacobian3D(alpha)
         IMPLICIT NONE
         REAL(KIND=RP) :: alpha(0:8)
         INTEGER       :: i

         Jacobian3D = HUGE(Jacobian3D)
         DO i = 0,7
            Jacobian3D = MIN(Jacobian3D,alpha(i))
         END DO
         Jacobian3D = MIN(Jacobian3D,alpha(8)/64.0_RP)

      END FUNCTION Jacobian3D
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION Volume3D(alpha)
         IMPLICIT NONE
         REAL(KIND=RP) :: alpha(0:8)

         Volume3D = alpha(8)/64.0_RP
      END FUNCTION Volume3D
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION Skew3D(X)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP) :: X(3,3)
!
!        ---------------
!        Local Variables
!        ---------------
!
         REAL(KIND=RP) :: XHat(3,3)
         INTEGER       :: i
         REAL(KIND=RP) :: dot12, dot13, dot23

         DO i = 1,3
            XHat(:,i) = X(:,i)
            CALL Normalize3D(XHat(:,i))
         END DO

         CALL Dot3D(u = XHat(:,1),v = XHat(:,2),dot = dot12)
         CALL Dot3D(u = XHat(:,1),v = XHat(:,3),dot = dot13)
         CALL Dot3D(u = XHat(:,2),v = XHat(:,3),dot = dot23)

         Skew3D = MAX(ABS(dot12), ABS(dot13), ABS(dot23))

      END FUNCTION Skew3D
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION Shape3D(alpha,A)
      IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP) :: alpha(0:8)
         REAL(KIND=RP) :: A(3,3,0:8)
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER       :: i
         REAL(KIND=RP) :: mag

         Shape3D = HUGE(shape3D )
         DO i = 0, 8
            mag = NORM2(A(:,1,i))**2 + NORM2(A(:,2,i))**2 + NORM2(A(:,3,i))**2
            Shape3d = MIN(Shape3D, (alpha(i)**(2.0_RP/3.0_RP))/mag)
         END DO

         Shape3D = 3.0_RP*Shape3D

      END FUNCTION Shape3D
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION determinant(A)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP) :: A(3,3)
!
!        ---------------
!        Local Variables
!        ---------------
!
         REAL(KIND=RP) :: cross(3), d

         CALL Cross3D(u = A(:,2),v = A(:,3),cross = cross)
         CALL Dot3D(u = A(:,1),v = cross,dot = d)
         determinant = d

      END FUNCTION determinant
!
!////////////////////////////////////////////////////////////////////////
!
!                 VECTOR OPS - From Geometry3D.
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE Cross3D(u,v,cross)
         IMPLICIT NONE
         REAL(KIND=RP), INTENT(IN)  :: u(3), v(3)
         REAL(KIND=RP), INTENT(OUT) :: cross(3)

         cross(1) =   u(2)*v(3) - v(2)*u(3)
         cross(2) = -(u(1)*v(3) - v(1)*u(3))
         cross(3) =   u(1)*v(2) - v(1)*u(2)
      END SUBROUTINE Cross3D
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE Dot3D(u,v,dot)
         IMPLICIT NONE
         REAL(KIND=RP), INTENT(IN)  :: u(3), v(3)
         REAL(KIND=RP), INTENT(OUT) :: dot

         dot = u(1)*v(1) + u(2)*v(2) + u(3)*v(3)

      END SUBROUTINE Dot3D
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE Norm3D(u,norm)
         IMPLICIT NONE
         REAL(KIND=RP), INTENT(IN)  :: u(3)
         REAL(KIND=RP), INTENT(OUT) :: norm

         norm = u(1)*u(1) + u(2)*u(2) + u(3)*u(3)
         norm = SQRT(norm)

      END SUBROUTINE Norm3D
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE Normalize3D(u)
         IMPLICIT NONE
         REAL(KIND=RP), INTENT(INOUT)  :: u(3)

         REAL(KIND=RP)                 :: norm

         CALL Norm3D(u,norm)
         u = u/norm
      END SUBROUTINE Normalize3D

      END Module MeshQualityAnalysisClass

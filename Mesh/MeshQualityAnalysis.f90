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
      IMPLICIT NONE 
!
!-------------------------------------------------------------------
!> Compute standard mesh quality measures. Optionally write them out
!! to a file for graphical analysis.
!-------------------------------------------------------------------
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
      INTEGER, PARAMETER :: NUMBER_OF_SHAPE_MEASURES = 8
      
      CHARACTER(LEN=16), DIMENSION(NUMBER_OF_SHAPE_MEASURES) :: &
              measureNames = (/  "Signed Area     ", &
                                 "Aspect Ratio    ", &
                                 "Condition       ", &
                                 "Edge Ratio      ", &
                                 "Jacobian        ", &
                                 "Minimum Angle   ", &
                                 "Maximum Angle   ", &
                                 "Area Sign       "  &
                             /)
                             
      REAL(KIND=RP)    , DIMENSION(NUMBER_OF_SHAPE_MEASURES) :: &
              refValues     = (/  1.0_RP , &
                                  1.0_RP , &
                                  1.0_RP , &
                                  1.0_RP , &
                                  1.0_RP , &
                                  90.0_RP, &
                                  90.0_RP, &
                                   1.0_RP  &
                             /)
                             
      REAL(KIND=RP)    , DIMENSION(NUMBER_OF_SHAPE_MEASURES) :: &
              acceptableLow = (/  0.0_RP , &
                                  1.0_RP , &
                                  1.0_RP , &
                                  1.0_RP , &
                                  0.0_RP , &
                                  40.0_RP, &
                                  90.0_RP,  &
                                   1.0_RP  &
                             /)
                             
      REAL(KIND=RP)    , DIMENSION(NUMBER_OF_SHAPE_MEASURES) :: &
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
!     ------------------
!     Statistics storage
!     ------------------
!
      TYPE MeshStatistics
         REAL(KIND=RP)    , DIMENSION(NUMBER_OF_SHAPE_MEASURES) :: maxValues
         REAL(KIND=RP)    , DIMENSION(NUMBER_OF_SHAPE_MEASURES) :: minValues
         REAL(KIND=RP)    , DIMENSION(NUMBER_OF_SHAPE_MEASURES) :: avgValues
      END TYPE MeshStatistics
      
      
      INTERFACE OPERATOR (.cross.)
         MODULE PROCEDURE CrossProduct
      END INTERFACE
      PRIVATE :: CrossProduct, Norm2
!
! note1: will have to change for nonplanar geometries
!
!     ========
      CONTAINS 
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE OutputMeshQualityMeasures( mesh, fUnit )
!
!     ------------------------------------------------------------------
!     Compute mesh quality for each element, print the elemental values 
!     and accumilate the statistics.
!     ------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMMesh), POINTER :: mesh
         INTEGER               :: fUnit
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedListIterator), POINTER :: elementIterator => NULL()
         CLASS(SMElement)           , POINTER :: e => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()
         REAL(KIND=RP)                        :: shapeMeasures(NUMBER_OF_SHAPE_MEASURES)
         CHARACTER(LEN=16)                    :: namesFmt = "(   8A16 )", valuesFmt = '(  8F16.3)', numb = "10"
         
         WRITE(numb,FMT='(I3)') NUMBER_OF_SHAPE_MEASURES
         namesFmt  = "(" // TRIM(numb) // "A16)"
         valuesFmt = "(" // TRIM(numb) // "(1PE16.4))"
         
         WRITE(fUnit,namesFmt) measureNames
         
         elementIterator => mesh % elementsIterator
         CALL elementIterator % setToStart()
         
         DO WHILE ( .NOT.elementIterator % isAtEnd() )
            obj => elementIterator % object()
            CALL cast(obj,e)
            CALL ComputeElementShapeMeasures( e, shapeMeasures )
            WRITE( fUnit,valuesFmt) shapeMeasures
            
            CALL elementIterator % moveToNext()
         END DO
      
      END SUBROUTINE OutputMeshQualityMeasures
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeMeshQualityStatistics( stats, mesh )
!
!     ------------------------------------------------------------------
!     Compute mesh quality for each element, print the elemental values 
!     and accumilate the statistics.
!     ------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(MeshStatistics)          :: stats
         CLASS(SMMesh)       , POINTER :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedListIterator), POINTER :: elementIterator => NULL()
         CLASS(SMElement)           , POINTER :: e               => NULL()
         CLASS(FTObject)            , POINTER :: obj             => NULL()
         REAL(KIND=RP)                        :: shapeMeasures(NUMBER_OF_SHAPE_MEASURES)
         INTEGER                              :: k, nValues
         
         stats % avgValues  = 0.0_RP
         stats % maxValues  = 0.0_RP
         stats % minValues  = HUGE(1.0_RP)
         
         elementIterator => mesh % elementsIterator
         CALL elementIterator % setToStart()
         
         nValues = 0
         DO WHILE ( .NOT.elementIterator % isAtEnd() )
            obj => elementIterator % object()
            CALL cast(obj,e)
            CALL ComputeElementShapeMeasures( e, shapeMeasures )
            
            DO k = 1, NUMBER_OF_SHAPE_MEASURES
               stats % avgValues(k) = stats % avgValues(k) + shapeMeasures(k)
               stats % maxValues(k) = MAX( stats % maxValues(k), shapeMeasures(k) )
               stats % minValues(k) = MIN( stats % minValues(k), shapeMeasures(k) )
            END DO
            
            CALL elementIterator % moveToNext()
            NValues = nValues + 1
         END DO
         stats % avgValues = stats % avgValues/NValues
      
      END SUBROUTINE ComputeMeshQualityStatistics
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeElementShapeMeasures( e, shapeMeasures ) 
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMElement)  , POINTER :: e
         REAL(KIND=RP)               :: shapeMeasures(NUMBER_OF_SHAPE_MEASURES)
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject), POINTER :: obj => NULL()
         CLASS(SMNode)  , POINTER :: node => NULL()
         
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
            obj => e % nodes % objectAtIndex(k)
            CALL cast(obj,node)
            P(:,k) = node % x
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

      END SUBROUTINE ComputeElementShapeMeasures
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
         CLASS(FTObject), POINTER :: obj  => NULL()
         CLASS(SMNode)  , POINTER :: node => NULL()
         
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
            obj => e % nodes % objectAtIndex(k)
            CALL cast(obj,node)
            P(:,k) = node % x
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
         CLASS(SMMesh)              , POINTER :: mesh
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
         REAL(KIND=RP) :: shapeMeasures(NUMBER_OF_SHAPE_MEASURES)
         INTEGER       :: k
         
         CALL ComputeElementShapeMeasures( e, shapeMeasures )
            
         elementIsBad = .false.
         DO k = 1, NUMBER_OF_SHAPE_MEASURES
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
         LOGICAL       :: info(NUMBER_OF_SHAPE_MEASURES)
         REAL(KIND=RP) :: shapeMeasures(NUMBER_OF_SHAPE_MEASURES)
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER       :: k
         
         info = .false.
         DO k = 1, NUMBER_OF_SHAPE_MEASURES
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
         REAL(KIND=RP)            :: shapeMeasures(NUMBER_OF_SHAPE_MEASURES)
         LOGICAL                  :: info(NUMBER_OF_SHAPE_MEASURES)
         CLASS(FTObject), POINTER :: obj => NULL()
         CLASS(SMNode)  , POINTER :: node => NULL()
         
         CALL ComputeElementShapeMeasures( e, shapeMeasures )
         CALL ExtractBadElementInfo( shapeMeasures, info )
         
         WRITE( fUnit, *) "Element ", e % id
         
         DO k = 1,4 
            obj => e % nodes % objectAtIndex(k)
            CALL cast(obj,node)
            WRITE( fUnit, *) "      ", node % x
         END DO
         
         WRITE( fUnit, *) "Problems:"
         DO k = 1, NUMBER_OF_SHAPE_MEASURES
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
      FUNCTION Norm2(u) 
      IMPLICIT NONE
      REAL(KIND=RP) :: u(3)
      REAL(KIND=RP) :: Norm2
      norm2 = SQRT(u(1)**2 + u(2)**2 )
      END FUNCTION Norm2
      
      END Module MeshQualityAnalysisClass
      
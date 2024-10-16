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
! --- End License
!
!////////////////////////////////////////////////////////////////////////
!
!      Sizer.f90
!      Created: August 15, 2013 1:01 PM
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
      MODULE MeshSizerClass
      USE SMConstants
      USE ChainedSegmentedCurveClass
      USE ProgramGlobals
      USE Geometry
      USE FTLinkedListClass
      USE FTLinkedListIteratorClass
      USE SizerControls
      USE SMTopographyClass

      IMPLICIT NONE
!
!     ---------------------
!     Class type definition
!     ---------------------
!
      TYPE, EXTENDS(FTObject) :: MeshSizer
         INTEGER                               :: noOfInnerBoundaries
         INTEGER                               :: noOfInterfaceBoundaries
         REAL(KIND=RP)                         :: baseSize
         REAL(KIND=RP)                         :: xMin(3), xMax(3)
         CLASS(FTLinkedList)         , POINTER :: controlsList            => NULL()
         CLASS(ChainedSegmentedCurve), POINTER :: outerBoundary           => NULL()
         CLASS(FTLinkedList)         , POINTER :: innerBoundariesList     => NULL()
         CLASS(FTLinkedList)         , POINTER :: interfaceBoundariesList => NULL()
         CLASS(SMTopography)         , POINTER :: topography              => NULL()
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithProperties => initMeshSizer
         FINAL     :: destructMeshSizer
         PROCEDURE :: printDescription   => printSizerDescription
         PROCEDURE :: addSizerCenterControl
         PROCEDURE :: addSizerLineControl
         PROCEDURE :: addBoundaryCurve
         PROCEDURE :: sizeFunctionMinimumOnBox
         PROCEDURE :: setBaseSize
         PROCEDURE :: setBottomTopography
         PROCEDURE :: sizeRatio

      END TYPE MeshSizer

      TYPE SizerCurvePtr
         CLASS(ChainedSegmentedCurve), POINTER :: curve => NULL()
      END TYPE SizerCurvePtr
!
!     ========
      CONTAINS
!     ========
!
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initMeshSizer( self, baseSize, xMin, xMax )
         IMPLICIT NONE
         CLASS(MeshSizer) :: self
         REAL(KIND=RP)    :: baseSize(3), xMin(3), xMax(3)

         CALL self % FTObject % init()

         CALL self % setBaseSize(MAXVAL(baseSize))

         self % noOfInnerBoundaries     = 0
         self % noOfInterfaceBoundaries = 0
         self % xMin                    = xMin
         self % xMax                    = xMax

         self % outerBoundary           => NULL()
         self % innerBoundariesList     => NULL()
         self % interfaceBoundariesList => NULL()
         self % topography              => NULL()

         ALLOCATE(self % controlsList)
         CALL self % controlsList % init()

      END SUBROUTINE initMeshSizer
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE destructMeshSizer(self)
         IMPLICIT NONE
         TYPE(MeshSizer) :: self
         CLASS(FTObject), POINTER  :: obj

         IF ( ASSOCIATED(self % controlsList) )     THEN
            obj => self % controlsList
            CALL release(obj)
         END IF

         IF ( ASSOCIATED(self % innerBoundariesList) )     THEN
            obj => self % innerBoundariesList
            CALL release(obj)
         END IF

         IF ( ASSOCIATED(self % interfaceBoundariesList) )     THEN
            obj => self % interfaceBoundariesList
            CALL release(obj)
         END IF

         IF ( ASSOCIATED(self % outerBoundary) )     THEN
            obj => self % outerBoundary
            CALL release(obj)
         END IF

         IF ( ASSOCIATED(self % topography) )     THEN
            obj => self % topography
            CALL release(obj)
         END IF

      END SUBROUTINE destructMeshSizer
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE releaseSizer(self)
         IMPLICIT NONE
         TYPE (MeshSizer), POINTER :: self
         CLASS(FTObject) , POINTER :: obj

         IF(.NOT. ASSOCIATED(self)) RETURN

         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL()
         END IF
      END SUBROUTINE releaseSizer
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE clearBoundaryCurves( self )
         IMPLICIT NONE
         TYPE (MeshSizer) :: self
         CLASS(FTObject), POINTER  :: obj

         IF ( ASSOCIATED(self % innerBoundariesList) )     THEN
            obj => self % innerBoundariesList
            CALL release(obj)
         END IF

         IF ( ASSOCIATED(self % interfaceBoundariesList) )     THEN
            obj => self % interfaceBoundariesList
            CALL release(obj)
         END IF

         IF ( ASSOCIATED(self % outerBoundary) )     THEN
            obj => self % outerBoundary
            CALL release(obj)
         END IF

         NULLIFY(self % outerBoundary)
         NULLIFY(self % interfaceBoundariesList)
         NULLIFY(self % innerBoundariesList)

         self % noOfInnerBoundaries     = 0
         self % noOfInterfaceBoundaries = 0

      END SUBROUTINE clearBoundaryCurves
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE setBaseSize(self, baseSize)
         IMPLICIT NONE
         CLASS(MeshSizer) :: self
         REAL(KIND=RP)    :: baseSize
         self % baseSize = baseSize
      END SUBROUTINE setBaseSize
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE addSizerCenterControl(self,center)
         IMPLICIT NONE
         CLASS(MeshSizer)                   :: self
         CLASS(SizerCenterControl), POINTER :: center
         CLASS(FTObject)          , POINTER :: obj => NULL()

         IF ( .NOT.ASSOCIATED(self % controlsList) )     THEN
            ALLOCATE(self % controlsList)
            CALL self % controlsList % init()
         END IF

         obj => center
         CALL self % controlsList % add(obj)

      END SUBROUTINE addSizerCenterControl
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE addSizerLineControl(self,line)
         IMPLICIT NONE
         CLASS(MeshSizer)                 :: self
         CLASS(SizerLineControl), POINTER :: line
         CLASS(FTObject)        , POINTER :: obj => NULL()

         IF ( .NOT.ASSOCIATED(self % controlsList) )     THEN
            ALLOCATE(self % controlsList)
            CALL self % controlsList % init()
         END IF

         obj => line
         CALL self % controlsList % add(obj)

      END SUBROUTINE addSizerLineControl
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE addBoundaryCurve( self, segmentedCurve, curveType )
         IMPLICIT NONE
         CLASS(MeshSizer)                      :: self
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedCurve
         INTEGER                               :: curveType ! = INNER, OUTER, INTERIOR_INTERFACE
         CLASS(FTObject), POINTER              :: obj => NULL()

         SELECT CASE ( curveType )

            CASE( INNER )

               IF ( .NOT.ASSOCIATED(self % innerBoundariesList) )     THEN
                  ALLOCATE(self % innerBoundariesList)
                  CALL self %  innerBoundariesList % init()
               END IF

               obj => segmentedCurve
               CALL self %  innerBoundariesList % add(obj)
               self % noOfInnerBoundaries = self % noOfInnerBoundaries + 1

            CASE( INTERIOR_INTERFACE )

               IF ( .NOT.ASSOCIATED(self % interfaceBoundariesList) )     THEN
                  ALLOCATE(self % interfaceBoundariesList)
                  CALL self %  interfaceBoundariesList % init()
               END IF

               obj => segmentedCurve
               CALL self %  interfaceBoundariesList % add(obj)
               self % noOfInterfaceBoundaries = self % noOfInterfaceBoundaries + 1

            CASE( OUTER )

               self % outerBoundary => segmentedCurve
               CALL self % outerBoundary % retain()
            CASE DEFAULT
         END SELECT

      END SUBROUTINE addBoundaryCurve
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE setBottomTopography(self, topography)
         IMPLICIT NONE
         CLASS(MeshSizer)             :: self
         CLASS(SMTopography), POINTER :: topography

         self % topography => topography
         CALL self % topography % retain()

      END SUBROUTINE setBottomTopography
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION sizeFunctionMinimumOnBox( sizer, xMin, xMax ) RESULT(hMin)
         !TODO: Should switch to a good minimization algorithm. Use linear search for now.
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MeshSizer) :: sizer
         REAL(KIND=RP)    :: xMin(3), xMax(3)
         REAL(KIND=RP)    :: hMin
!
!        ---------------
!        local variables
!        ---------------
!
         REAL(KIND=RP)                    :: dX(3)
         REAL(KIND=RP)                    :: x(3), boundingBox(6)
         REAL(KIND=RP)                    :: cSize, aSize
         REAL(KIND=RP)                    :: cHeight, cWidth, cDim
         INTEGER                          :: i,j, nX(3) = [10,10,0]
         REAL(KIND=RP), PARAMETER         :: SIZE_FACTOR = 0.9_RP

         TYPE (FTLinkedListIterator) , POINTER :: iterator            => NULL()
         CLASS(FTObject)             , POINTER :: obj                 => NULL()
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedCurveChain => NULL()
!
!        -----------------------------------------
!        Sizes determined by centers/line controls
!        -----------------------------------------
!
         hMin = HUGE(hMin)

         IF ( ASSOCIATED(sizer % controlsList) )     THEN
            dX = (xMax - xMin)/nX
            DO j = 0, nX(2)
               x(2) = xMin(2) + j*dx(2)
               DO i = 0, nX(1)
                  x(1) = xMin(1) + i*dx(1)
                  hMin = MIN( hMin, controlSize(sizer,x) )
              END DO
            END DO
         END IF
!
!        -----------------------------------------
!        Sizes determined by the bottom topography
!        -----------------------------------------
!
         IF ( ASSOCIATED(sizer % topography) )     THEN
            dX = (xMax - xMin)/nX
            TLoop : DO j = 0, nX(2)
               x(2) = xMin(2) + j*dx(2)
               DO i = 0, nX(1)
                  x(1) = xMin(1) + i*dx(1)
                  hMin = MIN( hMin, 1.0_RP/SQRT(sizer % topography % gaussianCurvatureAt(x)) )
                  IF(hMin < SIZE_FACTOR*dX(1)) EXIT TLoop
               END DO
            END DO TLoop
         END IF
!
!        ------------------------------------
!        Sizes given by curves in the domain.
!        ------------------------------------
!
         cSize = TINY(cSize)
         IF( ASSOCIATED(sizer%outerBoundary) )     THEN
            cSize = MAX( cSize, invCurveSizeForBox(sizer % outerBoundary, xMin, xMax ) )
         END IF

         IF ( ASSOCIATED( sizer % innerBoundariesList) )     THEN
            CALL cSizeForCurvesInList(sizer % innerBoundariesList, cSize, xMin, xMax)
         END IF

         IF( ASSOCIATED(sizer % interfaceBoundariesList) )     THEN
            CALL cSizeForCurvesInList(sizer % interfaceBoundariesList, cSize, xMin, xMax)
         END IF
         cSize = 1.0_RP/cSize
!
!        ------------------------------------------------------
!        Sizes given by areas of the outer and interface curves
!        ------------------------------------------------------
!
         aSize = HUGE(aSize)
         IF( ASSOCIATED(sizer % outerBoundary) )     THEN
            boundingBox = sizer % outerBoundary % boundingBox
            IF(Point_IsInsideBox(xMin, boundingBox) .OR. &
               Point_IsInsideBox(xMax, boundingBox)) THEN

               cHeight = Height( sizer % outerBoundary )
               cWidth  = Width ( sizer % outerBoundary )
               cDim    = MIN(cHeight,cWidth)
               aSize   = cDim/minNumberOfElementsInsideArea
            END IF
         END IF

         IF ( ASSOCIATED(sizer % interfaceBoundariesList) )     THEN
            ALLOCATE(iterator)
            CALL iterator % initwithFTLinkedList(sizer % interfaceBoundariesList)
            CALL iterator % setToStart
            DO WHILE (.NOT.iterator % isAtEnd())
               obj => iterator % object()
               CALL castToChainedSegmentedCurve(obj,segmentedCurveChain)

               IF(Point_IsInsideBox(xMin,segmentedCurveChain % boundingBox) .OR. &
                  Point_IsInsideBox(xMax,segmentedCurveChain % boundingBox)) THEN

                  cHeight = Height( segmentedCurveChain )
                  cWidth  = Width ( segmentedCurveChain )
                  cDim    = MIN(cHeight,cWidth)
                  aSize   = MIN(cDim/minNumberOfElementsInsideArea,aSize)
               END IF

               CALL iterator % moveToNext()
            END DO
            obj => iterator
            CALL release(obj)
         END IF
!
!        -------------------------
!        Final choice for the size
!        -------------------------
!
         hMin = MIN( hMin, cSize, sizer % baseSize, aSize )

      END FUNCTION sizeFunctionMinimumOnBox
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE cSizeForCurvesInList( list, cSize, xMin, xMax )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList), POINTER :: list
         REAL(KIND=RP)                :: cSize
         REAL(KIND=RP)                :: xMin(3), xMax(3)
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedListIterator) , POINTER :: iterator => NULL()
         CLASS(FTObject)             , POINTER :: obj => NULL()
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedCurveChain => NULL()

         ALLOCATE(iterator)
         CALL iterator % initWithFTLinkedList(list)
         CALL iterator % setToStart()

         DO WHILE (.NOT.iterator % isAtEnd())
            obj => iterator % object()
            CALL castToChainedSegmentedCurve(obj,segmentedCurveChain)
            cSize = MAX( cSize, invCurveSizeForBox(segmentedCurveChain, xMin, xMax ) )
            CALL iterator % moveToNext()
         END DO
         obj => iterator
         CALL release(obj)

      END SUBROUTINE cSizeForCurvesInList
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION controlSize( self, x )

         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MeshSizer) :: self
         REAL(KIND=RP)    :: x(3)
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()
         CLASS(SizerCenterControl)  , POINTER :: center => NULL()
         CLASS(SizerLineControl)    , POINTER :: line => NULL()
         REAL(KIND=RP)                        :: hFunInv

         hFunInv = 1.0_RP/self % baseSize
!
!        --------------------------------
!        Sizes given by centers and lines
!        --------------------------------
!
         IF ( ASSOCIATED(self % controlsList) )     THEN

            ALLOCATE(iterator)
            CALL iterator % initwithFTLinkedList( self % controlsList)
            CALL iterator % setToStart()

            DO WHILE (.NOT.iterator % isAtEnd())
               obj => iterator % object()
               SELECT TYPE(obj)
                  TYPE IS (SizerCenterControl)
                     center  => obj
                     hFunInv = MAX(hFunInv , hInvForCenter( center, x ) )
                  TYPE IS (sizerLineControl)
                     line => obj
                     hFunInv = MAX(hFunInv , hInvForLineControl( line, x ) )
                  CLASS DEFAULT

               END SELECT

               CALL iterator % moveToNext()
            END DO
            obj => iterator
            CALL release(obj)

         END IF
!
!        -------------------------
!        Compute the size function
!        -------------------------
!
         controlSize = 1.0_RP/hFunInv

      END FUNCTION controlSize
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION invCurveSizeForBox(chain, xMin, xMax )
      USE Geometry
      IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP)                         :: xMin(3), xMax(3)
         CLASS(ChainedSegmentedCurve), POINTER :: chain
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP)                    :: nodes(3,4), x(3)
         CLASS(FRSegmentedCurve), POINTER :: c => NULL()
         INTEGER                          :: k, N, j
         REAL(KIND=RP)                    :: s

         nodes(:,1) = xMin
         nodes(:,2) = (/xMax(1),xMin(2), 0.0_RP/)
         nodes(:,3) = xMax
         nodes(:,4) = (/xMin(1),xMax(2), 0.0_RP/)

         invCurveSizeForBox = 0.0_RP

         N = chain % curveCount()
         DO k = 1, N
            c => chain % segmentedCurveAtIndex(k)
            DO j = 1, c % COUNT()
               x = c % positionAtIndex(j)
               IF ( PointInQuad(nodes,x) )     THEN
                  s = c % invScaleAtIndex(j)
                  invCurveSizeForBox =  MAX(s, invCurveSizeForBox)
               END IF
            END DO
         END DO

      END FUNCTION invCurveSizeForBox
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeCurveDistanceScales( self )
!
!     -----------------------------------------------------------
!     Save the distances between the different curves in the mesh
!     -----------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (MeshSizer), POINTER :: self
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE(SizerCurvePtr)        , DIMENSION(:), ALLOCATABLE :: innerCurvesArray
         TYPE(FTLinkedListIterator) , POINTER                   :: iterator => NULL()
         CLASS(FTObject)            , POINTER                   :: obj => NULL()

         CLASS(ChainedSegmentedCurve), POINTER                  :: innerSegmentedCurveChain => NULL()
         INTEGER                                                :: k
         INTEGER                                                :: numberOfInsideBoundaries
!
!        ------------------------------------------------------
!        For convenience, save the inner boundaries in an array
!        ------------------------------------------------------
!
         numberOfInsideBoundaries = self % noOfInnerBoundaries + self % noOfInterfaceBoundaries

         IF( numberOfInsideBoundaries > 0 ) ALLOCATE( innerCurvesArray(numberOfInsideBoundaries) )

         k = 1
         IF( self % noOfInnerBoundaries > 0 )     THEN
            ALLOCATE(iterator)

            CALL iterator % initwithFTLinkedList(self % innerBoundariesList)
            CALL iterator % setToStart()
            DO WHILE (.NOT.iterator % isAtEnd())
               obj     => iterator % object()
               CALL castToChainedSegmentedCurve(obj,innerSegmentedCurveChain)
               innerCurvesArray(k) % curve => innerSegmentedCurveChain
               k = k + 1
               CALL iterator % moveToNext()
            END DO
            CALL releaseFTLinkedListIterator(iterator)
         END IF

         IF ( self % noOfInterfaceBoundaries > 0 )     THEN
            ALLOCATE(iterator)

            CALL iterator % initwithFTLinkedList(self % interfaceBoundariesList)
            CALL iterator % setToStart
            DO WHILE (.NOT.iterator % isAtEnd())
               obj     => iterator % object()
               CALL castToChainedSegmentedCurve(obj,innerSegmentedCurveChain)
               innerCurvesArray(k) % curve => innerSegmentedCurveChain
               k                           = k + 1
               CALL iterator % moveToNext()
            END DO
            CALL releaseFTLinkedListIterator(iterator)
         END IF
!
!        -----------------------------------------------
!        Find the distances of the outer boundary curve
!        to all inner curves
!        -----------------------------------------------
!
         IF ( ASSOCIATED(self%outerBoundary) .AND. numberOfInsideBoundaries > 0 )     THEN

            CALL OuterToInnerboundaryDistances(self,innerCurvesArray,numberOfInsideBoundaries)

         ELSE IF ( numberOfInsideBoundaries > 0 )     THEN

            CALL OuterBoxToInnerboundaryDistances(self,innerCurvesArray,numberOfInsideBoundaries)

         END IF
!
!        ------------------------------------
!        Do the same for the inner boundaries
!        The difference here is that facing
!        boundaries have normals that are
!        opposite to each other.
!        ------------------------------------
!
         IF ( numberOfInsideBoundaries > 1 )     THEN

            CALL InnerToInnerBoundaryDistances(innerCurvesArray,numberOfInsideBoundaries)

         END IF
!
!        ---------------------------------------------------
!        Finally, check to see if a curve is close to itself
!        ---------------------------------------------------
!
         IF(ASSOCIATED(self % outerBoundary))     THEN
            CALL CurveToCurveBoundaryDistances(segmentedCurveChain = self % outerBoundary, &
                                               isOuterBoundary = .TRUE.)
         END IF

         IF ( numberOfInsideBoundaries > 0 )     THEN
            DO k = 1, numberOfInsideBoundaries
               CALL CurveToCurveBoundaryDistances(segmentedCurveChain = innerCurvesArray(k) % curve, &
                                                  isOuterBoundary = .FALSE. )
            END DO
         END IF
!
!        -------
!        Cleanup
!        -------
!
         IF(ALLOCATED(innerCurvesArray)) DEALLOCATE( innerCurvesArray )

      END SUBROUTINE ComputeCurveDistanceScales
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE OuterToInnerboundaryDistances(self, innerCurvesArray, numberOfInsideBoundaries)
!
!      -------------------------------------------------------------------
!      Find the distance from the outer curve to all the inner curves
!      -------------------------------------------------------------------
!
      IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
      TYPE (MeshSizer)   , POINTER  :: self
      INTEGER                       :: numberOfInsideBoundaries
      TYPE(SizerCurvePtr)           :: innerCurvesArray(numberOfInsideBoundaries)
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER                                                :: i, j, k, l, m, N, nSegments
      CLASS(ChainedSegmentedCurve), POINTER                  :: innerSegmentedCurveChain => NULL()
      CLASS(FRSegmentedCurve)     , POINTER                  :: innerSegment, outerSegment
      REAL(KIND=RP)                                          :: x(3), y(3), d, outerInvScale, innerInvScale
      REAL(KIND=RP)                                          :: nHatInner(3), nHatOuter(3), dot

      N = self % outerBoundary % numberOfCurvesInChain
!
!     -----------------------------------------
!     For each segment in the outer curve chain
!     -----------------------------------------
!
      DO j = 1, N
         outerSegment => self % outerBoundary % segmentedCurveAtIndex(j)
         nSegments    =  outerSegment % COUNT()
!
!        --------------------------------------
!        For each point along the outer segment
!        --------------------------------------
!
         DO i = 1, nSegments
            x             = outerSegment % positionAtIndex(i)
            outerInvScale = outerSegment % invScaleAtIndex(i)
            nHatOuter     = outerSegment % normalAtIndex(i)
!
!           -----------------------
!           For each inner boundary
!           -----------------------
!
            DO k = 1, numberOfInsideBoundaries
               innerSegmentedCurveChain => innerCurvesArray(k) % curve
!
!              --------------------------------------
!              For each segment along the inner chain
!              --------------------------------------
!
               DO l = 1, innerSegmentedCurveChain % curveCount()
                  innerSegment => innerSegmentedCurveChain % segmentedCurveAtIndex(l)
!
!                 ----------------------------------------------
!                 For each point along the inner segmented curve
!                 ----------------------------------------------
!
                  DO m = 1, innerSegment % COUNT()
                     y             = innerSegment % positionAtIndex(m)
                     innerInvScale = innerSegment % invScaleAtIndex(m)
                     nHatInner     = innerSegment % normalAtIndex(m)

                     d = closeCurveFactor/SQRT( (x(1) - y(1))**2 + (x(2) - y(2))**2 ) ! Inverse length - 3 cells
!
!                    -----------------------------------------------------------
!                    Curves that are close and face each other should have their
!                    mesh sizes adjusted to have enough between them
!                    -----------------------------------------------------------
!
                     dot = DOT_PRODUCT(nHatInner,nHatOuter)
                     IF( dot > closeCurveNormalAlignment )     THEN
                        outerInvScale = MAX(d,outerInvScale)
                        innerInvScale = MAX(d,innerInvScale)
                        CALL outerSegment % setCurveInvScaleForIndex(outerInvScale,i)
                        CALL innerSegment % setCurveInvScaleForIndex(innerInvScale,m)
                     END IF

                  END DO
               END DO
            END DO
         END DO
      END DO


      END SUBROUTINE OuterToInnerboundaryDistances
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE OuterBoxToInnerboundaryDistances(self, innerCurvesArray, numberOfInsideBoundaries)
!
!      -------------------------------------------------------------------
!      Find the distance from the outer curve to all the inner curves
!      -------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE (MeshSizer)   , POINTER :: self
      INTEGER                      :: numberOfInsideBoundaries
      TYPE(SizerCurvePtr)          :: innerCurvesArray(numberOfInsideBoundaries)
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER                                                :: j, k, l, m
      CLASS(ChainedSegmentedCurve), POINTER                  :: innerSegmentedCurveChain => NULL()
      CLASS(FRSegmentedCurve)     , POINTER                  :: innerSegment
      REAL(KIND=RP)                                          :: y(3), d, innerInvScale
      REAL(KIND=RP)                                          :: nHatInner(3), nHatOuter(3), dot
      REAL(KIND=RP)              , DIMENSION(4)              :: A = (/0.0_RP,1.0_RP,0.0_RP,1.0_RP/)
      REAL(KIND=RP)              , DIMENSION(4)              :: B = (/1.0_RP,0.0_RP,1.0_RP,0.0_RP/)
      REAL(KIND=RP)               , DIMENSION(4)             :: C
      REAL(KIND=RP), DIMENSION(3,4) :: nHatBox = &
      RESHAPE((/0.0_RP,-1.0_RP,0.0_RP,1.0_RP,0.0_RP,0.0_RP,0.0_RP,1.0_RP,0.0_RP,-1.0_RP,0.0_RP,0.0_RP/),(/3,4/))
!
!     ------------------------------------------------------
!     There are inner curves but no outer curves, just a box
!     ------------------------------------------------------
!
      C(1) = self%xMin(2)
      C(2) = self%xMax(1)
      C(3) = self%xMax(2)
      C(4) = self%xMin(1)

      DO m = 1, 4
         nHatOuter = nHatBox(:,m)
!
!        -----------------------------------
!        For each inner boundary curve chain
!        -----------------------------------
!
         DO k = 1, numberOfInsideBoundaries
            innerSegmentedCurveChain => innerCurvesArray(k) % curve
!
!           -----------------------------
!           For each segment in the chain
!           -----------------------------
!
            DO l = 1, innerSegmentedCurveChain % curveCount()
               innerSegment => innerSegmentedCurveChain % segmentedCurveAtIndex(l)
!
!              --------------------------------
!              For each point along the segment
!              --------------------------------
!
               DO j = 1, innerSegment % COUNT()
                  y             = innerSegment % positionAtIndex(j)
                  innerInvScale = innerSegment % invScaleAtIndex(j)
                  nHatInner     = innerSegment % normalAtIndex(j)

                  d = ABS( A(m)*y(1) + B(m)*y(2) + C(m) ) !/SQRT(A(m)**2 + B(m)**2)
                  d = closeCurveFactor/d ! Inverse length - 3 cells
!
                  dot = DOT_PRODUCT(nHatInner,nHatOuter)
                  IF( dot < -closeCurveNormalAlignment )     THEN
                     innerInvScale = MAX(d,innerInvScale)
                     CALL innerSegment % setCurveInvScaleForIndex(innerInvScale,j)
                  END IF
               END DO
            END DO
         END DO
      END DO

      END SUBROUTINE OuterBoxToInnerboundaryDistances
!
!////////////////////////////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE InnerToInnerBoundaryDistances(innerCurvesArray, numberOfInsideBoundaries)
!
!      -------------------------------------------------------------------
!      Find the distance from the outer curve to all the inner curves
!      -------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER                       :: numberOfInsideBoundaries
      TYPE(SizerCurvePtr)           :: innerCurvesArray(numberOfInsideBoundaries)
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER                                                :: i, j, k, l, m, N
      CLASS(ChainedSegmentedCurve), POINTER                  :: innerSegmentedCurveChain => NULL()  ,&
                                                                outerSegmentedCurveChain => NULL()
      CLASS(FRSegmentedCurve)     , POINTER                  :: innerSegment, outerSegment
      REAL(KIND=RP)                                          :: x(3), y(3), d, outerInvScale, innerInvScale
      REAL(KIND=RP)                                          :: nHatInner(3), nHatOuter(3), dot

!
!     -----------------------------------
!     For each inner boundary curve chain
!     -----------------------------------
!
      DO k = 1, numberOfInsideBoundaries
         outerSegmentedCurveChain => innerCurvesArray(k) % curve
!
!        -----------------------------
!        For each segment in the chain
!        -----------------------------
!
         DO l = 1, outerSegmentedCurveChain % curveCount()
            outerSegment => outerSegmentedCurveChain % segmentedCurveAtIndex(l)
!
!           --------------------------------
!           For each point along the segment
!           --------------------------------
!
            DO j = 1, outerSegment % COUNT()
               x             = outerSegment % positionAtIndex(j)
               outerInvScale = outerSegment % invScaleAtIndex(j)
               nHatOuter     = outerSegment % normalAtIndex(j)
!
!              ------------------------------
!              For each of the *other* chains
!              ------------------------------
!
               DO m = k+1, numberOfInsideBoundaries
                  innerSegmentedCurveChain => innerCurvesArray(m) % curve
!
!                 -----------------------------------
!                 For each segment in the other chain
!                 -----------------------------------
!
                  DO n = 1, innerSegmentedCurveChain % curveCount()
                     innerSegment => innerSegmentedCurveChain % segmentedCurveAtIndex(n)
!
!                    --------------------------------------
!                    For each point along the other segment
!                    --------------------------------------
!
                     DO i = 1, innerSegment % COUNT()
                        y             = innerSegment % positionAtIndex(i)
                        innerInvScale = innerSegment % invScaleAtIndex(i)
                        nHatInner     = innerSegment % normalAtIndex(i)

                        d = closeCurveFactor/SQRT( (x(1) - y(1))**2 + (x(2) - y(2))**2 ) ! Inverse length - 3 cells
!
                        dot = DOT_PRODUCT(nHatInner,nHatOuter)

                        IF( dot < -closeCurveNormalAlignment )     THEN
                           outerInvScale = MAX(d,outerInvScale)
                           CALL outerSegment % setCurveInvScaleForIndex(outerInvScale,j)
                           innerInvScale = MAX(d,innerInvScale)
                           CALL innerSegment % setCurveInvScaleForIndex(innerInvScale,i)
                        END IF
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END DO

      END SUBROUTINE InnerToInnerBoundaryDistances
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeInterfaceCurveScales( self )
!
!     -----------------------------------------------------------
!     Use interface curve bounding boxes to ensure that enough
!     elements are placed inside an interface curve.
!     -----------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MeshSizer) :: self
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(ChainedSegmentedCurve), POINTER :: chain => NULL()
         CLASS(FRSegmentedCurve)     , POINTER :: segment => NULL()
         REAL(KIND=RP)                         :: cScale, cHeight, cWidth, cSize, invScale
         REAL(KIND=RP)                         :: segmentInvScale
         CLASS(FTLinkedListIterator) , POINTER :: iterator => NULL()
         CLASS(FTObject)             , POINTER :: obj => NULL()
         INTEGER                               :: k, j
!
         IF ( self % noOfInterfaceBoundaries == 0 )     RETURN

         ALLOCATE(iterator)
         CALL iterator % initWithFTLinkedList(self % interfaceBoundariesList)
         CALL iterator % setToStart
         DO WHILE (.NOT.iterator % isAtEnd())
            obj => iterator % object()
            CALL castToChainedSegmentedCurve(obj,chain)

            cHeight = Height( chain )
            cWidth  = Width ( chain )
            cSize   = MIN(cHeight,cWidth)
!
!           --------------------------------------------------------
!           Require at least minNumberOfElementsInsideCurve elements
!           --------------------------------------------------------
!
            cScale = cSize/ minNumberOfElementsInsideArea
            DO k = 1, chain % curveCount()
               segment => chain % segmentedCurveAtIndex(k)
               DO j = 1, segment % COUNT()
                  segmentInvScale = segment % invScaleAtIndex(j)
                  invScale        = MAX(1.0_RP/cScale,segmentInvScale)
                  CALL segment % setCurveInvScaleForIndex(invScale,j)
               END DO
            END DO

            CALL iterator % moveToNext()
         END DO
         obj => iterator
         CALL release(obj)

      END SUBROUTINE ComputeInterfaceCurveScales
!
!////////////////////////////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CurveToCurveBoundaryDistances(segmentedCurveChain, isOuterBoundary )
!
!      ----------------------------------------
!      Find the distance from a curve to itself
!      ----------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(ChainedSegmentedCurve), POINTER :: segmentedCurveChain
      LOGICAL                               :: isOuterBoundary
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER                               :: i, j, l, n
      CLASS(FRSegmentedCurve)     , POINTER :: innerSegment, outerSegment
      REAL(KIND=RP)                         :: x(3), y(3), d, outerInvScale, innerInvScale
      REAL(KIND=RP)                         :: nHatInner(3), nHatOuter(3), normalsDot, targetDot
      REAL(KIND=RP)                         :: vecToTarget(3)
      LOGICAL                               :: isProperTarget
!
!     -----------------------------
!     For each segment in the chain
!     -----------------------------
!
      DO l = 1, segmentedCurveChain % curveCount()
         outerSegment => segmentedCurveChain % segmentedCurveAtIndex(l)
!
!        --------------------------------
!        For each point along the segment
!        --------------------------------
!
         DO j = 1, outerSegment % COUNT()
            x             = outerSegment % positionAtIndex(j)
            outerInvScale = outerSegment % invScaleAtIndex(j)
            nHatOuter     = outerSegment % normalAtIndex(j)
!
!           ----------------------------------
!           For every other point along itself
!           ----------------------------------
!
            DO n = 1, segmentedCurveChain % curveCount()
               innerSegment => segmentedCurveChain % segmentedCurveAtIndex(n)
!
!              --------------------------------
!              For each point along the segment
!              --------------------------------
!
               DO i = 1, innerSegment % COUNT()
                  IF(n == l .AND. i == j)     CYCLE
                  y             = innerSegment % positionAtIndex(i)
                  innerInvScale = innerSegment % invScaleAtIndex(i)
                  nHatInner     = innerSegment % normalAtIndex(i)
                  vecToTarget   = y - x

!
                  normalsDot = DOT_PRODUCT(nHatInner,nHatOuter)
                  targetDot  = DOT_PRODUCT(vecToTarget,nHatOuter)


                  IF ( isOuterBoundary )     THEN
                     isProperTarget =  normalsDot  < -closeCurveNormalAlignment .AND. targetDot   < -normalTangentMin
                  ELSE
                     isProperTarget =  normalsDot  <  -closeCurveNormalAlignment .AND. targetDot   > normalTangentMin
                  END IF

                  IF( isProperTarget )     THEN
                     d = closeCurveFactor/SQRT( (x(1) - y(1))**2 + (x(2) - y(2))**2 ) ! Inverse length - 3 cells

                     outerInvScale = MAX(d,outerInvScale)
                     CALL outerSegment % setCurveInvScaleForIndex(outerInvScale,j)
                     innerInvScale = MAX(d,innerInvScale)
                     CALL innerSegment % setCurveInvScaleForIndex(innerInvScale,i)
                  END IF
               END DO
            END DO
         END DO
      END DO

      END SUBROUTINE CurveToCurveBoundaryDistances
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE sizeRatio(self, ratio, worstOffenderName)
!
!     ----------------------------------------------------------------------------
!     Goes through all the model curves and computes the ratio of the
!     background grid size to the smallest size inferred by the local
!     curvatures. The log_2(Ratio) gives the number of subdivsions that
!     would be needed (locally) to size the mesh. The routine also
!     returns the name of the curve with the smallest implied scale, so that
!     can be reported to the user, if desired.
!     ----------------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MeshSizer) :: self
         REAL(KIND=RP)    :: ratio
         CHARACTER(LEN=*) :: worstOffenderName
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP) :: backgroundGridSize, cSize, tSize

         backgroundGridSize = self % baseSize
         cSize              = -TINY(cSize)
         worstOffenderName  = "none"

         IF( ASSOCIATED(self % outerBoundary) )     THEN
            tSize = self % outerBoundary % maxInverseScale()
            IF ( tSize > cSize )     THEN
               cSize             = tSize
               worstOffenderName = self % outerBoundary % curveName
            END IF
         END IF

         IF ( ASSOCIATED( self % innerBoundariesList) )     THEN
            CALL maxInvSizeForCurvesInList(list      = self % innerBoundariesList, &
                                           cSize     = cSize,                      &
                                           curveName = worstOffenderName)
         END IF

         IF ( ASSOCIATED( self % interfaceBoundariesList) )     THEN
            CALL maxInvSizeForCurvesInList(list      = self % interfaceBoundariesList, &
                                           cSize     = cSize,                      &
                                           curveName = worstOffenderName)
         END IF

         ratio = backgroundGridSize*cSize

      END SUBROUTINE sizeRatio
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE maxInvSizeForCurvesInList( list, cSize, curveName )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList), POINTER :: list
         REAL(KIND=RP)                :: cSize
         CHARACTER(LEN=*)             :: curveName
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedListIterator) , POINTER :: iterator => NULL()
         CLASS(FTObject)             , POINTER :: obj => NULL()
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedCurveChain => NULL()
         REAL(KIND=RP)                         :: tSize

         ALLOCATE(iterator)
         CALL iterator % initWithFTLinkedList(list)
         CALL iterator % setToStart()

         DO WHILE (.NOT.iterator % isAtEnd())
            obj => iterator % object()
            CALL castToChainedSegmentedCurve(obj,segmentedCurveChain)
            tSize = segmentedCurveChain % maxInverseScale()
            IF ( tSize > cSize )     THEN
               cSize     = tSize
               curveName = segmentedCurveChain % curveName
            END IF

            CALL iterator % moveToNext()
         END DO
         obj => iterator
         CALL release(obj)

      END SUBROUTINE maxInvSizeForCurvesInList
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE printSizerDescription(self,iUnit)
         IMPLICIT NONE
         CLASS(MeshSizer) :: self
         INTEGER          :: iUnit

         WRITE(iUnit, *) "Number of inner boundaries = ", self % noOfInnerBoundaries
         WRITE(iUnit, *) "Number of interface boundaries = ", self % noOfInterfaceBoundaries
         WRITE(iUnit, *) "Base Size = ", self % baseSize
         WRITE(iUnit, *) "xMin = ", self % xMin
         WRITE(iUnit, *) "xMax = ", self % xMax
      END SUBROUTINE printSizerDescription
   END MODULE MeshSizerClass

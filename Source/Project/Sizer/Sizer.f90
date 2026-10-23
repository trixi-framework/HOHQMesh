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
!     ------------------------------------------------------------------
!     The points of a segmented curve chain copied into plain arrays, so
!     that the loops over all points do not go through the object
!     accessors. pointIndex/segmentIndex map back to the chain, and box
!     = [xMin, xMax, yMin, yMax] bounds the points.
!     ------------------------------------------------------------------
!
      TYPE ChainPoints
         INTEGER                                    :: count
         CLASS(ChainedSegmentedCurve), POINTER      :: chain => NULL()
         REAL(KIND=RP)                              :: box(4)
         REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE :: x, nHat
         REAL(KIND=RP), DIMENSION(:)  , ALLOCATABLE :: invScale
         INTEGER      , DIMENSION(:)  , ALLOCATABLE :: segmentIndex, pointIndex
      END TYPE ChainPoints
!
!     ----------------------------------------------------------------
!     The refinement centers and lines in arrays for controlSize. A
!     center only needs to be evaluated closer than the distance where
!     its size reaches the base size, cutoffSquared, see
!     BuildControlEntries.
!     ----------------------------------------------------------------
!
      TYPE SizerCenterEntry
         TYPE(SizerCenterControl), POINTER :: center => NULL()
         REAL(KIND=RP)                     :: cutoffSquared
      END TYPE SizerCenterEntry

      TYPE SizerLineEntry
         TYPE(SizerLineControl), POINTER :: line => NULL()
      END TYPE SizerLineEntry
!
!     ---------------------
!     Class type definition
!     ---------------------
!
      TYPE, EXTENDS(FTObject) :: MeshSizer
         INTEGER                                   :: noOfInnerBoundaries
         INTEGER                                   :: noOfInterfaceBoundaries
         REAL(KIND=RP)                             :: baseSize
         REAL(KIND=RP)                             :: xMin(3), xMax(3)
         CLASS(FTLinkedList)         , POINTER     :: controlsList            => NULL()
         CLASS(ChainedSegmentedCurve), POINTER     :: outerBoundary           => NULL()
         CLASS(FTLinkedList)         , POINTER     :: innerBoundariesList     => NULL()
         CLASS(FTLinkedList)         , POINTER     :: interfaceBoundariesList => NULL()
         CLASS(SMTopography)         , POINTER     :: topography              => NULL()
!
!        ------------------------------------------------------------
!        Positions of the points of all the boundary curves, used to
!        find the curve sizes in a box. Built on first use and
!        cleared when the boundary curves change.
!        ------------------------------------------------------------
!
         TYPE(ChainPoints), DIMENSION(:), ALLOCATABLE :: boxSearchPoints
!
!        ---------------------------------------------------------------
!        Refinement controls for controlSize, built on first use for the
!        base size controlsBaseSize and cleared when controls are added
!        ---------------------------------------------------------------
!
         TYPE(SizerCenterEntry), DIMENSION(:), ALLOCATABLE :: centerEntries
         TYPE(SizerLineEntry)  , DIMENSION(:), ALLOCATABLE :: lineEntries
         REAL(KIND=RP)                                     :: controlsBaseSize = -1.0_RP
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

         IF ( ALLOCATED(self % boxSearchPoints) )     DEALLOCATE(self % boxSearchPoints)

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

         IF ( ALLOCATED(self % centerEntries) )     DEALLOCATE(self % centerEntries, self % lineEntries)

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

         IF ( ALLOCATED(self % centerEntries) )     DEALLOCATE(self % centerEntries, self % lineEntries)

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

         IF ( ALLOCATED(self % boxSearchPoints) )     DEALLOCATE(self % boxSearchPoints)

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
         USE IntervalSearchModule
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
                  hMin = MIN( hMin, 1.0_RP/SQRT(sizer % topography % principalCurvatureAt(x)) )
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
         IF ( .NOT.ALLOCATED(sizer % boxSearchPoints) )     CALL BuildBoxSearchPoints(sizer)
         DO i = 1, SIZE(sizer % boxSearchPoints)
            cSize = MAX( cSize, invCurveSizeForBox(sizer % boxSearchPoints(i), xMin, xMax ) )
         END DO
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
            CALL iterator % initwithFTLinkedListClass(sizer % interfaceBoundariesList)
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
         TYPE(SizerCenterControl), POINTER :: center => NULL()
         REAL(KIND=RP)                     :: hFunInv, dSquared
         INTEGER                           :: k

         hFunInv = 1.0_RP/self % baseSize
!
!        --------------------------------
!        Sizes given by centers and lines
!        --------------------------------
!
         IF ( ASSOCIATED(self % controlsList) )     THEN

            IF ( .NOT.ALLOCATED(self % centerEntries) .OR. self % controlsBaseSize /= self % baseSize ) &
               CALL BuildControlEntries(self)

            DO k = 1, SIZE(self % centerEntries)
               center   => self % centerEntries(k) % center
               dSquared =  (x(1) - center % center(1))**2 + (x(2) - center % center(2))**2
               IF ( dSquared >= self % centerEntries(k) % cutoffSquared )     CYCLE
               hFunInv = MAX(hFunInv , hInvForCenter( center, x ) )
            END DO

            DO k = 1, SIZE(self % lineEntries)
               hFunInv = MAX(hFunInv , hInvForLineControl( self % lineEntries(k) % line, x ) )
            END DO

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
      SUBROUTINE BuildControlEntries( self )
!
!     -------------------------------------------------------------------
!     Copy the refinement controls into arrays. controlSize starts from
!     1/baseSize, so a center can only change the size where its 1/h is
!     larger than that. A smooth center has 1/h = 3^(-d/width)/meshSize,
!     which is below 1/baseSize for
!        d > width*LOG(baseSize/meshSize)/LOG(3).
!     The cutoff adds 0.01*width to that, where 1/h is at least 1% below
!     1/baseSize, so skipping the center beyond the cutoff cannot change
!     the result, not even by rounding. A sharp center is evaluated up to
!     slightly beyond its width. A center with meshSize >= baseSize never
!     changes the size and is always skipped.
!     -------------------------------------------------------------------
!
         IMPLICIT NONE
         CLASS(MeshSizer) :: self

         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         CLASS(FTObject)            , POINTER :: obj      => NULL()
         INTEGER                              :: numCenters, numLines
         REAL(KIND=RP)                        :: r

         IF ( ALLOCATED(self % centerEntries) )     DEALLOCATE(self % centerEntries, self % lineEntries)

         ALLOCATE(iterator)
         CALL iterator % initwithFTLinkedListClass( self % controlsList)
!
!        -----------------------
!        Count, then fill arrays
!        -----------------------
!
         numCenters = 0
         numLines   = 0
         CALL iterator % setToStart()
         DO WHILE (.NOT.iterator % isAtEnd())
            obj => iterator % object()
            SELECT TYPE(obj)
               TYPE IS (SizerCenterControl)
                  numCenters = numCenters + 1
               TYPE IS (SizerLineControl)
                  numLines = numLines + 1
               CLASS DEFAULT
            END SELECT
            CALL iterator % moveToNext()
         END DO
         ALLOCATE( self % centerEntries(numCenters), self % lineEntries(numLines) )

         numCenters = 0
         numLines   = 0
         CALL iterator % setToStart()
         DO WHILE (.NOT.iterator % isAtEnd())
            obj => iterator % object()
            SELECT TYPE(obj)
               TYPE IS (SizerCenterControl)
                  numCenters = numCenters + 1
                  self % centerEntries(numCenters) % center => obj
                  IF ( obj % meshSize >= self % baseSize )     THEN
                     self % centerEntries(numCenters) % cutoffSquared = -1.0_RP
                  ELSE IF ( obj % centerType == CENTER_SMOOTH )     THEN
                     r = obj % width*LOG(self % baseSize/obj % meshSize)/LOG(3.0_RP) + 0.01_RP*obj % width
                     self % centerEntries(numCenters) % cutoffSquared = r**2
                  ELSE
                     self % centerEntries(numCenters) % cutoffSquared = (1.000001_RP*obj % width)**2
                  END IF
               TYPE IS (SizerLineControl)
                  numLines = numLines + 1
                  self % lineEntries(numLines) % line => obj
               CLASS DEFAULT
            END SELECT
            CALL iterator % moveToNext()
         END DO
         CALL releaseFTLinkedListIteratorClass(iterator)

         self % controlsBaseSize = self % baseSize

      END SUBROUTINE BuildControlEntries
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION invCurveSizeForBox(points, xMin, xMax )
      USE Geometry
      IMPLICIT NONE
!
!     ----------------------------------------------------------
!     Largest inverse scale of the curve points inside the box.
!     Points clearly outside the box are skipped with a bounding
!     box test whose margin is larger than the tolerance used by
!     PointInQuad, which then decides for the remaining points.
!     ----------------------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP)     :: xMin(3), xMax(3)
         TYPE(ChainPoints) :: points
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP)                    :: nodes(3,4), x(3)
         CLASS(FRSegmentedCurve), POINTER :: c => NULL()
         INTEGER                          :: n
         REAL(KIND=RP)                    :: s, w, h, marginX, marginY
         REAL(KIND=RP)                    :: xLeft, xRight, yBottom, yTop

         invCurveSizeForBox = 0.0_RP

         w = xMax(1) - xMin(1)
         h = xMax(2) - xMin(2)
         marginX = 1.0E-6_RP*MAX(w,h) + 8.0_RP*EPSILON(1.0_RP)/h + 8.0_RP*SPACING(MAX(ABS(xMin(1)),ABS(xMax(1))))
         marginY = 1.0E-6_RP*MAX(w,h) + 8.0_RP*EPSILON(1.0_RP)/w + 8.0_RP*SPACING(MAX(ABS(xMin(2)),ABS(xMax(2))))
         xLeft   = xMin(1) - marginX
         xRight  = xMax(1) + marginX
         yBottom = xMin(2) - marginY
         yTop    = xMax(2) + marginY

         IF ( points % box(2) < xLeft   .OR. points % box(1) > xRight .OR. &
              points % box(4) < yBottom .OR. points % box(3) > yTop )     RETURN

         nodes(:,1) = xMin
         nodes(:,2) = (/xMax(1),xMin(2), 0.0_RP/)
         nodes(:,3) = xMax
         nodes(:,4) = (/xMin(1),xMax(2), 0.0_RP/)

         DO n = 1, points % count
            IF ( points % x(1,n) < xLeft   .OR. points % x(1,n) > xRight .OR. &
                 points % x(2,n) < yBottom .OR. points % x(2,n) > yTop )     CYCLE
            x = points % x(:,n)
            IF ( PointInQuad(nodes,x) )     THEN
               c => points % chain % segmentedCurveAtIndex(points % segmentIndex(n))
               s =  c % invScaleAtIndex(points % pointIndex(n))
               invCurveSizeForBox =  MAX(s, invCurveSizeForBox)
            END IF
         END DO

      END FUNCTION invCurveSizeForBox
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE BuildBoxSearchPoints( sizer )
!
!     -----------------------------------------------------------
!     Gather the points of the outer, inner, and interface curves
!     for invCurveSizeForBox. Only the positions are used, which
!     do not change; the scales are looked up on the curves.
!     -----------------------------------------------------------
!
         IMPLICIT NONE
         CLASS(MeshSizer) :: sizer

         INTEGER :: numberOfChains, k

         numberOfChains = sizer % noOfInnerBoundaries + sizer % noOfInterfaceBoundaries
         IF ( ASSOCIATED(sizer % outerBoundary) )     numberOfChains = numberOfChains + 1
         ALLOCATE( sizer % boxSearchPoints(numberOfChains) )

         k = 0
         IF ( ASSOCIATED(sizer % outerBoundary) )     THEN
            k = k + 1
            CALL GatherChainPoints(sizer % outerBoundary, sizer % boxSearchPoints(k))
         END IF

         IF ( ASSOCIATED(sizer % innerBoundariesList) )     &
            CALL GatherChainPointsInList(sizer % innerBoundariesList, sizer % boxSearchPoints, k)
         IF ( ASSOCIATED(sizer % interfaceBoundariesList) ) &
            CALL GatherChainPointsInList(sizer % interfaceBoundariesList, sizer % boxSearchPoints, k)

      END SUBROUTINE BuildBoxSearchPoints
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GatherChainPointsInList( list, points, k )
!
!     -----------------------------------------------------------
!     Gather the points of each curve chain in the list into
!     points(k+1), points(k+2), ... On return k is the index of
!     the last entry filled.
!     -----------------------------------------------------------
!
         IMPLICIT NONE
         CLASS(FTLinkedList), POINTER :: list
         TYPE(ChainPoints)            :: points(:)
         INTEGER                      :: k

         CLASS(FTLinkedListIterator) , POINTER :: iterator => NULL()
         CLASS(FTObject)             , POINTER :: obj      => NULL()
         CLASS(ChainedSegmentedCurve), POINTER :: chain    => NULL()

         ALLOCATE(iterator)
         CALL iterator % initWithFTLinkedListClass(list)
         CALL iterator % setToStart()
         DO WHILE (.NOT.iterator % isAtEnd())
            obj => iterator % object()
            CALL castToChainedSegmentedCurve(obj,chain)
            k = k + 1
            CALL GatherChainPoints(chain, points(k))
            CALL iterator % moveToNext()
         END DO
         CALL releaseFTLinkedListIteratorClass(iterator)

      END SUBROUTINE GatherChainPointsInList
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
         CLASS(FTLinkedListIterator), POINTER                   :: iterator => NULL()
         CLASS(FTObject)            , POINTER                   :: obj => NULL()

         CLASS(ChainedSegmentedCurve), POINTER                  :: innerSegmentedCurveChain => NULL()
         TYPE(ChainPoints)                                      :: outerPoints
         TYPE(ChainPoints)          , DIMENSION(:), ALLOCATABLE :: innerPoints
         INTEGER                                                :: k, m
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

            CALL iterator % initwithFTLinkedListClass(self % innerBoundariesList)
            CALL iterator % setToStart()
            DO WHILE (.NOT.iterator % isAtEnd())
               obj     => iterator % object()
               CALL castToChainedSegmentedCurve(obj,innerSegmentedCurveChain)
               innerCurvesArray(k) % curve => innerSegmentedCurveChain
               k = k + 1
               CALL iterator % moveToNext()
            END DO
            CALL releaseFTLinkedListIteratorClass(iterator)
         END IF

         IF ( self % noOfInterfaceBoundaries > 0 )     THEN
            ALLOCATE(iterator)

            CALL iterator % initwithFTLinkedListClass(self % interfaceBoundariesList)
            CALL iterator % setToStart
            DO WHILE (.NOT.iterator % isAtEnd())
               obj     => iterator % object()
               CALL castToChainedSegmentedCurve(obj,innerSegmentedCurveChain)
               innerCurvesArray(k) % curve => innerSegmentedCurveChain
               k                           = k + 1
               CALL iterator % moveToNext()
            END DO
            CALL releaseFTLinkedListIteratorClass(iterator)
         END IF
!
!        -------------------------------------------------------
!        Without an outer curve, find the distances of the inner
!        curves to the bounding box. This loop is linear in the
!        number of points so it works on the curves directly
!        -------------------------------------------------------
!
         IF ( .NOT.ASSOCIATED(self%outerBoundary) .AND. numberOfInsideBoundaries > 0 )     THEN
            CALL OuterBoxToInnerBoundaryDistances(self,innerCurvesArray,numberOfInsideBoundaries)
         END IF
!
!        ---------------------------------------------------------
!        The remaining searches are over all pairs of points. Copy
!        the points into arrays first. The scales only increase
!        (MAX), so the order in which they are updated does not
!        matter and they are copied back at the end.
!        ---------------------------------------------------------
!
         IF( ASSOCIATED(self % outerBoundary) )     CALL GatherChainPoints(self % outerBoundary, outerPoints)
         IF( numberOfInsideBoundaries > 0 )     THEN
            ALLOCATE( innerPoints(numberOfInsideBoundaries) )
            DO k = 1, numberOfInsideBoundaries
               CALL GatherChainPoints(innerCurvesArray(k) % curve, innerPoints(k))
            END DO
         END IF
!
!        -----------------------------------------------
!        Find the distances of the outer boundary curve
!        to all inner curves
!        -----------------------------------------------
!
         IF ( ASSOCIATED(self%outerBoundary) .AND. numberOfInsideBoundaries > 0 )     THEN
            DO k = 1, numberOfInsideBoundaries
               CALL OuterToInnerBoundaryDistances(outerPoints, innerPoints(k))
            END DO
         END IF
!
!        ------------------------------------
!        Do the same for the inner boundaries
!        The difference here is that facing
!        boundaries have normals that are
!        opposite to each other.
!        ------------------------------------
!
         DO k = 1, numberOfInsideBoundaries
            DO m = k+1, numberOfInsideBoundaries
               CALL InnerToInnerBoundaryDistances(innerPoints(k), innerPoints(m))
            END DO
         END DO
!
!        ---------------------------------------------------
!        Finally, check to see if a curve is close to itself
!        ---------------------------------------------------
!
         IF(ASSOCIATED(self % outerBoundary))     THEN
            CALL CurveToCurveBoundaryDistances(outerPoints, isOuterBoundary = .TRUE., isInterface = .FALSE.)
            CALL ScatterChainInvScales(outerPoints)
         END IF

         DO k = 1, numberOfInsideBoundaries
            CALL CurveToCurveBoundaryDistances(innerPoints(k), isOuterBoundary = .FALSE., &
                                               isInterface = k > self % noOfInnerBoundaries )
            CALL ScatterChainInvScales(innerPoints(k))
         END DO
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
      SUBROUTINE OuterToInnerBoundaryDistances(outer, inner)
!
!      -------------------------------------------------------------------
!      Find the distance from the outer curve to an inner curve
!      -------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(ChainPoints) :: outer, inner
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER       :: i, m
      REAL(KIND=RP) :: d, dot

      DO i = 1, outer % count
         DO m = 1, inner % count
            d = closeCurveFactor/SQRT( (outer % x(1,i) - inner % x(1,m))**2 &
                                     + (outer % x(2,i) - inner % x(2,m))**2 ) ! Inverse length - 3 cells
!
!           -----------------------------------------------------------
!           Curves that are close and face each other should have their
!           mesh sizes adjusted to have enough between them
!           -----------------------------------------------------------
!
            dot = DOT_PRODUCT(inner % nHat(:,m), outer % nHat(:,i))
            IF( dot > closeCurveNormalAlignment )     THEN
               outer % invScale(i) = MAX(d, outer % invScale(i))
               inner % invScale(m) = MAX(d, inner % invScale(m))
            END IF
         END DO
      END DO

      END SUBROUTINE OuterToInnerBoundaryDistances
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE OuterBoxToInnerBoundaryDistances(self, innerCurvesArray, numberOfInsideBoundaries)
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

      END SUBROUTINE OuterBoxToInnerBoundaryDistances
!
!////////////////////////////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE InnerToInnerBoundaryDistances(first, second)
!
!      -------------------------------------------------------------------
!      Find the distance between two inner curves
!      -------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(ChainPoints) :: first, second
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER       :: i, j
      REAL(KIND=RP) :: d, dot

      DO j = 1, first % count
         DO i = 1, second % count
            d = closeCurveFactor/SQRT( (first % x(1,j) - second % x(1,i))**2 &
                                     + (first % x(2,j) - second % x(2,i))**2 ) ! Inverse length - 3 cells
!
            dot = DOT_PRODUCT(second % nHat(:,i), first % nHat(:,j))

            IF( dot < -closeCurveNormalAlignment )     THEN
               first  % invScale(j) = MAX(d, first  % invScale(j))
               second % invScale(i) = MAX(d, second % invScale(i))
            END IF
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
         CALL iterator % initWithFTLinkedListClass(self % interfaceBoundariesList)
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
      SUBROUTINE CurveToCurveBoundaryDistances(points, isOuterBoundary, isInterface )
!
!      ---------------------------------------------------------------
!      Find the distance from a curve to itself. An interface curve is
!      meshed on both sides, so the distance across its inside counts
!      as well as the distance across the outside.
!      ---------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(ChainPoints) :: points
      LOGICAL           :: isOuterBoundary
      LOGICAL           :: isInterface
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER       :: i, j
      REAL(KIND=RP) :: d, normalsDot, targetDot
      REAL(KIND=RP) :: vecToTarget(3)
      LOGICAL       :: isProperTarget
!
!     ------------------------------------------------
!     For each point along the curve and every other
!     point along itself
!     ------------------------------------------------
!
      DO j = 1, points % count
         DO i = 1, points % count
            IF(i == j)     CYCLE
            vecToTarget = points % x(:,i) - points % x(:,j)

            normalsDot = DOT_PRODUCT(points % nHat(:,i), points % nHat(:,j))
            targetDot  = DOT_PRODUCT(vecToTarget, points % nHat(:,j))

            IF ( isInterface )     THEN
               isProperTarget = normalsDot < -closeCurveNormalAlignment .AND. ABS(targetDot) > normalTangentMin
            ELSE IF ( isOuterBoundary )     THEN
               isProperTarget = normalsDot < -closeCurveNormalAlignment .AND. targetDot < -normalTangentMin
            ELSE
               isProperTarget = normalsDot < -closeCurveNormalAlignment .AND. targetDot > normalTangentMin
            END IF

            IF( isProperTarget )     THEN
               d = closeCurveFactor/SQRT( (points % x(1,j) - points % x(1,i))**2 &
                                        + (points % x(2,j) - points % x(2,i))**2 ) ! Inverse length - 3 cells
               points % invScale(j) = MAX(d, points % invScale(j))
               points % invScale(i) = MAX(d, points % invScale(i))
            END IF
         END DO
      END DO

      END SUBROUTINE CurveToCurveBoundaryDistances
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE GatherChainPoints( chain, points )
!
!     ---------------------------------------------------------
!     Copy the positions, normals and scales of all the points
!     of a segmented curve chain into arrays
!     ---------------------------------------------------------
!
      IMPLICIT NONE
      CLASS(ChainedSegmentedCurve), POINTER :: chain
      TYPE(ChainPoints)                     :: points

      CLASS(FRSegmentedCurve), POINTER :: segment
      INTEGER                          :: l, j, n

      n = 0
      DO l = 1, chain % curveCount()
         segment => chain % segmentedCurveAtIndex(l)
         n = n + segment % COUNT()
      END DO

      points % count = n
      ALLOCATE( points % x(3,n), points % nHat(3,n), points % invScale(n) )
      ALLOCATE( points % segmentIndex(n), points % pointIndex(n) )

      n = 0
      DO l = 1, chain % curveCount()
         segment => chain % segmentedCurveAtIndex(l)
         DO j = 1, segment % COUNT()
            n = n + 1
            points % x(:,n)          = segment % positionAtIndex(j)
            points % nHat(:,n)       = segment % normalAtIndex(j)
            points % invScale(n)     = segment % invScaleAtIndex(j)
            points % segmentIndex(n) = l
            points % pointIndex(n)   = j
         END DO
      END DO

      points % chain => chain
      IF ( n > 0 )     THEN
         points % box = [MINVAL(points % x(1,:)), MAXVAL(points % x(1,:)), &
                         MINVAL(points % x(2,:)), MAXVAL(points % x(2,:))]
      ELSE
         points % box = [HUGE(1.0_RP), -HUGE(1.0_RP), HUGE(1.0_RP), -HUGE(1.0_RP)]
      END IF

      END SUBROUTINE GatherChainPoints
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ScatterChainInvScales( points )
!
!     --------------------------------------------------
!     Copy the (updated) scales back to the curve chain
!     --------------------------------------------------
!
      IMPLICIT NONE
      TYPE(ChainPoints) :: points

      CLASS(FRSegmentedCurve), POINTER :: segment
      INTEGER                          :: n

      DO n = 1, points % count
         segment => points % chain % segmentedCurveAtIndex(points % segmentIndex(n))
         CALL segment % setCurveInvScaleForIndex(points % invScale(n), points % pointIndex(n))
      END DO

      END SUBROUTINE ScatterChainInvScales
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
         CALL iterator % initWithFTLinkedListClass(list)
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

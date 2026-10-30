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
!>      SegmentedCurveArray.f95
!!      Created: 2010-08-09 12:55:36 -0400 
!!      By: David Kopriva
!!
!!      A SegmentedCurveArray represents a curve as an array
!!      of points. It can be used to determine if a point falls inside
!!      of a curve.
!!
!!     Usage:
!!
!!        *Creation*
!!
!!           allocate(curve)
!!           curve % initWithNumberOfSegmentsNameAndID(nSegments, name, id)
!!           curve % setProperties( boundaryName, x, t, id )
!!
!!         *Bounding Box (3D)*
!!
!!            CALL curve % setBoundingBox(box)
!!
!!         *Determining if a point is inside the curve*
!!
!!            LOGICAL test = PointIsInsideCurve(self, p)
!  
!
!////////////////////////////////////////////////////////////////////////
!
      Module SegmentedCurveArrayClass
      USE SMConstants
      USE ProgramGlobals
      USE FTObjectClass
      USE Geometry, ONLY: ACWindingFunction, segmentWindingIncrement
      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      INTEGER, PARAMETER :: SEGMENTED_CURVE_NAME_LENGTH = 32
!     ----------
!     Class type
!     ----------
!
      TYPE, EXTENDS(FTObject) :: SegmentedCurveArray
         INTEGER                                    :: id
         INTEGER                                    :: nSegments, direction
         REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE :: x
         REAL(KIND=RP), DIMENSION(:)  , ALLOCATABLE :: curvature
         REAL(KIND=RP), DIMENSION(:)  , ALLOCATABLE :: t
         REAL(KIND=RP)                              :: boundingBox(6)
         CHARACTER(LEN=SEGMENTED_CURVE_NAME_LENGTH) :: boundaryName_
!
!        -------------------------------------------------------------
!        Search structures built by setPoints, so that windingNumber and
!        nearestPoint only look at the segments near a point:
!        the segments that reach into each horizontal strip, and the
!        segments whose bounding boxes overlap each cell of a grid
!        -------------------------------------------------------------
!
         INTEGER                                    :: nStrips = 0
         REAL(KIND=RP)                              :: stripBottom, stripHeight
         INTEGER      , DIMENSION(:)  , ALLOCATABLE :: stripStart, stripSegments
         INTEGER                                    :: nCellsX = 0, nCellsY = 0
         REAL(KIND=RP)                              :: cellOrigin(2), cellSize
         INTEGER      , DIMENSION(:)  , ALLOCATABLE :: cellStart, cellSegments
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithNumberOfSegmentsNameAndID
         PROCEDURE :: setPoints
         PROCEDURE :: setBoundaryName
         PROCEDURE :: boundaryName
         PROCEDURE :: PointIsInsideCurve
         PROCEDURE :: windingNumber
         PROCEDURE :: nearestPoint
         PROCEDURE :: cellRangeForBox
         PROCEDURE :: setBoundingBox
         
         FINAL     :: destructSegmentedCurveArray
         PROCEDURE :: printDescription => PrintSegmentedCurveArray
         
      END TYPE SegmentedCurveArray
!
!     ========
      CONTAINS 
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initWithNumberOfSegmentsNameAndID( self, nSegments, bName, id ) 
         IMPLICIT NONE
         INTEGER                    :: nSegments
         CLASS(SegmentedCurveArray) :: self
         CHARACTER(LEN=*)           :: bName
         INTEGER                    :: id
         
         CALL self % setBoundaryName(bName)
         self % id           = id
         
         CALL self % FTObject % init()
         
         self % nSegments = nSegments
         
         ALLOCATE( self % x(3,0:nSegments) )
         ALLOCATE( self % t(0:nSegments) )
         ALLOCATE( self % curvature(0:nSegments) )
         
      END SUBROUTINE initWithNumberOfSegmentsNameAndID
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE setBoundaryName( self, bName )  
         IMPLICIT NONE
         CLASS(SegmentedCurveArray) :: self
         CHARACTER(LEN=*)           :: bName
         self % boundaryName_ = bName
      END SUBROUTINE setBoundaryName
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION boundaryName(self) RESULT(bName)
         IMPLICIT NONE  
         CLASS(SegmentedCurveArray)                 :: self
         CHARACTER(LEN=SEGMENTED_CURVE_NAME_LENGTH) :: bName
         bName = self % boundaryName_
      END FUNCTION  
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE setPoints(self, t, x)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SegmentedCurveArray)     :: self
         REAL(KIND=RP), DIMENSION(:,0:) :: x
         REAL(KIND=RP), DIMENSION(0:)   :: t
!
!        ---------------
!        Local variables
!        ---------------
!
         self % x = x
         self % t = t
         
         CALL ComputeCurvature( self )
         CALL BuildStripIndex( self )
         CALL BuildCellIndex( self )
         
      END SUBROUTINE setPoints
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DestructSegmentedCurveArray( self ) 
         IMPLICIT NONE
         TYPE(SegmentedCurveArray) :: self
         self % nSegments = 0
         DEALLOCATE( self % x, self % t, self % curvature )
         IF ( ALLOCATED(self % stripStart) )     DEALLOCATE( self % stripStart, self % stripSegments )
         IF ( ALLOCATED(self % cellStart) )      DEALLOCATE( self % cellStart, self % cellSegments )
         
      END SUBROUTINE DestructSegmentedCurveArray
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseCurveArray(self)  
         IMPLICIT NONE
         CLASS(SegmentedCurveArray), POINTER :: self
         CLASS(FTObject)           , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseCurveArray
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeCurvature( self )
      IMPLICIT NONE 
      TYPE(SegmentedCurveArray)                    :: self
      INTEGER                                      :: j, N
      REAL(KIND=RP), DIMENSION(3)                  :: xp, xpp
      REAL(KIND=RP), DIMENSION(0:self % nSegments) :: dt
      
      N = self % nSegments
      DO j = 1, N-1 
         dt(j) = self % t(j+1) - self % t(j)
      END DO
      dt(N) = dt(N-1)
      dt(0) = dt(1)
      
      DO j = 1, N-1 
         xp = -dt(j+1)*self % x(:,j-1)/(dt(j)*(dt(j)+dt(j+1))) &
              -(dt(j) - dt(j+1))/(dt(j)*dt(j+1))*self % x(:,j) &
              + dt(j-1)/(dt(j+1)*(dt(j)+dt(j+1)))*self % x(:,j+1)
              
         xpp = 2*self % x(:,j+1)/(dt(j)*(dt(j)+dt(j+1))) &
              -2/(dt(j)*dt(j+1))*self % x(:,j) &
              + 2/(dt(j+1)*(dt(j)+dt(j+1)))*self % x(:,j+1)
              
         self % curvature(j) = ABS( xp(1)*xpp(2) - xp(2)*xpp(1) )/(xp(1)**2 + xp(2)**2)**1.5_RP
      END DO
      self % curvature(0) = self % curvature(1)
      self % curvature(N) = self % curvature(N-1)
      
      END SUBROUTINE ComputeCurvature
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PrintSegmentedCurveArray( self, iUnit ) 
         IMPLICIT NONE 
         CLASS(SegmentedCurveArray) :: self
         INTEGER                    :: iUnit
         INTEGER                    :: j
         
         DO j = 0, self % nSegments 
            WRITE(iUnit,*) self % t(j),self % x(:,j)
         END DO

      END SUBROUTINE PrintSegmentedCurveArray
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetBoundingBox( self, bBox) 
         IMPLICIT NONE 
         CLASS(SegmentedCurveArray) :: self
         REAL(KIND=RP)             :: bBox(6)
         self % boundingBox = bBox
      END SUBROUTINE SetBoundingBox 
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION PointIsInsideCurve(self, p)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SegmentedCurveArray) :: self
         REAL(KIND=RP)              :: p(3)
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP) :: w
         
         PointIsInsideCurve = .true.
         w = ACWindingFunction( p, self % x, self % nSegments-1 )
         IF ( abs(w) <= EPSILON(w) ) THEN
            PointIsInsideCurve = .false.
         END IF
      END FUNCTION PointIsInsideCurve 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE BuildStripIndex( self )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        Sort the segments used by windingNumber into horizontal strips.
!        With n = nSegments, segment i < n-1 goes from x(:,i) to
!        x(:,i+1). The curve is closed either from x(:,n-1) to x(:,0)
!        (segment n+1), or through the last point, from x(:,n-1) to
!        x(:,n) (segment n-1) and on to x(:,0) (segment n). A segment
!        is put in every strip within a small tolerance of its heights,
!        which is well above the AlmostEqual tolerance of
!        segmentWindingIncrement.
!        ---------------------------------------------------------------
!
         CLASS(SegmentedCurveArray) :: self
         INTEGER                    :: i, j, k, n, lo, hi
         REAL(KIND=RP)              :: yMin, yMax, tol, a(3), b(3)
         INTEGER, ALLOCATABLE       :: fill(:)

         self % nStrips = 0
         IF ( ALLOCATED(self % stripStart) )     DEALLOCATE( self % stripStart, self % stripSegments )
         n = self % nSegments
         IF ( n < 2 )     RETURN

         yMin = MINVAL(self % x(2,0:n))
         yMax = MAXVAL(self % x(2,0:n))
         tol  = 1.0E-10_RP*(1.0_RP + MAX(ABS(yMin), ABS(yMax)))
         self % nStrips     = n
         self % stripBottom = yMin - tol
         self % stripHeight = (yMax - yMin + 2.0_RP*tol)/self % nStrips

         ALLOCATE( self % stripStart(self % nStrips + 1), fill(self % nStrips) )
         fill = 0
         DO k = 1, 2
            DO i = 0, n + 1
               CALL stripSegmentEnds( self, i, a, b )
               lo = stripForHeight( self, MIN(a(2), b(2)) - tol )
               hi = stripForHeight( self, MAX(a(2), b(2)) + tol )
               IF ( k == 1 )     THEN
                  fill(lo:hi) = fill(lo:hi) + 1
               ELSE
                  DO j = lo, hi
                     self % stripSegments(fill(j)) = i
                     fill(j) = fill(j) + 1
                  END DO
               END IF
            END DO
            IF ( k == 1 )     THEN
               self % stripStart(1) = 1
               DO j = 1, self % nStrips
                  self % stripStart(j+1) = self % stripStart(j) + fill(j)
               END DO
               ALLOCATE( self % stripSegments(self % stripStart(self % nStrips + 1) - 1) )
               fill = self % stripStart(1:self % nStrips)
            END IF
         END DO

      END SUBROUTINE BuildStripIndex
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE stripSegmentEnds( self, i, a, b )
         IMPLICIT NONE
!
!        ----------------------------------------------------
!        The end points of segment i of the strip index (see
!        BuildStripIndex)
!        ----------------------------------------------------
!
         CLASS(SegmentedCurveArray) :: self
         INTEGER                    :: i
         REAL(KIND=RP)              :: a(3), b(3)
         INTEGER                    :: n

         n = self % nSegments
         IF ( i < n )     THEN
            a = self % x(:,i)
            b = self % x(:,i+1)
         ELSE IF ( i == n )     THEN
            a = self % x(:,n)
            b = self % x(:,0)
         ELSE
            a = self % x(:,n-1)
            b = self % x(:,0)
         END IF

      END SUBROUTINE stripSegmentEnds
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION stripForHeight( self, y )
         IMPLICIT NONE
         CLASS(SegmentedCurveArray) :: self
         REAL(KIND=RP)              :: y
         stripForHeight = MIN(self % nStrips, MAX(1, INT((y - self % stripBottom)/self % stripHeight) + 1))
      END FUNCTION stripForHeight
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION windingNumber( self, p, throughLastPoint )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        ACWindingFunction( p, self % x, self % nSegments-1 ), or with
!        throughLastPoint = .TRUE., ACWindingFunction( p, self % x,
!        self % nSegments ), using only the segments in the strip of p.
!        Every segment that can contribute is in that strip, so the
!        result is the same.
!        ---------------------------------------------------------------
!
         CLASS(SegmentedCurveArray) :: self
         REAL(KIND=RP)              :: p(3)
         LOGICAL, OPTIONAL          :: throughLastPoint
         INTEGER                    :: k, i, j, n
         LOGICAL                    :: onCurve, lastPoint
         REAL(KIND=RP)              :: a(3), b(3)

         lastPoint = .FALSE.
         IF ( PRESENT(throughLastPoint) )     lastPoint = throughLastPoint
         n = self % nSegments

         IF ( self % nStrips == 0 )     THEN
            windingNumber = ACWindingFunction( p, self % x, MERGE(n, n-1, lastPoint) )
            RETURN
         END IF

         windingNumber = 0
         IF ( p(2) < self % stripBottom .OR. &
              p(2) > self % stripBottom + self % nStrips*self % stripHeight )     RETURN

         k = stripForHeight( self, p(2) )
         DO i = self % stripStart(k), self % stripStart(k+1) - 1
            j = self % stripSegments(i)
            IF ( lastPoint )     THEN
               IF ( j == n+1 )     CYCLE
            ELSE
               IF ( j == n-1 .OR. j == n )     CYCLE
            END IF
            CALL stripSegmentEnds( self, j, a, b )
            windingNumber = windingNumber + segmentWindingIncrement( p, a, b, onCurve )
            IF ( onCurve )     THEN
               windingNumber = 0
               RETURN
            END IF
         END DO

      END FUNCTION windingNumber
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE BuildCellIndex( self )
         IMPLICIT NONE
!
!        ---------------------------------------------------------------
!        Sort the segments used by nearestPoint, from x(:,j) to x(:,j+1)
!        for j = 0, ..., nSegments-1, into the cells of a uniform grid
!        that covers the curve, about one cell per segment. A segment is
!        put in every cell that its bounding box overlaps.
!        ---------------------------------------------------------------
!
         CLASS(SegmentedCurveArray) :: self
         INTEGER                    :: j, k, n, ix, iy, ix0, ix1, iy0, iy1, c
         REAL(KIND=RP)              :: xMin(2), xMax(2), width
         INTEGER, ALLOCATABLE       :: fill(:)

         self % nCellsX = 0
         self % nCellsY = 0
         IF ( ALLOCATED(self % cellStart) )     DEALLOCATE( self % cellStart, self % cellSegments )
         n = self % nSegments
         IF ( n < 1 )     RETURN

         xMin  = MINVAL(self % x(1:2,0:n), DIM = 2)
         xMax  = MAXVAL(self % x(1:2,0:n), DIM = 2)
         width = MAXVAL(xMax - xMin)
         IF ( width <= 0.0_RP )     RETURN
         self % cellSize   = width/MAX(1, NINT(SQRT(REAL(n, KIND=RP))))
         self % cellOrigin = xMin
         self % nCellsX    = INT((xMax(1) - xMin(1))/self % cellSize) + 1
         self % nCellsY    = INT((xMax(2) - xMin(2))/self % cellSize) + 1

         ALLOCATE( self % cellStart(self % nCellsX*self % nCellsY + 1), fill(self % nCellsX*self % nCellsY) )
         fill = 0
         DO k = 1, 2
            DO j = 0, n - 1
               ix0 = cellIndex( self, MIN(self % x(1,j), self % x(1,j+1)), 1 )
               ix1 = cellIndex( self, MAX(self % x(1,j), self % x(1,j+1)), 1 )
               iy0 = cellIndex( self, MIN(self % x(2,j), self % x(2,j+1)), 2 )
               iy1 = cellIndex( self, MAX(self % x(2,j), self % x(2,j+1)), 2 )
               DO iy = iy0, iy1
                  DO ix = ix0, ix1
                     c = ix + (iy - 1)*self % nCellsX
                     IF ( k == 2 )     self % cellSegments(fill(c)) = j
                     fill(c) = fill(c) + 1
                  END DO
               END DO
            END DO
            IF ( k == 1 )     THEN
               self % cellStart(1) = 1
               DO c = 1, SIZE(fill)
                  self % cellStart(c+1) = self % cellStart(c) + fill(c)
               END DO
               ALLOCATE( self % cellSegments(self % cellStart(SIZE(fill) + 1) - 1) )
               fill = self % cellStart(1:SIZE(fill))
            END IF
         END DO

      END SUBROUTINE BuildCellIndex
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION cellIndex( self, x, direction )
         IMPLICIT NONE
         CLASS(SegmentedCurveArray) :: self
         REAL(KIND=RP)              :: x
         INTEGER                    :: direction
         INTEGER                    :: nCells

         nCells = MERGE(self % nCellsX, self % nCellsY, direction == 1)
         cellIndex = MIN(nCells, MAX(1, INT((x - self % cellOrigin(direction))/self % cellSize) + 1))
      END FUNCTION cellIndex
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE cellRangeForBox( self, box, ix0, ix1, iy0, iy1 )
         IMPLICIT NONE
!
!        ------------------------------------------------------------
!        The cells of the segment grid that the box [xMin, xMax, yMin,
!        yMax] overlaps. Every segment whose bounding box overlaps the
!        box is in one of them. Without a grid (nCellsX = 0) the range
!        is empty.
!        ------------------------------------------------------------
!
         CLASS(SegmentedCurveArray) :: self
         REAL(KIND=RP)              :: box(4)
         INTEGER                    :: ix0, ix1, iy0, iy1

         IF ( self % nCellsX == 0 )     THEN
            ix0 = 1; ix1 = 0; iy0 = 1; iy1 = 0
            RETURN
         END IF
         ix0 = cellIndex( self, box(1), 1 )
         ix1 = cellIndex( self, box(2), 1 )
         iy0 = cellIndex( self, box(3), 2 )
         iy1 = cellIndex( self, box(4), 2 )
      END SUBROUTINE cellRangeForBox
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE nearestPoint( self, p, s, pImage, distance, searchRadius )
         IMPLICIT NONE
!
!        -----------------------------------------------------------------
!        Point on the segments nearest to p, its location s = j + fraction
!        along segment j (from x(:,j) to x(:,j+1)), and its distance from
!        p. Of equally near segments, the first is used. The cells around
!        the cell of p are searched in rings: every segment not yet seen
!        is in a ring further out, more than (ring - 1) cells away. One
!        more ring is searched to allow for rounding at the cell edges.
!        With a searchRadius, the search stops once every segment not yet
!        seen is further away than that. The result is then the nearest
!        point if it is within searchRadius; otherwise it is only known
!        to be further.
!        -----------------------------------------------------------------
!
         CLASS(SegmentedCurveArray) :: self
         REAL(KIND=RP)              :: p(3), s, pImage(2), distance
         REAL(KIND=RP), OPTIONAL    :: searchRadius
         INTEGER                    :: j, i, ring, ix, iy, ix0, iy0, c, jBest, maxRing
         REAL(KIND=RP)              :: t, dist

         distance = HUGE(distance)
         s        = 0.0_RP
         pImage   = self % x(1:2,0)
         jBest    = -1

         IF ( self % nCellsX == 0 )     THEN
            DO j = 0, self % nSegments - 1
               CALL distanceToSegment( self, p, j, t, dist )
               IF ( dist < distance )     CALL setNearest( j, t, dist )
            END DO
            RETURN
         END IF

         ix0     = cellIndex( self, p(1), 1 )
         iy0     = cellIndex( self, p(2), 2 )
         maxRing = MAX(self % nCellsX, self % nCellsY)
         DO ring = 0, maxRing
            IF ( distance < (ring - 2)*self % cellSize )     EXIT
            IF ( PRESENT(searchRadius) )     THEN
               IF ( searchRadius < (ring - 2)*self % cellSize )     EXIT
            END IF
            DO iy = MAX(1, iy0 - ring), MIN(self % nCellsY, iy0 + ring)
               DO ix = MAX(1, ix0 - ring), MIN(self % nCellsX, ix0 + ring)
                  IF ( MAX(ABS(ix - ix0), ABS(iy - iy0)) /= ring )     CYCLE
                  c = ix + (iy - 1)*self % nCellsX
                  DO i = self % cellStart(c), self % cellStart(c+1) - 1
                     j = self % cellSegments(i)
                     CALL distanceToSegment( self, p, j, t, dist )
                     IF ( dist < distance .OR. (dist <= distance .AND. j < jBest) )     CALL setNearest( j, t, dist )
                  END DO
               END DO
            END DO
         END DO

         CONTAINS

         SUBROUTINE setNearest( jNearest, tNearest, dNearest )
            INTEGER       :: jNearest
            REAL(KIND=RP) :: tNearest, dNearest
            distance = dNearest
            jBest    = jNearest
            s        = jNearest + tNearest
            pImage   = self % x(1:2,jNearest) + tNearest*(self % x(1:2,jNearest+1) - self % x(1:2,jNearest))
         END SUBROUTINE setNearest

      END SUBROUTINE nearestPoint
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE distanceToSegment( self, p, j, t, dist )
         IMPLICIT NONE
!
!        ---------------------------------------------------------
!        Distance from p to segment j and the fraction t along it
!        of the nearest point
!        ---------------------------------------------------------
!
         CLASS(SegmentedCurveArray) :: self
         REAL(KIND=RP)              :: p(3), t, dist
         INTEGER                    :: j
         REAL(KIND=RP)              :: a(2), d(2), lengthSquared

         a = self % x(1:2,j)
         d = self % x(1:2,j+1) - a
         lengthSquared = DOT_PRODUCT(d,d)
         t = 0.0_RP
         IF ( lengthSquared > 0.0_RP )     t = MAX(0.0_RP, MIN(1.0_RP, DOT_PRODUCT(p(1:2) - a, d)/lengthSquared))
         dist = NORM2(p(1:2) - a - t*d)

      END SUBROUTINE distanceToSegment
!
!////////////////////////////////////////////////////////////////////////
!
      END Module SegmentedCurveArrayClass

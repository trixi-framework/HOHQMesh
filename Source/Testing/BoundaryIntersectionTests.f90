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
!      BoundaryIntersectionTests.f90
!      Created: 2026-11-19
!      Unit tests for TwoCurvesAsPointsIntersect, the point-based check
!      that CheckForBoundaryIntersections uses when the bounding boxes
!      of two boundary curves overlap:
!        1. Curves that cross at a shared point are found
!        2. Parallel curves with overlapping boxes do not cross
!        3. A curve with no points in the overlap of the boxes does not
!           cross
!        4. Densely sampled curves that cross between their points are
!           found (two circles), and close ones that do not cross are not
!        5. The segment from the last point back to the first is tested
!        6. SegmentsIntersect: crossing, touching, collinear and
!           parallel segments
!      All coordinates are exact binary fractions so that the inclusive
!      box tests do not depend on round off.
!////////////////////////////////////////////////////////////////////////
      Module BoundaryIntersectionTestsModule
      USE SMConstants
      USE ProgramGlobals, ONLY : BBOX_TOP, BBOX_LEFT, BBOX_BOTTOM, BBOX_RIGHT
      USE Geometry      , ONLY : IntersectionOfBBoxes, SegmentsIntersect
      USE MeshBoundaryMethodsModule, ONLY : TwoCurvesAsPointsIntersect
      USE FTAssertions
      IMPLICIT NONE

      PRIVATE
      PUBLIC :: BoundaryIntersectionTests
!     ========
      CONTAINS
!     ========
!////////////////////////////////////////////////////////////////////////
      SUBROUTINE BoundaryIntersectionTests(optData)
         IMPLICIT NONE
!        Arguments
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused
!        Local variables
         REAL(KIND=RP) :: shallow(3,0:16), steep(3,0:8), shifted(3,0:16)
         REAL(KIND=RP) :: lShape(3,0:16), short(3,0:4)
         REAL(KIND=RP) :: circle(3,0:255), smallCircle(3,0:255), square(3,0:3), across(3,0:1)
         REAL(KIND=RP) :: t
         INTEGER       :: k
!
!        ---------------------------------------------------------------
!        A shallow line from (0,4.75) to (2,5.25) and a steep line from
!        (0.875,4) to (1.125,6). Both have a point at (1,5), where they
!        cross. The crossing must be found in a box whose height comes
!        from the y coordinates, not the x coordinates.
!        ---------------------------------------------------------------
!
         DO k = 0, 16
            shallow(:,k) = [0.125_RP*k, 4.75_RP + 0.03125_RP*k, 0.0_RP]
         END DO
         DO k = 0, 8
            steep(:,k)   = [0.875_RP + 0.03125_RP*k, 4.0_RP + 0.25_RP*k, 0.0_RP]
         END DO
         CALL FTAssert(test = curvesCross(shallow, steep), &
                       msg  = "Shallow and steep lines crossing at (1,5) should intersect")
         CALL FTAssert(test = curvesCross(steep, shallow), &
                       msg  = "Steep and shallow lines crossing at (1,5) should intersect (swapped order)")
!
!        ------------------------------------------------------------
!        The shallow line moved up by 1/4: parallel, the boxes overlap
!        and each curve has nine points in the overlap, but they do
!        not cross
!        ------------------------------------------------------------
!
         shifted        = shallow
         shifted(2,:)   = shifted(2,:) + 0.25_RP
         CALL FTAssert(test = .NOT.curvesCross(shallow, shifted), &
                       msg  = "Parallel lines with overlapping boxes should not intersect")
!
!        ----------------------------------------------------------------
!        An L along the bottom and right sides of [0,2]x[0,2] and a short
!        segment inside the square. The short segment's box lies inside
!        the L's box, but no point of the L is in it.
!        ----------------------------------------------------------------
!
         DO k = 0, 8
            lShape(:,k)   = [0.25_RP*k, 0.0_RP, 0.0_RP]
            lShape(:,k+8) = [2.0_RP, 0.25_RP*k, 0.0_RP]
         END DO
         DO k = 0, 4
            short(:,k) = [0.5_RP + 0.125_RP*k, 1.5_RP + 0.0625_RP*k, 0.0_RP]
         END DO
         CALL FTAssert(test = .NOT.curvesCross(lShape, short), &
                       msg  = "Segment inside an L-shaped curve's box should not intersect it")
!
!        -----------------------------------------------------------------
!        The unit circle and a circle of radius 1/2 centered at (c,c), 256
!        points each. For c = 0.9 they cross, but no point of one curve
!        is on the other. For c = 1.07 the gap between them is about 0.013.
!        -----------------------------------------------------------------
!
         DO k = 0, 255
            t = 2.0_RP*PI*k/256.0_RP
            circle(:,k)      = [COS(t), SIN(t), 0.0_RP]
            smallCircle(:,k) = [0.9_RP + 0.5_RP*COS(t), 0.9_RP + 0.5_RP*SIN(t), 0.0_RP]
         END DO
         CALL FTAssert(test = curvesCross(circle, smallCircle), &
                       msg  = "Circles that cross between their points should intersect")
         CALL FTAssert(test = curvesCross(smallCircle, circle), &
                       msg  = "Circles that cross between their points should intersect (swapped order)")
         smallCircle(1:2,:) = smallCircle(1:2,:) + 0.17_RP
         CALL FTAssert(test = .NOT.curvesCross(circle, smallCircle), &
                       msg  = "Circles 0.013 apart should not intersect")
!
!        ----------------------------------------------------------------
!        The unit square given by its corners only. The segment across
!        its left side crosses only the side from the last corner back to
!        the first.
!        ----------------------------------------------------------------
!
         square(:,0) = [0.0_RP, 0.0_RP, 0.0_RP]
         square(:,1) = [1.0_RP, 0.0_RP, 0.0_RP]
         square(:,2) = [1.0_RP, 1.0_RP, 0.0_RP]
         square(:,3) = [0.0_RP, 1.0_RP, 0.0_RP]
         across(:,0) = [-0.5_RP, 0.5_RP, 0.0_RP]
         across(:,1) = [ 0.5_RP, 0.5_RP, 0.0_RP]
         CALL FTAssert(test = curvesCross(square, across), &
                       msg  = "A crossing of the closing segment should be found")
!
!        -----------------
!        SegmentsIntersect
!        -----------------
!
         CALL FTAssert(test = SegmentsIntersect([0.0_RP,0.0_RP,0.0_RP], [1.0_RP,1.0_RP,0.0_RP], &
                                                [0.0_RP,1.0_RP,0.0_RP], [1.0_RP,0.0_RP,0.0_RP]), &
                       msg  = "Diagonals of a square intersect")
         CALL FTAssert(test = SegmentsIntersect([0.0_RP,0.0_RP,0.0_RP], [1.0_RP,0.0_RP,0.0_RP], &
                                                [0.5_RP,0.0_RP,0.0_RP], [0.5_RP,1.0_RP,0.0_RP]), &
                       msg  = "A segment that ends on another intersects it")
         CALL FTAssert(test = SegmentsIntersect([0.0_RP,0.0_RP,0.0_RP], [1.0_RP,0.0_RP,0.0_RP], &
                                                [0.5_RP,0.0_RP,0.0_RP], [2.0_RP,0.0_RP,0.0_RP]), &
                       msg  = "Overlapping collinear segments intersect")
         CALL FTAssert(test = .NOT.SegmentsIntersect([0.0_RP,0.0_RP,0.0_RP], [1.0_RP,0.0_RP,0.0_RP], &
                                                     [1.5_RP,0.0_RP,0.0_RP], [2.0_RP,0.0_RP,0.0_RP]), &
                       msg  = "Separate collinear segments do not intersect")
         CALL FTAssert(test = .NOT.SegmentsIntersect([0.0_RP,0.0_RP,0.0_RP], [1.0_RP,0.0_RP,0.0_RP], &
                                                     [0.0_RP,0.5_RP,0.0_RP], [1.0_RP,0.5_RP,0.0_RP]), &
                       msg  = "Parallel segments do not intersect")
         CALL FTAssert(test = .NOT.SegmentsIntersect([0.0_RP,0.0_RP,0.0_RP], [1.0_RP,0.0_RP,0.0_RP], &
                                                     [0.5_RP,0.25_RP,0.0_RP], [0.5_RP,1.0_RP,0.0_RP]), &
                       msg  = "A segment that stops short of another does not intersect it")

      END SUBROUTINE BoundaryIntersectionTests
!////////////////////////////////////////////////////////////////////////
      LOGICAL FUNCTION curvesCross(c1, c2)
!        Call TwoCurvesAsPointsIntersect the way TwoCurvesIntersect does:
!        with the intersection of the two curves' bounding boxes
         IMPLICIT NONE
!        Arguments
         REAL(KIND=RP) :: c1(:,0:), c2(:,0:)
!        Local variables
         REAL(KIND=RP), ALLOCATABLE :: crv1(:,:), crv2(:,:)
         REAL(KIND=RP)              :: intersection(6)
         INTEGER                    :: n1, n2

         n1 = SIZE(c1,2) - 1
         n2 = SIZE(c2,2) - 1
         ALLOCATE(crv1(3,0:n1), crv2(3,0:n2))
         crv1 = c1
         crv2 = c2

         intersection = IntersectionOfBBoxes(boxA = pointsBox(c1), boxB = pointsBox(c2))
         curvesCross  = TwoCurvesAsPointsIntersect(testCurve1   = crv1, nPts1 = n1, &
                                                   testCurve2   = crv2, nPts2 = n2, &
                                                   intersection = intersection)

      END FUNCTION curvesCross
!////////////////////////////////////////////////////////////////////////
      FUNCTION pointsBox(c) RESULT(bBox)
         IMPLICIT NONE
!        Arguments
         REAL(KIND=RP) :: c(:,0:)
         REAL(KIND=RP) :: bBox(6)

         bBox              = 0.0_RP
         bBox(BBOX_LEFT)   = MINVAL(c(1,:))
         bBox(BBOX_RIGHT)  = MAXVAL(c(1,:))
         bBox(BBOX_BOTTOM) = MINVAL(c(2,:))
         bBox(BBOX_TOP)    = MAXVAL(c(2,:))

      END FUNCTION pointsBox
!////////////////////////////////////////////////////////////////////////
      END Module BoundaryIntersectionTestsModule

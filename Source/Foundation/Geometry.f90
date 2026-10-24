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
!      Geometry.F95
!      Created: 2010-08-17 14:39:19 -0400
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
      Module Geometry
      USE SMConstants
      USE ProgramGlobals
      IMPLICIT NONE
!
!-------------------------------------------------------------------
!> Define generic geometry operations and constants
!-------------------------------------------------------------------
!
      INTEGER, PARAMETER :: UP = 1, DOWN = -1, CO_LINEAR = 0
      INTEGER, PARAMETER :: CLOCKWISE = 1, COUNTERCLOCKWISE = -1, NO_DIRECTION  = 0
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      PURE FUNCTION CrossProductDirection(u,v) RESULT(d)
         IMPLICIT NONE
         REAL(KIND=RP), DIMENSION(3), INTENT(IN) :: u, v
         INTEGER                                 :: d
         REAL(KIND=RP)                           :: c
         c = u(1)*v(2) - v(1)*u(2)
         IF( ABS(c) < EPSILON(c)     )   THEN
            d = CO_LINEAR
         ELSE
            d = INT(SIGN( 1.0_RP, c ))
         END IF
      END FUNCTION CrossProductDirection
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION Curvature( xPrime, xDoublePrime ) RESULT(c)
         IMPLICIT NONE
         REAL(KIND=RP) :: xPrime(3), xDoublePrime(3), c

         c = ABS( xPrime(1)*xDoublePrime(2) - xPrime(2)*xDoublePrime(1) ) &
                          /(xPrime(1)**2 + xPrime(2)**2)**1.5_RP

      END FUNCTION Curvature
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION Circulation(x)
         IMPLICIT NONE
         REAL(KIND=RP) :: x(:,:)
         INTEGER       :: N, j
         REAL(KIND=RP) :: s

         N = SIZE(x,2)
         s = 0.0_RP
         DO j = 1,N-1
            s = s + x(1,j)*x(2,j+1) - x(1,j+1)*x(2,j)
         END DO
         s = s + x(1,N)*x(2,1) - x(1,1)*x(2,N)
         IF(s > 0.0_RP) THEN
            Circulation = COUNTERCLOCKWISE
         ELSE IF (s < 0.0_RP)     THEN
            Circulation = CLOCKWISE
         ELSE
            CIRCULATION = NO_DIRECTION
         END IF

      END FUNCTION Circulation
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION PointInQuad( nodes, x ) RESULT(r)
         IMPLICIT NONE
         REAL(KIND=RP), DIMENSION(3,4) :: nodes
         REAL(KIND=RP), DIMENSION(3)   :: x
         LOGICAL                       :: r

         REAL(KIND=RP), DIMENSION(3,5) :: cNodes
         INTEGER                       :: k
         REAL(KIND=RP), DIMENSION(3)   :: u, v

         cNodes(:,1:4) = nodes
         cNodes(:,5)   = nodes(:,1)
         r = .true.

         DO k = 1,4
            u = cNodes(:,k+1) - cNodes(:,k)
            v = x - cNodes(:,k)
            IF( CrossProductDirection(u,v) == DOWN )     THEN
               r = .false.
               EXIT
            END IF
         END DO

      END FUNCTION PointInQuad
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION Box_IsInsideBox(testBox,box)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP) :: box(6), testBox(6)
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP) :: p(3)

         Box_IsInsideBox = .true.
!
!        -----------------------------------
!        Check bottom left corner of testBox
!        -----------------------------------
!
         p = [testBox(BBOX_LEFT), testBox(BBOX_BOTTOM), 0.0_RP]
         IF ( .NOT.Point_IsInsideBox(p,box) )     THEN
            Box_IsInsideBox = .false.
            RETURN
         END IF
!
!        -----------------------------------
!        Check upper right corner of testBox
!        -----------------------------------
!
         p = [testBox(BBOX_RIGHT), testBox(BBOX_TOP), 0.0_RP]
         IF ( .NOT.Point_IsInsideBox(p,box) )     THEN
            Box_IsInsideBox = .false.
         END IF

      END FUNCTION Box_IsInsideBox
!
!////////////////////////////////////////////////////////////////////////
!
      PURE LOGICAL FUNCTION Point_IsInsideBox(p,bBox)
         IMPLICIT NONE
         REAL(KIND=RP), INTENT(IN)  :: p(3)
         REAL(KIND=RP), INTENT(IN)  :: bBox(6)

         Point_IsInsideBox = .true.

         IF ( p(1) < bBox(BBOX_LEFT) )     THEN
            Point_IsInsideBox = .false.
            RETURN
         END IF

         IF ( p(1) > bBox(BBOX_RIGHT) )     THEN
            Point_IsInsideBox = .false.
            RETURN
         END IF

         IF ( p(2) > bBox(BBOX_TOP) )     THEN
            Point_IsInsideBox = .false.
            RETURN
         END IF

         IF ( p(2) < bBox(BBOX_BOTTOM) )     THEN
            Point_IsInsideBox = .false.
            RETURN
         END IF

      END FUNCTION Point_IsInsideBox
!
!////////////////////////////////////////////////////////////////////////
!
      PURE REAL(KIND=RP) FUNCTION BoundingBoxArea(bBox)
         IMPLICIT NONE
         REAL(KIND=RP), INTENT(IN) :: bBox(6)
         BoundingBoxArea = (bBox(BBOX_RIGHT) - bBox(BBOX_LEFT))*   &
                           (bBox(BBOX_TOP)   - bBox(BBOX_BOTTOM))
      END FUNCTION BoundingBoxArea
!
!////////////////////////////////////////////////////////////////////////
!
      PURE LOGICAL FUNCTION BBoxIntersects(boxA, boxB)
         IMPLICIT NONE
         REAL(KIND=RP), INTENT(IN) :: boxA(6), boxB(6)

         BBoxIntersects = .TRUE.

         IF( boxB(BBOX_LEFT)   > boxA(BBOX_RIGHT)   .OR. &
             boxB(BBOX_RIGHT)  < boxA(BBOX_LEFT)    .OR. &
             boxB(BBOX_TOP)    < boxA(BBOX_BOTTOM)  .OR. &
             boxB(BBOX_BOTTOM) > boxA(BBOX_TOP)          &
           ) BBoxIntersects = .FALSE.

      END FUNCTION BBoxIntersects
!
!////////////////////////////////////////////////////////////////////////
!
      PURE LOGICAL FUNCTION BBoxIsNested(boxA, boxB)
!
!     ------------------------------
!     Test if boxB is nested in boxA
!     ------------------------------
!
         IMPLICIT NONE
         REAL(KIND=RP), INTENT(IN) :: boxA(6), boxB(6)

         BBoxIsNested = .FALSE.

         IF( boxB(BBOX_LEFT)   > boxA(BBOX_LEFT)     .AND. &
             boxB(BBOX_RIGHT)  < boxA(BBOX_RIGHT)    .AND. &
             boxB(BBOX_TOP)    < boxA(BBOX_TOP)      .AND. &
             boxB(BBOX_BOTTOM) > boxA(BBOX_BOTTOM)         &
           ) BBoxIsNested = .TRUE.

      END FUNCTION BBoxIsNested
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION IntersectionOfBBoxes(boxA, boxB) RESULT(newBox)
      IMPLICIT NONE
          REAL(KIND=RP) :: boxA(6), boxB(6), newBox(6)
          newBox = 0.0_RP

          newBox(BBOX_LEFT)   = MAX(boxA(BBOX_LEFT)  , boxB(BBOX_LEFT))
          newBox(BBOX_BOTTOM) = MAX(boxA(BBOX_BOTTOM), boxB(BBOX_BOTTOM))
          newBox(BBOX_TOP)    = MIN(boxA(BBOX_TOP)   , boxB(BBOX_TOP))
          newBox(BBOX_RIGHT)  = MIN(boxA(BBOX_RIGHT) , boxB(BBOX_RIGHT))

      END FUNCTION IntersectionOfBBoxes
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION ACWindingFunction( p, curvePoints, N ) RESULT(w)
!
!     ---------------------------------------------------------------------
!>     Find if the the point p(3) is inside of a closed curve discretized by
!!     curvePoints. Closed means that curvePoints(:,N+1) = curvePoints(:,0).
!!     Computes a constant factor (2\Pi) times the winding function using an
!!     axis crossing method. This version sets winding function = 0 if the
!!     point is on the curve. w = 0 => outside. This version uses integer
!!     arithmetic to compute w.
!     ---------------------------------------------------------------------
!
      USE SMConstants
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER       :: N
      REAL(KIND=RP) :: p(3)               ! = (x,y,z)
      REAL(KIND=RP) :: curvePoints(3,0:N) ! = (x_i,y_j,z_j) i = 0, 1, \ldots , N
      INTEGER       :: w
!
!     ---------------
!     local variables
!     ---------------
!
      INTEGER       :: i, ip
      REAL(KIND=RP) :: r, x, y, xp, yp
!
!     ------------------------------------------------------------
!     The points relative to p are computed on the fly rather than
!     stored, since this is called very often with long curves
!     ------------------------------------------------------------
!
      w = 0
      DO i = 0, N
         ip = i + 1
         IF ( ip > N )     ip = 0
         x  = curvePoints(1, i)  - p(1); xp = curvePoints(1, ip) - p(1)
         y  = curvePoints(2, i)  - p(2); yp = curvePoints(2, ip) - p(2)

         IF( AlmostEqual(y,0.0_RP) .AND. AlmostEqual(yp,0.0_RP) )     THEN
            IF( x <= 0.0_RP .AND. xp >= 0.0_RP )     THEN ! point is on this segment
               w = 0
               RETURN
            ELSE
               CYCLE
            END IF
         END IF

         IF ( y*yp < 0.0_RP )     THEN
            r = x + y*(xp-x)/(y-yp)
            IF ( r > 0 )     THEN
               IF ( y < 0.0_RP )     THEN
                  w = w + 2
               ELSE
                  w = w - 2
               END IF
            ELSE IF ( AlmostEqual(r,0.0_RP) )     THEN
               w = 0
               RETURN
            END IF
         ELSE IF ( (y == 0.0_RP) .AND. (x > 0.0_RP) )     THEN
            IF ( yp > 0 )     THEN
               w = w + 1
            ELSE
               w = w - 1
            END IF
         ELSE IF ( (yp == 0.0_RP) .AND. (xp > 0.0_RP) )     THEN
            IF ( y < 0 )     THEN
               w = w + 1
            ELSE
               w = w - 1
            END IF
         END IF
      END DO

      END FUNCTION ACWindingFunction
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION bBoxWithMargin( bBox ) RESULT(box)
!
!     ---------------------------------------------------------------
!     The bounding box of a curve enlarged by a margin well above the
!     tolerances and the rounding errors of ACWindingFunction and
!     PointInQuad, for acWindingFunctionInBox
!     ---------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=RP) :: bBox(6)
      REAL(KIND=RP) :: box(6)
      REAL(KIND=RP) :: margin

      margin = 1.0E-8_RP*( MAXVAL(ABS(bBox)) + MAX(bBox(BBOX_RIGHT) - bBox(BBOX_LEFT), &
                                                   bBox(BBOX_TOP)   - bBox(BBOX_BOTTOM)) ) + 1.0E-12_RP
      box              = bBox
      box(BBOX_LEFT)   = bBox(BBOX_LEFT)   - margin
      box(BBOX_RIGHT)  = bBox(BBOX_RIGHT)  + margin
      box(BBOX_BOTTOM) = bBox(BBOX_BOTTOM) - margin
      box(BBOX_TOP)    = bBox(BBOX_TOP)    + margin

      END FUNCTION bBoxWithMargin
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION acWindingFunctionInBox( p, curvePoints, n, box )
!
!     ------------------------------------------------------------------
!     ACWindingFunction, skipped (w = 0) when p is outside box, the
!     bounding box of the curve from bBoxWithMargin
!     ------------------------------------------------------------------
!
      IMPLICIT NONE
      INTEGER       :: n
      REAL(KIND=RP) :: p(3), curvePoints(3,0:n), box(6)

      IF ( .NOT.Point_IsInsideBox(p, box) )     THEN
         acWindingFunctionInBox = 0
      ELSE
         acWindingFunctionInBox = ACWindingFunction( p, curvePoints, n )
      END IF

      END FUNCTION acWindingFunctionInBox
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION angleBetween( v1, v2 )
         IMPLICIT NONE
!
!        ------------------------------------
!        Angle between two vectors in degrees
!        ------------------------------------
!
         REAL(KIND=RP) :: v1(2), v2(2)
         angleBetween = ACOS( MAX(-1.0_RP, MIN(1.0_RP, DOT_PRODUCT(v1,v2)/(NORM2(v1)*NORM2(v2)))) )*180.0_RP/PI
      END FUNCTION angleBetween
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION cornerJacobian( xPrev, x, xNext )
         IMPLICIT NONE
!
!        --------------------------------------------------------------
!        Cross product of the two edges that meet at the corner x of a
!        quadrilateral. Positive at a convex corner of a counterclockwise
!        element, zero where the corner is flat, negative if folded.
!        --------------------------------------------------------------
!
         REAL(KIND=RP) :: xPrev(3), x(3), xNext(3)
         cornerJacobian = (xNext(1) - x(1))*(xPrev(2) - x(2)) - (xNext(2) - x(2))*(xPrev(1) - x(1))
      END FUNCTION cornerJacobian
!
!////////////////////////////////////////////////////////////////////////
!
      PURE LOGICAL FUNCTION AlmostEqual( x, y )
      USE SMConstants
      IMPLICIT NONE
      REAL(KIND=RP), INTENT(IN) :: x, y

      AlmostEqual = ABS(x - y) < 100*EPSILON(1.0_RP)

      END FUNCTION AlmostEqual
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeCentroid(x,centroid)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP) :: x(3,4)
         REAL(KIND=RP) :: centroid(3)
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER       :: i, k
         REAL(KIND=RP) :: area, tmp

         INTEGER, EXTERNAL :: Loop

         centroid = 0.0_RP
         area     = 0.0_RP
         DO i = 1, 4
            k    = Loop(i+1,4)
            tmp  = x(1,i)*x(2,k) - x(1,k)*x(2,i)
            area = area + tmp
            centroid = centroid + (x(:,i) + x(:,k))*tmp
         END DO
         area     = 0.5_RP*area
         centroid = centroid/(6*area)

      END SUBROUTINE ComputeCentroid

      END Module Geometry

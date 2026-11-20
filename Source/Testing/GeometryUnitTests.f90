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
!      GeometryUnitTests.f90
!      Created: 2026-11-19
!      Small unit tests of routines that the meshing tests do not reach:
!        1. The unit tangent of an elliptic arc, against a centered
!           difference of its position
!        2. The axis crossing winding function for points inside,
!           outside and on each side of a square
!////////////////////////////////////////////////////////////////////////
      Module GeometryUnitTestsModule
      USE SMConstants
      USE FTAssertions
      USE SMEllipticArcClass
      USE Geometry          , ONLY : ACWindingFunction
      IMPLICIT NONE
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE EllipticArcTangentTest(optData)
!
!     --------------------------------------------------------------
!     A rotated ellipse traversed counterclockwise and a rotated arc
!     traversed clockwise (endAngle < startAngle). The tangent must
!     be the unit vector along d(position)/dt.
!     --------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused

         TYPE(SMEllipticArc) :: arc

         CALL arc % initWithParametersNameAndID(center = [1.0_RP, -0.5_RP, 0.0_RP], xRadius = 2.0_RP, &
                                                yRadius = 0.75_RP, startAngle = 0.0_RP,               &
                                                endAngle = 2.0_RP*PI, rotation = 0.3_RP,              &
                                                cName = "ellipse", id = 1)
         CALL CheckTangent(arc, "Counterclockwise ellipse")
         CALL destructEllipticArc(self = arc)

         CALL arc % initWithParametersNameAndID(center = [0.0_RP, 0.0_RP, 0.0_RP], xRadius = 0.5_RP, &
                                                yRadius = 1.5_RP, startAngle = 0.75_RP*PI,           &
                                                endAngle = -0.25_RP*PI, rotation = -1.1_RP,          &
                                                cName = "arc", id = 2)
         CALL CheckTangent(arc, "Clockwise arc")
         CALL destructEllipticArc(self = arc)

      END SUBROUTINE EllipticArcTangentTest
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckTangent( arc, label )
         IMPLICIT NONE
         TYPE(SMEllipticArc) :: arc
         CHARACTER(LEN=*)    :: label

         REAL(KIND=RP), PARAMETER :: h = 1.0E-5_RP
         REAL(KIND=RP)            :: t, tangent(3), difference(3), maxError
         INTEGER                  :: j

         maxError = 0.0_RP
         DO j = 0, 8
            t          = REAL(j,RP)/8.0_RP
            tangent    = arc % tangentAt(t)
            difference = arc % positionAt(t + h) - arc % positionAt(t - h)
            difference = difference/NORM2(difference)
            maxError   = MAX(maxError, MAXVAL(ABS(tangent - difference)))
         END DO
         CALL FTAssert(test = maxError < 1.0E-8_RP, msg = label // ": tangent along the arc")

      END SUBROUTINE CheckTangent
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE WindingFunctionTest(optData)
!
!     ------------------------------------------------------------------
!     The counterclockwise unit square. The winding function is nonzero
!     inside, and zero outside and on the curve. The top side is
!     traversed from right to left, so the point on it checks that the
!     on curve test does not depend on the direction of the segment.
!     ------------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused

         REAL(KIND=RP) :: square(3,0:3)

         square(:,0) = [0.0_RP, 0.0_RP, 0.0_RP]
         square(:,1) = [1.0_RP, 0.0_RP, 0.0_RP]
         square(:,2) = [1.0_RP, 1.0_RP, 0.0_RP]
         square(:,3) = [0.0_RP, 1.0_RP, 0.0_RP]

         CALL FTAssert(test = ACWindingFunction([0.5_RP , 0.5_RP, 0.0_RP], square, 3) /= 0, msg = "Inside")
         CALL FTAssert(test = ACWindingFunction([0.25_RP, 0.75_RP, 0.0_RP], square, 3) /= 0, &
                       msg  = "Inside, level with no vertex")
         CALL FTAssertEqual(expectedValue = 0, actualValue = ACWindingFunction([2.0_RP , 0.5_RP, 0.0_RP], square, 3), &
                            msg = "Outside, right")
         CALL FTAssertEqual(expectedValue = 0, actualValue = ACWindingFunction([-1.0_RP, 1.0_RP, 0.0_RP], square, 3), &
                            msg = "Outside, level with the top")
         CALL FTAssertEqual(expectedValue = 0, actualValue = ACWindingFunction([-1.0_RP, 0.0_RP, 0.0_RP], square, 3), &
                            msg = "Outside, level with the bottom")
         CALL FTAssertEqual(expectedValue = 0, actualValue = ACWindingFunction([0.5_RP , 0.0_RP, 0.0_RP], square, 3), &
                            msg = "On the bottom")
         CALL FTAssertEqual(expectedValue = 0, actualValue = ACWindingFunction([1.0_RP , 0.5_RP, 0.0_RP], square, 3), &
                            msg = "On the right side")
         CALL FTAssertEqual(expectedValue = 0, actualValue = ACWindingFunction([0.5_RP , 1.0_RP, 0.0_RP], square, 3), &
                            msg = "On the top")
         CALL FTAssertEqual(expectedValue = 0, actualValue = ACWindingFunction([0.0_RP , 0.5_RP, 0.0_RP], square, 3), &
                            msg = "On the left side")

      END SUBROUTINE WindingFunctionTest

      END Module GeometryUnitTestsModule

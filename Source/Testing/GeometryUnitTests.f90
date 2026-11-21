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
!        3. The rotation transform for a direction that is opposite
!           to, or a multiple of, the start direction
!        4. The sharp refinement center and line controls
!        5. The test for a nested bounding box, and the round trip of
!           an integer array through the encoder
!////////////////////////////////////////////////////////////////////////
      Module GeometryUnitTestsModule
      USE SMConstants
      USE FTAssertions
      USE SMEllipticArcClass
      USE Geometry          , ONLY : ACWindingFunction, BBoxIsNested
      USE ProgramGlobals    , ONLY : BBOX_TOP, BBOX_LEFT, BBOX_BOTTOM, BBOX_RIGHT
      USE EncoderModule     , ONLY : encodeIntArray, decodeIntArray
      USE Geometry3DModule  , ONLY : RotationTransform, ConstructRotationTransform, &
                                     PerformRotationTransform, isIdentityRotation
      USE SizerControls     , ONLY : SizerCenterControl, SizerLineControl, CENTER_SHARP, &
                                     hInvForCenter, hInvForLineControl
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
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RotationTransformTest(optData)
!
!     ------------------------------------------------------------------
!     Turning the z axis onto -z has no unique rotation axis (the cross
!     product vanishes), so it is a half turn about the y axis. A
!     direction that is a multiple of z is no rotation at all.
!     ------------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused

         TYPE(RotationTransform) :: rotation
         REAL(KIND=RP)           :: y(3)

         CALL ConstructRotationTransform(self           = rotation,                     &
                                         rotationPoint  = [1.0_RP, 0.0_RP, 0.0_RP],     &
                                         startDirection = [0.0_RP, 0.0_RP, 1.0_RP],     &
                                         newDirection   = [0.0_RP, 0.0_RP, -1.0_RP])
         CALL FTAssert(test = .NOT.isIdentityRotation(rotation), msg = "Opposite direction is a rotation")
         y = PerformRotationTransform(x = [2.0_RP, 3.0_RP, 4.0_RP], transformation = rotation)
         CALL FTAssert(test = MAXVAL(ABS(y - [0.0_RP, 3.0_RP, -4.0_RP])) <= 1.0E-14_RP, &
                       msg  = "Opposite direction: half turn about the y axis through the rotation point")

         CALL ConstructRotationTransform(self           = rotation,                     &
                                         rotationPoint  = [0.0_RP, 0.0_RP, 0.0_RP],     &
                                         startDirection = [0.0_RP, 0.0_RP, 1.0_RP],     &
                                         newDirection   = [0.0_RP, 0.0_RP, 2.0_RP])
         CALL FTAssert(test = isIdentityRotation(rotation), msg = "Only the direction of the vector matters")

      END SUBROUTINE RotationTransformTest
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SharpSizerControlTest(optData)
!
!     ---------------------------------------------------------------
!     A sharp refinement center or line asks for its mesh size within
!     its width and nothing outside of it
!     ---------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused

         TYPE(SizerCenterControl) :: center
         TYPE(SizerLineControl)   :: line

         CALL center % initWithProperties(c = [1.0_RP, 1.0_RP, 0.0_RP], width = 0.5_RP, meshSize = 0.2_RP, &
                                          centerType = CENTER_SHARP)
         CALL FTAssert(test = ABS(hInvForCenter(center, [1.3_RP, 1.0_RP, 0.0_RP]) - 5.0_RP) <= 1.0E-12_RP, &
                       msg  = "Sharp center: 1/h inside its width")
         CALL FTAssert(test = hInvForCenter(center, [2.0_RP, 1.0_RP, 0.0_RP]) == 0.0_RP, &
                       msg  = "Sharp center: no refinement outside its width")

         CALL line % initWithProperties(lStart = [0.0_RP, 0.0_RP, 0.0_RP], lEnd = [2.0_RP, 0.0_RP, 0.0_RP], &
                                        width = 0.3_RP, meshSize = 0.25_RP, lineControlType = CENTER_SHARP)
         CALL FTAssert(test = ABS(hInvForLineControl(line, [1.0_RP, 0.2_RP, 0.0_RP]) - 4.0_RP) <= 1.0E-12_RP, &
                       msg  = "Sharp line: 1/h inside its width")
         CALL FTAssert(test = hInvForLineControl(line, [1.0_RP, 0.5_RP, 0.0_RP]) == 0.0_RP, &
                       msg  = "Sharp line: no refinement outside its width")

      END SUBROUTINE SharpSizerControlTest
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE BoxAndEncoderTest(optData)
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused

         REAL(KIND=RP)                 :: outer(6), inner(6), shifted(6)
         CHARACTER(LEN=1), ALLOCATABLE :: enc(:)
         INTEGER         , ALLOCATABLE :: decoded(:)
         INTEGER                       :: values(4) = [3, -7, 0, 123456]

         outer = 0.0_RP; inner = 0.0_RP
         outer(BBOX_LEFT) = 0.0_RP; outer(BBOX_RIGHT) = 4.0_RP; outer(BBOX_BOTTOM) = 0.0_RP; outer(BBOX_TOP) = 2.0_RP
         inner(BBOX_LEFT) = 1.0_RP; inner(BBOX_RIGHT) = 3.0_RP; inner(BBOX_BOTTOM) = 0.5_RP; inner(BBOX_TOP) = 1.5_RP
         shifted = inner
         shifted(BBOX_RIGHT) = 5.0_RP
         CALL FTAssert(test = BBoxIsNested(outer, inner)      , msg = "Box inside another box is nested")
         CALL FTAssert(test = .NOT.BBoxIsNested(inner, outer) , msg = "The larger box is not nested in the smaller")
         CALL FTAssert(test = .NOT.BBoxIsNested(outer, shifted), msg = "A box that sticks out is not nested")

         CALL encodeIntArray(values, enc)
         CALL decodeIntArray(enc, SIZE(values), decoded)
         CALL FTAssert(test = ALL(decoded == values), msg = "Integer array survives encoding and decoding")

      END SUBROUTINE BoxAndEncoderTest

      END Module GeometryUnitTestsModule

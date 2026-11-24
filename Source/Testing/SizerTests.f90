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
!      SizerTests.f90
!      Created: 2026-11-23
!      Tests of how the sizer tells a sharp corner of a curve from a
!      narrow gap across it. Two points of the same curve on the two
!      sides of a sharp corner face each other, but their distance is
!      set by how finely the curve is discretized, so it must not set
!      a mesh size. The corner rule (isAcrossCorner) is checked on
!      small closed curves, and a sharp-tipped interface is checked to
!      pass the size check with the curve resizer on.
!////////////////////////////////////////////////////////////////////////
      Module SizerTestsModule
      USE SMConstants
      USE FTAssertions
      USE FTValueDictionaryClass
      USE SharedExceptionManagerModule
      USE ErrorTypesModule
      USE MeshSizerClass         , ONLY : ChainPoints, SetChainCorners, isAcrossCorner
      USE MeshProjectClass
      USE ControlInputTestsModule, ONLY : ClearExceptions
      USE ProjectInputTestsModule, ONLY : BuildProjectFromText, controlInputText, runParametersText, &
                                          backgroundGridText, chainText, modelText
      IMPLICIT NONE

      INTEGER, PARAMETER, PRIVATE :: LL         = 80   ! Length of a control file line in these tests
      INTEGER, PARAMETER, PRIVATE :: MAX_POINTS = 1000 ! Of a test curve
!
!     ------------------------------------------------------------------
!     A closed test curve, built counterclockwise from straight sides and
!     circular arcs. Like the segments of a chain, each piece has its own
!     first and last point, so the points where two pieces meet are
!     doubled, each copy with the normal of its own piece. The normals
!     are to the right of the direction of travel.
!     ------------------------------------------------------------------
!
      TYPE TestCurve
         INTEGER       :: count = 0
         REAL(KIND=RP) :: x(2,MAX_POINTS), nHat(2,MAX_POINTS)
      END TYPE TestCurve
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE AddLine( curve, xStart, xEnd, nIntervals )
         IMPLICIT NONE
         TYPE(TestCurve) :: curve
         REAL(KIND=RP)   :: xStart(2), xEnd(2)
         INTEGER         :: nIntervals

         REAL(KIND=RP) :: d(2)
         INTEGER       :: k

         d = (xEnd - xStart)/NORM2(xEnd - xStart)
         DO k = 0, nIntervals
            curve % count = curve % count + 1
            curve % x(:,curve % count)    = xStart + (xEnd - xStart)*REAL(k,RP)/REAL(nIntervals,RP)
            curve % nHat(:,curve % count) = [d(2), -d(1)]
         END DO
      END SUBROUTINE AddLine
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE AddArc( curve, center, radius, startDegrees, endDegrees, nIntervals )
!
!        -----------------------------------------------------------
!        Counterclockwise around center, so the normal points away
!        from the center
!        -----------------------------------------------------------
!
         IMPLICIT NONE
         TYPE(TestCurve) :: curve
         REAL(KIND=RP)   :: center(2), radius, startDegrees, endDegrees
         INTEGER         :: nIntervals

         REAL(KIND=RP) :: theta
         INTEGER       :: k

         DO k = 0, nIntervals
            theta = (startDegrees + (endDegrees - startDegrees)*REAL(k,RP)/REAL(nIntervals,RP))*PI/180.0_RP
            curve % count = curve % count + 1
            curve % x(:,curve % count)    = center + radius*[COS(theta), SIN(theta)]
            curve % nHat(:,curve % count) = [COS(theta), SIN(theta)]
         END DO
      END SUBROUTINE AddArc
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MakeChainPoints( curve, points )
         IMPLICIT NONE
         TYPE(TestCurve)   :: curve
         TYPE(ChainPoints) :: points

         IF ( ALLOCATED(points % x) )    DEALLOCATE(points % x)
         IF ( ALLOCATED(points % nHat) ) DEALLOCATE(points % nHat)
         points % count = curve % count
         ALLOCATE( points % x(3,curve % count), points % nHat(3,curve % count) )
         points % x(1:2,:)    = curve % x(:,1:curve % count)
         points % x(3,:)      = 0.0_RP
         points % nHat(1:2,:) = curve % nHat(:,1:curve % count)
         points % nHat(3,:)   = 0.0_RP
         CALL SetChainCorners(points)
      END SUBROUTINE MakeChainPoints
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION nearestCurvePoint( points, x )
         IMPLICIT NONE
         TYPE(ChainPoints) :: points
         REAL(KIND=RP)     :: x(2)

         REAL(KIND=RP) :: d(points % count)
         INTEGER       :: k

         DO k = 1, points % count
            d(k) = NORM2(points % x(1:2,k) - x)
         END DO
         nearestCurvePoint = MINLOC(d, DIM = 1)
      END FUNCTION nearestCurvePoint
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION acrossCornerAt( points, xA, xB )
!
!        ---------------------------------------------------
!        isAcrossCorner for the points nearest to xA and xB
!        ---------------------------------------------------
!
         IMPLICIT NONE
         TYPE(ChainPoints) :: points
         REAL(KIND=RP)     :: xA(2), xB(2)

         acrossCornerAt = isAcrossCorner(points, nearestCurvePoint(points, xA), nearestCurvePoint(points, xB))
      END FUNCTION acrossCornerAt
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SharpCornerRuleTests(optData)
!
!     ---------------------------------------------------------------
!     isAcrossCorner on curves where the answer is known: a 30 degree
!     wedge (a corner at any resolution), a rounded channel and a
!     slot with square corners (gaps: no kink), and a lens with
!     curved sides meeting at sharp tips (a corner next to the tip,
!     a gap across the middle, where the sides have turned away)
!     ---------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused

         TYPE(TestCurve)   :: curve
         TYPE(ChainPoints) :: points
         REAL(KIND=RP)     :: c, s, L, d, radius, beta, s1, s2
         INTEGER           :: m, k
         INTEGER, PARAMETER :: wedgeIntervals(2) = [4, 64]
         CHARACTER(LEN=64) :: label
!
!        ---------------------------------------------------------
!        A wedge of 30 degrees: from the apex at the origin, sides
!        of length L at -15 and +15 degrees, closed by a base.
!        Points at the same distance from the apex on the two
!        sides are across the corner, however fine the sides are
!        ---------------------------------------------------------
!
         L = 2.0_RP
         c = COS(15.0_RP*PI/180.0_RP)
         s = SIN(15.0_RP*PI/180.0_RP)
         DO k = 1, SIZE(wedgeIntervals)
            m = wedgeIntervals(k)
            curve % count = 0
            CALL AddLine(curve, [0.0_RP, 0.0_RP], [L*c, -L*s], m)
            CALL AddLine(curve, [L*c, -L*s]     , [L*c,  L*s], 2)
            CALL AddLine(curve, [L*c,  L*s]     , [0.0_RP, 0.0_RP], m)
            CALL MakeChainPoints(curve, points)
            WRITE(label,'(A,I0,A)') "Wedge with ", m, " intervals per side"

            CALL FTAssertEqual(expectedValue = 1, actualValue = SIZE(points % kinkArcLength), &
                               msg = TRIM(label) // ": a kink at the apex only (the base corners turn 105 degrees)")
            s1 = L/REAL(m,RP)
            CALL FTAssert(test = acrossCornerAt(points, s1*[c, -s], s1*[c, s]), &
                          msg = TRIM(label) // ": the points next to the apex are across a corner")
            s2 = 0.5_RP*L
            CALL FTAssert(test = acrossCornerAt(points, s2*[c, -s], s2*[c, s]), &
                          msg = TRIM(label) // ": points halfway along the sides are across a corner")
         END DO
!
!        -----------------------------------------------------------
!        A channel of width 1 and length 10 with rounded ends. Near
!        an end the way around is short, but nothing on it is sharp
!        -----------------------------------------------------------
!
         curve % count = 0
         CALL AddLine(curve, [0.0_RP, -0.5_RP], [10.0_RP, -0.5_RP], 20)
         CALL AddArc (curve, [10.0_RP, 0.0_RP], 0.5_RP, -90.0_RP, 90.0_RP, 8)
         CALL AddLine(curve, [10.0_RP, 0.5_RP], [0.0_RP, 0.5_RP], 20)
         CALL AddArc (curve, [0.0_RP, 0.0_RP], 0.5_RP, 90.0_RP, 270.0_RP, 8)
         CALL MakeChainPoints(curve, points)
         CALL FTAssertEqual(expectedValue = 0, actualValue = SIZE(points % kinkArcLength), &
                            msg = "Rounded channel: no kinks")
         CALL FTAssert(test = .NOT.acrossCornerAt(points, [9.5_RP, -0.5_RP], [9.5_RP, 0.5_RP]), &
                       msg = "Rounded channel: the sides near an end are across a gap")
!
!        ---------------------------------------------------------------
!        A slot of width 1 and length 10 with square corners, with no
!        point between the two corners of each end. Each corner turns
!        90 degrees, which is not a kink, so the slot stays a gap even
!        though its ends turn 180 degrees within two points
!        ---------------------------------------------------------------
!
         curve % count = 0
         CALL AddLine(curve, [0.0_RP, -0.5_RP] , [10.0_RP, -0.5_RP], 10)
         CALL AddLine(curve, [10.0_RP, -0.5_RP], [10.0_RP, 0.5_RP] , 1)
         CALL AddLine(curve, [10.0_RP, 0.5_RP] , [0.0_RP, 0.5_RP]  , 10)
         CALL AddLine(curve, [0.0_RP, 0.5_RP]  , [0.0_RP, -0.5_RP] , 1)
         CALL MakeChainPoints(curve, points)
         CALL FTAssertEqual(expectedValue = 0, actualValue = SIZE(points % kinkArcLength), &
                            msg = "Square slot: no kinks")
         CALL FTAssert(test = .NOT.acrossCornerAt(points, [9.0_RP, -0.5_RP], [9.0_RP, 0.5_RP]), &
                       msg = "Square slot: the sides near an end are across a gap")
!
!        ---------------------------------------------------------------
!        A lens: two circular arcs of radius 2 that meet on the x axis at
!        60 degrees. Close to a tip the sides are nearly straight, so
!        the points there are across a corner. Across the middle the
!        way around through a tip is still short, but the sides have
!        turned 30 degrees away from their direction at the tip
!        ---------------------------------------------------------------
!
         radius = 2.0_RP
         beta   = 30.0_RP
         d      = radius*COS(beta*PI/180.0_RP)
         curve % count = 0
         CALL AddArc(curve, [0.0_RP, -d], radius, 90.0_RP - beta , 90.0_RP + beta , 60)
         CALL AddArc(curve, [0.0_RP,  d], radius, 270.0_RP - beta, 270.0_RP + beta, 60)
         CALL MakeChainPoints(curve, points)
         CALL FTAssertEqual(expectedValue = 2, actualValue = SIZE(points % kinkArcLength), &
                            msg = "Lens: a kink at each tip")
         CALL FTAssert(test = acrossCornerAt(points, [0.0_RP, -d] + radius*arcPoint(90.0_RP - beta + 2.0_RP), &
                                                     [0.0_RP,  d] + radius*arcPoint(270.0_RP + beta - 2.0_RP)), &
                       msg = "Lens: points next to a tip are across a corner")
         CALL FTAssert(test = .NOT.acrossCornerAt(points, [0.0_RP, radius - d], [0.0_RP, d - radius]), &
                       msg = "Lens: the points across the middle are across a gap")

      END SUBROUTINE SharpCornerRuleTests
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION arcPoint( degrees ) RESULT(x)
         IMPLICIT NONE
         REAL(KIND=RP) :: degrees, x(2)
         x = [COS(degrees*PI/180.0_RP), SIN(degrees*PI/180.0_RP)]
      END FUNCTION arcPoint
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION starText( name, outerRadius, tipDegrees ) RESULT(text)
!
!     ---------------------------------------------------------------
!     A five pointed star centered at the origin as a chain of
!     straight lines, with its tips at outerRadius and an angle of
!     tipDegrees at each tip
!     ---------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=*)               :: name
         REAL(KIND=RP)                  :: outerRadius, tipDegrees
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)

         REAL(KIND=RP)     :: innerRadius, t, vertices(2,10), theta
         CHARACTER(LEN=LL) :: xStart, xEnd
         CHARACTER(LEN=8)  :: sideName
         INTEGER           :: k, kNext
!
!        -----------------------------------------------------------
!        The inner vertices are 36 degrees around from a tip, at the
!        radius that gives the tip its angle
!        -----------------------------------------------------------
!
         t           = TAN(0.5_RP*tipDegrees*PI/180.0_RP)
         innerRadius = outerRadius*t/(SIN(PI/5.0_RP) + t*COS(PI/5.0_RP))
         DO k = 1, 10
            theta = PI/2.0_RP + REAL(k - 1,RP)*PI/5.0_RP
            IF ( MOD(k,2) == 1 )     THEN
               vertices(:,k) = outerRadius*[COS(theta), SIN(theta)]
            ELSE
               vertices(:,k) = innerRadius*[COS(theta), SIN(theta)]
            END IF
         END DO

         text = [CHARACTER(LEN=LL) ::]
         DO k = 1, 10
            kNext = MOD(k,10) + 1
            WRITE(sideName,'(A,I0)') "side", k
            WRITE(xStart,'(A,ES15.8,A,ES15.8,A)') "xStart = [", vertices(1,k), ",", vertices(2,k), ",0.0]"
            WRITE(xEnd  ,'(A,ES15.8,A,ES15.8,A)') "xEnd   = [", vertices(1,kNext), ",", vertices(2,kNext), ",0.0]"
            text = [CHARACTER(LEN=LL) :: text, "\begin{END_POINTS_LINE}", "name = " // TRIM(sideName), &
                                         xStart, xEnd, "\end{END_POINTS_LINE}"]
         END DO
         text = chainText(name, text)
      END FUNCTION starText
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SharpInterfaceSizingTests(optData)
!
!     ---------------------------------------------------------------
!     An interface star with 30 degree tips inside the circle of
!     radius 4. Both sides of an interface are meshed, so the two
!     sides of each tip face each other. Counted as a gap, their
!     distance shrinks every time the resizer adds points: after its
!     three passes the star asks for a size 236 times smaller than
!     the background grid (7.9 of the 8 allowed subdivision levels),
!     and the mesh has 6600 elements. Counted as a corner, the tips
!     ask for no refinement of their own (a ratio of about 2).
!     ---------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused
         CHARACTER(LEN=LL), PARAMETER :: none(0) = [CHARACTER(LEN=LL) ::]

         TYPE (FTValueDictionary), POINTER :: controlDict
         CLASS(MeshProject)      , POINTER :: project
         REAL(KIND=RP)                     :: sizeRatio
         CHARACTER(LEN=128)                :: tightestCurveName

         CALL ClearExceptions
         CALL BuildProjectFromText([controlInputText([runParametersText("ISM"), backgroundGridText()]), &
                                    modelText(none, starText("star", 2.0_RP, 30.0_RP), none)],         &
                                   controlDict, project)
         CALL FTAssert(test = .NOT.catch(FATAL_ERROR_EXCEPTION), &
                       msg = "A star interface with 30 degree tips passes the subdivision check")
         CALL project % sizer % sizeRatio(ratio = sizeRatio, worstOffenderName = tightestCurveName)
         CALL FTAssert(test = sizeRatio < 8.0_RP, &
                       msg = "The tips of a star interface do not ask for more than 3 subdivision levels")
         CALL releaseMeshProject(project)
         CALL releaseFTValueDictionary(controlDict)
         CALL ClearExceptions

      END SUBROUTINE SharpInterfaceSizingTests

      END Module SizerTestsModule

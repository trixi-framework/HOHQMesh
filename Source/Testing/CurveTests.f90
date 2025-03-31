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
!      CurveTests.f90
!      Created: May 25, 2021 at 9:33 AM
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE TestCurves
         USE SMConstants
         USE SMCurveClass
         USE SMEllipticArcClass
         USE SMParametricEquationCurveClass
         USE SMSplineCurveClass
         USE SMTopographyClass
         USE SMLineClass
         USE TestSuiteManagerClass
         USE FTAssertions
         USE SharedExceptionManagerModule
         USE LineReflectionModule
!
!        ----------------------------------------------------------------
!        Instantiate and test for correctness the different curve classes
!        ----------------------------------------------------------------
!
         IMPLICIT NONE

         REAL(KIND=RP)                           :: xExact(3)
         INTEGER                                 :: j
         REAL(KIND=RP)                           :: dt = 0.2_RP, t, e, tol = 1.0d-5
         INTEGER                                 :: iUnit = 200
!
!        ---------------
!        Line definition
!        ---------------
!
         TYPE(SMLine)  :: line
         REAL(KIND=RP) :: xStart(3), xEnd(3), x(3)
         REAL(KIND=RP) :: a,b,c, aT,bT,cT
!
!        -----------------------
!        Elliptic arc definition
!        -----------------------
!
         TYPE(SMEllipticArc) :: ellipse
         TYPE(SMEllipticArc) :: circle
         REAL(KIND=RP)       :: xc, yc, zc, theta, phi
!
!        ------------------------------
!        Parametric Equation definition
!        ------------------------------
!
         CHARACTER(LEN=EQUATION_STRING_LENGTH) :: sx = "f(t) = 0.2 + 1.3*cos(2*pi*t)"
         CHARACTER(LEN=EQUATION_STRING_LENGTH) :: sy = "f(t) = 0.05 + 0.3*sin(2*pi*t)"
         CHARACTER(LEN=EQUATION_STRING_LENGTH) :: sz = "f(t) = 0.0"
         TYPE(SMParametricEquationCurve)       :: curve
!
!        -----------------------
!        Spline Curve definition
!        -----------------------
!
         REAL(KIND=RP)       :: r(7), xA(7), yA(7), zA(7)
         TYPE(SMSplineCurve) :: spline
!
!        ---------
!        Test Line
!        ---------
!
         xStart = [1.1_RP,1.2_RP,0.0_RP] ; xEnd = [2.1_RP,3.2_RP,0.0_RP]
         CALL line % initWithStartEndNameAndID(xStart = xStart,       &
                                               xEnd   = xEnd,         &
                                               cName  = "Line Curve", &
                                               id     = 1)
         DO j = 1, 3
            t      = j*dt
            x      = line %positionAt(t)
            xExact = xStart*(1.0_RP - t) + xEnd*t
            e      = MAXVAL(ABS(x - xExact))
            CALL FTAssertEqual(expectedValue = 0.0_RP,         &
                               actualValue   = e,              &
                               relTol        = tol,            &
                               absTol        = tol/10.0_RP,    &
                               msg           = "Line evaluation error too large")
         END DO
         
         CALL FTAssert(test = line % curveIsStraight(), msg = "Line straightness")
         CALL destructLine(self = line)
         OPEN(NEWUNIT = iUnit, STATUS='SCRATCH')
            CALL printLineDescription(self = line, iUnit = iUnit)
            CALL TestPrintDescription("SMLine Object", iUnit)
         CLOSE(iUnit)
!
!        -----------
!        Test circle
!        -----------
!
         CALL circle % initWithParametersNameAndID(center     = [1.1_RP,1.1_RP,0.0_RP], &
                                                   radius     = 1.3_RP,                 &
                                                   startAngle = 0.1_RP,                 &
                                                   endAngle   = PI,                     &
                                                   cName      = "Circle",               &
                                                   id         = 1)
         DO j = 1,3
            t      = j*dt
            theta  = 0.1_RP + t*(PI - 0.1_RP)
            x      = circle % positionAt(t)
            xc     = 1.1_RP + 1.3_RP*COS(theta)
            yc     = 1.1_RP + 1.3_RP*SIN(theta)
            xExact = [xc,yc,0.0_RP]
            e      = MAXVAL(ABS(x - xExact))
            CALL FTAssertEqual(expectedValue = 0.0_RP,         &
                               actualValue   = e,              &
                               relTol        = tol,            &
                               absTol        = tol/10.0_RP,    &
                               msg           = "Circle evaluation error too large")
         END DO
         CALL FTAssert(test = .NOT. circle % curveIsStraight(), msg = "Circle shouldn't be straight")
         CALL destructEllipticArc(self = circle)
!
!        ------------
!        Test ellipse
!        ------------
!
         CALL ellipse % initWithParametersNameAndID(center      = [1.1_RP,1.1_RP,0.0_RP], &
                                                    xRadius     = 1.3_RP,                 &
                                                    yRadius     = 2.6_RP,                 &
                                                    startAngle  = 0.1_RP,                 &
                                                    endAngle    = PI,                     &
                                                    rotation    = PI*0.25_RP,             &
                                                    cName       = "Ellipse",              &
                                                    id          = 1)
         DO j = 1,3
            t      = j*dt
            theta  = 0.1_RP + t*(PI - 0.1_RP)
            phi    = PI*0.25_RP
            x      = ellipse % positionAt(t)
            xc     = 1.1_RP + 1.3_RP*COS(theta)*COS(phi) - 2.6_RP*SIN(theta)*SIN(phi)
            yc     = 1.1_RP + 1.3_RP*COS(theta)*SIN(phi) + 2.6_RP*SIN(theta)*COS(phi)
            xExact = [xc,yc,0.0_RP]
            e      = MAXVAL(ABS(x - xExact))
            CALL FTAssertEqual(expectedValue = 0.0_RP,         &
                               actualValue   = e,              &
                               relTol        = tol,            &
                               absTol        = tol/10.0_RP,    &
                               msg           = "Ellipse evaluation error too large")
         END DO
         CALL FTAssert(test = .NOT. ellipse % curveIsStraight(), msg = "Ellipse shouldn't be straight")
         CALL destructEllipticArc(self = ellipse)
         OPEN(NEWUNIT = iUnit, STATUS='SCRATCH')
            CALL printEllipticArcDescription(self = ellipse, iUnit = iUnit)
            CALL TestPrintDescription("SMEllipticArc Object", iUnit)
         CLOSE(iUnit)
!
!        ------------------------------
!        Parametric equation curve test
!        ------------------------------
!
         CALL curve % initWithEquationsNameAndID(xEqn      = sx,                &
                                                 yEqn      = sy,                &
                                                 zEqn      = sz,                &
                                                 curveName = "Parametric Curve",&
                                                 id        = 1)
         CALL FTAssert(test = .NOT.catch(), msg = "Equation has errors")
         CALL FTAssert(test = .NOT. curve % curveIsStraight(), msg = "Curve shouldn't be straight")

         DO j = 1, 3
            t      = j*dt
            x      = curve % positionAt(t)
            xc     = 0.2_RP + 1.3_RP*cos(2.0_RP*pi*t)
            yc     = 0.05_RP + 0.3_RP*sin(2.0_RP*pi*t)
            xExact = [xc,yc,0.0_RP]
            e      = MAXVAL(ABS(x - xExact))
            CALL FTAssertEqual(expectedValue = 0.0_RP,         &
                               actualValue   = e,              &
                               relTol        = tol,            &
                               absTol        = tol/10.0_RP,    &
                               msg           = "Equation evaluation error too large")
         END DO
         CALL destructPECurve(self = curve)
!
!        ----------------------------------------------------
!        Spline curve test. A cubic function should be exact.
!        ----------------------------------------------------
!
         DO j = 1, 7
            t = 1.0_RP - (j-1)/6.0_RP ! to define a counterclockwise curve
            r(j) =  t
            xA(j) = t**3
            yA(j) = t**2
            zA(j) = t
         END DO
         CALL spline % initWithPointsNameAndID(t         = r,              &
                                               x         = xA,             &
                                               y         = yA,             &
                                               z         = zA,             &
                                               curveName = "Spline Curve", &
                                               id        = 1)
         DO j = 1, 3
            t      = j*dt
            x      = spline % positionAt(t)
            xc     = t**3
            yc     = t**2
            zc     = t
            xExact = [xc,yc,zc]
            e      = MAXVAL(ABS(x - xExact))
            CALL FTAssertEqual(expectedValue = 0.0_RP,         &
                               actualValue   = e,              &
                               relTol        = tol,            &
                               absTol        = tol/10.0_RP,    &
                               msg           = "Spline error too large")
         END DO
         CALL FTAssert(test = .NOT. spline % curveIsStraight(), msg = "Spline shouldn't be straight")
         CALL DestructSplineCurve(spline)
!
!        ---------------------
!        Test reflecting lines
!        ---------------------
!
         CALL FTAssertEqual(expectedValue = 0.0_RP,                  &
                            actualValue   = lineCoefficientTestError(),&
                            relTol        = tol,                     &
                            absTol        = tol/10.0_RP,             &
                            msg           = "lineCoefficientTestError")
         CALL FTAssertEqual(expectedValue = 0.0_RP,                  &
                            actualValue   = lineReflectionTestError(),&
                            relTol        = tol,                     &
                            absTol        = tol/10.0_RP,             &
                            msg           = "lineReflectionTestError")
!
!        ----------------
!        Test colinearity
!        ----------------
!
         CALL GetTestLineCoefficients(a, b, c)
         aT = 1.5_RP*a; bT = 1.5_RP*b; cT = 1.5_RP*c
         CALL FTAssert(linesAreColinear(a,b,c,aT,bT,cT),msg = "Colinear lines are not colinear")
         
         aT = 1.5_RP*a; bT = 1.1_RP*b; cT = 1.75_RP*c
         CALL FTAssert(.NOT. linesAreColinear(a,b,c,aT,bT,cT),msg = "Colinear lines are not colinear")
         
      END SUBROUTINE TestCurves
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE TestPrincipalCurvature
         USE SMConstants
         USE TestSuiteManagerClass
         USE FTAssertions
         USE SharedExceptionManagerModule
         USE PrincipalCurvatureModule
         IMPLICIT NONE

         REAL(KIND=RP) :: gradF(2), exactGradF(2)
         REAL(KIND=RP) :: hessF(2,2), exactHess(2,2)
         REAL(KIND=RP) :: exact, computed

        CALL ExactAndComputedSphereCurvature(0.5_RP,0.5_RP, exact, computed)
        CALL FTAssertEqual(expectedValue = exact,    &
                           actualValue   = computed, &
                           relTol        = 2.0d-8,   &
                           absTol        = 2.0d-9,   &
                           msg           = "Computed Sphere Curvature error")

        CALL ExactAndComputedHyParCurvature(0.5_RP,0.5_RP, exact, computed)
        CALL FTAssertEqual(expectedValue = exact,    &
                           actualValue   = computed, &
                           relTol        = 2.0d-8,   &
                           absTol        = 1.0d-9,   &
                           msg           = "Computed Quadratic Curvature error")

        CALL ExactAndComputedQuadraticGradients(x     = 0.5_RP,     y        = 0.5_RP, &
                                                exact = exactGradF, computed = gradF)
        CALL FTAssertEqual(expectedValue = 0.0_RP,                        &
                           actualValue   = MAXVAL(ABS(exactGradF-gradF)), &
                           relTol        = 2.0d-8,                        &
                           absTol        = 1.0d-9,                        &
                           msg           = "Quadratic Gradients error")

        CALL ExactAndComputedQuadraticHessian(x = 0.5_RP,y = 0.5_RP,exact = exactHess,computed = hessF)
        CALL FTAssertEqual(expectedValue = 0.0_RP,                       &
                           actualValue   = MAXVAL(ABS(exactHess-hessF)), &
                           relTol        = 2.0d-8,                       &
                           absTol        = 1.0d-9,                       &
                           msg           = "Quadratic Hessian error")

      END SUBROUTINE TestPrincipalCurvature
!
!     ---------------------
!     Test printDescription
!     ---------------------
!
      SUBROUTINE TestPrintDescription(description, iUnit)
         USE FTAssertions
         IMPLICIT NONE
         INTEGER            :: diffCheck
         INTEGER            :: iUnit, k
         CHARACTER(len=*)   :: description
         CHARACTER(len=100) :: fileLine

         diffCheck = 0
         REWIND(iUnit)
         READ(iUnit, '(A)') fileLine
         IF (LEN_TRIM(fileLine) /= LEN_TRIM(description)) THEN
            diffCheck = diffCheck + 1
         ELSE
            DO k = 1, MIN(LEN_TRIM(fileLine), LEN_TRIM(description))
               IF (fileLine(k:k) /= description(k:k)) THEN
                  diffCheck = diffCheck + 1
               END IF
            END DO
         END IF
         CALL FTAssertEqual(expectedValue = 0,                                                 &
                            actualValue   = diffCheck,                                         &
                            msg           = "Description not writing correctly; should be:" // &
                            ACHAR(10) // "       " // description)

      END SUBROUTINE TestPrintDescription
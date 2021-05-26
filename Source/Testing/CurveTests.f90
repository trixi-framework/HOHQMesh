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
         USE SMCircularArcClass
         USE SMParametricEquationCurveClass
         USE SMSplineCurveClass
         USE SMTopographyClass
         USE SMLineClass
         USE TestSuiteManagerClass
         USE FTAssertions
         USE SharedExceptionManagerModule
!
!        -----------------------------------------------------------------
!        Instantiate and test for correctness the differrent curve classes
!        -----------------------------------------------------------------
!
         IMPLICIT NONE
         
         REAL(KIND=RP)                           :: xExact(3)
         INTEGER                                 :: j
         REAL(KIND=RP)                           :: dt = 0.2_RP, t, e, tol = 1.0d-5
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: msg
!
!        ---------------
!        Line definition
!        ---------------
!
         TYPE(SMLine)  :: line
         REAL(KIND=RP) :: xStart(3), xEnd(3), x(3)
!
!        -----------------------
!        Circular arc definition
!        -----------------------
!
         TYPE(SMCircularArc) :: circle
         REAL(KIND=RP)       :: xc, yc, zc, theta
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
            CALL FTAssertEqual(expectedValue = 0.0_RP, &
                               actualValue   = e,      &
                               tol           = tol,    &
                               msg           = "Line evaluation error too large")
         END DO 
         CALL destructLine(self = line)
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
            CALL FTAssertEqual(expectedValue = 0.0_RP, &
                               actualValue   = e,      &
                               tol           = tol,    &
                               msg           = "Circle evaluation error too large")
         END DO 
         CALL destructCircularArc(self = circle) 
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
         
         DO j = 1, 3 
            t      = j*dt
            x      = curve % positionAt(t)
            xc     = 0.2_RP + 1.3_RP*cos(2.0_RP*pi*t)
            yc     = 0.05_RP + 0.3_RP*sin(2.0_RP*pi*t)
            xExact = [xc,yc,0.0_RP]
            e      = MAXVAL(ABS(x - xExact))
            CALL FTAssertEqual(expectedValue = 0.0_RP, &
                               actualValue   = e,      &
                               tol           = tol,    &
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
            CALL FTAssertEqual(expectedValue = 0.0_RP, &
                               actualValue   = e,      &
                               tol           = tol,    &
                               msg           = "Spline error too large")
         END DO 
         
      END SUBROUTINE TestCurves

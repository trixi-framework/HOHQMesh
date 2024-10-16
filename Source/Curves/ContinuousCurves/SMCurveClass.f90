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
!      SMCurveClass.f90
!      Created: July 30, 2013 3:42 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module SMCurveClass
      USE SMConstants
      USE FTObjectClass
      IMPLICIT NONE 
!
!     ---------
!     Constants
!     ---------
!
      INTEGER, PARAMETER :: SM_CURVE_NAME_LENGTH = 32
      CHARACTER(LEN=SM_CURVE_NAME_LENGTH), PARAMETER :: SYMMETRY_CURVE_NAME = ":symmetry"
!
!     ---------------------
!     Base class definition
!     ---------------------
!
      TYPE, EXTENDS(FTObject) :: SMCurve
         INTEGER                            , PRIVATE :: id_
         CHARACTER(LEN=SM_CURVE_NAME_LENGTH), PRIVATE :: curveName_
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE, NON_OVERRIDABLE :: initWithNameAndID
         FINAL                      :: destructBaseCurve
         PROCEDURE                  :: printDescription   => printCurveDescription
         PROCEDURE                  :: positionAt
         PROCEDURE                  :: tangentAt
         PROCEDURE                  :: secondDerivativeAt
         PROCEDURE                  :: setID
         PROCEDURE                  :: id
         PROCEDURE                  :: setCurveName
         PROCEDURE                  :: curveName
         PROCEDURE                  :: className => CurveClassName
         PROCEDURE                  :: curveIsStraight
      END TYPE SMCurve
!
!     -------
!     Casting
!     -------
!
      INTERFACE cast
         MODULE PROCEDURE castToSMCurve
      END INTERFACE cast
!
!     ----------------
!     Module variables
!     ----------------
!
      REAL(KIND=RP) :: xTarget(3)
      PRIVATE       :: xTarget
      
      REAL(KIND=RP), PARAMETER, PRIVATE :: dt = 1.0d-5
!
!     ========
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initWithNameAndID( self, cName, id )  
         IMPLICIT NONE
         CLASS(SMCurve)   :: self
         CHARACTER(LEN=*) :: cName
         INTEGER          :: id
         
         CALL self % FTObject % init()
         CALL self % setCurveName(cName)
         CALL self % setID(id)
         
      END SUBROUTINE initWithNameAndID
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructBaseCurve(self)  
         IMPLICIT NONE
         TYPE(SMCurve) :: self
         
         CALL self % setCurveName("")
         CALL self % setID(0)
         
      END SUBROUTINE destructBaseCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseBaseCurve(self)  
         IMPLICIT NONE
         CLASS(SMCurve), POINTER :: self
         CLASS(FTObject)          , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseBaseCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE setID(self,id)  
         IMPLICIT NONE  
         CLASS(SMCurve) :: self
         INTEGER        :: id
         self % id_ = id
      END SUBROUTINE setID
!
!//////////////////////////////////////////////////////////////////////// 
! 
     INTEGER FUNCTION id(self)  
        IMPLICIT NONE  
        CLASS(SMCurve) :: self
        id = self % id_ 
     END FUNCTION id
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE setCurveName(self,cName)  
         IMPLICIT NONE  
         CLASS(SMCurve)  :: self
         CHARACTER(LEN=*):: cName
         self % curveName_ = cName
     END SUBROUTINE setCurveName
!
!//////////////////////////////////////////////////////////////////////// 
! 
     CHARACTER(LEN=SM_CURVE_NAME_LENGTH) FUNCTION curveName(self)  
        IMPLICIT NONE  
        CLASS(SMCurve) :: self
        curveName = self % curveName_ 
     END FUNCTION curveName
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> Class name returns a string with the name of the type of the object
!>
!>  ### Usage:
!>
!>        PRINT *,  obj % className()
!>        if( obj % className = "Curve")
!>
      FUNCTION CurveClassName(self)  RESULT(s)
         IMPLICIT NONE  
         CLASS(SMCurve)                             :: self
         CHARACTER(LEN=CLASS_NAME_CHARACTER_LENGTH) :: s
         
         s = "Curve"
         IF( self % refCount() >= 0 ) CONTINUE 
 
      END FUNCTION CurveClassName
!
!//////////////////////////////////////////////////////////////////////// 
! 
     FUNCTION positionAt(self,t)  RESULT(x)
        IMPLICIT NONE  
        CLASS(SMCurve) :: self
        REAL(KIND=RP)  :: t
        REAL(KIND=RP)  :: x(3)
        x = 0.0_RP
     END FUNCTION positionAt
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION tangentAt(self,t)  RESULT(x)
         IMPLICIT NONE  
         CLASS(SMCurve) :: self
         REAL(KIND=RP)  :: t
         REAL(KIND=RP)  :: x(3)

         REAL(KIND=RP), DIMENSION(3) :: xp, xm, dx
         REAL(KIND=RP)               :: tp, tm
         
         tp = MIN(1.0_RP,t + dt)
         tm = MAX(0.0_RP,t - dt)
         
         xp = self % positionAt(tp)
         xm = self % positionAt(tm)
         
         dx = xp - xm
         x  = dx/(tp - tm)
         x  = x/SQRT(x(1)**2 + x(2)**2 + x(3)**2)

      END FUNCTION tangentAt
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION secondDerivativeAt(self,t)  RESULT(x)
         IMPLICIT NONE  
         CLASS(SMCurve) :: self
         REAL(KIND=RP)  :: t
         REAL(KIND=RP)  :: x(3)

         REAL(KIND=RP), DIMENSION(3) :: xp, xm, x0
                  
         IF ( t < dt )     THEN
            xp = self % positionAt(t + 2.0_RP*dt)
            x0 = self % positionAt(t + dt)
            xm = self % positionAt(t)
         ELSE IF( t > 1.0_RP - dt)      THEN 
            xp = self % positionAt(t)
            x0 = self % positionAt(t - dt)
            xm = self % positionAt(t - 2.0_RP*dt)
         ELSE
            xp = self % positionAt(t + dt)
            x0 = self % positionAt(t)
            xm = self % positionAt(t - dt)
         END IF 
         
         x = (xp - 2.0_RP*x0 + xm)/dt**2

      END FUNCTION secondDerivativeAt
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToSMCurve(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject), POINTER :: obj
         CLASS(SMCurve) , POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            CLASS IS(SMCurve)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castToSMCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printCurveDescription(self,iUnit)  
         IMPLICIT NONE
         INTEGER        :: iUnit
         CLASS(SMCurve) :: self
         WRITE(iUnit,*) "Curve with id ",self % id_, "is named ", self % curveName_
      END SUBROUTINE printCurveDescription
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION ParametrizationAtPointNear( self, x, tOld )  RESULT(t)
         USE ProgramGlobals, ONLY : minimizationTolerance
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS( SMCurve )            :: self
         REAL(KIND=RP), DIMENSION(3) :: x
         REAL(KIND=RP)               :: t, tOld
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP) :: tLeft, tRight
         tLeft  = MAX(0.0_RP,tOld - 0.1_RP)
         tRight = MIN(tOld + 0.1_RP, 1.0_RP)
         xTarget = x
         t       = fmin(self,tLeft,tRight,minimizationTolerance)
         
      END FUNCTION ParametrizationAtPointNear
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION JointClassification( thisCurve, nextCurve, inOutFlag ) 
!
!     ----------------------------------------
!     Classify the joining of two curves
!     as one of:
!     ROW_END,ROW_SIDE,ROW_CORNER,ROW_REVERSAL
!     ----------------------------------------
!
         USE ProgramGlobals, ONLY: INNER, OUTER
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMCurve), POINTER  :: thisCurve, nextCurve
         INTEGER, INTENT(IN)      :: inOutFlag
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP)            :: a(3), b(3), p1(3), p2(3)
         REAL(KIND=RP)            :: nrmA, nrmB, alpha, gamma, aCrossB, aDotB
         REAL(KIND=RP), PARAMETER :: h = 0.001_RP
         
         p2    = thisCurve % positionAt(1.0_RP)
         p1    = thisCurve % positionAt(1.0_RP - h)
         a     = p2 - p1
         nrmA  = SQRT(a(1)**2 + a(2)**2 + a(3)**2)
         
         p2    = nextCurve % positionAt(0.0_RP + h)
         p1    = nextCurve % positionAt(0.0_RP)
         b     = p2 - p1
         nrmB  = SQRT(b(1)**2 + b(2)**2 + b(3)**2)
         
         aDotB = a(1)*b(1) + a(2)*b(2) + a(3)*b(3)
         gamma = ACOS( aDotB/(nrmA*nrmB) )

         alpha = PI - gamma
         aCrossB = a(1)*b(2) - b(1)*a(2)
         IF(  aCrossB < 0.0_RP )     THEN
            alpha = 2*PI - alpha
         END IF
         
         IF ( inOutFlag == INNER )     THEN
            alpha = 2*PI - alpha 
         END IF 
         
         JointClassification = Classification( alpha )
      END FUNCTION JointClassification
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION Classification( angle ) 
         USE ProgramGlobals, ONLY: ROW_END,ROW_SIDE,ROW_CORNER,ROW_REVERSAL
         IMPLICIT NONE
         REAL(KIND=RP) :: angle
         !Note: The range for a ROW_SIDE used to go up to 2PI/3. That
         ! missed some obvious joints. Note that these ranges are fuzzy
         ! A side should be "near" 180deg. End "near" 90deg.
         
         IF ( angle <= 2*PI/3 )     THEN
            Classification = ROW_END
         ELSE IF ( angle > 2*PI/3 .AND. angle < 1.15_RP*PI)     THEN
            Classification = ROW_SIDE
         ELSE IF ( angle >= 1.15_RP*PI.AND. angle <= 5*PI/3 )     THEN
            classification = ROW_CORNER
         ELSE
            Classification = ROW_REVERSAL
         END IF
         
      END FUNCTION Classification
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION ObjectiveFunction(self,t) RESULT(d)  
         IMPLICIT NONE  
         CLASS(SMCurve) :: self
         REAL(KIND=RP)  :: t, d
         REAL(KIND=RP)  :: x(3)
         
         x = self % positionAt(t)
         d = (x(1)-xTarget(1))**2 + (x(2)-xTarget(2))**2
      END FUNCTION ObjectiveFunction
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION distanceSquared(x,c,p,nHat)
         IMPLICIT NONE
         REAL(KIND=RP) :: x, p(3), z(3), nHat(3)
         CLASS(SMCurve)   :: c
         
         z               = c % positionAt(x)
         distanceSquared = DistanceSquaredBetweenPoints(z,p,nHat)
      END FUNCTION distanceSquared
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION DistanceSquaredBetweenPoints(z,p,nHat)
         USE ProgramGlobals, ONLY: directionPenalty
         IMPLICIT NONE
         REAL(KIND=RP) ::  p(3), z(3), nHat(3)
!
!        ----------------------------------------------------------------------------------------------------------
!        DAK: The nHat was put in to allow finding the minimum distance in a given direction, 
!        but it didn't pan out, i.e. it didn't seem to turn out to be helpful. It could 
!        be removed, but my thought is just to leave it in on the chance that we may want to use it later.         
!        ----------------------------------------------------------------------------------------------------------
!
         DistanceSquaredBetweenPoints = (z(1) - p(1))**2 + (z(2) - p(2))**2 &
                           - MIN((z(1)-p(1))*nHat(1) + (z(2) - p(2))*nHat(2),0.0d0)/directionPenalty
      END FUNCTION DistanceSquaredBetweenPoints
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION curveIsStraight(self)
         USE ProgramGlobals, ONLY: straightLineTol
!
!     -------------------------------------------------------------------------------
!     To see if a curve is straight, we check to see if second derivative vanishes
!     at an arbitrary point on the curve. This function will fail if there is an
!     inflection point at that chosen point.
!     -------------------------------------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(SMCurve) :: self
         REAL(KIND=RP)  :: sec(3)
         REAL(KIND=RP)  :: e
         
         sec = self % secondDerivativeAt(0.365_RP)
         e   = MAXVAL(ABS(sec))
         
         curveIsStraight = .FALSE.
         IF ( e <= straightLineTol )     THEN !Lies along the line
               curveIsStraight = .TRUE. 
         END IF

      END FUNCTION curveIsStraight
!
!//////////////////////////////////////////////////////////////////////// 
! 
   FUNCTION fMin(self, aIn, bIn, tol, pnt, nHat ) RESULT(z)
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(SMCurve)          :: self
      REAL(KIND=RP)           :: aIn, bIN, tol
      REAL(KIND=RP)           :: z
!
!     ----------------------------------------------------------------------------------------------------------
!     DAK: The nHat was put in to allow finding the minimum distance in a given direction, 
!     but it didn't pan out, i.e. it didn't seem to turn out to be helpful. It could 
!     be removed, but my thought is just to leave it in on the chance that we may want to use it later.         
!     ----------------------------------------------------------------------------------------------------------
!
      REAL(KIND=RP), OPTIONAL :: pnt(3), nHat(3)
!
!     ---------------
!     local Variables
!     ---------------
!
      REAL(KIND=RP) :: a, b, c, d, e, m, p, q, r, t2, u, v, w, fu, fv, fw, fx, t, x
      REAL(KIND=RP) :: t3, eps
!
!     -----
!     Setup
!     -----
!
      eps = EPSILON(1.0_RP)
      t3  = tol/3.0_RP
      eps = SQRT(eps)
      
      c  = 0.5_RP*(3.0_RP - SQRT(5.0_RP))
      a  = aIn
      b  = bIn
      x  = a + c*(b-a)   ; v = x  ; w = x   ; e = 0.0_RP; d = 0.0_RP
      
      IF ( PRESENT(pnt) )     THEN
         fx = distanceSquared(x,self,pnt,nHat) 
      ELSE 
         fx = ObjectiveFunction(self,x)
      END IF 
      fv = fx; fw = fx
!
!     ---------
!     Main loop
!     ---------
!
      DO
         m  = 0.5_RP*(a + b)
         t  = eps*ABS(x) + t3
         t2 = 2.0_RP*t
         
         IF ( ABS(x-m) <= t2 - 0.5_RP*(b-a) )     EXIT
         
         p = 0.0_RP; q = 0.0_RP; r = 0.0_RP
         
         IF ( ABS(e) > t )     THEN !Fit parabola
            r = (x - w)*(fx - fv)
            q = (x - v)*(fx - fw)
            p = (x - v)*q - (x - w)*r
            q = 2.0_RP*(q - r)
            IF ( q > 0.0_RP )     THEN
               p = -p 
            ELSE 
               q = -q 
            END IF
            r = e
            e = d
         END IF
         
         IF ( ABS(p) < ABS(0.5_RP*q*r) .AND. &
              p < q*(a - x)            .AND. &
              p < q*(b - x) )                    THEN !Parabolic interpolation step
              
            d = p/q
            u = x + d
            
            IF ( u - a < t2 .OR. b - u < t2 )     THEN
               IF ( x < m )     THEN
                  d = t 
               ELSE 
                  d = -t 
               END IF 
            END IF 
            
         ELSE ! Golden section step
         
            IF ( x < m )     THEN
               e = b - x 
            ELSE 
               e = a - x 
            END IF 
            d = c*e  
         END IF 
         
         IF ( ABS(d) >= t )     THEN
            u = x + d 
         ELSE IF (d > 0.0_RP)    THEN 
            u = x + t
         ELSE 
            u = x - t
         END IF
         
         IF ( PRESENT(pnt) )     THEN
            fu = distanceSquared(u,self,pnt,nHat) 
         ELSE 
            fu = ObjectiveFunction(self,u)
         END IF 
         
         IF ( fu <= fx )     THEN
            IF ( u < x )     THEN
               b = x 
            ELSE 
               a = x 
            END IF 
            v = w; fv = fw; w = x; fw = fx; x = u; fx = fu
         ELSE
            IF ( u < x )     THEN
               a = u 
            ELSE 
               b = u 
            END IF 
            IF ( fu <= fw .OR. w == x )     THEN
               v  = w
               fv = fw
               w  = u
               fw = fu 
            ELSE IF(fu <= fv .OR. v == x .OR. v == w)     THEN 
               v = u; fv = fu 
            END IF 
              
         END IF 
      END DO
!
!     ------------------
!     RETURN the result 
!     ------------------
!
      z = x
      
   END FUNCTION fMin
     
     END Module SMCurveClass

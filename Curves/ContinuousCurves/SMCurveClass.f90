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
         PROCEDURE                  :: destruct           => destructBaseCurve
         PROCEDURE                  :: printDescription   => printCurveDescription
         PROCEDURE                  :: positionAt
         PROCEDURE                  :: tangentAt
         PROCEDURE                  :: setID
         PROCEDURE                  :: id
         PROCEDURE                  :: setCurveName
         PROCEDURE                  :: curveName       
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

      PRIVATE :: fmin
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
         CLASS(SMCurve) :: self
         
         CALL self % setCurveName("")
         CALL self % setID(0)
         
         CALL self % FTObject % destruct()
         
      END SUBROUTINE destructBaseCurve
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
         REAL(KIND=RP)               :: tp, tm, dt = 1.0d-9
         
         tp = MIN(1.0_RP,t + dt)
         tm = MAX(0.0_RP,t - dt)
         
         xp = self % positionAt(tp)
         xm = self % positionAt(tm)
         
         dx = xp - xm
         x  = dx/(tp - tm)
         x  = x/SQRT(x(1)**2 + x(2)**2 + x(3)**2)

      END FUNCTION tangentAt
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
         tRight = MIN(tOld + 0.1_RP, 1.0)
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
         REAL(KIND=RP), PARAMETER :: h = 0.01_RP
         
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
         
         IF( inOutFlag == OUTER )     THEN ! 2D ONLY
            alpha = PI - gamma
            aCrossB = a(1)*b(2) - b(1)*a(2)
            IF(  aCrossB < 0.0_RP )     THEN
               alpha = 2*PI - alpha
            END IF
         ELSE
            alpha = PI - gamma
            aCrossB = a(1)*b(2) - b(1)*a(2)
            IF(  aDotB < 0.0_RP )     THEN
               alpha = 2*PI - alpha
            END IF
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
         
         IF ( angle <= 2*PI/3 )     THEN
            Classification = ROW_END
         ELSE IF ( angle > 2*PI/3 .AND. angle < 3*PI/2 )     THEN
            Classification = ROW_SIDE
         ELSE IF ( angle >= 3*PI/2 .AND. angle <= 5*PI/3 )     THEN
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
      double precision function fmin(self,ax,bx,tol)
      double precision ax,bx,tol
!
!      an approximation  x  to the point where  f  attains a minimum  on
!  the interval  (ax,bx)  is determined.
!
!  input..
!
!  ax    left endpoint of initial interval
!  bx    right endpoint of initial interval
!  f     function subprogram which evaluates  f(x)  for any  x
!        in the interval  (ax,bx)
!  tol   desired length of the interval of uncertainty of the final
!        result (.ge.0.)
!
!  output..
!
!  fmin  abcissa approximating the point where  f  attains a
!        minimum
!
!      the method used is a combination of  golden  section  search  and
!  successive parabolic interpolation.  convergence is never much slower
!  than  that  for  a  fibonacci search.  if  f  has a continuous second
!  derivative which is positive at the minimum (which is not  at  ax  or
!  bx),  then  convergence  is  superlinear, and usually of the order of
!  about  1.324....
!      the function  f  is never evaluated at two points closer together
!  than  eps*abs(fmin)+(tol/3), where eps is  approximately  the  square
!  root  of  the  relative  machine  precision.   if   f   is a unimodal
!  function and the computed values of   f   are  always  unimodal  when
!  separated  by  at least  eps*abs(x)+(tol/3), then  fmin  approximates
!  the abcissa of the global minimum of  f  on the interval  ax,bx  with
!  an error less than  3*eps*abs(fmin)+tol.  if   f   is  not  unimodal,
!  then fmin may approximate a local, but perhaps non-global, minimum to
!  the same accuracy.
!      this function subprogram is a slightly modified  version  of  the
!  algol  60 procedure  localmin  given in richard brent, algorithms for
!  minimization without derivatives, prentice-hall, inc. (1973).
!
!
      CLASS(SMCurve) :: self
      double precision  a,b,c,d,e,eps,xm,p,q,r,tol1,t2,u,v,w,fu,fv,fw,&
     &    fx,x,tol3
      double precision  dabs,dsqrt
!
!  c is the squared inverse of the golden ratio
      c=0.5d0*(3.0d0-dsqrt(5.0d0))
!
!  eps is approximately the square root of the relative machine
!  precision.
!
      eps=EPSILON(1.0d0)
      tol1=eps+1.0d0
      eps=dsqrt(eps)
!
      a=ax
      b=bx
      v=a+c*(b-a)
      w=v
      x=v
      e=0.0d0
      fx=ObjectiveFunction(self,x)!f(x)
      fv=fx
      fw=fx
      tol3=tol/3.0d0
!
!  main loop starts here
!
   20 xm=0.5d0*(a+b)
      tol1=eps*dabs(x)+tol3
      t2=2.0d0*tol1
!
!  check stopping criterion
!
      if (dabs(x-xm).le.(t2-0.5d0*(b-a))) go to 190
      p=0.0d0
      q=0.0d0
      r=0.0d0
      if (dabs(e).le.tol1) go to 50
!
!  fit parabola
!
      r=(x-w)*(fx-fv)
      q=(x-v)*(fx-fw)
      p=(x-v)*q-(x-w)*r
      q=2.0d0*(q-r)
      if (q.le.0.0d0) go to 30
      p=-p
      go to 40
   30 q=-q
   40 r=e
      e=d
   50 if ((dabs(p).ge.dabs(0.5d0*q*r)).or.(p.le.q*(a-x))&
     &          .or.(p.ge.q*(b-x))) go to 60
!
!  a parabolic-interpolation step
!
      d=p/q
      u=x+d
!
!  f must not be evaluated too close to ax or bx
!
      if (((u-a).ge.t2).and.((b-u).ge.t2)) go to 90
      d=tol1
      if (x.ge.xm) d=-d
      go to 90
!
!  a golden-section step
!
   60 if (x.ge.xm) go to 70
      e=b-x
      go to 80
   70 e=a-x
   80 d=c*e
!
!  f must not be evaluated too close to x
!
   90 if (dabs(d).lt.tol1) go to 100
      u=x+d
      go to 120
  100 if (d.le.0.0d0) go to 110
      u=x+tol1
      go to 120
  110 u=x-tol1
  120 fu=ObjectiveFunction(self,u)!f(u)
!
!  update  a, b, v, w, and x
!
      if (fx.gt.fu) go to 140
      if (u.ge.x) go to 130
      a=u
      go to 140
  130 b=u
  140 if (fu.gt.fx) go to 170
      if (u.ge.x) go to 150
      b=x
      go to 160
  150 a=x
  160 v=w
      fv=fw
      w=x
      fw=fx
      x=u
      fx=fu
      go to 20
  170 if ((fu.gt.fw).and.(w.ne.x)) go to 180
      v=w
      fv=fw
      w=u
      fw=fu
      go to 20
  180 if ((fu.gt.fv).and.(v.ne.x).and.(v.ne.w)) go to 20
      v=u
      fv=fu
      go to 20
!
!  end of main loop
!
  190 fmin=x
      return
      END FUNCTION fmin
     
     END Module SMCurveClass

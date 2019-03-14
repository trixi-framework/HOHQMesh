MODULE fMinModule
CONTAINS

      DOUBLE PRECISION FUNCTION fmin(ax, bx, crve, pnt, nHat, tol)
      USE SMCurveClass
      IMPLICIT NONE
      double precision ax,bx,tol,pnt(3), nHat(3)
      !EXTERNAL    :: f
      TYPE(SMCurve) :: crve
!      INTERFACE 
!         DOUBLE PRECISION FUNCTION distanceSquared(x, crv, p, nHat)
!         USE SMCurveClass
!         IMPLICIT NONE
!         DOUBLE PRECISION :: x, p(3), nHat(3)
!         TYPE(SMCurve)      :: crv
!         DOUBLE PRECISION :: directionPenalty = 1.d-3
!         END FUNCTION distanceSquared
!      END INTERFACE
!                            
!      an approximation  x  to the point where  f  attains a minimum  on
!  the interval  (ax,bx)  is determined.                                
!                            
!  input..                   
!                            
!  ax    left endpoint of initial interval                              
!  bx    right endpoint of initial interval                             
!  f     function subprogram which evaluates  distanceSquared(x,c,p,nHat)  for any  x          
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
      double precision  a,b,c,d,e,eps,xm,p,q,r,tol1,t2,u,v,w,fu,fv,fw,  &
     &    fx,x,tol3          
!                            
!  c is the squared inverse of the golden ratio                         
      c=0.5d0*(3.0d0-sqrt(5.0d0)) 
!                            
!  eps is approximately the square root of the relative machine         
!  precision.                
!                            
      eps=EPSILON(eps)
      tol1=eps+1.0d0 
      eps=sqrt(eps) 
!                            
      a=ax 
      b=bx 
      v=a+c*(b-a) 
      w=v 
      x=v 
      e=0.0d0 
      fx=distanceSquared(x,crve,pnt,nHat) 
      fv=fx 
      fw=fx 
      tol3=tol/3.0d0 
!                            
!  main loop starts here     
!                            
   20 xm=0.5d0*(a+b) 
      tol1=eps*abs(x)+tol3 
      t2=2.0d0*tol1 
!                            
!  check stopping criterion  
!                            
      if (abs(x-xm).le.(t2-0.5d0*(b-a))) go to 190 
      p=0.0d0 
      q=0.0d0 
      r=0.0d0 
      if (abs(e).le.tol1) go to 50 
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
   50 if ((abs(p).ge.abs(0.5d0*q*r)).or.(p.le.q*(a-x))                &
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
   90 if (abs(d).lt.tol1) go to 100 
      u=x+d 
      go to 120 
  100 if (d.le.0.0d0) go to 110 
      u=x+tol1 
      go to 120 
  110 u=x-tol1 
  120 fu=distanceSquared(u,crve,pnt,nHat) 
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
      END FUNCTION fMin 
!
!////////////////////////////////////////////////////////////////////////
!
      DOUBLE PRECISION FUNCTION distanceSquared(x,c,p,nHat)
         USE SMCurveClass
         IMPLICIT NONE
         DOUBLE PRECISION :: x, p(3), z(3), nHat(3)
         CLASS(SMCurve)   :: c
         
         z               = c % positionAt(x)
         distanceSquared = DistanceSquaredBetweenPoints(z,p,nHat)
      END FUNCTION distanceSquared
!
!////////////////////////////////////////////////////////////////////////
!
      DOUBLE PRECISION FUNCTION DistanceSquaredBetweenPoints(z,p,nHat)
         USE ProgramGlobals, ONLY: directionPenalty
         IMPLICIT NONE
         DOUBLE PRECISION ::  p(3), z(3), nHat(3)
         
         DistanceSquaredBetweenPoints = (z(1) - p(1))**2 + (z(2) - p(2))**2 &
                           - MIN((z(1)-p(1))*nHat(1) + (z(2) - p(2))*nHat(2),0.0d0)/directionPenalty
      END FUNCTION DistanceSquaredBetweenPoints
END MODULE fMinModule

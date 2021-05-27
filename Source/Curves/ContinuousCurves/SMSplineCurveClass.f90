!
!////////////////////////////////////////////////////////////////////////
!
!      SMSplineCurveClass.f90
!      Created: July 31, 2013 12:27 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module SMSplineCurveClass
      USE SMCurveClass
      USE SMConstants
      IMPLICIT NONE
!
!     ---------------------
!     Class type definition
!     ---------------------
!
      TYPE, EXTENDS(SMCurve) :: SMSplineCurve
         INTEGER                                  :: numKnots
         REAL(KIND=RP), DIMENSION(:), ALLOCATABLE :: bX,cX,dX
         REAL(KIND=RP), DIMENSION(:), ALLOCATABLE :: bY,cY,dY
         REAL(KIND=RP), DIMENSION(:), ALLOCATABLE :: bZ,cZ,dZ
         REAL(KIND=RP), DIMENSION(:), ALLOCATABLE :: t, x, y, z
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithPointsNameAndID
         FINAL     :: destructSplineCurve
         PROCEDURE :: positionAt => positionOnSplineCurveAt
      END TYPE SMSplineCurve
      
      PRIVATE :: spline, seval, swapOrder
!
!     ========
      CONTAINS
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initWithPointsNameAndID( self, t, x, y, z, curveName, id )  
         USE Geometry
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMSplineCurve)        :: self
         REAL(KIND=RP), DIMENSION(:) :: x, y, z, t 
         CHARACTER(LEN=*)            :: curveName
         INTEGER                     :: id
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER       :: N, nDim
         REAL(KIND=RP) :: xx(2,SIZE(x))
         INTEGER       :: circ, j
!
!        -----------------------------
!        Check orientation of the data
!        -----------------------------
!
         N       = SIZE(x)
         xx(1,:) = x
         xx(2,:) = y
         circ = Circulation(x = xx)
         IF(circ == CLOCKWISE)     THEN
            CALL swapOrder(x = x,N = N)
            CALL swapOrder(x = y,N = N)
            CALL swapOrder(x = z,N = N)
            CALL swapOrder(x = t,N = N)
            DO j = 1, N 
               t(j) = 1.0_RP - t(j)
            END DO 
         END IF

         CALL self % SMCurve % initWithNameAndID(curveName,id)
         
         nDim           = N
         self%numKnots  = N
         
         ALLOCATE( self%bX(N) )
         ALLOCATE( self%cX(N) )
         ALLOCATE( self%dX(N) )
         ALLOCATE( self%bY(N) )
         ALLOCATE( self%cY(N) )
         ALLOCATE( self%dY(N) )
         ALLOCATE( self%bZ(N) )
         ALLOCATE( self%cZ(N) )
         ALLOCATE( self%dZ(N) )
         ALLOCATE( self%t(N) )
         ALLOCATE( self%x(N) )
         ALLOCATE( self%y(N) )
         ALLOCATE( self%z(N) )
         
         self%t = t
         self%x = x
         self%y = y
         self%z = z
         
         CALL spline( N, nDim, t , x, self%bX, self%cX, self%dX )
         CALL spline( N, nDim, t , y, self%bY, self%cY, self%dY )
         CALL spline( N, nDim, t , z, self%bZ, self%cZ, self%dZ )
         
      END SUBROUTINE initWithPointsNameAndID
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DestructSplineCurve( self )
         IMPLICIT NONE 
         TYPE(SMSplineCurve)           :: self
         
         self%numKnots = 0

      END SUBROUTINE DestructSplineCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseSplineCurve(self)  
         IMPLICIT NONE
         TYPE (SMSplineCurve) , POINTER :: self
         CLASS(FTObject), POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseSplineCurve
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION positionOnSplineCurveAt( self, t ) RESULT(x)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS( SMSplineCurve )      :: self
         REAL(KIND=RP)               :: t
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP), DIMENSION(3) :: x
         INTEGER                     :: N, nDim
         
         N    = self%numKnots
         nDim = N
         
         x(1) = seval( nDim, N, t, self%t, self%x, self%bX, self%cX, self%dX )
         x(2) = seval( nDim, N, t, self%t, self%y, self%bY, self%cY, self%dY )
         x(3) = seval( nDim, N, t, self%t, self%z, self%bZ, self%cZ, self%dZ )
         
      END FUNCTION positionOnSplineCurveAt
!                                                                       
!///////////////////////////////////////////////////////////////////////
!                                                                       
   SUBROUTINE spline(ndim,n,x,y,b,c,d) 
!                                                                       
!     .. scalar arguments ..                                            
      integer n,ndim 
!     ..                                                                
!     .. array arguments ..                                             
      REAL(KIND=RP) :: b(ndim),c(ndim),d(ndim),x(ndim),y(ndim) 
!     ..                                                                
!     .. local scalars ..                                               
      REAL(KIND=RP) :: t 
      integer i,ib,nm1 
!     ..                                                                
      nm1 = n - 1 
!                                                                       
!      set up tri-diagonal system                                       
!                                                                       
      d(1) = x(2) - x(1) 
      c(2) = (y(2)-y(1))/d(1) 
      do 100 i = 2,nm1 
          d(i) = x(i+1) - x(i) 
          b(i) = 2* (d(i-1)+d(i)) 
          c(i+1) = (y(i+1)-y(i))/d(i) 
          c(i) = c(i+1) - c(i) 
  100 continue 
!                                                                       
!      end conditions                                                   
!                                                                       
      b(1) = -d(1) 
      b(n) = -d(n-1) 
      c(1) = c(3)/ (x(4)-x(2)) - c(2)/ (x(3)-x(1)) 
      c(n) = c(n-1)/ (x(n)-x(n-2)) - c(n-2)/ (x(n-1)-x(n-3)) 
      c(1) = c(1)*d(1)**2/ (x(4)-x(1)) 
      c(n) = -c(n)*d(n-1)**2/ (x(n)-x(n-3)) 
!                                                                       
!     forward elimination                                               
!                                                                       
      do 200 i = 2,n 
          t = d(i-1)/b(i-1) 
          b(i) = b(i) - t*d(i-1) 
          c(i) = c(i) - t*c(i-1) 
  200 continue 
!                                                                       
!     back substitution                                                 
!                                                                       
      c(n) = c(n)/b(n) 
      do 300 ib = 1,nm1 
          i = n - ib 
          c(i) = (c(i)-d(i)*c(i+1))/b(i) 
  300 continue 
!                                                                       
!     compute polynomial coefficients                                   
!                                                                       
      b(n) = (y(n)-y(nm1))/d(nm1) + d(nm1)* (c(nm1)+2.d0*c(n)) 
      do 400 i = 1,nm1 
          b(i) = (y(i+1)-y(i))/d(i) - d(i)* (c(i+1)+2.d0*c(i)) 
          d(i) = (c(i+1)-c(i))/d(i) 
          c(i) = 3.d0*c(i) 
  400 continue 
      c(n) = 3.d0*c(n) 
      d(n) = d(n-1) 
!                                                                       
      return 
                                                                        
   END SUBROUTINE spline                                          
!                                                                       
!///////////////////////////////////////////////////////////////////////
!                                                                       
   REAL(KIND=RP) FUNCTION seval(ndim,n,u,x,y,b,c,d) 
!                                                                       
!     .. scalar arguments ..                                            
      REAL(KIND=RP) :: u 
      integer n,ndim 
!     ..                                                                
!     .. array arguments ..                                             
      REAL(KIND=RP) :: b(ndim),c(ndim),d(ndim),x(ndim),y(ndim) 
!     ..                                                                
!     .. local scalars ..                                               
      REAL(KIND=RP) :: dx 
      integer i,j,k 
!     ..                                                                
!     .. save statement ..                                              
      save i 
!     ..                                                                
!     .. data statements ..                                             
      data i/1/ 
!     ..                                                                
!                                                                       
      if (i.ge.n) i = 1 
      if (u.lt.x(i)) go to 100 
      if (u.le.x(i+1)) go to 300 
  100 continue 
      i = 1 
      j = n + 1 
  200 continue 
      k = (i+j)/2 
      if (u.lt.x(k)) j = k 
      if (u.ge.x(k)) i = k 
      if (j.gt.i+1) go to 200 
!                                                                       
!     evaluate spline                                                   
!                                                                       
  300 continue 
      dx = u - x(i) 
      seval = y(i) + dx* (b(i)+dx* (c(i)+dx*d(i))) 
!                                                                       
      return 
                                                                        
   END FUNCTION seval                                          
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE swapOrder(x,N)  
         IMPLICIT NONE
         INTEGER       :: N
         REAL(KIND=RP) :: x(N)
         INTEGER       :: j
         REAL(KIND=RP) :: tmp
         
         DO j = 1, N/2
            tmp      =  x(N-j+1)
            x(N-j+1) = x(j)
            x(j)     = tmp
         END DO 
         
      END SUBROUTINE swapOrder
      END Module SMSplineCurveClass

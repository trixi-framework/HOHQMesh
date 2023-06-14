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
! HOHQMesh contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend, 
!    https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
! * `fmin`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
! * `spline`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
! * `seval`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
!
! --- End License
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
!     ---------
!     Constants
!     ---------
!
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), PARAMETER :: SPLINE_FILE_KEY = 'file'
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
         PROCEDURE :: initWithDataFile => initWithDataFile_SMSplineCurve
         PROCEDURE :: initWithPointsNameAndID
         FINAL     :: destructSplineCurve
         PROCEDURE :: positionAt => positionOnSplineCurveAt
         PROCEDURE :: className  => SplineClassName
      END TYPE SMSplineCurve
      
      PRIVATE :: ComputeSplineCoefs, SplineEval, swapOrder
!
!     ========
      CONTAINS
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initWithDataFile_SMSplineCurve( self, datafile, curveName, id )
         USE SharedExceptionManagerModule
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMSplineCurve)        :: self
         CHARACTER(LEN=*)            :: datafile
         CHARACTER(LEN=*)            :: curveName
         INTEGER                     :: id
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, EXTERNAL                        :: UnusedUnit
         REAL(KIND=RP), DIMENSION(:), ALLOCATABLE :: x, y, z, t 
         INTEGER                                  :: N
         INTEGER                                  :: iUnit
         INTEGER                                  :: i
         TYPE(FTException), POINTER               :: exception

         iUnit = UnusedUnit()
         OPEN( UNIT = iUnit, FILE = dataFile )

  
         READ(iUnit,*)N
         IF ( N < 4 )     THEN
            ALLOCATE(exception)
            CALL exception % initFatalException(msg = "A spline curve must have at least 4 points")
            CALL throw(exception)
            CALL releaseFTException(exception)
            RETURN
         END IF 

         ALLOCATE( x(1:N), y(1:N), z(1:N), t(1:N) )
         DO i = 1, N
            READ(iUnit,*) t(i), x(i), y(i), z(i)
         ENDDO

         CALL self % initWithPointsNameAndID( t, x, y, z, curveName, id)

         DEALLOCATE(x,y,z,t)


      END SUBROUTINE initWithDataFile_SMSplineCurve

      SUBROUTINE initWithPointsNameAndID( self, t, x, y, z, curveName, id )  
         USE SharedExceptionManagerModule
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
         INTEGER                     :: N, nDim
         REAL(KIND=RP)               :: xx(2,SIZE(x))
         INTEGER                     :: circ, j
         TYPE(FTException), POINTER  :: exception
!
!        ---------------
!        Check integrity
!        ---------------
!
         N       = SIZE(x)
         
         IF ( N < 4 )     THEN
            ALLOCATE(exception)
            CALL exception % initFatalException(msg = "A spline curve must have at least 4 points")
            CALL throw(exception)
            CALL releaseFTException(exception)
            RETURN
         END IF 
!
!        -----------------------------
!        Check orientation of the data
!        -----------------------------
!
        IF(AlmostEqual(x(1),x(N)) .AND. &
            AlmostEqual(y(1),y(N)))     THEN 
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
         
         CALL ComputeSplineCoefs( nDim, t , x, self%bX, self%cX, self%dX )
         CALL ComputeSplineCoefs( nDim, t , y, self%bY, self%cY, self%dY )
         CALL ComputeSplineCoefs( nDim, t , z, self%bZ, self%cZ, self%dZ )
        
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
!      -----------------------------------------------------------------
!> Class name returns a string with the name of the type of the object
!>
!>  ### Usage:
!>
!>        PRINT *,  obj % className()
!>        if( obj % className = "Spline")
!>
      FUNCTION SplineClassName(self)  RESULT(s)
         IMPLICIT NONE  
         CLASS(SMSplineCurve)                       :: self
         CHARACTER(LEN=CLASS_NAME_CHARACTER_LENGTH) :: s
         
         s = "Spline"
         IF( self % refCount() >= 0 ) CONTINUE 
 
      END FUNCTION SplineClassName
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castCurveToSplineCurve(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(SMCurve)      , POINTER :: obj
         CLASS(SMSplineCurve), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            CLASS IS(SMSplineCurve)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castCurveToSplineCurve
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
         
         x(1) = SplineEval( N, t, self%t, self%x, self%bX, self%cX, self%dX )
         x(2) = SplineEval( N, t, self%t, self%y, self%bY, self%cY, self%dY )
         x(3) = SplineEval( N, t, self%t, self%z, self%bZ, self%cZ, self%dZ )
         
      END FUNCTION positionOnSplineCurveAt
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ComputeSplineCoefs(n, x, y, b, c, d)
!
!     -------------------------------------------------------------------
!     For derivations, see COMPUTER METHODS FOR MATHEMATICAL COMPUTATIONS
!     by Forysyth, Malcom & Molder, Prentice-Hall 1977.
!     -------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER, INTENT(IN)        :: n
         REAL(KIND=RP), INTENT(IN)  :: x(n), y(n)
         REAL(KIND=RP), INTENT(OUT) :: b(n), c(n), d(n)
!
!        ---------------
!        Local Variables
!        ---------------
!
         REAL(KIND=RP) :: h(n), alpha(n), beta(n)
         INTEGER       :: i
         
         IF ( n < 4 )     THEN
            b = 0.0_RP
            c = 0.0_RP
            d = 0.0_RP
            RETURN 
         END IF 
!
!        --------------------
!        Tri-diagonal system
!        Re-use storage as:
!        alpha/Diag
!        Delta/rhs = beta
!        See FMM page 74
!        --------------------
!
         h(1)    =  x(2) - x(1)
         beta(2) = (y(2) - y(1))/h(1)
         
         DO i = 2, n-1 
            h(i)      = x(i+1) - x(i)
            alpha(i)  = 2.0_RP*(h(i-1) + h(i))
            beta(i+1) = (y(i+1) - y(i))/h(i)
            beta(i)   = beta(i+1) - beta(i)
         END DO
!
!        --------------------------------
!        End point conditions: FMM pg. 73
!        --------------------------------
!
         alpha(1) = -h(1)
         alpha(n) = -h(n-1)
         beta(1)  =  beta(3)/(x(4) - x(2)) - beta(2)/(x(3) - x(1))
         beta(n)  =  beta(n-1)/(x(n) - x(n-2)) - beta(n-2)/(x(n-1) - x(n-3))
         beta(1)  =  beta(1)*h(1)**2/(x(4) - x(1))
         beta(n)  = -beta(n)*h(n-1)**2/(x(n) - x(n-3))
!
!        ------------------
!        Solver: FMM pg. 75
!        ------------------
!
         DO i = 2, n 
            alpha(i) = alpha(i) - h(i-1)**2/alpha(i-1)
            beta(i)  = beta(i)  - h(i-1)*beta(i-1)/alpha(i-1)
         END DO 
         
         beta(n) = beta(n)/alpha(n)
         DO i = n-1,1,-1 
            beta(i) = (beta(i) - h(i)*beta(i+1))/alpha(i) 
         END DO
!
!        ------------------------------
!        Final coefficients: FMM pg 76.
!        ------------------------------
!
         b(n) = (y(n) - y(n-1))/h(n-1) + h(n-1)*(beta(n-1) + 2.0_RP*beta(n))
         DO i = 1, n-1 
            b(i) = (y(i+1) - y(i))/h(i) - h(i)* (beta(i+1) + 2.0_RP*beta(i))
            c(i) = 3.0_RP*beta(i)
            d(i) = (beta(i+1) - beta(i))/h(i)
         END DO 
         c(n) = 3.0_RP*beta(n)
         d(n) = d(n-1)
         
      END SUBROUTINE ComputeSplineCoefs
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION SplineEval(n, xLoc, x, y, b, c, d) RESULT(s)
!
!     -------------------------------------------------------------------
!     For derivations, see COMPUTER METHODS FOR MATHEMATICAL COMPUTATIONS
!     by Forysyth, Malcom & Molder, Prentice-Hall 1977.
!     -------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER       :: n
         REAL(KIND=RP) :: b(n),c(n),d(n),x(n),y(n)
         REAL(KIND=RP) :: xLoc, s
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER, SAVE  :: left = 1
         INTEGER        :: right, mid, k
         REAL (KIND=RP) :: dx
!
!        -------------------------------------
!        Binary search for the spline interval
!        -------------------------------------
!
         IF (left >= n) left = 1
         IF ( xLoc < x(left) .OR. xloc > x(left+1)  )     THEN 
            right = n+1
            left  = 1
            DO k = 1, n 
               mid = (right + left)/2 
               IF ( xLoc < x(mid) )     THEN
                  right = mid 
               ELSE 
                  left = mid 
               END IF 
               IF (right-left == 1) EXIT  
            END DO 
         END IF 
!
!        --------------------------------------------
!        Evaluation of the spline using Horner's rule
!        FMM pg. 75.
!        --------------------------------------------
!
         dx = xLoc - x(left) 
         s  = y(left) + dx*(b(left)+dx*(c(left) + dx*d(left))) 

      END FUNCTION SplineEval
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

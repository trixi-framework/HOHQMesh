!
!////////////////////////////////////////////////////////////////////////
!
!      BiCubicClass.f90
!      Created: March 26, 2022 at 9:43 AM 
!      By: David Kopriva  
!      From: SMTopographyFromFileClass by Andrew Winters
!
!////////////////////////////////////////////////////////////////////////
!
   Module BiCubicClass 
      USE FTObjectClass
      USE SMConstants
      IMPLICIT NONE  
!
!     ----------------
!     Class definition
!     ----------------
!
      TYPE, EXTENDS(FTObject) :: BiCubicInterpolation
         INTEGER                                    :: nx
         INTEGER                                    :: ny
         REAL(KIND=RP), DIMENSION(:)  , ALLOCATABLE :: x_values
         REAL(KIND=RP), DIMENSION(:)  , ALLOCATABLE :: y_values
         REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE :: z_values
         REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE :: dzdx
         REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE :: dzdy
         REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE :: d2zdxy
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initBiCubicInterpolation
         FINAL     :: destructBiCubicInterpolation
         PROCEDURE :: valueAt => BiCubicInterpolationValue
      END TYPE BiCubicInterpolation
      
      PRIVATE :: TestFunction
!
!     ----------------------------------------------------------------------------
!     Inverse matrix necessary to find the 16 coefficients needed
!     for the bicubic interpolation routine. Details on this matrix can
!     found here https://www.giassa.net/?page_id=371. Or, in the words of
!     Numerical Recipes in Fortran 77, the formulas for the bicubic coefficents
!     "are just a complicated linear transformation [which] having been determined
!      once in the mists of numerical history, can be tabulated and forgotten."
!     This is one implementation of such a tabulation using compressed column
!     storage (CCS) strategy because the matrix is sparse.
!     ----------------------------------------------------------------------------
!
      REAL(KIND=RP), PARAMETER, DIMENSION(100), PRIVATE :: values_weight_Matrix = (/ &
             1.0_RP, -3.0_RP,  2.0_RP, -3.0_RP,  9.0_RP, -6.0_RP,  2.0_RP, -6.0_RP,  4.0_RP,  3.0_RP, &
            -9.0_RP,  6.0_RP, -2.0_RP,  6.0_RP, -4.0_RP,  9.0_RP, -6.0_RP, -6.0_RP,  4.0_RP,  3.0_RP, &
            -2.0_RP, -9.0_RP,  6.0_RP,  6.0_RP, -4.0_RP,  1.0_RP, -3.0_RP,  2.0_RP, -2.0_RP,  6.0_RP, &
            -4.0_RP,  1.0_RP, -3.0_RP,  2.0_RP, -1.0_RP,  3.0_RP, -2.0_RP,  1.0_RP, -3.0_RP,  2.0_RP, &
            -3.0_RP,  2.0_RP,  3.0_RP, -2.0_RP,  3.0_RP, -2.0_RP, -6.0_RP,  4.0_RP,  3.0_RP, -2.0_RP, &
             1.0_RP, -2.0_RP,  1.0_RP, -3.0_RP,  6.0_RP, -3.0_RP,  2.0_RP, -4.0_RP,  2.0_RP,  3.0_RP, &
            -6.0_RP,  3.0_RP, -2.0_RP,  4.0_RP, -2.0_RP, -3.0_RP,  3.0_RP,  2.0_RP, -2.0_RP, -1.0_RP, &
             1.0_RP,  3.0_RP, -3.0_RP, -2.0_RP,  2.0_RP,  1.0_RP, -2.0_RP,  1.0_RP, -2.0_RP,  4.0_RP, &
            -2.0_RP,  1.0_RP, -2.0_RP,  1.0_RP, -1.0_RP,  2.0_RP, -1.0_RP,  1.0_RP, -2.0_RP,  1.0_RP, &
             1.0_RP, -1.0_RP, -1.0_RP,  1.0_RP, -1.0_RP,  1.0_RP,  2.0_RP, -2.0_RP, -1.0_RP,  1.0_RP /)

      INTEGER, PARAMETER, DIMENSION(100), PRIVATE :: row_index_weight_Matrix = (/ &
             1, 3 , 4 , 9 , 11, 12, 13, 15, 16, 9 , 11, 12, 13, 15, 16, 11, 12, &
            15, 16, 3 , 4 , 11, 12, 15, 16, 5 , 7 , 8 , 9 , 11, 12, 13, 15, 16, &
             9, 11, 12, 13, 15, 16, 11, 12, 15, 16, 7 , 8 , 11, 12, 15, 16, 2 , &
             3, 4 , 10, 11, 12, 14, 15, 16, 10, 11, 12, 14, 15, 16, 11, 12, 15, &
            16, 3 ,  4, 11, 12, 15, 16, 6 , 7 , 8 , 10, 11, 12, 14, 15, 16, 10, &
            11, 12, 14, 15, 16, 11, 12, 15, 16, 7 , 8 , 11, 12, 15, 16 /)

      INTEGER, PARAMETER, DIMENSION(17), PRIVATE :: column_pointers_weight_Matrix = (/ &
             1, 10, 16, 20, 26, 35, 41, 45, 51, 60, 66, 70, 76, 85, 91, 95, 101 /)
!  ========
   CONTAINS
!  ========
   
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initBiCubicInterpolation(self, Nx, Ny, x, y, z)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(BiCubicInterpolation) :: self
         INTEGER                    :: Nx, Ny
         REAL(KIND=RP)              :: x(Nx), y(Ny), z(Nx,Ny)
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER       :: k, j
         REAL(KIND=RP) :: inv_dx, inv_dy
!
         CALL self % FTObject % init()
         
         self % nx = Nx; self % ny = Ny
         
         ALLOCATE(self % x_values(1:nx), self % y_values(1:ny))
         ALLOCATE(self % z_values(1:nx, 1:ny))
         ALLOCATE(self % dzdx(1:nx, 1:ny), self % dzdy(1:nx, 1:ny))
         ALLOCATE(self % d2zdxy(1:nx, 1:ny))
         
         self % x_values = x
         self % y_values = y
         self % z_values = z
         self % dzdx     = 0.0_RP
         self % dzdy     = 0.0_RP
         self % d2zdxy   = 0.0_RP
!
!        ---------------------------------------------------------------
!        Create central derivative approximations for z_x, z_y, and z_xy
!        Note for convenience the edge cases are kept to be 0.0_RP
!        ---------------------------------------------------------------
!
         DO k = 2,ny-1
            inv_dy = 1.0_RP / ( self % y_values(k+1) - self % y_values(k-1) )
            DO j = 2,nx-1
               inv_dx = 1.0_RP / ( self % x_values(j+1) - self % x_values(j-1) )
               self % dzdx(j,k)   = inv_dx * ( self % z_values(j+1, k  ) - self%z_values(j-1, k  ) )
               self % dzdy(j,k)   = inv_dy * ( self % z_values(j  , k+1) - self%z_values(j  , k-1) )
               self % d2zdxy(j,k) = inv_dx * inv_dy * (  self % z_values(j+1, k+1) + self % z_values(j-1, k-1) &
                                                       - self % z_values(j-1, k+1) - self % z_values(j+1, k-1))
            END DO ! j
         END DO ! k
         
      END SUBROUTINE initBiCubicInterpolation
 !
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE destructBiCubicInterpolation(self)
         IMPLICIT NONE
         TYPE(BiCubicInterpolation) :: self

         self % nx = -1
         self % ny = -1
         DEALLOCATE( self % x_values, self % y_values, self % z_values)
         DEALLOCATE( self % dzdx, self % dzdy, self % d2zdxy)

      END SUBROUTINE destructBiCubicInterpolation
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE releaseBiCubicInterpolation(self)
         IMPLICIT NONE
         TYPE (BiCubicInterpolation), POINTER :: self
         CLASS(FTObject)            , POINTER :: obj

         IF(.NOT. ASSOCIATED(self)) RETURN

         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL()
         END IF
         
      END SUBROUTINE releaseBiCubicInterpolation
 !
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION BiCubicInterpolationValue(self, t)  RESULT(x)
         IMPLICIT NONE
         CLASS(BiCubicInterpolation) :: self
         REAL(KIND=RP)               :: t(2)
         REAL(KIND=RP)               :: x

         x = evaluateBicubicInterpolant(self, t)

      END FUNCTION BiCubicInterpolationValue
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION evaluateBicubicInterpolant(self, t)  RESULT(z_val)
         ! The form of this bicubic interpolation was mainly taken from the blog post
         ! https://www.giassa.net/?page_id=371 that has a very nice explanation of where
         ! the different terms come from.
         IMPLICIT NONE
         CLASS(BiCubicInterpolation) :: self
         REAL(KIND=RP)               :: t(2)
         ! local variables
         INTEGER       :: i, j, k
         REAL(KIND=RP) :: u, v, z_corners(4), dz_dx(4), dz_dy(4), dz_dxy(4)
         REAL(KIND=RP) :: dx, dy, temp
         REAL(KIND=RP), DIMENSION(16) :: work_vec, coeff_vec
         REAL(KIND=RP), DIMENSION(4,4) :: coeffs

         ! Get the indexing of the current test point
         j = getLeftIndex( self % x_values, t(1), self % nx )
         k = getLeftIndex( self % y_values, t(2), self % ny )

         ! Get the grid sizes
         dx = self % x_values(j+1) - self % x_values(j)
         dy = self % y_values(k+1) - self % y_values(k)

         ! Pull the corner and gradient information in the correct orientation
         z_corners = pullFourCorners( self % z_values, j, k, self % nx, self % ny )
         dz_dx  = pullFourCorners( self % dzdx  , j, k, self % nx, self % ny )
         dz_dy  = pullFourCorners( self % dzdy  , j, k, self % nx, self % ny )
         dz_dxy = pullFourCorners( self % d2zdxy, j, k, self % nx, self % ny )
!
!        --------------------------------
!        Compute the bicubic coefficients
!        --------------------------------
!
         ! Fill a work vector
         work_vec(1:4)   = z_corners(:)
         work_vec(5:8)   = dz_dx(:) * dx
         work_vec(9:12)  = dz_dy(:) * dy
         work_vec(13:16) = dz_dxy(:) * dx * dy

         ! Sparse matrix multiplication with the known inverse weighting matrix
         ! pre-computed and stored above
         coeff_vec = SparseCCS_MxV( work_vec )

         ! Store resulting coefficents in a form better for Horner's rule
         coeffs = RESHAPE( coeff_vec, (/4, 4/), ORDER = (/2, 1/) )
!
!        -------------------------------------------------------------------
!        Compute the bicubic interpolation with the auxiliary variables u, v
!        -------------------------------------------------------------------
!
         z_val = 0.0_RP
         v = ( t(1) - self % x_values(j) ) / dx
         u = ( t(2) - self % y_values(k) ) / dy
         DO i = 4, 1, -1
            z_val = z_val * v + ( ( coeffs(i,4) * u + coeffs(i,3) ) * u + coeffs(i,2) ) * u + coeffs(i,1)
         END DO ! i

      END FUNCTION evaluateBicubicInterpolant
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION SparseCCS_MxV(x) RESULT(y)
      ! Compressed column storage (CCS) version of matrix vector product y = Ax
      ! The CCS version of this matrix is precomputed and stored above in the
      ! variables 'values_weight_Matrix', 'row_index_weight_Matrix', and
      ! 'column_pointers_weight_Matrix'.
      IMPLICIT NONE
      REAL(KIND=RP) :: x(16)
      REAL(KIND=RP) :: y(16)
      ! local variables
      INTEGER :: j, k1, k2

      y = 0.0_RP
      DO j = 1,16
         k1 = column_pointers_weight_Matrix(j)
         k2 = column_pointers_weight_Matrix(j + 1) - 1
         y(row_index_weight_Matrix(k1:k2)) = y(row_index_weight_Matrix(k1:k2)) &
                                             + values_weight_Matrix(k1:k2) * x(j)
      END DO ! j

      END FUNCTION SparseCCS_MxV
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION getLeftIndex(values_list, test_point, N) RESULT(idx)
         ! Naive loop through a set of data in values_list to determine the left index
         ! of the interval that contains test_point
         ! ACHTUNG! Assumes that the data in values_list is ordered
         IMPLICIT NONE
         INTEGER                    , INTENT(IN) :: N
         REAL(KIND=RP)              , INTENT(IN) :: test_point
         REAL(KIND=RP), DIMENSION(N), INTENT(IN) :: values_list
         ! local variables
         INTEGER :: j

         DO j = 1,N-1
            IF ( ( test_point .GE. values_list(j) ) .AND. ( test_point .LE. values_list(j+1) ) ) THEN
               idx = j
               EXIT
            END IF
         END DO ! j
      END FUNCTION getLeftIndex
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION pullFourCorners(z_array, x_idx, y_idx, nx, ny)  RESULT(corns)
         ! Extract four corner values (in anti-clockwise orientation)
         ! ACHTUNG 1! Assumes ordered grid data is provided by the file
         ! ACHTUNG 2! Assumes the same number of points in each direction for the z values array
         IMPLICIT NONE
         INTEGER                         , INTENT(IN) :: nx, ny, x_idx, y_idx
         REAL(KIND=RP), DIMENSION(nx, ny), INTENT(IN) :: z_array
         REAL(KIND=RP)                                :: corns(4)

         corns(1) = z_array( x_idx     , y_idx     )
         corns(2) = z_array( x_idx + 1 , y_idx     )
         corns(3) = z_array( x_idx + 1 , y_idx + 1 )
         corns(4) = z_array( x_idx     , y_idx + 1 )

      END FUNCTION pullFourCorners
!
!//////////////////////////////////////////////////////////////////////// 
!
!     TESTING FUNCTIONS FOLLOW
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ConstructTestInterpolant(self, Nx, Ny)
!
!     --------------------------------------------------------------------
!     Construct an interpolant using the stest function at Nx,Ny uniformly
!     spaced points. Returns an interpolant with reference count 1.
!     --------------------------------------------------------------------
!      
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                    :: Nx, Ny
         TYPE(BiCubicInterpolation) :: self
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER :: i, j
         REAL(KIND=RP) :: x(Nx), y(Ny), z(Nx,Ny)
         REAL(KIND=RP) :: dx, dy
         
         dx = 2.0_RP/(Nx-1)
         dy = 2.0_RP/(Ny-1)
         
         DO i = 1, Nx
            x(i) = -1.0_RP + dx*(i-1) 
         END DO 
         DO j = 1, Ny
            y(j) = -1.0_RP + dy*(j-1) 
         END DO 
         
         DO j = 1, Ny 
            DO i = 1, Nx 
               z(i,j) = TestFunction(x(i),y(j)) 
            END DO 
         END DO 
         
         CALL initBiCubicInterpolation(self,Nx,Ny,x,y,z)
                     
      END SUBROUTINE ConstructTestInterpolant
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION interpolantIsWithinTolerance(self, x, y, absTol)
!
!     -----------------------------------------------------------------------
!     Returns .true. if the interpolant is correctly evaluated to within the 
!     given absolute tolerance, .false. otherwise.
!     -----------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(BiCubicInterpolation) :: self
         REAL(KIND=RP)              :: x, y, absTol
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP) :: zInterp, zExact, eror
         
         zInterp = evaluateBicubicInterpolant(self,t = [x,y])
         zExact  = TestFunction(x,y)
         eror = ABS(zInterp - zExact)
         
         IF ( eror <= absTol )     THEN
            interpolantIsWithinTolerance = .TRUE. 
         ELSE 
            interpolantIsWithinTolerance = .FALSE. 
         END IF 
         
      END FUNCTION interpolantIsWithinTolerance
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION TestFunction(x,y)  RESULT(z)
!
!     --------------------------------------------------------------
!     A parabolic function for which the interpolant should be exact
!     In this class, the first derivatives are approximated by 
!     second order finite differences, so only a quadratic is exact.
!     --------------------------------------------------------------
!
         IMPLICIT NONE  
         REAL(KIND=RP) :: x, y, z
         
         z = x**2 + y**2
      END FUNCTION TestFunction
 
   END Module BiCubicClass
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE TestBiCubicInterpolation  
         USE BiCubicClass
         USE TestSuiteManagerClass
         USE FTAssertions
         USE SharedExceptionManagerModule
         IMPLICIT NONE  
         
         TYPE(BiCubicInterpolation) :: bQ
         REAL(KIND=RP)              :: z, x, y
         INTEGER                    :: k
         LOGICAL                    :: tst
         
         CALL ConstructTestInterpolant(self = bQ, Nx = 10,Ny = 10)
      
         DO k = 1, 6 
           CALL RANDOM_NUMBER(HARVEST = x) 
           CALL RANDOM_NUMBER(HARVEST = y)
           x = 1.5_RP*x - 0.75_RP
           y = 1.5_RP*y - 0.75_RP
           tst =  interpolantIsWithinTolerance(bQ, x ,y , absTol = 1.0d-4)
           CALL FTAssert(test = tst,msg = "BiCubic Interpolation is not within tolerance")
           IF ( .NOT.tst )     THEN
               PRINT *, x, y, tst 
            END IF 
          END DO 

      END SUBROUTINE TestBiCubicInterpolation

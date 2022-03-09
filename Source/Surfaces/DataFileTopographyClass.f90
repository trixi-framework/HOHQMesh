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
!      DataFileTopographyClass.f90
!      Created: November 09, 2021 4:02 PM
!      By: Andrew Winters
!
!      Computes the values of z(x,y) using a bicubic interpolation from data
!      provided in a file.
!
!////////////////////////////////////////////////////////////////////////
!
      Module SMTopographyFromFileClass

      USE SMTopographyClass
      USE SMConstants
      USE SharedExceptionManagerModule
      USE ProgramGlobals, ONLY: STRING_CONSTANT_LENGTH
      USE EquationEvaluatorClass, ONLY: ERROR_MESSAGE_LENGTH

      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: TOPOGRAPHY_FROM_FILE_KEY = "data file"
!
!     ----------------------
!     Class type definition
!     ----------------------
!
      TYPE, EXTENDS(SMTopography) :: SMTopographyFromFile
         CHARACTER(LEN=STRING_CONSTANT_LENGTH)      :: file_name
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
         PROCEDURE :: initWithDataFile
         FINAL     :: destructDFTopography
         PROCEDURE :: heightAt => positionOnDFTopographyAt
         PROCEDURE :: printDescription => printDFDescription
      END TYPE SMTopographyFromFile
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
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initWithDataFile(self, topographyFile)
         USE ErrorTypesModule
         IMPLICIT NONE
         CLASS(SMTopographyFromFile) :: self
         CHARACTER(LEN=*)            :: topographyFile

         INTEGER :: nx, ny, j, k, file_unit
         REAL(KIND=RP) :: inv_dx, inv_dy
         CHARACTER(LEN=ERROR_MESSAGE_LENGTH) :: msg
         LOGICAL :: a_data_file

         ! Check to make sure the input topographyFile is valid
         INQUIRE( FILE=topographyFile, EXIST=a_data_file )
         IF (.NOT.a_data_file) THEN
            WRITE(msg, *)"Unable to open the topography data file "//TRIM(ADJUSTL(topographyFile))
            CALL ThrowErrorExceptionOfType(poster = "DataFileTopography/initWithDataFile", &
                                           msg    = msg, &
                                           typ    = FT_ERROR_FATAL)
            RETURN
         END IF

         CALL self % SMTopography % initTopography()

         ! Save the data source
         self % file_name = topographyFile

         ! Open the file and make sure everything is okay
         OPEN(NEWUNIT=file_unit, FILE=topographyFile)

         ! Read the number of points available in the bottom topography data
         READ(file_unit, *) ! eat the header
         READ(file_unit, *) nx, ny
         self % nx = nx
         self % ny = ny

         ! Allocate the memory and zero it out
         ALLOCATE(self % x_values(1:nx), self % y_values(1:ny))
         ALLOCATE(self % z_values(1:nx, 1:ny))
         ALLOCATE(self % dzdx(1:nx, 1:ny), self % dzdy(1:nx, 1:ny))
         ALLOCATE(self % d2zdxy(1:nx, 1:ny))
         self % x_values = 0.0_RP
         self % y_values = 0.0_RP
         self % z_values = 0.0_RP
         self % dzdx     = 0.0_RP
         self % dzdy     = 0.0_RP
         self % d2zdxy   = 0.0_RP

         ! Read the data into appropriate storage arrays
         READ(file_unit, *) ! skip the header for x_values list
         DO j = 1,nx
            READ(file_unit, *) self % x_values(j)
         END DO ! j

         READ(file_unit, *) ! skip the header for y_values list
         DO k = 1,ny
            READ(file_unit, *) self % y_values(k)
         END DO ! k

         READ(file_unit, *) ! skip the header for z_nodes list
         DO k = 1,ny
            DO j = 1,nx
               READ(file_unit, *) self % z_values(j, k)
            END DO ! j
         END DO ! k

         ! Close the file
         CLOSE(file_unit)

         ! Create central derivative approximations for z_x, z_y, and z_xy
         ! Note for convenience the edge cases are kept to be 0.0_RP
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

      END SUBROUTINE initWithDataFile
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE destructDFTopography(self)
         IMPLICIT NONE
         TYPE(SMTopographyFromFile) :: self

         self % file_name = "nothing"
         self % nx = -1
         self % ny = -1
         DEALLOCATE( self % x_values, self % y_values, self % z_values)
         DEALLOCATE( self % dzdx, self % dzdy, self % d2zdxy)

      END SUBROUTINE destructDFTopography
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE releaseDFTopography(self)
         IMPLICIT NONE
         TYPE (SMTopographyFromFile), POINTER :: self
         CLASS(FTObject)            , POINTER :: obj

         IF(.NOT. ASSOCIATED(self)) RETURN

         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL()
         END IF
      END SUBROUTINE releaseDFTopography
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION positionOnDFTopographyAt(self, t)  RESULT(x)
         IMPLICIT NONE
         CLASS(SMTopographyFromFile) :: self
         REAL(KIND=RP)               :: t(2)
         REAL(KIND=RP)               :: x(3)

          x(1) = t(1)
          x(2) = t(2)
          x(3) = evaluateBicubicInterpolant(self, t)

      END FUNCTION positionOnDFTopographyAt
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION evaluateBicubicInterpolant(self, t)  RESULT(z_val)
         ! The form of this bicubic interpolation was mainly taken from the blog post
         ! https://www.giassa.net/?page_id=371 that has a very nice explanation of where
         ! the different terms come from.
         IMPLICIT NONE
         CLASS(SMTopographyFromFile) :: self
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
!        --------------------------------------------------------------------
!        Compute the bi-cubic interpolation with the auxiliary variables u, v
!        --------------------------------------------------------------------
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
      SUBROUTINE printDFDescription(self, iUnit)
        IMPLICIT NONE
        CLASS(SMTopographyFromFile) :: self
        INTEGER                     :: iUnit

        WRITE(iUnit,*) TRIM(self % file_name)
      END SUBROUTINE printDFDescription

      END Module SMTopographyFromFileClass

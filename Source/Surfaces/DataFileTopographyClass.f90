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
!      TODO: Remove legacy bilinear interpolation. Only kept currently for testing.
!
!////////////////////////////////////////////////////////////////////////
!
      Module SMDataFileTopographyClass

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
      TYPE, EXTENDS(SMTopography) :: SMDataFileTopography
         CHARACTER(LEN=STRING_CONSTANT_LENGTH)      :: file_name
         INTEGER                                    :: nnodes
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
      END TYPE SMDataFileTopography
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
         CLASS(SMDataFileTopography) :: self
         CHARACTER(LEN=*)            :: topographyFile

         INTEGER :: nnodes, j, k, file_check
         REAL(KIND=RP) :: inv_dx, inv_dy
         CHARACTER(LEN=ERROR_MESSAGE_LENGTH) :: msg
         LOGICAL :: a_data_file

         ! Check to make sure the input topographyFile is valid
         INQUIRE( FILE=topographyFile, EXIST=a_data_file )
         IF (.NOT.a_data_file) THEN
            WRITE(msg, *)"Unable to open the topography data file "//TRIM(ADJUSTL(topographyFile))
            CALL ThrowErrorExceptionOfType("initWithDataFile", msg, FT_ERROR_FATAL)
            RETURN
         END IF

         CALL self % SMTopography % initTopography()

         ! Save the data source
         self % file_name = topographyFile

         ! Open the file and make sure everything is okay
         ! TODO: How to pick the UNIT? I always use 24601 because I know it is free, but that is a bit hacky
         OPEN(UNIT=24601, FILE=topographyFile, IOSTAT=file_check)

         ! Read the number of points available in the bottom topography data
         READ(24601, *) ! eat the header
         READ(24601, *) nnodes
         self % nnodes = nnodes

         ! Allocate the memory and zero it out
         ALLOCATE(self % x_values(1:nnodes), self % y_values(1:nnodes))
         ALLOCATE(self % z_values(1:nnodes, 1:nnodes))
         ALLOCATE(self % dzdx(1:nnodes, 1:nnodes), self % dzdy(1:nnodes, 1:nnodes))
         ALLOCATE(self % d2zdxy(1:nnodes, 1:nnodes))
         self % x_values = 0.0_RP
         self % y_values = 0.0_RP
         self % z_values = 0.0_RP
         self % dzdx     = 0.0_RP
         self % dzdy     = 0.0_RP
         self % d2zdxy   = 0.0_RP

         ! Read the data into appropriate storage arrays
         READ(24601, *) ! skip the header for x_values list
         DO j = 1,nnodes
            READ(24601, *) self % x_values(j)
         END DO ! j

         READ(24601, *) ! skip the header for y_values list
         DO k = 1,nnodes
            READ(24601, *) self % y_values(k)
         END DO ! k

         READ(24601, *) ! skip the header for z_nodes list
         DO k = 1,nnodes
            DO j = 1,nnodes
               READ(24601, *) self % z_values(j, k)
            END DO ! j
         END DO ! k

         ! Close the file
         CLOSE(24601)

         ! Create central derivative approximations for z_x, z_y, and z_xy
         ! Note for convenience the edge cases are kept to be 0.0_RP
         DO k = 2,nnodes-1
            inv_dy = 1.0_RP / ( self % y_values(k+1) - self % y_values(k-1) )
            DO j = 2,nnodes-1
               inv_dx = 1.0_RP / ( self % x_values(j+1) - self % x_values(j-1) )
               self % dzdx(j,k)   = inv_dx * ( self % z_values(j+1, k  ) - self%z_values(j-1, k  ) )
               self % dzdy(j,k)   = inv_dy * ( self % z_values(j  , k+1) - self%z_values(j  , k-1) )
               self % d2zdxy(j,k) = inv_dx * inv_dy * (  self % z_values(j+1, k+1) + self % z_values(j-1, k-1) &
                                                       - self % z_values(j-1, k+1) - self % z_values(j+1, k-1))
            END DO ! j
         END DO ! k

         RETURN
       END SUBROUTINE initWithDataFile
!
!////////////////////////////////////////////////////////////////////////
!
       SUBROUTINE destructDFTopography(self)
         IMPLICIT NONE
         TYPE(SMDataFileTopography) :: self

         self % file_name = "nothing"
         self % nnodes = -1
         DEALLOCATE( self % x_values, self % y_values, self % z_values)
         DEALLOCATE( self % dzdx, self % dzdy, self % d2zdxy)

       END SUBROUTINE destructDFTopography
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE releaseDFTopography(self)
         IMPLICIT NONE
         TYPE (SMDataFileTopography), POINTER :: self
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
      FUNCTION pullFourCorners(z_array, x_idx, y_idx, N)  RESULT(corns)
         ! Extract four corner values (in anti-clockwise orientation)
         ! ACHTUNG 1! Assumes ordered grid data is provided by the file
         ! ACHTUNG 2! Assumes the same number of points in each direction for the z values array
         IMPLICIT NONE
         INTEGER                       , INTENT(IN) :: N, x_idx, y_idx
         REAL(KIND=RP), DIMENSION(N, N), INTENT(IN) :: z_array
         REAL(KIND=RP)                              :: corns(4)

         corns(1) = z_array( x_idx     , y_idx     )
         corns(2) = z_array( x_idx + 1 , y_idx     )
         corns(3) = z_array( x_idx + 1 , y_idx + 1 )
         corns(4) = z_array( x_idx     , y_idx + 1 )

      END FUNCTION pullFourCorners
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION evaluateBilinearInterpolant(self, t)  RESULT(z_val)
         IMPLICIT NONE
         CLASS(SMDataFileTopography) :: self
         REAL(KIND=RP)               :: t(2)
         ! local variables
         INTEGER       :: j, k
         REAL(KIND=RP) :: u, v, z_corners(4)

         ! Get the indexing of the current test point
         j = getLeftIndex( self % x_values, t(1), self % nnodes )
         k = getLeftIndex( self % y_values, t(2), self % nnodes )

         ! Get the four corners in the correct orientation
         z_corners = pullFourCorners( self % z_values, j, k, self % nnodes )

         ! Compute the bi-linear interpolation with the auxiliary variables u, v
         v = ( t(1) - self % x_values(j) ) / ( self % x_values(j+1) - self % x_values(j) )
         u = ( t(2) - self % y_values(k) ) / ( self % y_values(k+1) - self % y_values(k) )

         z_val =   (1.0_RP - v) * (1.0_RP - u) * z_corners(1) &
                 +           v  * (1.0_RP - u) * z_corners(2) &
                 +           v  *           u  * z_corners(3) &
                 + (1.0_RP - v) *           u  * z_corners(4)

      END FUNCTION evaluateBilinearInterpolant
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION evaluateBicubicInterpolant(self, t)  RESULT(z_val)
         ! The form of this bicubic interpolation was mainly taken from the blog post
         ! https://www.giassa.net/?page_id=371 that has a very nice explanation of where
         ! the different terms come from.
         IMPLICIT NONE
         CLASS(SMDataFileTopography) :: self
         REAL(KIND=RP)               :: t(2)
         ! local variables
         INTEGER       :: i, j, k
         REAL(KIND=RP) :: u, v, z_corners(4), dz_dx(4), dz_dy(4), dz_dxy(4)
         REAL(KIND=RP) :: dx, dy, temp
         REAL(KIND=RP), DIMENSION(16) :: work_vec, coeff_vec
         REAL(KIND=RP), DIMENSION(4,4) :: coeffs
         REAL(KIND=RP), DIMENSION(16,16) :: weight_Matrix

         ! Get the indexing of the current test point
         j = getLeftIndex( self % x_values, t(1), self % nnodes )
         k = getLeftIndex( self % y_values, t(2), self % nnodes )

         ! Get the grid sizes
         dx = self % x_values(j+1) - self % x_values(j)
         dy = self % y_values(k+1) - self % y_values(k)

         ! Pull the corner and gradient information in the correct orientation
         z_corners = pullFourCorners( self % z_values, j, k, self % nnodes )
         dz_dx  = pullFourCorners( self % dzdx  , j, k, self % nnodes )
         dz_dy  = pullFourCorners( self % dzdy  , j, k, self % nnodes )
         dz_dxy = pullFourCorners( self % d2zdxy, j, k, self % nnodes )
!
!        --------------------------------
!        Compute the bicubic coefficients
!        --------------------------------
!
         ! Get the pre-computed weigthing matrix for the coefficients
         weight_Matrix = fillBicubicWeightMatrix()

         ! Fill a work vector
         work_vec(1:4)   = z_corners(:)
         work_vec(5:8)   = dz_dx(:) * dx
         work_vec(9:12)  = dz_dy(:) * dy
         work_vec(13:16) = dz_dxy(:) * dx * dy

         ! Matrix multiplication with the known inverse weighting matrix
         !   TODO: Speedup?! This is a sparse matrix-vector product so could be improved
         coeff_vec = MATMUL( weight_Matrix, work_vec )

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
            z_val = z_val * v + ( (coeffs(i,4) * u + coeffs(i,3) ) * u + coeffs(i,2) ) * u + coeffs(i,1)
         END DO ! i

      END FUNCTION evaluateBicubicInterpolant
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION positionOnDFTopographyAt(self, t)  RESULT(x)
        IMPLICIT NONE
        CLASS(SMDataFileTopography) :: self
        REAL(KIND=RP)               :: t(2)
        REAL(KIND=RP)               :: x(3)

         x(1) = t(1)
         x(2) = t(2)
         x(3) = evaluateBicubicInterpolant(self, t)
      !     x(3) = evaluateBilinearInterpolant(self, t)

      END FUNCTION positionOnDFTopographyAt
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION fillBicubicWeightMatrix()  RESULT(M)
         ! Inverse matrix necessary to find the 16 coefficients needed
         ! for the bicubic interpolation routine. Details on this matrix can
         ! found here https://www.giassa.net/?page_id=371. Or, in the words of
         ! Numerical Recipes in Fortran 77, the formulas for the bicubic coefficents
         ! "are just a complicated linear transformation [which] having been determined
         !  once in the mists of numerical history, can be tabulated and forgotten."
         ! This is one implementation of such a tabulation.
         IMPLICIT NONE
         REAL(KIND=RP) :: M(16,16)

         M = 0.0_RP
         ! row 1
         M(1, 1) =  1.0_RP
         ! row 2
         M(2, 9) =  1.0_RP
         ! row 3
         M(3, 1) = -3.0_RP
         M(3, 4) =  3.0_RP
         M(3, 9) = -2.0_RP
         M(3,12) = -1.0_RP
         ! row 4
         M(4, 1) =  2.0_RP
         M(4, 4) = -2.0_RP
         M(4, 9) =  1.0_RP
         M(4,12) =  1.0_RP
         ! row 5
         M(5, 5) =  1.0_RP
         ! row 6
         M(6,13) =  1.0_RP
         ! row 7
         M(7, 5) = -3.0_RP
         M(7, 8) =  3.0_RP
         M(7,13) = -2.0_RP
         M(7,16) = -1.0_RP
         ! row 8
         M(8, 5) =  2.0_RP
         M(8, 8) = -2.0_RP
         M(8,13) =  1.0_RP
         M(8,16) =  1.0_RP
         ! row 9
         M(9, 1) = -3.0_RP
         M(9, 2) =  3.0_RP
         M(9, 5) = -2.0_RP
         M(9, 6) = -1.0_RP
         ! row 10
         M(10, 9)  = -3.0_RP
         M(10, 10) =  3.0_RP
         M(10, 13) = -2.0_RP
         M(10, 14) = -1.0_RP
         ! row 11
         M(11, 1)  =  9.0_RP
         M(11, 2)  = -9.0_RP
         M(11, 3)  =  9.0_RP
         M(11, 4)  = -9.0_RP
         M(11, 5)  =  6.0_RP
         M(11, 6)  =  3.0_RP
         M(11, 7)  = -3.0_RP
         M(11, 8)  = -6.0_RP
         M(11, 9)  =  6.0_RP
         M(11, 10) = -6.0_RP
         M(11, 11) = -3.0_RP
         M(11, 12) =  3.0_RP
         M(11, 13) =  4.0_RP
         M(11, 14) =  2.0_RP
         M(11, 15) =  1.0_RP
         M(11, 16) =  2.0_RP
         ! row 12
         M(12, 1)  = -6.0_RP
         M(12, 2)  =  6.0_RP
         M(12, 3)  = -6.0_RP
         M(12, 4)  =  6.0_RP
         M(12, 5)  = -4.0_RP
         M(12, 6)  = -2.0_RP
         M(12, 7)  =  2.0_RP
         M(12, 8)  =  4.0_RP
         M(12, 9)  = -3.0_RP
         M(12, 10) =  3.0_RP
         M(12, 11) =  3.0_RP
         M(12, 12) = -3.0_RP
         M(12, 13) = -2.0_RP
         M(12, 14) = -1.0_RP
         M(12, 15) = -1.0_RP
         M(12, 16) = -2.0_RP
         ! row 13
         M(13, 1) =  2.0_RP
         M(13, 2) = -2.0_RP
         M(13, 5) =  1.0_RP
         M(13, 6) =  1.0_RP
         ! row 14
         M(14, 9) =  2.0_RP
         M(14,10) = -2.0_RP
         M(14,13) =  1.0_RP
         M(14,14) =  1.0_RP
         ! row 15
         M(15, 1)  = -6.0_RP
         M(15, 2)  =  6.0_RP
         M(15, 3)  = -6.0_RP
         M(15, 4)  =  6.0_RP
         M(15, 5)  = -3.0_RP
         M(15, 6)  = -3.0_RP
         M(15, 7)  =  3.0_RP
         M(15, 8)  =  3.0_RP
         M(15, 9)  = -4.0_RP
         M(15, 10) =  4.0_RP
         M(15, 11) =  2.0_RP
         M(15, 12) = -2.0_RP
         M(15, 13) = -2.0_RP
         M(15, 14) = -2.0_RP
         M(15, 15) = -1.0_RP
         M(15, 16) = -1.0_RP
         ! row 16
         M(16, 1)  =  4.0_RP
         M(16, 2)  = -4.0_RP
         M(16, 3)  =  4.0_RP
         M(16, 4)  = -4.0_RP
         M(16, 5)  =  2.0_RP
         M(16, 6)  =  2.0_RP
         M(16, 7)  = -2.0_RP
         M(16, 8)  = -2.0_RP
         M(16, 9)  =  2.0_RP
         M(16, 10) = -2.0_RP
         M(16, 11) = -2.0_RP
         M(16, 12) =  2.0_RP
         M(16, 13) =  1.0_RP
         M(16, 14) =  1.0_RP
         M(16, 15) =  1.0_RP
         M(16, 16) =  1.0_RP

      END FUNCTION fillBicubicWeightMatrix
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE printDFDescription(self, iUnit)
        IMPLICIT NONE
        CLASS(SMDataFileTopography) :: self
        INTEGER                     :: iUnit

        WRITE(iUnit,*) TRIM(self % file_name)
      END SUBROUTINE printDFDescription
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE addStringToDictionary(key, str, dict)
         IMPLICIT NONE
         CHARACTER(LEN=*)    :: str, key
         CLASS(FTDictionary) :: dict

         CLASS(FTObject)      , POINTER :: obj => NULL()
         CLASS(FTValue)       , POINTER :: v => NULL()

         ALLOCATE(v)
         CALL v % initWithValue(str)
         obj => v
         CALL dict % addObjectForKey(obj,key)
         CALL release(obj)

      END SUBROUTINE addStringToDictionary

      END Module SMDataFileTopographyClass

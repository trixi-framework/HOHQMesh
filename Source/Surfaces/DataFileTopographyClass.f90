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
      USE BiCubicClass

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
         CHARACTER(LEN=STRING_CONSTANT_LENGTH) :: file_name
         TYPE(BiCubicInterpolation)            :: biCubic
         TYPE(BiCubicInterpolation), POINTER   :: curvature
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithDataFile
         FINAL     :: destructDFTopography
         PROCEDURE :: heightAt => positionOnDFTopographyAt
         PROCEDURE :: printDescription => printDFDescription
         PROCEDURE :: gaussianCurvatureAt => gaussianCurvatureFromInterp
      END TYPE SMTopographyFromFile
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
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMTopographyFromFile) :: self
         CHARACTER(LEN=*)            :: topographyFile
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP), DIMENSION(:)  , ALLOCATABLE :: x_values
         REAL(KIND=RP), DIMENSION(:)  , ALLOCATABLE :: y_values
         REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE :: z_values
         INTEGER                                    :: nx, ny, j, k, file_unit
         REAL(KIND=RP)                              :: inv_dx, inv_dy, xx(3)
         CHARACTER(LEN=ERROR_MESSAGE_LENGTH)        :: msg
         LOGICAL                                    :: a_data_file
!
!        ----------------------------------------------------
!        Check to make sure the input topographyFile is valid
!        ----------------------------------------------------
!
         INQUIRE( FILE=topographyFile, EXIST=a_data_file )
         IF (.NOT.a_data_file) THEN
            WRITE(msg, *)"Unable to open the topography data file "//TRIM(ADJUSTL(topographyFile))
            CALL ThrowErrorExceptionOfType(poster = "DataFileTopography/initWithDataFile", &
                                           msg    = msg, &
                                           typ    = FT_ERROR_FATAL)
            RETURN
         END IF

         CALL self % SMTopography % initTopography()
!
!        --------------------
!        Save the data source
!        --------------------
!
         self % file_name = topographyFile
!
!        ----------------------------------------------
!        Open the file and make sure everything is okay
!        ----------------------------------------------
!
         OPEN(NEWUNIT=file_unit, FILE=topographyFile)
!
!        -----------------------------------------------------------------
!        Read the number of points available in the bottom topography data
!        -----------------------------------------------------------------
!
         READ(file_unit, *) ! eat the header
         READ(file_unit, *) nx, ny
!
!        -----------------------------------
!        Allocate the memory and zero it out
!        -----------------------------------
!
         ALLOCATE(x_values(1:nx), y_values(1:ny))
         ALLOCATE(z_values(1:nx, 1:ny))
         x_values = 0.0_RP
         y_values = 0.0_RP
         z_values = 0.0_RP
!
!        ---------------------------------------------
!        Read the data into appropriate storage arrays
!        ---------------------------------------------
!
         READ(file_unit, *) ! skip the header for x_values list
         DO j = 1,nx
            READ(file_unit, *) x_values(j)
         END DO ! j

         READ(file_unit, *) ! skip the header for y_values list
         DO k = 1,ny
            READ(file_unit, *) y_values(k)
         END DO ! k

         READ(file_unit, *) ! skip the header for z_nodes list
         DO k = 1,ny
            DO j = 1,nx
               READ(file_unit, *) z_values(j, k)
            END DO ! j
         END DO ! k

         CLOSE(file_unit)
         
         CALL self % biCubic % initBiCubicInterpolation(Nx = nx, Ny = ny, &
                                                        x  = x_values,    &
                                                        y  = y_values,    &
                                                        z  = z_values)
!
!        -------------------------------------------------------------
!        Compute gaussian curvature for bottom topography size control
!        -------------------------------------------------------------
!
         NULLIFY( self % curvature)
         ALLOCATE( self % curvature)
         DO k = 1,ny
            DO j = 1,nx
               z_values(j, k) = GaussianCurvatureBaseAt(self, [x_values(j), y_values(k)])
            END DO ! j
         END DO ! k
         CALL self % curvature % initBiCubicInterpolation(Nx = nx, Ny = ny, &
                                                          x  = x_values,    &
                                                          y  = y_values,    &
                                                          z  = z_values)
        
      END SUBROUTINE initWithDataFile
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE destructDFTopography(self)
         IMPLICIT NONE
         TYPE(SMTopographyFromFile) :: self

         self % file_name = "nothing"
         IF(ASSOCIATED(self % curvature)) CALL releaseBiCubicInterpolation(self % curvature)

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

         x(3) = self % biCubic % valueAt(t)
         x(1) = t(1)
         x(2) = t(2)
         
      END FUNCTION positionOnDFTopographyAt
!
!//////////////////////////////////////////////////////////////////////// 
! 
      REAL(KIND=RP) FUNCTION gaussianCurvatureFromInterp(self, t)  
         IMPLICIT NONE  
         CLASS(SMTopographyFromFile) :: self
         REAL(KIND=RP)               :: t(2)
         
         gaussianCurvatureFromInterp = self % curvature % valueAt(t)
         IF(gaussianCurvatureFromInterp > 2.0)     then
            gaussianCurvatureFromInterp = 0.0_RP
         END IF 
         
      END FUNCTION gaussianCurvatureFromInterp
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

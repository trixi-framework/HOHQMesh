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
      USE Geometry
      USE ErrorTypesModule

      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: TOPOGRAPHY_FROM_FILE_KEY = "data file"
      CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: SIZING_KEY               = "sizing"
!
!     ----------------------
!     Class type definition
!     ----------------------
!
      TYPE, EXTENDS(SMTopography) :: SMTopographyFromFile
         CHARACTER(LEN=STRING_CONSTANT_LENGTH) :: file_name
         TYPE(BiCubicInterpolation)            :: biCubic
         TYPE(BiCubicInterpolation), POINTER   :: curvature
         REAL(KIND=RP)                         :: box(6) !TOP, LEFT, BOTTOM, RIGHT (2 unused in 2D)
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
      SUBROUTINE initWithDataFile(self, topographyFile, sizingIsOn)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMTopographyFromFile) :: self
         CHARACTER(LEN=*)            :: topographyFile
         LOGICAL                     :: sizingIsOn
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP), DIMENSION(:)  , ALLOCATABLE :: x_values
         REAL(KIND=RP), DIMENSION(:)  , ALLOCATABLE :: y_values
         REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE :: z_values
         INTEGER                                    :: nx, ny, j, k, file_unit
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
         self % box(BBOX_LEFT)   =  HUGE(1.0_RP)
         self % box(BBOX_RIGHT)  = -HUGE(1.0_RP)
         self % box(BBOX_TOP)    = -HUGE(1.0_RP)
         self % box(BBOX_BOTTOM) =  HUGE(1.0_RP)
         
         READ(file_unit, *) ! skip the header for x_values list
         DO j = 1,nx
            
            READ(file_unit, *) x_values(j)
            
            self % box(BBOX_LEFT)  = MIN(x_values(j), self % box(BBOX_LEFT))
            self % box(BBOX_RIGHT) = MAX(x_values(j), self % box(BBOX_RIGHT))
            
         END DO ! j

         READ(file_unit, *) ! skip the header for y_values list
         DO k = 1,ny
            READ(file_unit, *) y_values(k)
            
            self % box(BBOX_BOTTOM) = MIN(y_values(k), self % box(BBOX_BOTTOM))
            self % box(BBOX_TOP)    = MAX(y_values(k), self % box(BBOX_TOP))
            
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
!        ---------------------------------------------------------------------
!        It is best not to interpolate outside of the boundaries of the data. 
!        However, we allow some slop due to approximation and rounding errors.
!        The amount of slope allowed here is basically arbitrary.
!        ---------------------------------------------------------------------
!
         self % box(BBOX_LEFT)   = self % box(BBOX_LEFT)*  (1.0_RP - SIGN(0.01_RP,self % box(BBOX_LEFT)))
         self % box(BBOX_RIGHT)  = self % box(BBOX_RIGHT)* (1.0_RP + SIGN(0.01_RP,self % box(BBOX_RIGHT)))
         self % box(BBOX_TOP)    = self % box(BBOX_TOP)*   (1.0_RP + SIGN(0.01_RP,self % box(BBOX_TOP)))
         self % box(BBOX_BOTTOM) = self % box(BBOX_BOTTOM)*(1.0_RP - SIGN(0.01_RP,self % box(BBOX_BOTTOM)))
         IF ( self % box(BBOX_LEFT) == 0.0_RP )     THEN
            self % box(BBOX_LEFT) = -0.01_RP 
         END IF
         IF ( self % box(BBOX_RIGHT) == 0.0_RP )     THEN
            self % box(BBOX_RIGHT) = 0.01_RP 
         END IF
         IF ( self % box(BBOX_TOP) == 0.0_RP )     THEN
            self % box(BBOX_TOP) = 0.01_RP 
         END IF
         IF ( self % box(BBOX_BOTTOM) == 0.0_RP )     THEN
            self % box(BBOX_BOTTOM) = -0.01_RP 
         END IF
!
!        -------------------------------------------------------------
!        Compute gaussian curvature for bottom topography size control
!        -------------------------------------------------------------
!
         NULLIFY( self % curvature)
         
         IF( sizingIsOn)     THEN 
         
            ALLOCATE( self % curvature)
            DO k = 1,ny
               DO j = 1,nx
                  z_values(j, k) = GaussianCurvatureBaseAt(self, [x_values(j), y_values(k)])
               END DO ! j
            END DO ! k
!
!           ---------------------------------------------------------------
!           Use zeroth order extrapolation of the curvature to the boundary
!           ---------------------------------------------------------------
!
         
            DO k = 2, Ny-1 
               z_values(1,k)   = z_values(2,k)
               z_values(Nx,k)  = z_values(Nx-1,k)
            END DO 
            
            DO j = 2, Nx-1 
               z_values(j,1)  = z_values(j,2)
               z_values(j,Ny) = z_values(j,Ny-1)
            END DO 
   
            z_values(1,1)   = z_values(2,2)
            z_values(Nx,1)  = z_values(Nx-1,2)
            z_values(1,Ny)  = z_values(2,Ny-1)
            z_values(Nx,Ny) = z_values(Nx-1,Ny-1)
            
            CALL self % curvature % initBiCubicInterpolation(Nx = nx, Ny = ny, &
                                                             x  = x_values,    &
                                                             y  = y_values,    &
                                                             z  = z_values)
         END IF
            
       
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
         CLASS(SMTopographyFromFile)          :: self
         REAL(KIND=RP)                        :: t(2)
         REAL(KIND=RP)                        :: x(3)
         CHARACTER(LEN=ERROR_MESSAGE_LENGTH)  :: msg

         IF ( .NOT. Point_IsInsideBox([t(1),t(2),0.0_RP],self % box) )     THEN
            WRITE(msg, *)"Interpolation point (", t(1), t(2) ,") outside of: ", self % box(1:4)
            CALL ThrowErrorExceptionOfType(poster = "positionOnDFTopographyAt", &
                                           msg    = msg, &
                                           typ    = FT_ERROR_WARNING)
            RETURN
         END IF 
          
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
         CHARACTER(LEN=ERROR_MESSAGE_LENGTH)  :: msg
         
         gaussianCurvatureFromInterp = 0.0_RP
         IF ( .NOT. Point_IsInsideBox([t(1),t(2),0.0_RP],self % box) )     THEN
            WRITE(msg, *)"Interpolation point ()", t(1), t(2) ,") is outside of data bounding box: ", self % box(1:4)
            CALL ThrowErrorExceptionOfType(poster = "gaussianCurvatureFromInterp", &
                                           msg    = msg, &
                                           typ    = FT_ERROR_WARNING)
            RETURN
         END IF 
         gaussianCurvatureFromInterp = self % curvature % valueAt(t)
         
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

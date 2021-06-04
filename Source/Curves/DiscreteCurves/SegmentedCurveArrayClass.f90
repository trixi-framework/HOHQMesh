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
!>      SegmentedCurveArray.f95
!!      Created: 2010-08-09 12:55:36 -0400 
!!      By: David Kopriva
!!
!!      A SegmentedCurveArray represents a curve as an array
!!      of points. It can be used to determine if a point falls inside
!!      of a curve.
!!
!!     Usage:
!!
!!        *Creation*
!!
!!           allocate(curve)
!!           curve % initWithNumberOfSegmentsNameAndID(nSegments, name, id)
!!           curve % setProperties( boundaryName, x, t, id )
!!
!!         *Bounding Box (3D)*
!!
!!            CALL curve % setBoundingBox(box)
!!
!!         *Determining if a point is inside the curve*
!!
!!            LOGICAL test = PointIsInsideCurve(self, p)
!  
!
!////////////////////////////////////////////////////////////////////////
!
      Module SegmentedCurveArrayClass
      USE SMConstants
      USE ProgramGlobals
      USE FTObjectClass
      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      INTEGER, PARAMETER :: SEGMENTED_CURVE_NAME_LENGTH = 32
!     ----------
!     Class type
!     ----------
!
      TYPE, EXTENDS(FTObject) :: SegmentedCurveArray
         INTEGER                                    :: id
         INTEGER                                    :: nSegments, direction
         REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE :: x
         REAL(KIND=RP), DIMENSION(:)  , ALLOCATABLE :: curvature
         REAL(KIND=RP), DIMENSION(:)  , ALLOCATABLE :: t
         REAL(KIND=RP)                              :: boundingBox(6)
         CHARACTER(LEN=SEGMENTED_CURVE_NAME_LENGTH) :: boundaryName_
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithNumberOfSegmentsNameAndID
         PROCEDURE :: setPoints
         PROCEDURE :: setBoundaryName
         PROCEDURE :: boundaryName
         PROCEDURE :: PointIsInsideCurve
         PROCEDURE :: setBoundingBox
         
         FINAL     :: destructSegmentedCurveArray
         PROCEDURE :: printDescription => PrintSegmentedCurveArray
         
      END TYPE SegmentedCurveArray
!
!     ========
      CONTAINS 
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initWithNumberOfSegmentsNameAndID( self, nSegments, bName, id ) 
         IMPLICIT NONE
         INTEGER                    :: nSegments
         CLASS(SegmentedCurveArray) :: self
         CHARACTER(LEN=*)           :: bName
         INTEGER                    :: id
         
         CALL self % setBoundaryName(bName)
         self % id           = id
         
         CALL self % FTObject % init()
         
         self % nSegments = nSegments
         
         ALLOCATE( self % x(3,0:nSegments) )
         ALLOCATE( self % t(0:nSegments) )
         ALLOCATE( self % curvature(0:nSegments) )
         
      END SUBROUTINE initWithNumberOfSegmentsNameAndID
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE setBoundaryName( self, bName )  
         IMPLICIT NONE
         CLASS(SegmentedCurveArray) :: self
         CHARACTER(LEN=*)           :: bName
         self % boundaryName_ = bName
      END SUBROUTINE setBoundaryName
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION boundaryName(self) RESULT(bName)
         IMPLICIT NONE  
         CLASS(SegmentedCurveArray)                 :: self
         CHARACTER(LEN=SEGMENTED_CURVE_NAME_LENGTH) :: bName
         bName = self % boundaryName_
      END FUNCTION  
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE setPoints(self, t, x)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SegmentedCurveArray)     :: self
         REAL(KIND=RP), DIMENSION(:,0:) :: x
         REAL(KIND=RP), DIMENSION(0:)   :: t
!
!        ---------------
!        Local variables
!        ---------------
!
         self % x = x
         self % t = t
         
         CALL ComputeCurvature( self )
         
      END SUBROUTINE setPoints
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DestructSegmentedCurveArray( self ) 
         IMPLICIT NONE
         TYPE(SegmentedCurveArray) :: self
         self % nSegments = 0
         DEALLOCATE( self % x, self % t, self % curvature )
         
      END SUBROUTINE DestructSegmentedCurveArray
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseCurveArray(self)  
         IMPLICIT NONE
         CLASS(SegmentedCurveArray), POINTER :: self
         CLASS(FTObject)           , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseCurveArray
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeCurvature( self )
      IMPLICIT NONE 
      CLASS(SegmentedCurveArray)                   :: self
      INTEGER                                      :: j, N
      REAL(KIND=RP), DIMENSION(3)                  :: xp, xpp
      REAL(KIND=RP), DIMENSION(0:self % nSegments) :: dt
      
      N = self % nSegments
      DO j = 1, N-1 
         dt(j) = self % t(j+1) - self % t(j)
      END DO
      dt(N) = dt(N-1)
      dt(0) = dt(1)
      
      DO j = 1, N-1 
         xp = -dt(j+1)*self % x(:,j-1)/(dt(j)*(dt(j)+dt(j+1))) &
              -(dt(j) - dt(j+1))/(dt(j)*dt(j+1))*self % x(:,j) &
              + dt(j-1)/(dt(j+1)*(dt(j)+dt(j+1)))*self % x(:,j+1)
              
         xpp = 2*self % x(:,j+1)/(dt(j)*(dt(j)+dt(j+1))) &
              -2/(dt(j)*dt(j+1))*self % x(:,j) &
              + 2/(dt(j+1)*(dt(j)+dt(j+1)))*self % x(:,j+1)
              
         self % curvature(j) = ABS( xp(1)*xpp(2) - xp(2)*xpp(1) )/(xp(1)**2 + xp(2)**2)**1.5_RP
      END DO
      self % curvature(0) = self % curvature(1)
      self % curvature(N) = self % curvature(N-1)
      
      END SUBROUTINE ComputeCurvature
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PrintSegmentedCurveArray( self, iUnit ) 
         IMPLICIT NONE 
         CLASS(SegmentedCurveArray) :: self
         INTEGER                    :: iUnit
         INTEGER                    :: j
         
         DO j = 0, self % nSegments 
            WRITE(iUnit,*) self % t(j),self % x(:,j)
         END DO

      END SUBROUTINE PrintSegmentedCurveArray
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetBoundingBox( self, bBox) 
         IMPLICIT NONE 
         CLASS(SegmentedCurveArray) :: self
         REAL(KIND=RP)             :: bBox(6)
         self % boundingBox = bBox
      END SUBROUTINE SetBoundingBox 
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION PointIsInsideCurve(self, p)
         USE Geometry, ONLY:ACWindingFunction
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SegmentedCurveArray) :: self
         REAL(KIND=RP)              :: p(3)
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP) :: w
         
         PointIsInsideCurve = .true.
         w = ACWindingFunction( p, self % x, self % nSegments-1 )
         IF ( abs(w) <= EPSILON(w) ) THEN
            PointIsInsideCurve = .false.
         END IF
      END FUNCTION PointIsInsideCurve 
!
!////////////////////////////////////////////////////////////////////////
!
      END Module SegmentedCurveArrayClass

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
         
         PROCEDURE :: destruct         => destructSegmentedCurveArray
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
         CLASS(SegmentedCurveArray) :: self
         self % nSegments = 0
         DEALLOCATE( self % x, self % t, self % curvature )
         
         CALL self % FTObject % destruct()
         
      END SUBROUTINE DestructSegmentedCurveArray
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

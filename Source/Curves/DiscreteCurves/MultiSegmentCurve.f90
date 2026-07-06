!
!////////////////////////////////////////////////////////////////////////
!
!      MultiSegmentCurve.f90
!      Created: April 26, 2026 at 11:09 AM 
!      By: David Kopriva  
!
!      The base class for:
!         MultiSegmentCurve
!         MultiSegmentModalCurve
!
!////////////////////////////////////////////////////////////////////////
!
   Module MultiSegmentCurveClass 
   USE SMConstants
   USE SMCurveClass
   USE IntervalSearchModule
   USE GaussQuadratureModule
   IMPLICIT NONE
   
   TYPE, EXTENDS(SMCurve) :: MultiSegmentCurve
      INTEGER                    :: nSegments
      INTEGER                    :: polyOrder
      REAL(KIND=RP), ALLOCATABLE :: cuts(:)
!
!     ========
      CONTAINS
!     ========
!         
      PROCEDURE :: construct => ConstructMultiSegmentCurve
      FINAL     :: DestructMultiSegmentCurve
      PROCEDURE :: positionAt => EvaluateMultiSegmentCurve
      PROCEDURE :: derivativeAt => EvaluateMultiSegmentCurveD 
      PROCEDURE :: derivativeInSegment => baseDerivativeInSegment
      PROCEDURE :: arcLength
      PROCEDURE :: className  => MSCClassName
   END TYPE MultiSegmentCurve
   
!
!  ========
   CONTAINS  
!  ========
!
      SUBROUTINE castObjToMultiSegmentCurve(curve,cast)
!
!     -----------------------------------------------------
!     Cast the base class SMCurve to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)          , POINTER :: curve
         CLASS(MultiSegmentCurve) , POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => curve)
            CLASS IS(MultiSegmentCurve)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castObjToMultiSegmentCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ConstructMultiSegmentCurve(self, cuts, N, curveName, id )  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MultiSegmentCurve) :: self
         CLASS(SMCurve), POINTER       :: parentCurve
         REAL(KIND=RP)            :: cuts(0:)
         INTEGER                  :: N
         CHARACTER(LEN=*)         :: curveName
         INTEGER                  :: id
          
         CALL self % SMCurve % initWithNameAndID(curveName,id)
!
!        ----------
!        Set values
!        ----------
!        
         self % cuts      = cuts
         self % polyOrder = N
         self % nSegments = SIZE(cuts)-1
!
      END SUBROUTINE ConstructMultiSegmentCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE DestructMultiSegmentCurve(self)  
         IMPLICIT NONE  
         TYPE(MultiSegmentCurve) :: self

      END SUBROUTINE DestructMultiSegmentCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseMultiSegmentCurve(self)  
         IMPLICIT NONE
         TYPE (MultiSegmentCurve), POINTER :: self
         CLASS(FTObject)              , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseMultiSegmentCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION EvaluateMultiSegmentCurve(self,t)  RESULT(x)
         IMPLICIT NONE  
         CLASS(MultiSegmentCurve) :: self
         REAL(KIND=RP)                 :: t
         REAL(KIND=RP)                 :: x(3)
         
         x = 0.0_RP
         
      END FUNCTION EvaluateMultiSegmentCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION EvaluateMultiSegmentCurveD(self,t)  RESULT(x)
         IMPLICIT NONE  
         CLASS(MultiSegmentCurve) :: self
         REAL(KIND=RP)                 :: t
         REAL(KIND=RP)                 :: x(3)

         x = 0.0_RP     
             
      END FUNCTION EvaluateMultiSegmentCurveD
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION baseDerivativeInSegment(self, t, k)  RESULT(x)
!
!        ---------------------------------------------------------------
!        Returns the derivative in segment k as needed by the superclass
!        procedure to compute the arc length
!        ---------------------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(MultiSegmentCurve) :: self
         REAL(KIND=RP)            :: t
         REAL(KIND=RP)            :: x(3)
         INTEGER                  :: k
         
         x = 0.0_RP
         
      END FUNCTION baseDerivativeInSegment
!
!//////////////////////////////////////////////////////////////////////// 
! 
   REAL(KIND=RP) FUNCTION arcLength(self, gQuad)
!
!  -------------------------------------------
!  Compute the arc length of a segmented curve
!  -------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(MultiSegmentCurve) :: self
      TYPE(GaussQuadratureType)     :: gQuad
!
!     ----------------
!     Local variables 
!     ----------------
!
      INTEGER                    :: nSegments
      INTEGER                    :: k
      
      nSegments = self % nSegments
      
      arcLength = 0.0_RP
      DO k = 1, nSegments
         arcLength = arcLength + segmentArcLength(self, k, gQuad % nodes, gQuad % weights)
      END DO 
      
   END FUNCTION arcLength
!
!//////////////////////////////////////////////////////////////////////// 
! 
   REAL(KIND=RP) FUNCTION segmentArcLength(self, k, nodes, weights)
!
!  -------------------------------------
!  Compute the arc length of a segmented
!  curve
!  -------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(MultiSegmentCurve) :: self
      REAL(KIND=RP)            :: nodes(0:), weights(0:)
      INTEGER                  :: k
!
!     ----------------
!     Local variables 
!     ----------------
!
      INTEGER                    :: N
      INTEGER                    :: qOrder
      INTEGER                    :: nSegments
      INTEGER                    :: j
      REAL(KIND=RP)              :: t, h, e1(3)
      REAL(KIND=RP)              :: dsdt
      
      nSegments = self % nSegments

      N      = self % polyOrder
      qOrder = SIZE(nodes) - 1
            
      segmentArcLength = 0.0_RP
      h    = self % cuts(k) - self % cuts(k-1)
      dsdt = 2.0_RP/h
      DO j = 0, qOrder
         t                = self % cuts(k-1) + h*0.5_RP*(nodes(j) + 1.0_RP)
         e1               = dsdt*self % derivativeInSegment(t,k)
         segmentArcLength = segmentArcLength + 0.5_RP*h*weights(j)*SQRT(e1(1)**2 + e1(2)**2)
      END DO 
      
   END FUNCTION segmentArcLength
!
!//////////////////////////////////////////////////////////////////////// 
! 
      REAL(KIND=RP) FUNCTION affineMap(t0,t1,s)  
         IMPLICIT NONE  
         REAL(KIND=RP) :: t0, t1, s !\in [-1,1]
         
         affineMap = t0 + (t1 - t0)*0.5_RP*(1.0_RP + s)
         
      END FUNCTION affineMap
!
!//////////////////////////////////////////////////////////////////////// 
! 
      REAL(KIND=RP) FUNCTION InvAffineMap(t0,t1,t)  
         IMPLICIT NONE  
         REAL(KIND=RP) :: t0, t1, t !\in [t0,t1]
         
         InvAffineMap = 2.0_RP*(t - t0)/(t1 - t0) - 1.0_RP
         
      END FUNCTION InvAffineMap
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> Class name returns a string with the name of the type of the object
!>
!>  ### Usage:
!>
!>        PRINT *,  obj % className()
!>        if( obj % className =="MultiSegmentCurve")
!>
      FUNCTION MSCClassName(self)  RESULT(s)
         IMPLICIT NONE  
         CLASS(MultiSegmentCurve)              :: self
         CHARACTER(LEN=CLASS_NAME_CHARACTER_LENGTH) :: s
         
         s = "MultiSegmentCurve"
         IF( self % refCount() >= 0 ) CONTINUE 
 
      END FUNCTION MSCClassName

   END Module MultiSegmentCurveClass

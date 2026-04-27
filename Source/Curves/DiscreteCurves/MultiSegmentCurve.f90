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

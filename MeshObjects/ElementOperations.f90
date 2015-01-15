!
!////////////////////////////////////////////////////////////////////////
!
!      ElementOperations.f90
!      Created: October 9, 2013 9:24 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module ElementOperations
      USE SMMeshClass
      IMPLICIT NONE 
      CONTAINS 
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION ElementLocalNodeIDForNodeID( nodeID, e ) RESULT(localID)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                   :: nodeID
         CLASS(SMElement), POINTER :: e
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                  :: k
         CLASS(FTobject), POINTER :: obj  => NULL()
         CLASS(SMNode)  , POINTER :: node => NULL()
         
         localID = NONE
         DO k = 1, e % nodes % COUNT()
            obj => e % nodes % objectAtIndex(k)
            CALL cast(obj,node)
            IF ( node % id == nodeID )     THEN
               localID = k
               RETURN
            END IF
         END DO
         
      END FUNCTION ElementLocalNodeIDForNodeID 
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION AngleAtLocalNode_ForElement(k,e) RESULT(theta)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                   :: k
         CLASS(SMElement), POINTER :: e
!
!        ---------------
!        local Variables
!        ---------------
!
         REAL(KIND=RP), DIMENSION(3) :: P1, P2, P3     ! Location of the four corners
         REAL(KIND=RP), DIMENSION(3) :: L1, L2         ! The two edge vectors
         REAL(KIND=RP)               :: LNorm1, Lnorm2 ! lengths of the edge vectors
         REAL(KIND=RP), EXTERNAL     :: Norm2
         
         CLASS(FTObject), POINTER :: obj  => NULL()
         CLASS(SMNode)  , POINTER :: node => NULL()
!
!        -------------------------------------------------------------
!        Grab the two sides. They are ordered counter-clockwise, so to
!        compute the interior angle, reverse the order.
!        -------------------------------------------------------------
!
         obj => e % nodes % objectAtIndex(k)
         CALL cast(obj,node)
         P1(:) = node % x
         
         obj => e % nodes % objectAtIndex(sourceNodeLocalID(1,k))
         CALL cast(obj,node)
         P2(:) = node % x
         
         obj => e % nodes % objectAtIndex(sourceNodeLocalID(2,k))
         CALL cast(obj,node)
         P3(:) = node % x
!
!        ----------------------------
!        Compute lengths of the sides
!        ----------------------------
!
         L1     = P2 - P1
         L2     = P3 - P1
         LNorm1 = Norm2(L1)
         LNorm2 = Norm2(L2)
         
         theta = 180.0_RP - ACOS( -(L1(1)*L2(1) + L1(2)*L2(2))/(LNorm1*LNorm2))*180.0_RP/PI

      END FUNCTION AngleAtLocalNode_ForElement 
      
      END MODULE ElementOperations

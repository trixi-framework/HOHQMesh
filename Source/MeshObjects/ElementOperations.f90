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
! !
! --- End License
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
         INTEGER                  :: nodeID
         TYPE(SMElement), POINTER :: e
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                  :: k
         
         localID = NONE
         DO k = 1, e % eType
            IF ( e % nodes(k) % node % id == nodeID )     THEN
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
         INTEGER                  :: k
         TYPE(SMElement), POINTER :: e
!
!        ---------------
!        local Variables
!        ---------------
!
         REAL(KIND=RP), DIMENSION(3) :: P1, P2, P3     ! Location of the four corners
         REAL(KIND=RP), DIMENSION(3) :: L1, L2         ! The two edge vectors
         REAL(KIND=RP)               :: LNorm1, Lnorm2 ! lengths of the edge vectors
!
!        -------------------------------------------------------------
!        Grab the two sides. They are ordered counter-clockwise, so to
!        compute the interior angle, reverse the order.
!        -------------------------------------------------------------
!
         P1(:) = e % nodes(k) % node % x
         P2(:) = e % nodes(sourceNodeLocalID(1,k)) % node % x
         P3(:) = e % nodes(sourceNodeLocalID(2,k)) % node % x
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

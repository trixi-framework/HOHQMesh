!
!////////////////////////////////////////////////////////////////////////
!
!      TransfiniteMapClass.f
!      Created: 2007-06-21 11:19:33 -0400 
!      By: David Kopriva
!
!      Unlike the text, this is wrapped in a class form.
!
!      Contains:
!                       TYPE(TransfiniteQuadMap) FUNCTION NewTransfiniteQuadMap( boundaryCurves, ownership )
!         ALGORITHM 98: SUBROUTINE EvaluateTransfiniteMapAt( this, xi, eta, res )
!
!      This version is modified so that the curves are in (x,y,z). 6/5/15, 3:44 PM
!      It also remove the derivative routines, since they are not needed in this project.
!
!////////////////////////////////////////////////////////////////////////
!
      MODULE TransfiniteMapClass
      USE CurveInterpolantClass
      IMPLICIT NONE 
!
!-------------------------------------------------------------------
! Defines data and methods for a transfinite interpolant
!-------------------------------------------------------------------
!
      TYPE TransfiniteQuadMap
         TYPE(CurveInterpolant), DIMENSION(:), POINTER :: boundaryCurves
         INTEGER                                       :: ownership
      END TYPE TransfiniteQuadMap
      
      INTERFACE Destruct
         MODULE PROCEDURE DestructTransfiniteQuadMap
      END INTERFACE Destruct
      
      INTEGER, PARAMETER :: MAP_OWNS_CURVES = 0, MAP_DOESNT_OWN_CURVES = 1
!
!     ========
      CONTAINS 
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      TYPE(TransfiniteQuadMap) FUNCTION NewTransfiniteQuadMap( boundaryCurves, ownership ) 
!
!-------------------------------------------------------------------
! Constructor for the transfinite map. Reads in and saves the four
! boundary curves.
!-------------------------------------------------------------------
!
         TYPE(CurveInterpolant), DIMENSION(:), POINTER :: boundaryCurves
         INTEGER, OPTIONAL                             :: ownership

         NewTransfiniteQuadMap%boundaryCurves => boundaryCurves

         IF( PRESENT(ownership) ) THEN
            NewTransfiniteQuadMap%ownership = ownership
         ELSE
            NewTransfiniteQuadMap%ownership = MAP_DOESNT_OWN_CURVES
         END IF 
      END FUNCTION NewTransfiniteQuadMap
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DestructTransfiniteQuadMap(this)
         TYPE(TransfiniteQuadMap) :: this
         INTEGER                  :: k
         
         IF( this%ownership == MAP_OWNS_CURVES ) THEN
            DO k = 1, 4 
               CALL Destruct( this%boundaryCurves(k) )
            END DO
            DEALLOCATE( this%boundaryCurves )
         END IF
         
         NULLIFY(this%boundaryCurves)
      END SUBROUTINE DestructTransfiniteQuadMap
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE EvaluateTransfiniteMapAt( this, xi, eta, res ) 
!
         TYPE(TransfiniteQuadMap)   :: this
         REAL(KIND=RP), INTENT(IN)  :: xi, eta
         REAL(KIND=RP), INTENT(OUT) :: res(3)
         
         REAL(KIND=RP) :: x(3,4), cX(3,4)
         INTEGER       :: k
!
!        -------------------------------
!        Evaluate positions along curves
!        -------------------------------
!
         CALL EvaluateAt( this%boundaryCurves(1), atLocation = -1.0_RP, givingResult = x(:,1) )
         CALL EvaluateAt( this%boundaryCurves(1), atLocation =  1.0_RP, givingResult = x(:,2) )
         CALL EvaluateAt( this%boundaryCurves(3), atLocation =  1.0_RP, givingResult = x(:,3) )
         CALL EvaluateAt( this%boundaryCurves(3), atLocation = -1.0_RP, givingResult = x(:,4) )
         
         CALL EvaluateAt( this%boundaryCurves(1), atLocation = xi , givingResult = cX(:,1) )
         CALL EvaluateAt( this%boundaryCurves(2), atLocation = eta, givingResult = cX(:,2) )
         CALL EvaluateAt( this%boundaryCurves(3), atLocation = xi , givingResult = cX(:,3) )
         CALL EvaluateAt( this%boundaryCurves(4), atLocation = eta, givingResult = cX(:,4) )
!
!        ----------------------------
!        Evaluate on reference square
!        ----------------------------
!
         DO k = 1, 3 
            res(k) = 0.5_RP*( (1.0_RP-xi)*cX(k,4) + (1+xi)*cX(k,2) + (1.0_RP-eta)*cX(k,1) + (1+eta)*cX(k,3) ) &
                   - 0.25_RP*( (1.0_RP-xi)*( (1.0_RP-eta)*x(k,1) + (1+eta)*x(k,4) ) &
                   + (1+xi)*( (1.0_RP-eta)*x(k,2) + (1+eta)*x(k,3) ))
         END DO
      
      END SUBROUTINE EvaluateTransfiniteMapAt
      
      END MODULE TransfiniteMapClass
      
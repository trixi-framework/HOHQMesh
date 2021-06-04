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
!      CurveInterpolantClass.f
!      Created: 2007-06-12 15:37:20 -0400 
!      By: David Kopriva
!
!      Contains: 
!         ALGORITHM 96: CurveInterpolant (Class)
!         ALGORITHM 97: CurveInterpolantProcedures
!            SUBROUTINE ConstructCurveInterpolant( this, N, nodes, values)
!            SUBROUTINE EvaluateAt( this, atLocation, givingResult )
!            SUBROUTINE DerivativeAt( this, atLocation, givingResult )
!
!      this version is modified so that the curves are defined in 3-space, (x,y,z). 6/5/15, 3:46 PM
!
!////////////////////////////////////////////////////////////////////////
!
      MODULE CurveInterpolantClass
      USE PolynomialInterpolationModule
      IMPLICIT NONE 
!
!---------------------------------------------------------------------
! This module defines a class that creates a curve from an interpolant
!---------------------------------------------------------------------
!
      TYPE CurveInterpolant
         INTEGER                    :: numberOfNodes
         REAL(KIND=RP), ALLOCATABLE :: nodes(:)
         REAL(KIND=RP), ALLOCATABLE :: values(:,:)
         REAL(KIND=RP), ALLOCATABLE :: bWeights(:)
      END TYPE CurveInterpolant
!
!     --------
!     Generics
!     --------
!
      INTERFACE Construct
         MODULE PROCEDURE ConstructCurveInterpolant
      END INTERFACE Construct
      INTERFACE Destruct
         MODULE PROCEDURE DestructCurveInterpolant
      END INTERFACE Destruct
!
!     ========
      CONTAINS 
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ConstructCurveInterpolant( this, N, nodes, values)
!
!-------------------------------------------------------------------
! Constructor pfor a curve interpolant
!-------------------------------------------------------------------
!
         TYPE(CurveInterpolant)         , INTENT(OUT) :: this
         INTEGER                        , INTENT(IN)  :: N
         REAL(KIND=RP), DIMENSION(0:N)  , INTENT(IN)  :: nodes
         REAL(KIND=RP), DIMENSION(0:N,3), INTENT(IN)  :: values
         
         ALLOCATE( this%nodes(0:N) )
         ALLOCATE( this%values(0:N,3) )
         ALLOCATE( this%bWeights(0:N) )
         
         this%numberOfNodes = N
         this%nodes         = nodes
         this%values        = values
         CALL BarycentricWeights( N, nodes, this%bWeights )
      
      END SUBROUTINE ConstructCurveInterpolant
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetValues( this, values )
!
!-------------------------------------------------------------------
! Constructor pfor a curve interpolant
!-------------------------------------------------------------------
!
         TYPE(CurveInterpolant)        , INTENT(INOUT) :: this
         REAL(KIND=RP), DIMENSION(0:,:), INTENT(IN)  :: values
         
         this%values        = values
      
      END SUBROUTINE SetValues
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DestructCurveInterpolant( this ) 
         TYPE(CurveInterpolant)       :: this
         IF(ALLOCATED(this%nodes))     DEALLOCATE( this%nodes )
         IF(ALLOCATED(this%values))    DEALLOCATE( this%values )
         IF(ALLOCATED(this%bWeights))  DEALLOCATE( this%bWeights )
      END SUBROUTINE DestructCurveInterpolant
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE EvaluateAt( this, atLocation, givingResult ) 
!
!     -------------------------------------------------------------------
!     Evaluate the interpolant of this at s returning v
!     -------------------------------------------------------------------
!
         TYPE(CurveInterpolant), INTENT(IN)  :: this
         REAL(KIND=RP)         , INTENT(IN)  :: atLocation
         REAL(KIND=RP)         , INTENT(OUT) :: givingResult(3)
         
         givingResult(1) = LagrangeInterpolation( atLocation, this%numberOfNodes, &
                           this%nodes, this%values(:,1), this%bWeights)
         givingResult(2) = LagrangeInterpolation( atLocation, this%numberOfNodes, &
                           this%nodes, this%values(:,2), this%bWeights)
         givingResult(3) = LagrangeInterpolation( atLocation, this%numberOfNodes, &
                           this%nodes, this%values(:,3), this%bWeights)
   
      END SUBROUTINE EvaluateAt
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DerivativeAt( this, atLocation, givingResult ) 
!
!     -------------------------------------------------------------------
!     Evaluate the derivative of the interpolant of this at atLocation
!     returning givingResult.
!     -------------------------------------------------------------------
!
         TYPE(CurveInterpolant), INTENT(IN)  :: this
         REAL(KIND=RP)         , INTENT(IN)  :: atLocation
         REAL(KIND=RP)         , INTENT(OUT) :: givingResult(3)
         
         givingResult(1) = LagrangeInterpolantDerivative( atLocation, this%numberOfNodes, &
                           this%nodes, this%values(:,1), this%bWeights)
         givingResult(2) = LagrangeInterpolantDerivative( atLocation, this%numberOfNodes, &
                           this%nodes, this%values(:,2), this%bWeights)
         givingResult(3) = LagrangeInterpolantDerivative( atLocation, this%numberOfNodes, &
                           this%nodes, this%values(:,3), this%bWeights)
  
      END SUBROUTINE DerivativeAt

      END MODULE CurveInterpolantClass

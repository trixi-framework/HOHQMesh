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
! FTObjectLibrary contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend,
! https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
!
! --- End License
!
!////////////////////////////////////////////////////////////////////////
!
!      BoundaryErrors.f90
!      Created: April 21, 2026 at 1:38 PM
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
   Module BoundaryErrorModule
   USE MeshProjectClass
   USE MultiSegmentNodalCurveClass
   USE MultiSegmentModalCurveClass
   USE LegendreAlgorithms
   IMPLICIT NONE

!  ========
   CONTAINS
!  ========
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE ComputeBoundaryErrors(project)
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(MeshProject), POINTER :: project
!
!     ---------------
!     Local Variables
!     ---------------
!
      TYPE(SMModel)                 , POINTER  :: model               !An alias
      CLASS(FTMutableObjectArray)   , POINTER  :: boundaryPolynomials !An alias
      CLASS(ObjectPointerWrapper)   , POINTER  :: modelChains(:)      !An alias

      CLASS(SMChainedCurve)         , POINTER  :: modelChain
      CLASS(MultiSegmentCurve)      , POINTER  :: boundaryPolynomial
      CLASS(SMCurve)                , POINTER  :: modelCurve, polyCurve
      CLASS(FTObject)               , POINTER  :: obj

      INTEGER                                  :: N, Ng, c
      INTEGER                                  :: m, j
      REAL(KIND=RP)                            :: t, dt
      REAL(KIND=RP)                            :: gTStart, gTEnd
      REAL(KIND=RP)                            :: e, eD, eL2Norm, eH1Norm, el2Max, eH1Max
      REAL(KIND=RP)                            :: xC(3), xP(3)
      REAL(KIND=RP)              , ALLOCATABLE :: nodes(:), weights(:)
      REAL(KIND=RP)                            :: arc
!
!     -------------------------------------------
!     Use Gauss quadrature to compute error norms
!     -------------------------------------------
!
      N  = project % runParams % polynomialOrder
      nG = N ! The gauss quadrature order.
      ALLOCATE(nodes(0:Ng), weights(0:Ng))
      CALL GaussLegendreNodesAndWeights( Ng, nodes, weights )
!
!     -------
!     Aliases
!     -------
!
      model               => project % model
      modelChains         => model % allChains
      boundaryPolynomials => project % boundaryPolynomialsArray
!
!     ------------------------------------
!     For each boundary curve in the model
!     ------------------------------------
!
      IF(ALLOCATED( project % L2BoundaryError)) DEALLOCATE(project % L2BoundaryError)
      IF(ALLOCATED( project % H1BoundaryError)) DEALLOCATE(project % H1BoundaryError)
      ALLOCATE(project % L2BoundaryError(model % numberOfChains()) )
      ALLOCATE(project % H1BoundaryError(model % numberOfChains()) )

      DO j = 1, model % numberOfChains()
         el2Max = -HUGE(1.0_RP)
         eH1Max = -HUGE(1.0_RP)
!
!        ------------------------------------------------------------------
!        Get the curve chain from the model, modelChain is the exact chain.
!        boundaryPolynomial is its approximation by a (PW) polynomial
!        which is saved in the boundaryPolynomialsArray of the
!        project at the same index.
!        ------------------------------------------------------------------
!
         obj => modelChains(j) % object
         CALL castToSMChainedCurve(obj, modelChain)
         obj => boundaryPolynomials % objectAtIndex(j)
         CALL castObjToMultiSegmentCurve(obj,boundaryPolynomial)
!
!        ---------------------------------------------
!        Walk through each segment in the polynomial
!        approximation to the chain, which corresponds
!        to an element edge along a boundary and
!        compute the pointwise, L2 and H1 error norms,
!        since it is convenient to do it all at once.
!        ---------------------------------------------
!
         ALLOCATE(project % L2BoundaryError(j) % array(boundaryPolynomial % nSegments) )
         ALLOCATE(project % H1BoundaryError(j) % array(boundaryPolynomial % nSegments) )

         DO c = 1, boundaryPolynomial % nSegments
!
            gTStart = boundaryPolynomial % cuts(c-1)
            gTEnd   = boundaryPolynomial % cuts(c)
            dt      = gTEnd - gTStart
!
!           -----------------------------------------------
!           Evaluate the model curve and polynomial curve
!           at the Gauss quadrature nodes. Write the errors
!           and compute the error norms at the same time.
!           -----------------------------------------------
!
            eL2Norm = 0.0_RP
            eH1Norm = 0.0_RP

            DO m = 0, Ng
               t = gTStart + dt*0.5_RP*(nodes(m) + 1.0_RP)
!
!              ------------------
!              Derivative error
!              ------------------
!
               xC      = modelChain % derivativeAt(t)
               xP      = boundaryPolynomial % derivativeAt(t)

               eD       = (xP(1)-xC(1))**2 + (xP(2)-xC(2))**2
!
!              --------------
!              Location error
!              --------------
!
               xC      = modelChain % positionAt(t)
               xP      = boundaryPolynomial % positionAt(t)
               e       = (xP(1)-xC(1))**2 + (xP(2)-xC(2))**2

               eL2Norm = eL2Norm + e*weights(m)
               eH1Norm = eH1Norm + eL2Norm + eD*weights(m)

            END DO

            arc = segmentArcLength(boundaryPolynomial, c, nodes, weights)

            eL2Norm = SQRT(0.5_RP*dt*eL2Norm/arc)
            eH1Norm = SQRT(0.5_RP*dt*eH1Norm/arc)

            project % L2BoundaryError(j) % array(c) = eL2Norm
            project % H1BoundaryError(j) % array(c) = eH1Norm

            el2Max  = MAX(el2Max, eL2Norm)
            eH1Max  = MAX(eH1Max, eH1Norm)
         END DO

         project % L2ErrorMax(j) = el2Max
         project % H1ErrorMax(j) = eH1Max

      END DO !All boundary chains

   END SUBROUTINE ComputeBoundaryErrors
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE WriteBoundaryErrors(project)
!
!  ----------------------------------------------------------------------------
!  Write the L2 and H1 errors within each segment along each boundary.
!  It writes to the path of the stats file name appended with the string
!  "_ErrorNorms" provided in the control file unless the value for the
!  stats file name is "none" or not included.
!
!  The format is:
!
!  "Number of boundary curves = " # boundary curves
!  For each boundary curve
!     Boundary name,  # Segments
!     For each segment
!        t_{start}  x_{start}  y_{start}  t_{end}  x_{end}  y_{end}  L2Error  H1Error
!     end
!  end
!
!  where
!  t_{start}           = start parametrization for the segment
!  t_{end}             = end parametrization for the segment
!  x_{start},y_{start} = physical space location of segment start
!  x_{end},y_{end}     = physical space location of segment end
!  L2Error             = L2Error of the segment
!  H1Error             = H1 Error of the segment
!
!  The format is redundant in that it duplicates the start and end points,
!  but should make it easier to read the file and draw from it.
!  ----------------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(MeshProject), POINTER :: project
!
!     ---------------
!     Local Variables
!     ---------------
!
      TYPE(SMModel)                 , POINTER  :: model               !An alias
      CLASS(FTMutableObjectArray)   , POINTER  :: boundaryPolynomials !An alias
      TYPE (ObjectPointerWrapper)   , POINTER  :: modelChains(:)      !An alias
      CLASS(FTObject)               , POINTER  :: obj
      CLASS(SMChainedCurve)         , POINTER  :: modelChain
      CLASS(MultiSegmentCurve)      , POINTER  :: boundaryPolynomial

      CHARACTER(DEFAULT_CHARACTER_LENGTH)      :: str
      REAL(KIND=RP)                            :: gTStart, gTEnd
      REAL(KIND=RP)                            :: xs(3), xe(3)
      REAL(KIND=RP)                            :: eL2Norm, eH1Norm
      INTEGER                                  :: normUnit
      INTEGER                                  :: m, j, c
!
!     ----------------------------------------------------------------------
!     If stats are not requested then boundary errors are not written either
!     ----------------------------------------------------------------------
!
      IF ( project % runParams % statsFileName == "none" )     RETURN
!
!     -------
!     Aliases
!     -------
!
      model               => project % model
      modelChains         => model % allChains
      boundaryPolynomials => project % boundaryPolynomialsArray
!
!     --------------------------
!     Where to write the results
!     --------------------------
!
      m = INDEX(STRING = project % runParams % statsFileName, SUBSTRING = ".")
      IF ( m == -1 )     THEN
         OPEN(NEWUNIT = normUnit, FILE = project % runParams % statsFileName //"_ErrorNorms")
      ELSE
         str = project % runParams % statsFileName
         str = str(1:m-1) //"_ErrorNorms.txt"
         OPEN(NEWUNIT = normUnit, FILE = str)
      END IF
!
!     ----------------------------
!     Compute the results to write
!     ----------------------------
!
      CALL ComputeBoundaryErrors(project)
!
!     ----------
!     Write them
!     ----------
!
      WRITE(normUnit,*) "Number of boundary curves = ", model % numberOfChains()
      DO j = 1, model % numberOfChains()
         obj => modelChains(j) % object
         CALL castToSMChainedCurve(obj, modelChain)

         obj => boundaryPolynomials % objectAtIndex(j)
         CALL castObjToMultiSegmentCurve(obj,boundaryPolynomial)

         WRITE(normUnit,*) TRIM(modelChain % curveName()), ",", boundaryPolynomial % nSegments

         DO c = 1, boundaryPolynomial % nSegments
            gTStart = boundaryPolynomial % cuts(c-1)
            gTEnd   = boundaryPolynomial % cuts(c)
            eL2Norm = project % L2BoundaryError(j) % array(c)
            eH1Norm = project % H1BoundaryError(j) % array(c)
            xs      = boundaryPolynomial % positionAt(gTStart)
            xe      = boundaryPolynomial % positionAt(gTEnd)

            WRITE(normUnit,*) gTStart, xs(1:2), gTEnd, xe(1:2), eL2Norm, eH1Norm

         END DO

      END DO

      CLOSE(normUnit)

   END SUBROUTINE WriteBoundaryErrors

   END Module BoundaryErrorModule

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
      CLASS(FTMutableObjectArray)   , POINTER  :: modelChains         !An alias
      
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
         obj => modelChains % objectAtIndex(j)
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
!              --------------
!              Location error
!              --------------
!
               xC      = modelChain % positionAt(t)
               xP      = boundaryPolynomial % positionAt(t)
               e       = (xP(1)-xC(1))**2 + (xP(2)-xC(2))**2
               eL2Norm = eL2Norm + e*weights(m)
!
!              ------------------
!              Derivative error  
!              ------------------
!
               xC      = modelChain % derivativeAt(t)
               xP      = boundaryPolynomial % derivativeAt(t)

               eD      = (xP(1)-xC(1))**2 + (xP(2)-xC(2))**2
               eH1Norm = eH1Norm + eL2Norm + eD*weights(m)
               
            END DO

            eL2Norm = SQRT(0.5_RP*dt*eL2Norm)
            eH1Norm = SQRT(0.5_RP*dt*eH1Norm)
            
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
      CLASS(FTMutableObjectArray)   , POINTER  :: modelChains         !An alias
      CLASS(FTObject)               , POINTER  :: obj
      CLASS(SMChainedCurve)         , POINTER  :: modelChain
      CLASS(MultiSegmentCurve)      , POINTER  :: boundaryPolynomial
      
      CHARACTER(DEFAULT_CHARACTER_LENGTH)      :: str
      REAL(KIND=RP)                            :: gTStart, gTEnd
      REAL(KIND=RP)                            :: eL2Norm, eH1Norm
      INTEGER                                  :: normUnit
      INTEGER                                  :: m, j, c
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
      m = INDEX(STRING = project % runParams % errorFileName, SUBSTRING = ".")
      IF ( m == -1 )     THEN
         OPEN(NEWUNIT = normUnit, FILE = project % runParams % errorFileName //"_Norms")
      ELSE 
         str = project % runParams % errorFileName
         str = str(1:m-1) //"_Norms.txt"
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
      DO j = 1, model % numberOfChains()
         obj => modelChains % objectAtIndex(j)
         CALL castToSMChainedCurve(obj, modelChain)
         WRITE(normUnit,*) TRIM(modelChain % curveName())
         
         obj => boundaryPolynomials % objectAtIndex(j)
         CALL castObjToMultiSegmentCurve(obj,boundaryPolynomial)
         
         DO c = 1, boundaryPolynomial % nSegments
            gTStart = boundaryPolynomial % cuts(c-1)
            gTEnd   = boundaryPolynomial % cuts(c)
            eL2Norm = project % L2BoundaryError(j) % array(c)
            eH1Norm = project % H1BoundaryError(j) % array(c)
            
            WRITE(normUnit,*) 0.5_RP*(gTStart + gTEnd), LOG10(eL2Norm + 1.0d-15), LOG10(eH1Norm + 1.0d-15)
            
         END DO
         
      END DO
      
      CLOSE(normUnit)
      
   END SUBROUTINE WriteBoundaryErrors
   
   END Module BoundaryErrorModule

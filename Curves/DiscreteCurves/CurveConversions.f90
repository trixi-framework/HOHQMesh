!
!////////////////////////////////////////////////////////////////////////
!
!      CurveConversions.f95
!      Created: 2010-08-25 13:15:14 -0400 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module CurveConversionsModule
      USE SMChainedCurveClass
      USE ChainedSegmentedCurveClass
      USE SegmentedCurveArrayClass
      IMPLICIT NONE
!
!     ========
      CONTAINS 
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION allocAndInitSegmentedChainFromChain( chain, h, id ) RESULT(SegmentedChain)
         IMPLICIT NONE
!
!        ----------------------------------------------
!        Takes a chain of continuous curves and returns
!        a discrete version of it.
!        ----------------------------------------------
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMChainedCurve)       , POINTER :: chain
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedChain
         REAL(KIND=RP)                         :: h
         INTEGER                               :: id
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMCurve)         , POINTER :: cCurve   => NULL()
         CLASS(FRSegmentedCurve), POINTER :: frsCurve => NULL()
         INTEGER                          :: k
         
         ALLOCATE(segmentedChain)
         CALL segmentedChain % initwithNameAndID(chain % curveName(),id)
         
         DO k = 1, chain % COUNT()
            cCurve => chain % curveAtIndex(k)
            ALLOCATE(frsCurve)
            CALL frsCurve % initWithCurve(cCurve, h, cCurve % id())
            CALL segmentedChain % add(frsCurve)
            CALL frsCurve % release()
         END DO
         
         CALL segmentedChain % complete()
         CALL ComputeBoundingBox( segmentedChain )
      
      END FUNCTION allocAndInitSegmentedChainFromChain       
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION allocateAndInitSegmentedCurveFromSegmentedChain(segmentedChain)  RESULT(segmentedCurve)
!
!        ---------------------------------------------------------------------------
!        The incoming ChainedSegmentedCurve has an array of FRSegementedCurve's that
!        match at their endpoints. Thus there are duplicates at the joints. This
!        function takes the points out of the chain and puts them into a single
!        curve without the duplicates. The argument field (t) is set to zero since
!        it has no meaning in this conversion.
!        ---------------------------------------------------------------------------
!
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedChain
         CLASS(SegmentedCurveArray)  , POINTER :: segmentedCurve
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTobject)        , POINTER           :: obj      => NULL()
         CLASS(FRSegmentedCurve), POINTER           :: frsCurve => NULL()
         INTEGER                                    :: nCurves, nSegmentedPoints, nSegments
         INTEGER                                    :: j, k, n
         REAL(KIND=RP), DIMENSION(:)  , ALLOCATABLE :: t
         REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE :: x
         REAL(KIND=RP)                              :: tt = 0.0_RP, xx(3)
         
         nCurves          = segmentedChain % curvecount()
         nSegmentedPoints = segmentedChain % nodeCount()
         nSegments        = nSegmentedPoints - nCurves - 1
         
         ALLOCATE( x(3,0:nSegments), t(0:nSegments) )
!
!        ----------------------------------------------------------------------
!        Extract the points from the chain, skipping the duplicates at the ends
!        ----------------------------------------------------------------------
!
         n = 0
         DO k = 1, nCurves
            obj => segmentedChain % chain % objectAtIndex(k)
            CALL castToSegmentedCurve(obj,frsCurve)
            DO j = 1, frsCurve % COUNT() - 1
               xx     = frsCurve % positionAtIndex(j)
               tt     = frsCurve % argumentAtIndex(j)
               x(:,n) = xx
               t(n)   = tt
               n      = n + 1
            END DO  
         END DO  
!
!        -------------------------------------------
!        Generate the segmentedCurve from the points
!        -------------------------------------------
!
         ALLOCATE(segmentedCurve)
         CALL segmentedCurve % initWithNumberOfSegmentsNameAndID( nSegments, &
                                 segmentedChain % curveName, segmentedChain % id )
         CALL segmentedCurve % setPoints( t, x )
         CALL segmentedCurve % setboundingBox(segmentedChain % boundingBox)
         
      END FUNCTION allocateAndInitSegmentedCurveFromSegmentedChain
      
      
      END Module CurveConversionsModule
      
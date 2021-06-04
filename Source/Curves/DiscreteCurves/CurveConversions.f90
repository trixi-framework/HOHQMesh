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
      FUNCTION allocAndInitSegmentedChainFromChain( chain, h, controls, id ) RESULT(SegmentedChain)
         USE SizerControls
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
         CLASS(FTLinkedList)         , POINTER :: controls
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
            CALL frsCurve % initWithCurve(cCurve, h, controls, cCurve % id())
            CALL segmentedChain % add(frsCurve)
            CALL releaseFRSegmentedCurve(self = frsCurve)
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
      
!
!////////////////////////////////////////////////////////////////////////
!
!      Sizer.f90
!      Created: August 15, 2013 1:01 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      MODULE MeshSizerClass
      USE SMConstants
      USE ChainedSegmentedCurveClass
      USE ProgramGlobals
      USE Geometry
      USE FTLinkedListClass
      USE FTLinkedListIteratorClass
      USE SizerControls
      
      IMPLICIT NONE
!
!     ---------------------
!     Class type definition
!     ---------------------
!
      TYPE, EXTENDS(FTObject) :: MeshSizer
         INTEGER                               :: noOfInnerBoundaries
         INTEGER                               :: noOfInterfaceBoundaries
         REAL(KIND=RP)                         :: baseSize
         REAL(KIND=RP)                         :: xMin(3), xMax(3)
         CLASS(FTLinkedList)         , POINTER :: controlsList            => NULL()
         CLASS(ChainedSegmentedCurve), POINTER :: outerBoundary           => NULL()
         CLASS(FTLinkedList)         , POINTER :: innerBoundariesList     => NULL()
         CLASS(FTLinkedList)         , POINTER :: interfaceBoundariesList => NULL()
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithProperties => initMeshSizer
         PROCEDURE :: destruct           => destructMeshSizer
         PROCEDURE :: printDescription   => printSizerDescription
         PROCEDURE :: addSizerCenterControl
         PROCEDURE :: addSizerLineControl
         PROCEDURE :: addBoundaryCurve
         PROCEDURE :: sizeFunctionMinimumOnBox
         
      END TYPE MeshSizer
!
!     ========
      CONTAINS 
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initMeshSizer( self, baseSize, xMin, xMax )
         IMPLICIT NONE  
         CLASS(MeshSizer) :: self
         REAL(KIND=RP)    :: baseSize(3), xMin(3), xMax(3)
         
         CALL self % FTObject % init()
         
         self % baseSize                = MAXVAL(baseSize)
         self % noOfInnerBoundaries     = 0
         self % noOfInterfaceBoundaries = 0
         self % xMin                    = xMin
         self % xMax                    = xMax
         
         self % outerBoundary           => NULL()
         self % innerBoundariesList     => NULL()
         self % interfaceBoundariesList => NULL()
         
         ALLOCATE(self % controlsList)
         CALL self % controlsList % init()
         
      END SUBROUTINE initMeshSizer
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructMeshSizer(self)  
         IMPLICIT NONE  
         CLASS(MeshSizer) :: self
         
         IF ( ASSOCIATED(self % controlsList) )     THEN
            CALL self % controlsList % release()
            IF ( self % controlsList % isUnreferenced() )     THEN
               DEALLOCATE(self % controlsList) 
               self % controlsList => NULL()
            END IF 
         END IF 
         
         IF ( ASSOCIATED(self % innerBoundariesList) )     THEN
            CALL self % innerBoundariesList % release()
            IF ( self % innerBoundariesList % isUnreferenced() )     THEN
               DEALLOCATE(self % innerBoundariesList) 
               self % innerBoundariesList => NULL()
            END IF 
         END IF 
         
         IF ( ASSOCIATED(self % interfaceBoundariesList) )     THEN
            CALL self % interfaceBoundariesList % release()
            IF ( self % interfaceBoundariesList % isUnreferenced() )     THEN
               DEALLOCATE(self % interfaceBoundariesList) 
               self % interfaceBoundariesList => NULL()
            END IF 
         END IF 
         
         IF ( ASSOCIATED(self % outerBoundary) )     THEN
            CALL self % outerBoundary % release()
            IF ( self % outerBoundary % isUnreferenced() )     THEN
               DEALLOCATE(self % outerBoundary) 
               self % outerBoundary => NULL()
            END IF 
         END IF 
         
         CALL self % controlsList % release()
         DEALLOCATE(self % controlsList)
         
         CALL self % FTObject % destruct()
         
      END SUBROUTINE destructMeshSizer
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addSizerCenterControl(self,center)  
         IMPLICIT NONE
         CLASS(MeshSizer)                   :: self
         CLASS(SizerCenterControl), POINTER :: center
         CLASS(FTObject)          , POINTER :: obj => NULL()
         
         IF ( .NOT.ASSOCIATED(self % controlsList) )     THEN
            ALLOCATE(self % controlsList)
            CALL self % controlsList % init() 
         END IF
         
         obj => center
         CALL self % controlsList % add(obj)
         
      END SUBROUTINE addSizerCenterControl
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addSizerLineControl(self,line)  
         IMPLICIT NONE
         CLASS(MeshSizer)                 :: self
         CLASS(SizerLineControl), POINTER :: line
         CLASS(FTObject)        , POINTER :: obj => NULL()
         
         IF ( .NOT.ASSOCIATED(self % controlsList) )     THEN
            ALLOCATE(self % controlsList)
            CALL self % controlsList % init() 
         END IF
         
         obj => line
         CALL self % controlsList % add(obj)
         
      END SUBROUTINE addSizerLineControl
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addBoundaryCurve( self, segmentedCurve, curveType )  
         IMPLICIT NONE  
         CLASS(MeshSizer)                      :: self
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedCurve
         INTEGER                               :: curveType ! = INNER, OUTER, INTERIOR_INTERFACE
         CLASS(FTObject), POINTER              :: obj => NULL()
         
         SELECT CASE ( curveType )
         
            CASE( INNER ) 
            
               IF ( .NOT.ASSOCIATED(self % innerBoundariesList) )     THEN
                  ALLOCATE(self % innerBoundariesList)
                  CALL self %  innerBoundariesList % init()
               END IF 
               
               obj => segmentedCurve
               CALL self %  innerBoundariesList % add(obj)
               self % noOfInnerBoundaries = self % noOfInnerBoundaries + 1
               
            CASE( INTERIOR_INTERFACE )
            
               IF ( .NOT.ASSOCIATED(self % interfaceBoundariesList) )     THEN
                  ALLOCATE(self % interfaceBoundariesList)
                  CALL self %  interfaceBoundariesList % init()
               END IF 
               
               obj => segmentedCurve
               CALL self %  interfaceBoundariesList % add(obj)
               self % noOfInterfaceBoundaries = self % noOfInterfaceBoundaries + 1
               
            CASE( OUTER )
            
               self % outerBoundary => segmentedCurve
               CALL self % outerBoundary % retain()
            CASE DEFAULT 
         END SELECT 
         
      END SUBROUTINE addBoundaryCurve
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION sizeFunctionMinimumOnBox( sizer, xMin, xMax ) RESULT(hMin)
         !TODO: Should switch to a good minimization algorithm. Use linear search for now.
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MeshSizer) :: sizer
         REAL(KIND=RP)    :: xMin(3), xMax(3)
         REAL(KIND=RP)    :: hMin
!
!        ---------------
!        local variables
!        ---------------
!
         REAL(KIND=RP)                    :: dX(3)
         REAL(KIND=RP)                    :: x(3), boundingBox(6)
         REAL(KIND=RP)                    :: cSize, aSize
         REAL(KIND=RP)                    :: cHeight, cWidth, cDim
         INTEGER                          :: i,j, nX(3) = [10,10,0]
         
         CLASS(FTLinkedListIterator) , POINTER :: iterator => NULL()
         CLASS(FTObject)             , POINTER :: obj => NULL()
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedCurveChain => NULL()
!
!        -----------------------------------------
!        Sizes determined by centers/line controls
!        -----------------------------------------
!
         dX = (xMax - xMin)/nX
         hMin = HUGE(hMin)
         DO j = 0, nX(2)
            x(2) = xMin(2) + j*dx(2)
            DO i = 0, nX(1)
               x(1) = xMin(1) + i*dx(1)
               hMin = MIN( hMin, controlSize(sizer,x) )
            END DO
         END DO
!
!        ------------------------------------
!        Sizes given by curves in the domain.
!        ------------------------------------
!
         cSize = TINY(cSize)
         IF( ASSOCIATED(sizer%outerBoundary) )     THEN
            cSize = MAX( cSize, invCurveSizeForBox(sizer % outerBoundary, xMin, xMax ) )
         END IF
         
         IF ( ASSOCIATED( sizer % innerBoundariesList) )     THEN
            CALL cSizeForCurvesInList(sizer % innerBoundariesList, cSize, xMin, xMax)
         END IF 
         
         IF( ASSOCIATED(sizer % interfaceBoundariesList) )     THEN
            CALL cSizeForCurvesInList(sizer % interfaceBoundariesList, cSize, xMin, xMax)
         END IF
         cSize = 1.0_RP/cSize
!
!        ------------------------------------------------------
!        Sizes given by areas of the outer and interface curves
!        ------------------------------------------------------
!
         aSize = HUGE(aSize)
         IF( ASSOCIATED(sizer % outerBoundary) )     THEN
            boundingBox = sizer % outerBoundary % boundingBox
            IF(Point_IsInsideBox(xMin, boundingBox) .OR. &
               Point_IsInsideBox(xMax, boundingBox)) THEN

               cHeight = Height( sizer % outerBoundary )
               cWidth  = Width ( sizer % outerBoundary )
               cDim    = MIN(cHeight,cWidth)
               aSize   = cDim/minNumberOfElementsInsideArea
            END IF
         END IF
         
         IF ( ASSOCIATED(sizer % interfaceBoundariesList) )     THEN
            ALLOCATE(iterator)
            CALL iterator % initwithFTLinkedList(sizer % interfaceBoundariesList)
            CALL iterator % setToStart
            DO WHILE (.NOT.iterator % isAtEnd())
               obj => iterator % object()
               CALL castToChainedSegmentedCurve(obj,segmentedCurveChain)
               
               IF(Point_IsInsideBox(xMin,segmentedCurveChain % boundingBox) .OR. &
                  Point_IsInsideBox(xMax,segmentedCurveChain % boundingBox)) THEN
                  
                  cHeight = Height( segmentedCurveChain )
                  cWidth  = Width ( segmentedCurveChain )
                  cDim    = MIN(cHeight,cWidth)
                  aSize   = MIN(cDim/minNumberOfElementsInsideArea,aSize)
               END IF
   
               CALL iterator % moveToNext()
            END DO
            CALL iterator % release()
            DEALLOCATE(iterator)
         END IF 
!
!        -------------------------
!        Final choice for the size
!        -------------------------
!
         hMin = MIN( hMin, cSize, sizer%baseSize, aSize )
      
      END FUNCTION sizeFunctionMinimumOnBox
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE cSizeForCurvesInList( list, cSize, xMin, xMax )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList), POINTER :: list
         REAL(KIND=RP)                :: cSize
         REAL(KIND=RP)                :: xMin(3), xMax(3)
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedListIterator) , POINTER :: iterator => NULL()
         CLASS(FTObject)             , POINTER :: obj => NULL()
         CLASS(ChainedSegmentedCurve), POINTER :: segmentedCurveChain => NULL()

         ALLOCATE(iterator)
         CALL iterator % initWithFTLinkedList(list)
         CALL iterator % setToStart()
         
         DO WHILE (.NOT.iterator % isAtEnd())
            obj => iterator % object()
            CALL castToChainedSegmentedCurve(obj,segmentedCurveChain)
            cSize = MAX( cSize, invCurveSizeForBox(segmentedCurveChain, xMin, xMax ) )
            CALL iterator % moveToNext()
         END DO
         
         CALL iterator % release()
         DEALLOCATE(iterator)
   
      END SUBROUTINE cSizeForCurvesInList
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION controlSize( self, x ) 
      
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MeshSizer) :: self
         REAL(KIND=RP)    :: x(3)
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()
         CLASS(SizerCenterControl)  , POINTER :: center => NULL()
         CLASS(SizerLineControl)    , POINTER :: line => NULL()
         REAL(KIND=RP)                        :: hFunInv
         
         hFunInv = 1.0_RP/self%baseSize
!
!        --------------------------------
!        Sizes given by centers and lines
!        --------------------------------
!
         IF ( ASSOCIATED(self % controlsList) )     THEN
         
            ALLOCATE(iterator)
            CALL iterator % initwithFTLinkedList( self % controlsList)
            CALL iterator % setToStart()
            
            DO WHILE (.NOT.iterator % isAtEnd())
               obj => iterator % object()
               SELECT TYPE(obj)
                  TYPE IS (SizerCenterControl)
                     center  => obj
                     hFunInv = MAX(hFunInv , hInvForCenter( center, x ) )
                  TYPE IS (sizerLineControl)
                     line => obj
                     hFunInv = MAX(hFunInv , hInvForLineControl( line, x ) )
                  CLASS DEFAULT
                  
               END SELECT 
               
               CALL iterator % moveToNext()
            END DO
            
            CALL iterator % release()
            DEALLOCATE(iterator)
            
         END IF 
!
!        -------------------------
!        Compute the size function
!        -------------------------
!
         controlSize = 1.0_RP/hFunInv
         
      END FUNCTION controlSize
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION invCurveSizeForBox(chain, xMin, xMax ) 
      USE Geometry
      IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP)                         :: xMin(3), xMax(3)
         CLASS(ChainedSegmentedCurve), POINTER :: chain
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP)                    :: nodes(3,4), x(3)
         CLASS(FRSegmentedCurve), POINTER :: c => NULL()
         INTEGER                          :: k, N, j
         REAL(KIND=RP)                    :: s
         
         nodes(:,1) = xMin
         nodes(:,2) = (/xMax(1),xMin(2), 0.0_RP/)
         nodes(:,3) = xMax
         nodes(:,4) = (/xMin(1),xMax(2), 0.0_RP/)
         
         invCurveSizeForBox = 0.0_RP
         
         N = chain % curveCount()
         DO k = 1, N
            c => chain % segmentedCurveAtIndex(k)
            DO j = 1, c % COUNT()
               x = c % positionAtIndex(j) 
               IF ( PointInQuad(nodes,x) )     THEN
                  s = c % invScaleAtIndex(j)
                  invCurveSizeForBox =  MAX(s, invCurveSizeForBox) 
               END IF 
            END DO
         END DO
         
      END FUNCTION invCurveSizeForBox
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeCurveDistanceScales( self )
!
!     -----------------------------------------------------------
!     Save the distances between the different curves in the mesh
!     -----------------------------------------------------------
!
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MeshSizer), POINTER :: self
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE SizerCurvePtr
            CLASS(ChainedSegmentedCurve), POINTER :: curve => NULL()
         END TYPE SizerCurvePtr
         
         TYPE(SizerCurvePtr)        , DIMENSION(:), ALLOCATABLE :: innerCurvesArray
         CLASS(FTLinkedListIterator), POINTER                   :: iterator => NULL()
         CLASS(FTObject)            , POINTER                   :: obj => NULL()
         
         CLASS(ChainedSegmentedCurve), POINTER                  :: innerSegmentedCurveChain => NULL()  ,&
                                                                   outerSegmentedCurveChain => NULL()
         CLASS(FRSegmentedCurve)     , POINTER                  :: innerSegment, outerSegment
         REAL(KIND=RP)                                          :: x(3), y(3), d, outerInvScale, innerInvScale
         REAL(KIND=RP)                                          :: nHatInner(3), nHatOuter(3), dot
         REAL(KIND=RP)              , DIMENSION(4)              :: A = (/0.0_RP,1.0_RP,0.0_RP,1.0_RP/)
         REAL(KIND=RP)              , DIMENSION(4)              :: B = (/1.0_RP,0.0_RP,1.0_RP,0.0_RP/)
         REAL(KIND=RP)              , DIMENSION(4)              :: C
         INTEGER                                                :: i, j, k, l, m, N, nSegments
         INTEGER                                                :: numberOfBoundaries
         
         REAL(KIND=RP), DIMENSION(3,4) :: nHatBox = &
         RESHAPE((/0.0_RP,-1.0_RP,0.0_RP,1.0_RP,0.0_RP,0.0_RP,0.0_RP,1.0_RP,0.0_RP,-1.0_RP,0.0_RP,0.0_RP/),(/3,4/))
!
!        ------------------------------------------------------
!        For convenience, save the inner boundaries in an array
!        ------------------------------------------------------
!
         numberOfBoundaries = self % noOfInnerBoundaries + self % noOfInterfaceBoundaries
         
         IF( numberOfBoundaries > 0 ) ALLOCATE( innerCurvesArray(numberOfBoundaries) )
         
         k = 1
         IF( self % noOfInnerBoundaries > 0 )     THEN
            ALLOCATE(iterator)
            
            CALL iterator % initwithFTLinkedList(self % innerBoundariesList)
            CALL iterator % setToStart()
            DO WHILE (.NOT.iterator % isAtEnd())
               obj     => iterator % object()
               CALL castToChainedSegmentedCurve(obj,innerSegmentedCurveChain)
               innerCurvesArray(k) % curve => innerSegmentedCurveChain
               k = k + 1
               CALL iterator % moveToNext()           
            END DO  
            CALL iterator % release()
            DEALLOCATE(iterator)
         END IF 
            
         IF ( self % noOfInterfaceBoundaries > 0 )     THEN
            ALLOCATE(iterator)
            
            CALL iterator % initwithFTLinkedList(self % interfaceBoundariesList)
            CALL iterator % setToStart
            DO WHILE (.NOT.iterator % isAtEnd())
               obj     => iterator % object()
               CALL castToChainedSegmentedCurve(obj,innerSegmentedCurveChain)
               innerCurvesArray(k) % curve => innerSegmentedCurveChain
               k                           = k + 1
               CALL iterator % moveToNext()           
            END DO  
            CALL iterator % release()
            DEALLOCATE(iterator)
         END IF
!
!        -----------------------------------------------
!        Find the distances of the outer boundary curve 
!        to all inner curves
!        -----------------------------------------------
!
         IF ( ASSOCIATED(self%outerBoundary) .AND. numberOfBoundaries > 0 )     THEN
         
            N = self % outerBoundary % numberOfCurvesInChain
!
!           -----------------------------------------
!           For each segment in the outer curve chain
!           -----------------------------------------
!
            DO j = 1, N
               outerSegment => self % outerBoundary % segmentedCurveAtIndex(j)
               nSegments    =  outerSegment % COUNT()
!
!              --------------------------------------
!              For each point along the outer segment
!              --------------------------------------
!
               DO i = 1, nSegments
                  x             = outerSegment % positionAtIndex(i)
                  outerInvScale = outerSegment % invScaleAtIndex(i)
                  nHatOuter     = outerSegment % normalAtIndex(i)
!
!                 -----------------------
!                 For each inner boundary
!                 -----------------------
!   
                  DO k = 1, numberOfBoundaries
                     innerSegmentedCurveChain => innerCurvesArray(k) % curve
!
!                    --------------------------------------
!                    For each segment along the inner chain
!                    --------------------------------------
!
                     DO l = 1, innerSegmentedCurveChain % curveCount()
                        innerSegment => innerSegmentedCurveChain % segmentedCurveAtIndex(l)
!
!                       -----------------------------------------------
!                       For each point along the inner segemented curve
!                       -----------------------------------------------
!
                        DO m = 1, innerSegment % COUNT()
                           y             = innerSegment % positionAtIndex(m)
                           innerInvScale = innerSegment % invScaleAtIndex(m)
                           nHatInner     = innerSegment % normalAtIndex(m)
                           
                           d = closeCurveFactor/SQRT( (x(1) - y(1))**2 + (x(2) - y(2))**2 ) ! Inverse length - 3 cells
!
!                          -----------------------------------------------------------
!                          Curves that are close and face each other should have their
!                          mesh sizes adjusted to have enough between them
!                          -----------------------------------------------------------
!
                           dot = DOT_PRODUCT(nHatInner,nHatOuter)
                           
                           IF( dot > closeCurveNormalAlignment )     THEN
                              outerInvScale = MAX(d,outerInvScale)
                              innerInvScale = MAX(d,innerInvScale)
                              CALL outerSegment % setCurveInvScaleForIndex(outerInvScale,i)
                              CALL innerSegment % setCurveInvScaleForIndex(innerInvScale,m)
                           END IF
                           
                        END DO  
                     END DO  
                  END DO  
               END DO
            END DO  
            
         ELSE IF ( numberOfBoundaries > 0 )     THEN
!
!           ------------------------------------------------------
!           There are inner curves but no outer curves, just a box
!           ------------------------------------------------------
!
            C(1) = self%xMin(2)
            C(2) = self%xMax(1)
            C(3) = self%xMax(2)
            C(4) = self%xMin(1)

            DO m = 1, 4
               nHatOuter = nHatBox(:,m)
!
!              -----------------------------------
!              For each inner boundary curve chain
!              -----------------------------------
!
               DO k = 1, numberOfBoundaries
                  innerSegmentedCurveChain => innerCurvesArray(k) % curve
!
!                 -----------------------------
!                 For each segment in the chain
!                 -----------------------------
!
                  DO l = 1, innerSegmentedCurveChain % curveCount()
                     innerSegment => innerSegmentedCurveChain % segmentedCurveAtIndex(l)
!
!                    --------------------------------
!                    For each point along the segment
!                    --------------------------------
!
                     DO j = 1, innerSegment % COUNT()
                        y             = innerSegment % positionAtIndex(j)
                        innerInvScale = innerSegment % invScaleAtIndex(j)
                        nHatInner     = innerSegment % normalAtIndex(j)
                     
                        d = ABS( A(m)*y(1) + B(m)*y(2) + C(m) ) !/SQRT(A(m)**2 + B(m)**2)
                        d = closeCurveFactor/d ! Inverse length - 3 cells
!
                        dot = DOT_PRODUCT(nHatInner,nHatOuter)
                        IF( dot > closeCurveNormalAlignment )     THEN
                           innerInvScale = MAX(d,innerInvScale)
                           CALL innerSegment % setCurveInvScaleForIndex(innerInvScale,j)
                        END IF
                     END DO 
                  END DO
               END DO
            END DO
         END IF
!
!        ------------------------------------
!        Do the same for the inner boundaries
!        The difference here is that facing
!        boundaries have normals that are
!        opposite to each other.
!        ------------------------------------
!
         IF ( numberOfBoundaries > 1 )     THEN
!
!           -----------------------------------
!           For each inner boundary curve chain
!           -----------------------------------
!
            DO k = 1, numberOfBoundaries
               outerSegmentedCurveChain => innerCurvesArray(k) % curve
!
!              -----------------------------
!              For each segment in the chain
!              -----------------------------
!
               DO l = 1, outerSegmentedCurveChain % curveCount()
                  outerSegment => outerSegmentedCurveChain % segmentedCurveAtIndex(l)
!
!                 --------------------------------
!                 For each point along the segment
!                 --------------------------------
!
                  DO j = 1, outerSegment % COUNT()
                     x             = outerSegment % positionAtIndex(j)
                     outerInvScale = outerSegment % invScaleAtIndex(j)
                     nHatOuter     = outerSegment % normalAtIndex(j)
!
!                    ------------------------------
!                    For each of the *other* chains
!                    ------------------------------
!
                     DO m = k+1, numberOfboundaries
                        innerSegmentedCurveChain => innerCurvesArray(m) % curve
!
!                       -----------------------------------
!                       For each segment in the other chain
!                       -----------------------------------
!
                        DO n = 1, innerSegmentedCurveChain % curveCount()
                           innerSegment => innerSegmentedCurveChain % segmentedCurveAtIndex(n)
!
!                          --------------------------------------
!                          For each point along the other segment
!                          --------------------------------------
!
                           DO i = 1, innerSegment % COUNT()
                              y             = innerSegment % positionAtIndex(i)
                              innerInvScale = innerSegment % invScaleAtIndex(i)
                              nHatInner     = innerSegment % normalAtIndex(i)
                              
                              d = closeCurveFactor/SQRT( (x(1) - y(1))**2 + (x(2) - y(2))**2 ) ! Inverse length - 3 cells
!
                              dot = DOT_PRODUCT(nHatInner,nHatOuter)

                              IF( dot < -closeCurveNormalAlignment )     THEN
                                 outerInvScale = MAX(d,outerInvScale)
                                 CALL outerSegment % setCurveInvScaleForIndex(outerInvScale,j)
                                 innerInvScale = MAX(d,innerInvScale)
                                 CALL innerSegment % setCurveInvScaleForIndex(innerInvScale,i)
                              END IF
                           END DO  
                        END DO  
                     END DO  
                  END DO
               END DO  
            END DO

         END IF
!
!        -------
!        Cleanup
!        -------
!
         IF(ALLOCATED(innerCurvesArray)) DEALLOCATE( innerCurvesArray )
         
      END SUBROUTINE ComputeCurveDistanceScales      
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ComputeInterfaceCurveScales( self )
!
!     -----------------------------------------------------------
!     Use interface curve bounding boxes to ensure that enough
!     elements are placed inside an interface curve.
!     -----------------------------------------------------------
!
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MeshSizer) :: self
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(ChainedSegmentedCurve), POINTER :: chain => NULL()
         CLASS(FRSegmentedCurve)     , POINTER :: segment => NULL()
         REAL(KIND=RP)                         :: cScale, cHeight, cWidth, cSize, invScale
         REAL(KIND=RP)                         :: segmentInvScale
         CLASS(FTLinkedListIterator) , POINTER :: iterator => NULL()
         CLASS(FTObject)             , POINTER :: obj => NULL()
         INTEGER                               :: k, j
!
         IF ( self % noOfInterfaceBoundaries == 0 )     RETURN
         
         ALLOCATE(iterator)
         CALL iterator % initWithFTLinkedList(self % interfaceBoundariesList)
         CALL iterator % setToStart
         DO WHILE (.NOT.iterator % isAtEnd())
            obj => iterator % object()
            CALL castToChainedSegmentedCurve(obj,chain)
            
            cHeight = Height( chain )
            cWidth  = Width ( chain )
            cSize   = MIN(cHeight,cWidth)
!
!           --------------------------------------------------------
!           Require at least minNumberOfElementsInsideCurve elements
!           --------------------------------------------------------
!
            cScale = cSize/ minNumberOfElementsInsideArea
            DO k = 1, chain % curveCount()
               segment => chain % segmentedCurveAtIndex(k)
               DO j = 1, segment % COUNT()
                  segmentInvScale = segment % invScaleAtIndex(j) 
                  invScale        = MAX(1.0_RP/cScale,segmentInvScale)
                  CALL segment % setCurveInvScaleForIndex(invScale,j)
               END DO
            END DO  
            
            CALL iterator % moveToNext()
         END DO  
         CALL iterator % release()
         DEALLOCATE(iterator)
               
      END SUBROUTINE ComputeInterfaceCurveScales
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printSizerDescription(self,iUnit)  
         IMPLICIT NONE  
         CLASS(MeshSizer) :: self
         INTEGER          :: iUnit
         
         WRITE(iUnit,*) "MeshSizer object"
      END SUBROUTINE printSizerDescription
      END MODULE MeshSizerClass

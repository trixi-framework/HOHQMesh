!
!////////////////////////////////////////////////////////////////////////
!
!      FRSegmentedCurveClass
!!      Created: 2010-08-18 16:58:26 -0400 
!!      By: David Kopriva  
!!
!!
!!      ------------------------------------------------------------------
!!      This defines a fixed resolution segmented curve class that
!!      automatically chooses the nodes so that the curve is well-resolved
!!      ------------------------------------------------------------------
!!
!!     Usage:
!!
!!        *Creation*
!!
!!           ALLOCATE(curve)
!!           CALL curve % initWithCurve(ContinuousCurve, h, id)
!!           CALL curve % initWithCurveFunction(curveExternalFunction, h, curveName, id)
!!
!!        *Reversing direction*
!!
!!           CALL ReverseFRSegmentedCurve( curve )
!!
!!        *Destruction*
!!
!!           CALL ReleaseFRSegmentedCurve( curve )
!!
!!        *Getting location*
!!
!!           x    = curve % positionAtIndex( j )
!!           t    = curve % argumentAtIndex( j )
!!           c    = curve % invScaleAtIndex( j )
!!           nHat = curve % normalAtIndex( j )
!!
!!        *Setters*
!!
!!           CALL curve % setCurveInvScaleForIndex( s,j )
!!
!!        *Printing*
!!
!!           CALL curve % printDescription( iUnit )
!!
!!        *Number of nodes in the curve*
!!
!!           N = curve % count()
!
!////////////////////////////////////////////////////////////////////////
!
      Module FRSegmentedCurveClass
      USE SMCurveClass
      USE SMConstants
      USE FTLinkedListClass
      USE FTLinkedListIteratorClass
      USE ObjectArrayAdditionsModule
      USE FTMutableObjectArrayClass
      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      INTEGER, PARAMETER :: FRSEGMENTED_CURVE_NAME_LENGTH = 32
!
!     ------------------------
!     Linked list node records
!     ------------------------
!
      TYPE, EXTENDS(FTObject) :: SegmentedCurveNode
         REAL(KIND=RP) :: x(3), nHat(3), invScale, t
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initSegmentedCurveNode
      END TYPE SegmentedCurveNode
!
!     ----------
!     Class type
!     ----------
!
      TYPE, EXTENDS(FTObject) :: FRSegmentedCurve
         INTEGER                                      :: id
         INTEGER                                      :: direction
         LOGICAL                                      :: isCircular
         CHARACTER(LEN=FRSEGMENTED_CURVE_NAME_LENGTH) :: curveName
         REAL(KIND=RP)                                :: h
         CLASS(FTMutableObjectArray), POINTER         :: nodeArray => NULL()
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: destruct         => DestructFRSegmentedCurve
         PROCEDURE :: printDescription => printFRSegmentedCurve;
         PROCEDURE :: reverse          => reverseFRSegmentedCurve
         PROCEDURE :: COUNT            => FRSegmentedCurveCount
         PROCEDURE :: initWithCurve
         PROCEDURE :: setCurveInvScaleForIndex
         PROCEDURE :: positionAtIndex
         PROCEDURE :: argumentAtIndex
         PROCEDURE :: invScaleAtIndex
         PROCEDURE :: normalAtIndex
         
      END TYPE FRSegmentedCurve
      
      PRIVATE :: RefineFRSegmentedCurve
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initSegmentedCurveNode( self, x, t )
         CLASS( SegmentedCurveNode ) :: self
         REAL(KIND=RP)               :: x(3), t
         
         CALL self  %  FTObject  %  init()
         
         self % x        = x
         self % t        = t
         self % invScale = HUGE(t)
         
      END SUBROUTINE initSegmentedCurveNode
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE setInvScale(self,s)  
         IMPLICIT NONE  
         CLASS( SegmentedCurveNode ), POINTER :: self
         REAL(KIND=RP)                        :: s
         self % invScale = s
      END SUBROUTINE  
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initWithCurve( self, theCurve, h, id )
         USE Geometry, ONLY:Curvature
         USE ProgramGlobals, ONLY:curvatureFactor, INNER, ROW_SIDE
         USE FTMutableObjectArrayClass
!
!     ------------------------------------------------
!     Construct the curve given a Curve class instance
!     ------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve)          :: self
         CLASS(SMCurve)         , POINTER :: theCurve
         REAL(KIND=RP)                    :: h
         INTEGER                          :: id
!
!        ---------------
!        Local Variables
!        ---------------
!
         CLASS(SegmentedCurveNode), POINTER     :: left => NULL(), right => NULL(), middle => NULL(), p => NULL()
         CLASS(FTLinkedListRecord), POINTER     :: leftRecord => NULL(), rightRecord => NULL(), middleRecord => NULL()
         REAL(KIND=RP)                          :: t, x(3)
         REAL(KIND=RP)                          :: xL(3), xM(3), xR(3), tL, tM, tR, c, s
         REAL(KIND=RP)                          :: xPrimeL(3), xPrimeR(3), xPrime(3), xDoublePrime(3)
         REAL(KIND=RP)                          :: norm
         INTEGER                                :: j, N, jointType
         LOGICAL                                :: isCircular
         CLASS(FTObject)            , POINTER   :: obj   => NULL()
         CLASS(FTLinkedList)        , POINTER   :: nodes => NULL()
!
!        ----
!        Self
!        ----
!
         CALL self % FTobject % init()
         
         self % h          = h
         self % curveName  = theCurve % curveName()
         self % id         = id
         self % isCircular = .FALSE.
         
         ALLOCATE(self % nodeArray)
!
!        -----
!        Nodes
!        -----
!
         ALLOCATE(nodes)
         CALL nodes % init()
         ALLOCATE(left, right, middle)
         
         t = 0.0_RP
         x = theCurve % positionAt( t )
         CALL left % initSegmentedCurveNode( x, t )
         obj => left
         CALL nodes % add(obj)
         CALL left % release()
         leftRecord => nodes % tail
                  
         t  = 0.5_RP
         x = theCurve % positionAt( t )
         CALL middle % initSegmentedCurveNode( x, t )
         obj => middle
         CALL nodes % add(obj)
         CALL middle % release()
         middleRecord => nodes % tail

         t = 1.0_RP
         x = theCurve % positionAt( t )
         CALL right % initSegmentedCurveNode( x, t )
         obj => right
         CALL nodes % add(obj)
         CALL right % release()
         rightRecord => nodes % tail
!
!        -------------------
!        Refine as necessary
!        -------------------
!
         CALL RefineFRSegmentedCurve( nodes, leftRecord  , left  , middle, h, theCurve )
         CALL RefineFRSegmentedCurve( nodes, middleRecord, middle, right , h, theCurve )
!
!        ---------------------------------------------
!        Re-Evaluate the curvatures on the final curve
!        Convert the list to a array for easy access.
!        ---------------------------------------------
!
         CALL initArrayWithLinkedList( self % nodeArray, nodes )
         N = self % nodeArray % COUNT()
!
!        ----------------------------
!        See if the curve is circular
!        ----------------------------
!
         isCircular = .FALSE.
         obj => self % nodeArray % objectAtIndex(1)
         CALL castToSegmentedCurveNode(obj,left)
         obj => self % nodeArray % objectAtIndex(N)
         CALL castToSegmentedCurveNode(obj,right)
         x = right % x - left % x
         t = MAXVAL(ABS(x))
         IF ( t < 100*EPSILON(1.0_RP) )     THEN
            self % isCircular = .TRUE. 
         END IF
         
         IF ( self % isCircular )     THEN
            jointType = JointClassification(theCurve,theCurve,INNER)
         END IF 
!
!        ----------------------
!        Compute the curvatures
!        ----------------------
!
         DO j = 1, N
            obj => self % nodeArray % objectAtIndex(j)
            CALL castToSegmentedCurveNode(obj,p)
            
            IF ( j == 1 )     THEN
               obj => self % nodeArray % objectAtIndex(1)
               CALL castToSegmentedCurveNode(obj,left)
               
               obj => self % nodeArray % objectAtIndex(2)
               CALL castToSegmentedCurveNode(obj,right)
               xL           = left  % x
               xR           = right % x
               tL           = left  % t
               tR           = right % t
               xPrime       = (xR - xL)/(tR - tL)
               xDoublePrime = 0.0_RP
            ELSE IF (j == N)     THEN 
               obj => self % nodeArray % objectAtIndex(N-1)
               CALL castToSegmentedCurveNode(obj,left)
               obj => self % nodeArray % objectAtIndex(N)
               CALL castToSegmentedCurveNode(obj,right)
               xL           = left  % x
               xR           = right % x
               tL           = left  % t
               tR           = right % t
               xPrime       = (xR - xL)/(tR - tL)
               xDoublePrime = 0.0_RP
            ELSE
               xM = p % x
               tM = p % t
               obj => self % nodeArray % objectAtIndex(j-1)
               CALL castToSegmentedCurveNode(obj,left)
               obj => self % nodeArray % objectAtIndex(j+1)
               CALL castToSegmentedCurveNode(obj,right)
               xL           = left  % x
               xR           = right % x
               tL           = left  % t
               tR           = right % t
               xPrimeR      = (xR - xM)/(tR - tM)
               xPrimeL      = (xM - xL)/(tM - tL)
               xPrime       = 0.5*(xPrimeR + xPrimeL)
               xDoublePrime = 2.0_RP*(xPrimeR - xPrimeL)/(tR - tL)
            END IF 
            
            c = Curvature( xPrime, xDoublePrime )
            s = curvatureFactor/MAX( 1.0_RP/h, c ) !Choose the smaller of h or radius of curvature          
            p % invScale = 1.0_RP/s
            norm         = SQRT(xPrime(1)**2 + xPrime(2)**2)
            p % nHat(1)  =  xPrime(2)/norm
            p % nHat(2)  = -xPrime(1)/norm
            p % nHat(3)  = 0.0_RP
         END DO
!
!        -------------------------------------------
!        Match ends if the curve is a closed loop
!        and if there is no sharp corner. Otherwise,
!        extend the curvature from the second and
!        next to last points.
!        -------------------------------------------
!
         IF ( self % isCircular .AND. jointType == ROW_SIDE )     THEN
         
            obj => self % nodeArray % objectAtIndex(1)
            CALL castToSegmentedCurveNode(obj,p)
            
            xM = p % x
            tM = p % t

            obj => self % nodeArray % objectAtIndex(N-1)
            CALL castToSegmentedCurveNode(obj,left)
            obj => self % nodeArray % objectAtIndex(2)
            CALL castToSegmentedCurveNode(obj,right)
            
            xL           = left  % x
            xR           = right % x
            tL           = left  % t - 1.0_RP
            tR           = right % t
            xPrimeR      = (xR - xM)/(tR - tM)
            xPrimeL      = (xM - xL)/(tM - tL)
            xPrime       = 0.5*(xPrimeR + xPrimeL)
            xDoublePrime = 2.0_RP*(xPrimeR - xPrimeL)/(tR - tL)

            c = Curvature( xPrime, xDoublePrime )
            s = curvatureFactor/MAX( 1.0_RP/h, c ) !Choose the smaller of h or radius of curvature          
            p % invScale = 1.0_RP/s
            norm         = SQRT(xPrime(1)**2 + xPrime(2)**2)
            p % nHat(1)  =  xPrime(2)/norm
            p % nHat(2)  = -xPrime(1)/norm
            p % nHat(3)  = 0.0_RP

            obj => self % nodeArray % objectAtIndex(N)
            CALL castToSegmentedCurveNode(obj,left)
            
            left % x        = p % x
            left % nHat     = p % nHat
            left % invScale = p % invScale
         ELSE
            obj => self % nodeArray % objectAtIndex(1)
            CALL castToSegmentedCurveNode(obj,p)

            obj => self % nodeArray % objectAtIndex(2)
            CALL castToSegmentedCurveNode(obj,right)
            
            p % nHat     = right % nHat
            p % invScale = right % invScale
            
            obj => self % nodeArray % objectAtIndex(N)
            CALL castToSegmentedCurveNode(obj,p)

            obj => self % nodeArray % objectAtIndex(N-1)
            CALL castToSegmentedCurveNode(obj,left)
            
            p % nHat     = left % nHat
            p % invScale = left % invScale

         END IF 

!
!        --------
!        Clean up
!        --------
!
         CALL nodes % release()
         DEALLOCATE(nodes)
!
      END SUBROUTINE initWithCurve
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DestructFRSegmentedCurve( self )
         CLASS( FRSegmentedCurve )      :: self

         CALL self % nodeArray % release()
         IF ( self % nodeArray % isUnreferenced() )     THEN
            DEALLOCATE(self % nodeArray) 
         END IF 
      
      END SUBROUTINE DestructFRSegmentedCurve
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION FRSegmentedCurveCount(self)  RESULT(n)
         IMPLICIT NONE  
         CLASS( FRSegmentedCurve )  :: self
         INTEGER                    :: n
         n = self % nodeArray % COUNT() 
      END FUNCTION FRSegmentedCurveCount
!
!////////////////////////////////////////////////////////////////////////
!
      RECURSIVE SUBROUTINE RefineFRSegmentedCurve( list, leftRecord, left, right, h, theCurve ) 
         USE Geometry, ONLY: Curvature
         USE ProgramGlobals, ONLY:numCurvePoints
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList)      , POINTER :: list
         CLASS(FTLinkedListRecord), POINTER :: leftRecord
         CLASS(SegmentedCurveNode), POINTER :: left, right
         CLASS(SMCurve)                     :: theCurve
         REAL(KIND=RP)                      :: h
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)          , POINTER :: obj          => NULL()
         CLASS(SegmentedCurveNode), POINTER :: middle       => NULL()
         CLASS(FTLinkedListRecord), POINTER :: middleRecord => NULL()
         
         REAL(KIND=RP) :: xL(3), xR(3), x(3), xM(3), xPrime(3), xDoublePrime(3)
         REAL(KIND=RP) :: tL, tR, d, t, dt, c, s, tM
         
         xL = left % x
         xR = right % x
         tL = left % t
         tR = right % t
!
!        ------------------------------------------------------
!        Compute the radius of curvature for these three points
!        ------------------------------------------------------
!
         dt = 0.5_RP*(tR - tL)
         tM = tL + dt
         xM = theCurve % positionAt( tM )
         
         xPrime       = 0.5_RP*(xR - xL)/dt             
         xDoublePrime = (xR - 2*xM + xL)/dt**2
              
         c = Curvature( xPrime, xDoublePrime )
         
         s = 1.0_RP/MAX( 1.0_RP/h, c ) !Choose the smaller of h or radius of curvature          
         d = SQRT( (xR(1)-xL(1))**2 + (xR(2)-xL(2))**2 )
         !TODO must get the curves accurately
         IF( d > s .OR. dt > 1.0_RP/numCurvePoints )     THEN
            t = tL + 0.5_RP*(tR - tL)
            
            ALLOCATE(middle)
            x = theCurve % positionAt( t )
            
            CALL middle % initSegmentedCurveNode( x, t )
            
            obj => middle
            CALL list % insertObjectAfterRecord(obj,leftRecord)
            middleRecord => leftRecord % next
                        
            CALL RefineFRSegmentedCurve( list, leftRecord  , left  , middle, h, theCurve )
            CALL RefineFRSegmentedCurve( list, middleRecord, middle, right , h, theCurve )
         END IF
         
      END SUBROUTINE RefineFRSegmentedCurve
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReverseFRSegmentedCurve( self )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve)               :: self
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTMutableObjectArray), POINTER  :: newNodes  => NULL()
         CLASS(FTObject)            , POINTER  :: obj       => NULL()
         INTEGER                               :: N, j
!         
         N = self % nodeArray % COUNT()
         ALLOCATE(newNodes)
         CALL newNodes % initWithSize(N)
         
         DO j = 1, N
            obj => self % nodeArray % objectAtIndex(N-j+1)
            CALL newNodes % addObject(obj)
         END DO
         
         CALL self % nodeArray % release()
         IF ( self % nodeArray % isUnreferenced() )     THEN
            DEALLOCATE(self % nodeArray)
            self % nodeArray => NULL() 
         END IF 
         
         self % nodeArray => newNodes
         
      END SUBROUTINE ReverseFRSegmentedCurve
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE PrintFRSegmentedCurve( self, iUnit )
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve) :: self
         INTEGER                 :: iUnit
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)            , POINTER :: objectPtr => NULL()
         CLASS(SegmentedCurveNode)  , POINTER :: node      => NULL()
         INTEGER                              :: N, j
         
         N = self % nodeArray % COUNT()
         
         DO j = 1, N
            objectPtr => self % nodeArray % objectAtIndex(j)
            CALL castToSegmentedCurveNode(objectPtr,node)
            WRITE(iUnit,*) node % x, node % nHat, node % invScale
         END DO  
         WRITE(iUnit,*) "---------------------------------"

      END SUBROUTINE PrintFRSegmentedCurve
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE setCurveInvScaleForIndex( self, s, j )
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve) :: self
         REAL(KIND=RP)           :: s
         INTEGER                 :: j
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)            , POINTER :: objectPtr => NULL()
         CLASS(SegmentedCurveNode)  , POINTER :: node      => NULL()
         
         objectPtr => self % nodeArray % objectAtIndex(j)
         CALL castToSegmentedCurveNode(objectPtr,node)
         
         CALL setInvScale(node,s) !Gets around an optimizer bug in gfortran 4.8.0
!         node % invScale = s ! Yes, it took a long time to find it.

      END SUBROUTINE setCurveInvScaleForIndex
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION positionAtIndex(self,j)  RESULT(x)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve) :: self
         INTEGER                 :: j
         REAL(KIND=RP)           :: x(3)
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)            , POINTER :: objectPtr => NULL()
         CLASS(SegmentedCurveNode)  , POINTER :: node      => NULL()

         objectPtr => self % nodeArray % objectAtIndex(j)
         CALL castToSegmentedCurveNode(objectPtr,node)
         
         x = node % x
      END FUNCTION positionAtIndex
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION normalAtIndex(self,j)  RESULT(x)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve) :: self
         INTEGER                 :: j
         REAL(KIND=RP)           :: x(3)
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)            , POINTER :: objectPtr => NULL()
         CLASS(SegmentedCurveNode)  , POINTER :: node      => NULL()

         objectPtr => self % nodeArray % objectAtIndex(j)
         CALL castToSegmentedCurveNode(objectPtr,node)
         
         x = node % nHat
      END FUNCTION normalAtIndex
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION invScaleAtIndex(self,j)  RESULT(x)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve) :: self
         INTEGER                 :: j
         REAL(KIND=RP)           :: x
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)            , POINTER :: objectPtr  => NULL()
         CLASS(SegmentedCurveNode)  , POINTER :: node       => NULL()

         objectPtr => self % nodeArray % objectAtIndex(j)
         CALL castToSegmentedCurveNode(objectPtr,node)
         
         x = node % invScale
      END FUNCTION invScaleAtIndex
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION argumentAtIndex(self,j)  RESULT(x)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FRSegmentedCurve) :: self
         INTEGER                 :: j
         REAL(KIND=RP)           :: x
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)            , POINTER :: objectPtr => NULL()
         CLASS(SegmentedCurveNode)  , POINTER :: node      => NULL()

         objectPtr => self % nodeArray % objectAtIndex(j)
         CALL castToSegmentedCurveNode(objectPtr,node)
         
         x = node % t
      END FUNCTION argumentAtIndex
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToSegmentedCurveNode(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)          , POINTER :: obj
         CLASS(SegmentedCurveNode), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            CLASS IS(SegmentedCurveNode)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castToSegmentedCurveNode
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToSegmentedCurve(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)        , POINTER :: obj
         CLASS(FRSegmentedCurve), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            CLASS IS(FRSegmentedCurve)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castToSegmentedCurve
      
      
      END Module FRSegmentedCurveClass
      
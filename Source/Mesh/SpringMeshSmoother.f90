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
!      SpringMeshSmoother.f90
!      Created: 2010-10-05 09:54:22 -0400 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module SpringMeshSmootherClass 
      USE SMMeshClass
      USE SMModelClass
      USE MeshBoundaryMethodsModule
      USE MeshSmootherClass
      IMPLICIT NONE 
!
!     ---------
!     Constants
!     ---------
!
      INTEGER, PARAMETER :: LINEAR_SPRING = 0, CROSS_SPRING = 1, BOTH = 2
!
!     ---------------------
!     Class type definition
!     ---------------------
!
      TYPE, EXTENDS(MeshSmoother) :: SpringMeshSmoother
         REAL(KIND=RP) :: springConstant     ! Spring constant
         REAL(KIND=RP) :: restLength         ! Natural LENGTH
         REAL(KIND=RP) :: dampingCoefficient ! Damping coefficient
         REAL(KIND=RP) :: mass               ! Node mass
         REAL(KIND=RP) :: deltaT
         INTEGER       :: springType
         INTEGER       :: numSmoothingSteps
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: init       => initSmoother
         FINAL     :: DestructSpringMeshSmoother
         PROCEDURE :: smoothMesh => SpringSmoothMesh
      END TYPE SpringMeshSmoother
!
!     ----------------------
!     Other type definitions
!     ----------------------
!
      TYPE SpringSmootherParameters
         LOGICAL       :: smoothingOn
         REAL(KIND=RP) :: springConstant     ! Spring constant
         REAL(KIND=RP) :: restLength         ! Natural length
         REAL(KIND=RP) :: dampingCoefficient ! Damping coefficient
         REAL(KIND=RP) :: mass               ! Node mass
         REAL(KIND=RP) :: deltaT
         INTEGER       :: numSteps
         INTEGER       :: springType
      END TYPE SpringSmootherParameters
!
!     ---------------------------
!     Module arrays for smoothing
!     ---------------------------
!
      REAL(KIND=RP), DIMENSION(:,:), ALLOCATABLE, PRIVATE :: nodeAcceleration, nodeVelocity
!
!     Default parameters
!
      REAL(KIND=RP), PRIVATE :: defaultSpringConstant           = 1.0_RP
      REAL(KIND=RP), PRIVATE :: defaultSpringMass               = 1.0_RP
      REAL(KIND=RP), PRIVATE :: defaultSpringRestLength         = 0.0_RP
      REAL(KIND=RP), PRIVATE :: defaultSpringDampingCoefficient = 5.0_RP
      REAL(KIND=RP), PRIVATE :: defaultSpringTimeStep           = 0.1_RP
      INTEGER      , PRIVATE :: defaultSpringIterations         = 15
!
!     ========
      CONTAINS 
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initSmoother( self, springConstant, mass, restLength, &
                               dampingCoefficient, springType, deltaT, numSteps  )
         IMPLICIT NONE  
         CLASS(SpringMeshSmoother) :: self
         REAL(KIND=RP)             :: springConstant, restLength
         REAL(KIND=RP)             :: dampingCoefficient, mass, deltaT
         INTEGER                   :: springType, numSteps
         
         self % springConstant     = springConstant
         self % restLength         = restLength
         self % dampingCoefficient = dampingCoefficient
         self % springType         = springType
         self % mass               = mass
         self % deltaT             = deltaT
         self % numSmoothingSteps  = numSteps
      END SUBROUTINE initSmoother
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE DestructSpringMeshSmoother( self ) 
         IMPLICIT NONE 
         TYPE(SpringMeshSmoother) :: self
         IF(self % restLength >= 0.0)     CONTINUE 
         !Do nothing
      END SUBROUTINE DestructSpringMeshSmoother
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SpringSmoothMesh( self, mesh, model, errorCode )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS (SpringMeshSmoother)    :: self
         TYPE  (SMMesh)      , POINTER :: mesh
         TYPE  (SMModel)     , POINTER :: model
         INTEGER                       :: errorCode
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                      :: k, nodeID, N
         REAL(KIND=RP)                :: maxV
         CLASS(SMNode)      , POINTER :: node => NULL()
         CLASS(FTObject)    , POINTER :: obj => NULL()
         CLASS(FTLinkedList), POINTER :: boundaryNodesList => NULL()
!
!        -------------------------
!        Allocate temporary memory
!        -------------------------
!
         CALL renumberObjects(mesh,NODES) ! To guarantee sync.
         N = mesh % nodes % COUNT() 
         ALLOCATE( nodeAcceleration(3,N) )
         ALLOCATE( nodeVelocity(3,N) )
         
         nodeVelocity     = 0.0_RP
         nodeAcceleration = 0.0_RP
         
         CALL mesh % nodesIterator % setToStart()
         DO WHILE ( .NOT.mesh % nodesIterator % isAtEnd() )
            obj => mesh % nodesIterator % object()
            CALL cast(obj,node)
            node  %  activeStatus = ACTIVE
            CALL mesh % nodesIterator % moveToNext()
         END DO
         CALL SetNodeActiveStatus( mesh, model, errorCode )
!
!        ------------------------
!        Do spring-mass smoothing
!        ------------------------
!
         DO k = 1, self % numSmoothingSteps
            nodeAcceleration = 0.0_RP
            CALL AccumulateAcceleration( self, mesh )
            CALL ConstrainBoundaryNodes( self, model, mesh  %  nodesIterator )
            
            maxV = 0.0_RP
            CALL mesh % nodesIterator % setToStart()
            DO WHILE ( .NOT.mesh % nodesIterator % isAtEnd() )
               obj                => mesh % nodesIterator % object()
               CALL cast(obj,node)
               nodeID                 = node % id
               node % x               = node % x + self % deltaT*nodeVelocity(:,nodeID)
               nodeVelocity(:,nodeID) = nodeVelocity(:,nodeID) + &
                                        self % deltaT*( nodeAcceleration(:,nodeID) - &
                                        self % dampingCoefficient*nodeVelocity(:,nodeID) )
               CALL mesh % nodesIterator % moveToNext()
               maxV = MAX(maxV, ABS(nodeVelocity(1,nodeID)), ABS(nodeVelocity(2,nodeID)) )
            END DO

         END DO
!
!        ---------------------------
!        Deallocate temporary Memory
!        ---------------------------
!
         DEALLOCATE( nodeAcceleration )
         DEALLOCATE( nodeVelocity )
!
!        --------------------------------------------------------------------
!        The boundary nodes have been moved. Find their parametrized location
!        along their respective boundaries
!        --------------------------------------------------------------------
!
         ALLOCATE(boundaryNodesList)
         CALL boundaryNodesList % init()
         CALL CollectBoundaryAndInterfaceNodes( mesh % nodesIterator, boundaryNodesList )
         CALL FindCurveLocationsforNodes      ( boundaryNodesList, model )
         CALL releaseFTLinkedList(self = boundaryNodesList)
      
      END SUBROUTINE SpringSmoothMesh
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE LinearSpringForce( springConstant, restLength, selfLoc, neighborLoc, force )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP)              , INTENT(IN)  :: springConstant, restLength
         REAL(KIND=RP), DIMENSION(3), INTENT(IN)  :: selfLoc, neighborLoc
         REAL(KIND=RP), DIMENSION(3), INTENT(OUT) :: force
!
!        ---------------
!        Local variables
!        ---------------
!
         REAL(KIND=RP), DIMENSION(3) ::  v
         REAL(KIND=RP)               :: vNorm
         
         v     = neighborLoc - selfLoc
         vNorm = SQRT( v(1)*v(1) + v(2)*v(2) ) + restLength
         force = springConstant*v
         
      END SUBROUTINE LinearSpringForce
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE AccumulateAcceleration( self, mesh ) 
         IMPLICIT NONE
         TYPE (SpringMeshSmoother) :: self
         TYPE (SMMesh)       :: mesh
         
         IF ( self % springType == LINEAR_SPRING )     THEN
            CALL LinearAcceleration( self, mesh )
         ELSE IF ( self % springType == CROSS_SPRING )   THEN
            CALL CrossAcceleration( self, mesh )
         ELSE IF ( self % springType == BOTH )     THEN 
            CALL LinearAcceleration( self, mesh )
            CALL CrossAcceleration( self, mesh )
         END IF
      
      END SUBROUTINE AccumulateAcceleration
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE LinearAcceleration( self, mesh ) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SpringMeshSmoother) :: self
         TYPE (SMMesh)       :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                              :: id1, id2
         REAL(KIND=RP)                        :: force(3), x1(3), x2(3)
         CLASS(SMEdge)              , POINTER :: edge => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()
         
         iterator => mesh % edgesIterator
         CALL iterator % setToStart()
         
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL cast(obj,edge)
            
            x1  = edge % nodes(1) % node % x
            x2  = edge % nodes(2) % node % x
            id1 = edge % nodes(1) % node % id
            id2 = edge % nodes(2) % node % id
            CALL LinearSpringForce( self % springConstant, self % restLength, x1, x2, force )
            
            IF( edge % nodes(1)  %  node  %  bCurveID >= NONE )     THEN
               nodeAcceleration(:,id1) = nodeAcceleration(:,id1) + force/self % mass
            END IF
            IF( edge % nodes(2) % node % bCurveID >= NONE )     THEN
               nodeAcceleration(:,id2) = nodeAcceleration(:,id2) - force/self % mass
            END IF
            
            CALL iterator % moveToNext()
         END DO
         
      END SUBROUTINE LinearAcceleration
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CrossAcceleration( self, mesh ) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE (SpringMeshSmoother) :: self
         TYPE (SMMesh)       :: mesh
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER                              :: k, cK, nID
         REAL(KIND=RP)                        :: force(3), x1(3), x2(3)
         CLASS(SMNode)              , POINTER :: node1 => NULL(), node2 => NULL()
         CLASS(SMElement)           , POINTER :: e => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         CLASS(FTObject)            , POINTER :: obj => NULL()
         
         iterator => mesh % elementsIterator
         CALL iterator % setToStart()
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL cast(obj,e)
            DO k = 1, 4 
               obj => e % nodes % objectAtIndex(k)
               CALL cast(obj,node1)
               IF( HasFixedPosition( node1 ) )     CYCLE ! Is on boundary. 
               nID = node1 % id
               cK  = k + 2
               IF( cK > 4 ) cK = cK - 4
               
               obj => e % nodes % objectAtIndex(cK)
               CALL cast(obj,node2)
               
               x1 = node1 % x
               x2 = node2 % x
               CALL LinearSpringForce( self % springConstant, self % restLength, x1, x2, force )
               nodeAcceleration(:,nID) = nodeAcceleration(:,nID) + 0.5_RP*force/self % mass
            END DO
            
            CALL iterator % moveToNext()
         END DO
         
      END SUBROUTINE CrossAcceleration
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ConstrainBoundaryNodes( self, model, nodeIterator )  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS (SpringMeshSmoother)            :: self
         TYPE  (FTLinkedListIterator), POINTER :: nodeIterator
         TYPE  (SMModel)             , POINTER :: model
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMNode)        , POINTER :: node => NULL()
         CLASS(SMChainedCurve), POINTER :: chain => NULL()
         CLASS(SMCurve)       , POINTER :: c => NULL()
         CLASS(FTObject)      , POINTER :: obj => NULL()
         INTEGER                        :: id
         REAL(KIND=RP)                  :: tangent(3)

         CALL nodeIterator % setToStart()
         DO WHILE ( .NOT.nodeIterator % isAtEnd() )
            obj => nodeIterator % object()
            CALL cast(obj,node)

            IF (node  %  activeStatus == INACTIVE)     THEN
               nodeAcceleration(:,node  %  id) = 0.0_RP
               
            ELSE IF ( node % distToBoundary == 0.0_RP .AND. node  %  bCurveID > NONE )     THEN
              c                      => model % curveWithID( node  %  bCurveID, chain )
              tangent                =  c % tangentAt( node  %  whereOnBoundary )
              id                     =  node  %  id
              nodeAcceleration(:,id) = (nodeAcceleration(1,id)*tangent(1) + &
                                        nodeAcceleration(2,id)*tangent(2))*tangent
            END IF 
            CALL nodeIterator % moveToNext()
         END DO
         
         IF(self % restLength >= 0.0_RP)     CONTINUE 
         
      END SUBROUTINE ConstrainBoundaryNodes
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SetSpringSmootherBlock( smootherDict, smp ) 
         USE FTValueDictionaryClass
         USE ErrorTypesModule
!
!        Example block is:
!
!         \begin{Smoother}
!            smoothing            = "ON" (optional)
!            smoothing type       = "linearSpring"
!            spring constant      = 1.0 (optional)
!            mass                 = 1.0 (optional)
!            rest length          = 1.0 (optional)
!            damping coefficient  = 5.0 (optional)
!            number of iterations = 25 (optional)
!            time step            = 0.1 (optional)
!         \end{Smoother}
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTValueDictionary), POINTER :: smootherDict
         TYPE(SpringSmootherParameters)    :: smp
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=32)          :: str
         REAL(KIND=RP), EXTERNAL    :: GetRealValue
         INTEGER      , EXTERNAL    :: GetIntValue
!
!        -------------------
!        Smoothing On or OFF
!        -------------------
!
         str = "ON"
         IF ( smootherDict % containsKey(key = "smoothing") )     THEN
            str = smootherDict % stringValueForKey(key = "smoothing", requestedLength = 32) 
         END IF 
         
         IF( str == "ON" )     THEN
            smp%smoothingON = .true.
         ELSE
            smp%smoothingON = .false.
         END IF
!
!        -----------------
!        Type of smoothing 
!        -----------------
!
         IF ( smootherDict % containsKey(key = "smoothing type") )     THEN
            str = smootherDict % stringValueForKey(key = "smoothing type", requestedLength = 32) 
         ELSE 
            str = "LinearAndCrossBarSpring"
            CALL ThrowErrorExceptionOfType(poster = "SetSpringSmootherBlock", &
                                           msg    = "smoother block is missing smoothing type keyword. &
                                                  &  Using default, smoothing TYPE = LinearAndCrossBarSpring.", &
                                           typ    = FT_ERROR_WARNING)
         END IF 
         
         IF( str == "LinearSpring" )     THEN
            smp%springType = LINEAR_SPRING
         ELSE IF (str == "CrossBarSpring" )     THEN
            smp%springType = CROSS_SPRING
         ELSE
            smp%springType = BOTH
         END IF
!
!        -------------------
!        Smoother parameters
!        -------------------
!
         smp % springConstant = defaultSpringConstant
         IF ( smootherDict % containsKey(key = "spring constant") )     THEN
            smp % springConstant = smootherDict % doublePrecisionValueForKey(key = "spring constant") 
         END IF 
         
         smp % mass = defaultSpringMass
         IF ( smootherDict % containsKey(key = "mass") )     THEN
            smp % mass = smootherDict % doublePrecisionValueForKey(key = "mass") 
         END IF 
         
         smp % restLength = defaultSpringRestLength
         IF ( smootherDict % containsKey(key = "rest length") )     THEN
            smp % restLength = smootherDict % doublePrecisionValueForKey(key = "rest length") 
         END IF 
         
         smp % dampingCoefficient = defaultSpringDampingCoefficient
         IF ( smootherDict % containsKey(key = "damping coefficient") )     THEN
            smp % dampingCoefficient = smootherDict % doublePrecisionValueForKey(key = "damping coefficient") 
         END IF 
         
         smp % numSteps = defaultSpringIterations
         IF ( smootherDict % containsKey(key = "number of iterations") )     THEN
            smp % numSteps = smootherDict % integerValueForKey(key = "number of iterations") 
         END IF 
         
         smp % deltaT = defaultSpringTimeStep
         IF ( smootherDict % containsKey(key = "time step") )     THEN
            smp % deltaT = smootherDict % doublePrecisionValueForKey(key = "time step") 
         END IF 

      END SUBROUTINE SetSpringSmootherBlock
      
      END Module SpringMeshSmootherClass
      
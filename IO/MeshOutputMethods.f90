!
!////////////////////////////////////////////////////////////////////////
!
!      MeshOutputMethods.f95
!      Created: 2010-09-24 09:32:48 -0400 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module MeshOutputMethods
      USE MeshOperationsModule
      USE SMModelClass
      USE SMMeshObjectsModule
      IMPLICIT NONE
      
      TYPE ElementOutputInfo
         INTEGER                        :: nodeIDs(4)
         INTEGER                        :: bCurveFlag(4)
         CHARACTER(LEN=32)              :: bCurveName(4)
         REAL(KIND=RP)    , ALLOCATABLE :: x (:,:,:)
      END TYPE ElementOutputInfo
      
      PRIVATE :: ElementOutputInfo        
!
!     ========
      CONTAINS 
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE WriteToTecplot( mesh, fName ) 
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE( SMMesh )   :: mesh
         CHARACTER(LEN=*) :: fName
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER           :: iUnit, j
         INTEGER, EXTERNAL :: UnusedUnit
         
         CLASS(SMElement), POINTER :: e    => NULL()
         CLASS(FTObject) , POINTER :: obj  => NULL()
         CLASS(SMNode)   , POINTER :: node => NULL()
         INTEGER                   :: ids(8)
!
!        -----------
!        Open a file
!        -----------
!
         iUnit = UnusedUnit()
         OPEN( UNIT = iUnit, FILE = fName )
!
!        -------------
!        Set up header
!        -------------
!
         WRITE(iUnit,*) 'VARIABLES = "X", "Y", "Z", "Material ID"'
         WRITE(iUnit,*) 'ZONE F=FEPOINT, ET=QUADRILATERAL, N=',mesh % nodes % COUNT(), &
                        'E=',mesh % elements % COUNT()
!
!        ---------------
!        Write node data
!        ---------------
!
         CALL mesh % nodesIterator % setToStart()
         DO WHILE ( .NOT.mesh % nodesIterator % isAtEnd() )
            obj => mesh % nodesIterator % object()
            CALL castToSMNode(obj,node)
            WRITE( iUnit, *) node % x, node % materialID
            CALL mesh % nodesIterator % moveToNext()
         END DO
!
!        --------------------------
!        Write element connectivity
!        --------------------------
!
         CALL mesh % elementsIterator % setToStart()
         DO WHILE ( .NOT.mesh % elementsIterator % isAtEnd() )
            obj => mesh % elementsIterator % object()
            CALL castToSMelement(obj,e)
            DO j = 1, 4
               obj => e % nodes % objectAtIndex(j)
               CALL castToSMNode(obj, node)
               ids(j) = node % id
            END DO  
            WRITE( iUnit, *) (ids(j), j = 1, 4)
            CALL mesh % elementsIterator % moveToNext()
         END DO

         CLOSE( iUnit )
         
      END SUBROUTINE WriteToTecplot
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE WriteISMMeshFile( mesh, fName, model, N, version )
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS( SMMesh )   , POINTER :: mesh
         CLASS( SMModel)   , POINTER :: model
         CHARACTER(LEN=*)            :: fName
         INTEGER                     :: N       ! The polynomial order of the boundaries.
         INTEGER                     :: version !version number of the ISM format.
!
!        ---------------
!        Local Variables
!        ---------------
!
         CLASS(FTObject)    , POINTER         :: obj  => NULL()
         CLASS(SMNode)      , POINTER         :: node => NULL()
         CLASS(SMEdge)      , POINTER         :: edge => NULL()
         CLASS(SMElement)   , POINTER         :: e    => NULL()
         TYPE(ElementOutputInfo)              :: elementInfo
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()
         
         INTEGER                  :: iUnit, j, k
         INTEGER, DIMENSION(6)    :: edgeInfoArray
         INTEGER, EXTERNAL        :: UnusedUnit
!
!        ------------------------------------------
!        Get set up - ensure everything is in order
!        ------------------------------------------
!
         CALL mesh % renumberAllLists()
!
!        -----------
!        Open a file
!        -----------
!
         iUnit = UnusedUnit()
         OPEN( UNIT = iUnit, FILE = fName )
!!
!!        ----------------
!!        Print out header
!!        ----------------
!!
         IF ( version == ISM )     THEN
            !WRITE(iUnit,*) "ISM-V1"
            WRITE(iUnit, *) mesh % nodes % COUNT(), mesh % elements % COUNT(), N
         ELSE IF( version == ISM2 )     THEN
            WRITE(iUnit,*) "ISM-V2"
            WRITE(iUnit, *) mesh % nodes % COUNT(), mesh % edges % COUNT(), mesh % elements % COUNT(), N
         ELSE IF( version == ISM_MM )     THEN
            WRITE(iUnit,*) "ISM-MM"
            WRITE(iUnit, *) mesh % nodes % COUNT(), mesh % edges % COUNT(), mesh % elements % COUNT(), N
         ELSE
            PRINT *, "Unknown file format type... mesh file not written" !DEBUG
            CLOSE( iUnit )
            RETURN
         END IF
!
!        -----------
!        Print Nodes
!        -----------
!
         iterator => mesh % nodesIterator
         CALL iterator % setToStart
         DO WHILE(.NOT.iterator % isAtEnd())
            obj => iterator % object()
            CALL cast(obj,node)
            WRITE(iUnit,*) node % x
            CALL iterator % moveToNext()
         END DO  
!
!        --------------------------------------------
!        In version 2, print out the edge information
!        --------------------------------------------
!
         IF( version == ISM2 )     THEN
!
!           ------------------------
!           Ensure edges are in sync
!           ------------------------
!
            CALL mesh % syncEdges()
!
!           --------------
!           Write them out
!           --------------
!
            iterator => mesh % edgesIterator
            CALL iterator % setToStart()
            DO WHILE(.NOT.iterator % isAtEnd())
               obj => iterator % object()
               CALL cast(obj,edge)
               CALL gatherEdgeInfo(edge, edgeInfoArray)
               WRITE(iUnit, '(6(I6,2x))') edgeInfoArray
               CALL iterator % moveToNext()
            END DO  
         END IF
!
!        ---------------------------------------------------------
!        Print element connectivity with boundary edge information
!        ---------------------------------------------------------
!
         CALL elementOutputInfoInit( elementInfo, N)
         iterator => mesh % elementsIterator
         CALL iterator % setToStart
         
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL cast(obj,e)
            CALL gatherElementOutputInfo( elementInfo, e, model )
            
            IF ( version == ISM_MM )     THEN
               WRITE( iUnit, *) elementInfo % nodeIDs, TRIM(e % materialName)
            ELSE
               WRITE( iUnit, *) elementInfo % nodeIDs
            END IF
            
            WRITE( iUnit, *) elementInfo % bCurveFlag            
!
            DO k = 1, 4
               IF( elementInfo%bCurveFlag(k) == ON )     THEN
                  DO j = 0, N 
                     WRITE( iUnit, * ) elementInfo % x(:,j,k)
                  END DO
               END IF
            END DO
            
            WRITE( iUnit, *) (TRIM(elementInfo%bCurveName(k)), " ", k = 1, 4)
            
            CALL iterator % moveToNext()
         END DO
!         
      END SUBROUTINE WriteISMMeshFile
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE gatherElementOutputInfo( self, e, model  )
!
!     --------------------------------------------------------------------------
!     Gather together the boundary information for the four
!     edges of the element. This info is
!
!     nodeIDs(4)    = integer id # of the 4 corner nodes
!     bCurveFlag(4) = integer ON or OFF of whether a boundary 
!                     curve is defined or NOT
!     bCurveName(4) = Name of the 4 boundary curves. Equals "---" IF
!                     the element side is interior,
!     x(2,0:N,4)    = location (x,y) of the j=0:N Chebyshev-Lobatto points
!                     along boundary k = 1,2,3,4. The kth entry will be zero if
!                     bCurveFlag(k) = OFF.
!     -------------------------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(ElementOutputInfo)            :: self
         CLASS(SMElement)         , POINTER :: e
         CLASS(SMModel)           , POINTER :: model
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER                        :: j, k
         INTEGER                        :: N
         CLASS(SMNode)        , POINTER :: node1 => NULL(), node2 => NULL()
         CLASS(SMCurve)       , POINTER :: c     => NULL()
         CLASS(SMChainedCurve), POINTER :: chain => NULL()
         CLASS(FTObject)      , POINTER :: obj   => NULL()
         
         REAL(KIND=RP)            :: tStart(4), tEnd(4), t_j, deltaT
         INTEGER                  :: curveId(4)
         CHARACTER(LEN=32)        :: noCurveName(-4:-1) = (/"Right ", "Left  ", "Bottom", "Top   " /)
         
         N = SIZE(self%x,2)-1
!
!        -----
!        Nodes
!        -----
!
         DO k = 1, 4 
            obj => e % nodes % objectAtIndex(k)
            CALL cast(obj,node1)
            self % nodeIDs(k) = node1 % id
         END DO
!
!        -----------------------------------
!        Gather up boundary edge information
!        -----------------------------------
!
         self%bCurveName = "---"
         self%bCurveFlag = OFF
         
         DO k = 1, 4 
            obj => e % nodes % objectAtIndex(edgeMap(1,k))
            CALL cast(obj,node1) 
            obj => e % nodes % objectAtIndex(edgeMap(2,k))
            CALL cast(obj,node2)
!
!           ---------------------------------------------------------------------------
!           See if this edge is on a boundary. One of the two nodes should be
!           a ROW_SIDE, and that one is on a curve rather than the joint of two curves.
!           The edge could be on an outer box, in which case it is a straight line, but
!           still needs boundary name information
!           ---------------------------------------------------------------------------
!
            IF( IsOnBoundaryCurve(node1) .AND. IsOnBoundaryCurve(node2) )     THEN
!
!              -----------------------------------------------------------
!              Mark as on a boundary curve needing interpolant information
!              -----------------------------------------------------------
!
               self % bCurveFlag(k) = ON
               IF( node1 % nodeType == ROW_SIDE )     THEN
                  curveID(k)    = node1 % bCurveID
                  c             => model % curveWithID(node1 % bCurveID, chain)
               ELSE
                  curveID(k)    = node2 % bCurveID
                  c             => model % curveWithID(node2 % bCurveID, chain)
               END IF
               
               self % bCurveName(k) = c % curveName()
               tStart(k)            = node1 % gWhereOnBoundary
               tEnd(k)              = node2 % gWhereOnBoundary
               
            ELSE IF ( IsOnOuterBox(node1) .AND. IsOnOuterBox(node2) )     THEN
!
!              --------------------------------------------------------------
!              Only mark the boundary names for output, no interpolant needed
!              --------------------------------------------------------------
!
               IF( node1 % nodeType == CORNER_NODE )     THEN
                  self % bCurveName(k) = noCurveName(node2 % bCurveID)
               ELSE
                  self % bCurveName(k) = noCurveName(node1 % bCurveID)
               END IF
               
            END IF
         END DO
!
!        --------------------------------------------
!        Construct boundary information
!        Use a Chebyshev interpolant for the points.
!        --------------------------------------------
!
         DO k = 1, 4
         
            IF( self%bCurveFlag(k) == ON )     THEN
              c      => model % curveWithID(curveID(k), chain)
              
              deltaT = tEnd(k) - tStart(k)
              IF( deltaT > maxParameterChange )     THEN !Crossing over the start
                 deltaT = deltaT - 1.0_RP
              ELSE IF (deltaT < -maxParameterChange ) THEN
                 deltaT = 1.0_RP + deltaT
              END IF
              
              DO j = 0, N 
              
                  t_j = tStart(k) + deltaT*(1.0_RP - COS(j*PI/N))/2.0_RP
                  IF( t_j > 1.0_RP )     THEN
                     t_j = t_j - 1.0_RP
                  ELSE IF( t_j < 0.0_RP )     THEN
                     t_j = t_j + 1.0_RP
                  END IF
                  
                  self%x(:,j,k) = chain % PositionAt( t_j )
                  
                END DO
             END IF
         END DO

      END SUBROUTINE gatherElementOutputInfo
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE elementOutputInfoInit( self, N ) 
         IMPLICIT NONE
         TYPE(ElementOutputInfo) :: self
         INTEGER                 :: N ! = polynomial order
         
         ALLOCATE( self%x(3,0:N,4) )
         
         self % x          = 0.0_RP
         self % bCurveFlag = 0
         self % bCurveName = '---'
      END SUBROUTINE elementOutputInfoInit
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE gatherEdgeInfo( edge, info )
!
!     --------------------------------------------
!     Extract/Construct information about the edge
!
!     Info Returned:
!
!            info(1) = start node id
!            info(2) = end node id
!            info(3) = left element id
!            info(4) = right element id (or 0, if a boundary edge)
!            info(5) = element side for left element
!            info(6) = element side for right element signed for direction (or 0 for boundary edge)
!     
!     --------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(SMEdge), POINTER     :: edge
         INTEGER      , INTENT(OUT) :: info(6)
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER                  :: i, j
         INTEGER, DIMENSION(4,4)  :: s = RESHAPE((/-1,-1,1,1,-1,-1,1,1,1,1,-1,-1,1,1,-1,-1/),(/4,4/))
         
         IF( ASSOCIATED(edge % elements(2) % element ) )    THEN
            i = edge % elementSide(1)
            j = edge % elementSide(2)
            j = j*s(i,j)
            info(1) = edge % nodes(1)    % node % id
            info(2) = edge % nodes(2)    % node % id
            info(3) = edge % elements(1) % element % id
            info(4) = edge % elements(2) % element % id
            info(5) = i
            info(6) = j
         ELSE
            i = edge % elementSide(1)
            j = edge % elementSide(2)
            info(1) = edge % nodes(1)    % node % id
            info(2) = edge % nodes(2)    % node % id
            info(3) = edge % elements(1) % element % id
            info(4) = 0
            info(5) = i
            info(6) = 0
         END IF 
         
      END SUBROUTINE gatherEdgeInfo
      
      END Module MeshOutputMethods
      
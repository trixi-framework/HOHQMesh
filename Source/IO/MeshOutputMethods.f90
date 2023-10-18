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
! --- End License
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
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE WriteSkeletonToTecplot( mesh, fName )
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

         TYPE(SMElement) , POINTER :: e    => NULL()
         CLASS(FTObject) , POINTER :: obj  => NULL()
         TYPE(SMNode)    , POINTER :: node => NULL()
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
         WRITE(iUnit,*) 'VARIABLES = "X", "Y", "Z"'
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
            WRITE( iUnit, *) node % x
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
               ids(j) = e % nodes(j) % node % id
            END DO
            WRITE( iUnit, *) (ids(j), j = 1, 4)
            CALL mesh % elementsIterator % moveToNext()
         END DO

         CLOSE( iUnit )

      END SUBROUTINE WriteSkeletonToTecplot
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE WriteSEMMeshToTecplot( mesh, fName, N )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE( SMMesh )   :: mesh
         CHARACTER(LEN=*) :: fName
         INTEGER          :: N
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER            :: fUnit
         INTEGER            :: i, j

         TYPE (SMElement), POINTER :: e    => NULL()
         CLASS(FTObject) , POINTER :: obj  => NULL()
!
!        ----------
!        Interfaces
!        ----------
!
         INTEGER, EXTERNAL  :: UnusedUnit
!
!        -----------
!        Open a file
!        -----------
!
         fUnit = UnusedUnit()
         OPEN( UNIT = fUnit, FILE = fName )
!

         WRITE(fUnit,*) ' TITLE = "SEM Quad mesh" '
         WRITE(fUnit,*) ' VARIABLES = "x", "y", "z", "material ID"'

         CALL mesh % elementsIterator % setToStart()
         DO WHILE ( .NOT.mesh % elementsIterator % isAtEnd() )
            obj => mesh % elementsIterator % object()
            CALL castToSMelement(obj,e)

            WRITE(fUnit,*) "ZONE I=", N+1, ",J=",N+1,", F=POINT"
            DO j= 0, N
               DO i = 0, N
                  WRITE(fUnit,'(3E13.5,I2)') e % xPatch(1, i, j), e % xPatch(2, i, j), e % xPatch(3, i, j), &
                                             e % materialID
               END DO
            END DO

            CALL mesh % elementsIterator % moveToNext()
         END DO

      END SUBROUTINE WriteSEMMeshToTecplot
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE WriteISMMeshFile( mesh, fName, N, version )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE ( SMMesh )   , POINTER :: mesh
         CHARACTER(LEN=*)            :: fName
         INTEGER                     :: N       ! The polynomial order of the boundaries.
         INTEGER                     :: version !version number of the ISM format.
!
!        ---------------
!        Local Variables
!        ---------------
!
         CLASS(FTObject)    , POINTER         :: obj  => NULL()
         TYPE(SMNode)       , POINTER         :: node => NULL()
         TYPE(SMEdge)       , POINTER         :: edge => NULL()
         TYPE(SMElement)    , POINTER         :: e    => NULL()
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
            WRITE(iUnit, *) mesh % nodes % COUNT(), mesh % elements % COUNT(), N
         ELSE IF( version == ISM2 )     THEN
            WRITE(iUnit,*) "ISM-V2"
            WRITE(iUnit, *) mesh % nodes % COUNT(), mesh % edges % COUNT(), mesh % elements % COUNT(), N
         ELSE IF( version == ISM_MM )     THEN
            WRITE(iUnit,*) "ISM-MM"
            WRITE(iUnit, *) mesh % nodes % COUNT(), mesh % edges % COUNT(), mesh % elements % COUNT(), N
         ELSE
            PRINT *, "Unknown file format type... mesh file not written"
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
            CALL castToSMNode(obj,node)
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
               CALL castToSMEdge(obj,edge)
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
         iterator => mesh % elementsIterator
         CALL iterator % setToStart

         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMElement(obj,e)

            IF ( version == ISM_MM )     THEN
               WRITE( iUnit, *) e % boundaryInfo % nodeIDs, TRIM(mesh % materialNameForID(e % materialID))
            ELSE
               WRITE( iUnit, *) e % boundaryInfo % nodeIDs
            END IF

            WRITE( iUnit, *) e % boundaryInfo % bCurveFlag
!
            DO k = 1, 4
               IF( e % boundaryInfo % bCurveFlag(k) == ON )     THEN
                  DO j = 0, N
                     WRITE( iUnit, * ) e % boundaryInfo % x(:,j,k)
                  END DO
               END IF
            END DO

            WRITE( iUnit, *) (TRIM(e % boundaryInfo % bCurveName(k)), " ", k = 1, 4)

            CALL iterator % moveToNext()
         END DO

         CLOSE(iUnit)
!
      END SUBROUTINE WriteISMMeshFile
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE WriteABAQUSMeshFile( mesh, fName, N )
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE ( SMMesh )   , POINTER :: mesh
         CHARACTER(LEN=*)            :: fName
         INTEGER                     :: N       ! The polynomial order of the boundaries.
!
!        ---------------
!        Local Variables
!        ---------------
!
         CLASS(FTObject)    , POINTER         :: obj  => NULL()
         TYPE (SMNode)      , POINTER         :: node => NULL()
         TYPE (SMElement)   , POINTER         :: e    => NULL()
         CLASS(FTLinkedListIterator), POINTER :: iterator => NULL()

         INTEGER                  :: iUnit, j, k
         INTEGER, DIMENSION(4)    :: bndyNameRemap
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
!
!        ----------------
!        Print out header
!        ----------------
!
         WRITE(iUnit, '(A8)')"*Heading"
         WRITE(iUnit, *)"File created by HOHQMesh"
!
!        -----------
!        Print Nodes
!        -----------
!
         WRITE(iUnit, '(A5)')"*NODE"
         iterator => mesh % nodesIterator
         CALL iterator % setToStart
!
!        -----------------------------------------------------------------------------------
!        The ABAQUS format is incredibly picky with respect to leading/trailing white spaces
!        and comma placement. Also, ABAQUS carries the node index as its first entry on the
!        file line, so we track and output this with the helper j variable.
!        -----------------------------------------------------------------------------------
!
         j = 1
         DO WHILE(.NOT.iterator % isAtEnd())
            obj => iterator % object()
            CALL castToSMNode(obj,node)
            WRITE(iUnit, '(I0,3(", ", F18.13))') j, node % x
            CALL iterator % moveToNext()
            j = j + 1
         END DO
!
!        --------------------------
!        Print element connectivity
!        --------------------------
!
         WRITE(iUnit, '(A35)')"*ELEMENT, type=CPS4, ELSET=Surface1"
         iterator => mesh % elementsIterator
         CALL iterator % setToStart
         j = 1
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMElement(obj,e)
            WRITE( iUnit, '(I0,4(", ", I0))') j, e % boundaryInfo % nodeIDs
            CALL iterator % moveToNext()
            j = j + 1
         END DO
!
!        -------------------------------------------------------------
!        Print element IDs with high-order boundary edge information
!        This additional data is prefaced with "** " which is the
!        comment character in the ABAQUS format. This makes the data
!        invisible to standard ABAQUS mesh file parsers but available.
!        -------------------------------------------------------------
!
         WRITE(iUnit, '(A47)')"** ***** HOHQMesh boundary information ***** **"
         WRITE(iUnit, '(A28,I0)')"** mesh polynomial degree = ",N
         iterator => mesh % elementsIterator
         CALL iterator % setToStart
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMElement(obj,e)
            ! Print the corner IDs
            WRITE( iUnit, '(A3,4(" ", I0))') "** ", e % boundaryInfo % nodeIDs
            ! Print flag if an edge is curved
            WRITE( iUnit, '(A3,4(" ", I0))') "** ", e % boundaryInfo % bCurveFlag
            ! Print the high-order boundary curve information
            DO k = 1, 4
               IF( e % boundaryInfo % bCurveFlag(k) == ON )     THEN
                  DO j = 0, N
                     WRITE(iUnit, '(A3,3(F18.13))') "** ", e % boundaryInfo % x(:,j,k)
                  END DO
               END IF
            END DO
            CALL iterator % moveToNext()
         END DO
!
!        ---------------------------------------------------------------------------------------------
!        Print boundary name information. In general, we can think of an element to have sides labeled
!                          +y
!                   -----------------
!                   |               |
!                   | ^ y           |
!                -x | |             | +x
!                   | |             |
!                   | ---> x        |
!                   -----------------
!                           -y
!        For this mesh file output the boundary names are reordered to adopt the order convention of
!        -x +x -y +y that is expected by p4est to create its mesh connectivity. In contrast, the
!        default HOHQMesh ordering of the boundary names is -y +x +y -x.
!        ---------------------------------------------------------------------------------------------
!
         bndyNameRemap = (/ 4, 2, 1, 3 /)
         iterator => mesh % elementsIterator
         CALL iterator % setToStart
         DO WHILE ( .NOT.iterator % isAtEnd() )
            obj => iterator % object()
            CALL castToSMElement(obj,e)
            WRITE( iUnit, '(9A)') "** ", (TRIM(e % boundaryInfo % bCurveName(bndyNameRemap(k))), " ", k = 1, 4)
            CALL iterator % moveToNext()
         END DO
!
      END SUBROUTINE WriteABAQUSMeshFile
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE Write2DMeshStatistics( mesh, statsFileName)
         USE FTMutableObjectArrayClass
         USE MeshQualityAnalysisClass
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE ( SMMesh )  , POINTER :: mesh
         CHARACTER(LEN=*)           :: statsFileName
!
!        ---------------
!        Local variables
!        ---------------
!
         TYPE (FTMutableObjectArray) , POINTER :: badElements => NULL()
         INTEGER                               :: statsFileUnit, k
         CLASS(FTObject)             , POINTER :: obj => NULL()
         TYPE (SMElement)            , POINTER :: e   => NULL()
         INTEGER, EXTERNAL                     :: UnusedUnit

         statsFileUnit = UnusedUnit()
         OPEN(FILE = statsFileName, UNIT = statsFileUnit)
         badElements => BadElementsInMesh( mesh )

         IF ( ASSOCIATED(POINTER = badElements) )     THEN
            IF (PrintMessage) PRINT *, badElements % COUNT()," Bad element(s) Found"
            WRITE(statsFileUnit,*) " "
            WRITE(statsFileUnit,*) "----------------"
            WRITE(statsFileUnit,*) "Bad Element Info"
            WRITE(statsFileUnit,*) "----------------"
            WRITE(statsFileUnit,*) " "

            DO k = 1, badElements % COUNT()
               obj => badElements % objectAtIndex(indx = k)
               CALL castToSMElement(obj,e)
               CALL PrintBadElementInfo( e, statsFileUnit )
            END DO
            CALL releaseFTMutableObjectArray(badElements)

         ELSE IF (PrintMessage)     THEN
            PRINT *, "********* Elements are OK *********"
         END IF

         WRITE(statsFileUnit,*) " "
         WRITE(statsFileUnit,*) "------------------------"
         WRITE(statsFileUnit,*) "2D Mesh Quality Measures"
         WRITE(statsFileUnit,*) "------------------------"
         WRITE(statsFileUnit,*) " "
         CALL OutputMeshQualityMeasures( mesh, fUnit  = statsFileUnit )
         CLOSE(statsFileUnit)

      END SUBROUTINE Write2DMeshStatistics
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
         TYPE (SMEdge), POINTER     :: edge
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


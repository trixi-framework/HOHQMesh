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
!      LibTests.f90
!      Created: June 22, 2021 at 8:13 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   Module LibTestModule 
   USE TestSuiteManagerClass
   USE ISO_C_BINDING
   USE EncoderModule
   USE FTAssertions
   USE ProjectInterfaceModule
   IMPLICIT NONE
!
   CHARACTER(LEN=64) :: controlFile = "Benchmarks/ControlFiles/Box.control"
!
!  --------
   CONTAINS
!  --------
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE HML_RunTests(cPathToRepo, numberOfFailedTests) BIND(C)
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CHARACTER(KIND=c_char), DIMENSION(*) :: cPathToRepo
      INTEGER(C_INT)                       :: numberOfFailedTests
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE(TestSuiteManager)                  :: testSuite
      CHARACTER(LEN=1), POINTER               :: optData(:) 
      CHARACTER(LEN=1), ALLOCATABLE           :: optDataA(:) 
      CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: fullPath
      EXTERNAL                                :: TestCurves
      CHARACTER(len=:)        , ALLOCATABLE   :: fFilePath
!
!     ------------------------------------------------------------------------------------
!     The control files are located in a Benchmarks directory at the end of (if not empty)
!     the directory given by the "path" to the HOHQMesh directory. That path will be added
!     to the control file names. 
!     ------------------------------------------------------------------------------------
!
      CALL testSuite % init()
!
!     -------------------------------
!     Add the test mesh to test suite
!     -------------------------------
!
      fFilePath = c_to_f_string(c_string = cPathToRepo )

      IF ( TRIM(fFilePath) == "" )     THEN
         fullPath =  controlFile
      ELSE
         CALL ConvertToPath(fFilePath)
         fullPath = TRIM(fFilePath) // controlFile
      END IF 
          
      optData => NULL()
      CALL encode(str = TRIM(fullPath),enc = optDataA)
      ALLOCATE(optData(SIZE(optDataA)))
      optData = optDataA

      CALL testSuite % addTestSubroutineWithName(testAPIWithControlFile,"ControlFile: " // TRIM(controlFile), optData)
      DEALLOCATE(optDataA)
!
!     -------------
!     Run the tests
!     -------------
!
      CALL testSuite % performTests(numberOfFailedTests)
      CALL finalizeSharedAssertionsManager
      
   END SUBROUTINE HML_RunTests
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE testAPIWithControlFile(optData)
         USE ProjectInterfaceModule
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:) ! Contains path to the control file
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH)                 :: controlFileName
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH)                 :: path
         
         REAL(KIND=C_DOUBLE)   , DIMENSION(:,:)    , ALLOCATABLE :: locationsArray
         INTEGER(KIND=C_INT)   , DIMENSION(:,:)    , ALLOCATABLE :: connectivityArray
         INTEGER(KIND=C_INT)   , DIMENSION(:,:)    , ALLOCATABLE :: edgesArray
         INTEGER(KIND=C_INT)   , DIMENSION(:,:)    , ALLOCATABLE :: bCurveFlags
         REAL(KIND=C_DOUBLE)   , DIMENSION(:,:,:,:), ALLOCATABLE :: boundaryPoints
         CHARACTER(KIND=c_char), DIMENSION(:)      , ALLOCATABLE :: cFName
         
         TYPE( MeshProject )       , POINTER                     :: project
         CLASS(MeshProject)        , POINTER                     :: projAsClass
         TYPE(SMMesh)              , POINTER                     :: testMesh
         CLASS(FTObject)           , POINTER                     :: obj
         CLASS(SMNode)             , POINTER                     :: node
         CLASS(SMEdge)             , POINTER                     :: edge
         CLASS(SMElement)          , POINTER                     :: e
         TYPE(FTLinkedListIterator), POINTER                     :: iterator
         
         TYPE(c_ptr)                                             :: projCPtr
         INTEGER(C_INT)                                          :: flag
         INTEGER(C_INT)                                          :: nNodes, nElements, nEdges
         INTEGER(C_INT)                                          :: pOrder
         INTEGER                                                 :: j, lc, k, i
         REAL(KIND=RP)                                           :: eNorm
         INTEGER                                                 :: iNorm
         INTEGER                                                 :: edgeInfo(6), ids(4)
!
!        ----------------------
!        Allocate a new project
!        ----------------------
!
         projCPtr = HML_NewProject()
         IF ( .NOT. CptrIsProjectPtr(projCPtr) )     THEN
            CALL FTAssert(test = .FALSE., msg = "c_ptr is not a pointer to a MeshProject")
            RETURN 
         END IF 
!
!        ---------------------------------------------------------------------------
!        Get access to the actual project since not everything is exposed in the API
!        ---------------------------------------------------------------------------
!
         CALL C_F_POINTER(cPtr = projCPtr, FPTR = project)
         projAsClass => project
!
!        -----------------------------------------------------------
!        Find the path and modify output file locations if necessary
!        -----------------------------------------------------------
!
         controlFileName = ""
         CALL DECODE(enc = optData, strOut = controlFileName)
         cFName = f_to_c_string(f_string = controlFileName)
!
!        -----------------------------------
!        Initialize it with the control file
!        -----------------------------------
!
         CALL HML_InitWithControlFile(cPtr      = projCPtr, &
                                      cFileName = cFName,   &
                                      errFlag   = flag)
         IF ( flag > HML_ERROR_NONE )     THEN
            CALL FTAssertEqual(expectedValue = HML_ERROR_NONE, &
                               actualValue   = flag,           &
                               msg           = "Project initialization with control file") 
            RETURN 
         END IF 
         
         path = ""
         lc = INDEX(STRING = controlFileName, SUBSTRING = "Benchmarks")
         IF ( lc > 1 )     THEN
            path = controlFileName(1:lc-1)
            CALL ConvertToPath(path)
         END IF
         CALL AddPathToProjectFiles(self = project, path = path)
!
!        -----------------
!        Generate the Mesh
!        -----------------
!
         CALL HML_GenerateMesh(cPtr = projCPtr, errFlag = flag)
         IF ( flag > HML_ERROR_NONE )     THEN
            CALL FTAssertEqual(expectedValue = HML_ERROR_NONE, &
                               actualValue   = flag,           &
                               msg           = "Meshing the project") 
            RETURN 
         END IF 
!
!        -----------------
!        Get the test data
!        -----------------
!
         testMesh => project % mesh
         
         nNodes    = HML_NumberOfNodes(cPtr    = projCPtr, errFlag = flag)
         nElements = HML_NumberOfElements(cPtr = projCPtr, errFlag = flag)
         nEdges    = HML_NumberOfEdges(cPtr    = projCPtr, errFlag = flag)
         
         CALL FTAssertEqual(expectedValue = testMesh % nodes % count(), &
                            actualValue   = nNodes,       &
                            msg           = "Number of nodes in the mesh")
         CALL FTAssertEqual(expectedValue = testMesh % edges % count(), &
                            actualValue   = nEdges,       &
                            msg           = "Number of edges in the mesh")
         CALL FTAssertEqual(expectedValue = testMesh % elements % count(), &
                            actualValue   = nElements,       &
                            msg           = "Number of edges in the mesh")
!
!        -------------------------
!        Test the accessors: Nodes
!        -------------------------
!
         IF(nNodes == testMesh % nodes % count())     THEN 

            ALLOCATE(locationsArray(3,nNodes))

            CALL HML_NodeLocations(cPtr           = projCPtr,       &
                                   locationsArray = locationsArray, &
                                   N              = nNodes,         &
                                   errFlag        = flag)
            CALL FTAssertEqual(expectedValue = 0,    &
                               actualValue   = flag, &
                               msg           = "Error flag calling HML_NodeLocations")
            
            iterator => testMesh % nodesIterator
            CALL iterator % setToStart()
            j = 1
            DO WHILE(.NOT. iterator % isAtEnd()) 
               obj => iterator % object()
               CALL cast(obj,node)
               eNorm = MAXVAL(ABS(node % x - locationsArray(:,j)))
               CALL FTAssertEqual(expectedValue = 0.0_RP,actualValue = eNorm,tol = 1.d-6)
               j = j + 1
               CALL iterator % moveToNext()
            END DO 
            
            DEALLOCATE( locationsArray)
         END IF 
!
!        -----------------------
!        Test Acessors: Elements
!        -----------------------
!
         IF(nElements == testMesh % elements % count())     THEN 

            ALLOCATE(connectivityArray(4,nElements))
!
            CALL HML_2DElementConnectivity(cPtr              = projCPtr,          &
                                           connectivityArray = connectivityArray, &
                                           N                 = nElements,         &
                                           errFlag           = flag)
            CALL FTAssertEqual(expectedValue = 0,    &
                               actualValue   = flag, &
                               msg           = "Error flag calling HML_2DElementConnectivity")            
!
!           -----------------------
!           Access the bCurve Flags
!           -----------------------
!
            ALLOCATE(bCurveFlags(4,nElements))
            CALL HML_2DElementEdgeFlag(cPtr      = projCPtr,   &
                                       curveFlag = bCurveFlags,&
                                       N         = nElements,  &
                                       errFlag   = flag)

            CALL FTAssertEqual(expectedValue = 0,    &
                               actualValue   = flag, &
                               msg           = "Error flag calling HML_2DElementEdgeFlag")
!
!           ----------------------
!           Access the edge points
!           ----------------------
!
            CALL HML_PolynomialOrder(cPtr = projCPtr,p = pOrder,errFlag = flag)
            CALL FTAssertEqual(expectedValue = 0,    &
                               actualValue   = flag, &
                               msg           = "Error flag calling HML_2DElementEdgeFlag")
            CALL FTAssertEqual(expectedValue = project % runParams % polynomialOrder,     &
                               actualValue   = pOrder,           &
                               msg           = "Error flag calling HML_2DElementEdgeFlag")
!            
            ALLOCATE(boundaryPoints(3,pOrder+1,4,nElements))
            
            CALL HML_2DElementBoundaryPoints(cPtr = projCPtr,                 &
                                             boundaryPoints = boundaryPoints, &
                                             p = pOrder,                      &
                                             N = nElements,                   &
                                             errFlag = flag)
            CALL FTAssertEqual(expectedValue = 0,    &
                               actualValue   = flag, &
                               msg           = "Error flag calling HML_2DElementEdgeFlag")
!
!           ---------------
!           Check integrity
!           ---------------
!
            iterator => testMesh % elementsIterator
            CALL iterator % setToStart()
            j = 1
            DO WHILE(.NOT. iterator % isAtEnd()) 
               obj => iterator % object()
               CALL cast(obj,e)
!
!              ---------------
!              Boundary values
!              ---------------
!
               DO k = 1, 4
                  DO i = 1, pOrder + 1 
                     eNorm = MAXVAL(ABS(e % boundaryInfo % x(:,i-1,k) - boundaryPoints(:,i,k,j)))
                     CALL FTAssertEqual(expectedValue = 0.0_RP,actualValue = eNorm,tol = 1.d-6)
                  END DO 
               END DO 
!
!              --------------
!              Boundary flags
!              --------------
!
               iNorm = MAXVAL(ABS(e % boundaryInfo % bCurveFlag - bCurveFlags(:,j)))
               CALL FTAssertEqual(expectedValue = 0,actualValue = iNorm)
!
!              ------------
!              Connectivity
!              ------------
!
               DO k = 1, 4
                  obj => e % nodes % objectAtIndex(k)
                  CALL castToSMNode(obj, node)
                  ids(k) = node % id
               END DO  
               iNorm = MAXVAL(ABS(ids - connectivityArray(:,j)))
               CALL FTAssertEqual(expectedValue = 0,actualValue = iNorm)

               j = j + 1
               CALL iterator % moveToNext()
            END DO 
            
            DEALLOCATE(bcurveFlags)         
            DEALLOCATE(connectivityArray)
            DEALLOCATE(boundaryPoints)         
         END IF 
!
!        --------------------
!        Test Acessors: Edges
!        --------------------
!
         IF(nEdges == testMesh % edges % count())     THEN 

            ALLOCATE(edgesArray(6,nEdges))
            
            CALL HML_2DEdgeConnectivity(cPtr              = projCPtr,   &
                                        connectivityArray = edgesArray, &
                                        N                 = nEdges,     &
                                        errFlag           = flag)
                                        
            CALL FTAssertEqual(expectedValue = 0,    &
                               actualValue   = flag, &
                               msg           = "Error flag calling HML_2DEdgeConnectivity")

            iterator => testMesh % edgesIterator
            CALL iterator % setToStart()
            j = 1
            DO WHILE(.NOT. iterator % isAtEnd()) 
               obj => iterator % object()
               CALL cast(obj,edge)
               CALL gatherEdgeInfo(edge = edge,info = edgeInfo)
               iNorm = MAXVAL(ABS(edgeInfo - edgesArray(:,j)))
               CALL FTAssertEqual(expectedValue = 0, actualValue = iNorm)
               j = j + 1
               CALL iterator % moveToNext()
            END DO 
            
            DEALLOCATE(edgesArray)
         END IF
!
!        ---------------------
!        Close out the project
!        ---------------------
!
         CALL HML_CloseProject(cPtr = projCPtr, errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,    &
                            actualValue   = flag, &
                            msg           = "Error flag calling HML_CloseProject")
         
      END SUBROUTINE testAPIWithControlFile
      
   END Module LibTestModule

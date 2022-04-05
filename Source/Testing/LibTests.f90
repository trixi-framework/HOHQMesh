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
      EXTERNAL                                :: BasicCollectionTests
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
!     ------------------------------------------------------------
!     Add the basic tests of the collection interface to the suite
!     ------------------------------------------------------------
!
      CALL testSuite % addTestSubroutineWithName(BasicCollectionTests, "Basic Collection Tests")
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
         CHARACTER(KIND=C_CHAR), DIMENSION(:,:,:)  , ALLOCATABLE :: namesArray
         
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
         CHARACTER(LEN=LENGTH_OF_BC_STRING)                      :: bName, testName
!
!        ----------------------
!        Allocate a new project
!        ----------------------
!
         projCPtr = HML_AllocProject()
         IF ( .NOT. CptrIsProjectPtr(projCPtr) )     THEN
            CALL FTAssert(test = .FALSE., msg = "c_ptr is not a pointer to a MeshProject 1")
            RETURN 
         END IF 
!
!        ---------------------
!        Close out the project
!        ---------------------
!
         CALL HML_ReleaseProject(cPtr = projCPtr, errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,    &
                            actualValue   = flag, &
                            msg           = "Error flag calling HML_ReleaseProject 1")
!
!        --------------------------
!        Allocate new project again
!        --------------------------
!
         projCPtr = HML_AllocProject()
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
!           --------------
!           Boundary names
!           --------------
!
            ALLOCATE(namesArray(LENGTH_OF_BC_STRING+1,4,nElements))
            CALL HML_2DElementBoundaryNames(cPtr       = projCPtr,   &
                                            namesArray = namesArray, &
                                            N          = nElements,  &
                                            errFlag    = flag)
            CALL FTAssertEqual(expectedValue = 0,    &
                               actualValue   = flag, &
                               msg           = "Error flag calling HML_2DElementBoundaryNames")
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
                     CALL FTAssertEqual(expectedValue = 0.0_RP, &
                                        actualValue   = eNorm,  &
                                        tol = 1.d-6,            &
                                        msg = "Boundary points")
                  END DO 
               END DO 
!
!              --------------
!              Boundary flags
!              --------------
!
               iNorm = MAXVAL(ABS(e % boundaryInfo % bCurveFlag - bCurveFlags(:,j)))
               CALL FTAssertEqual(expectedValue = 0,actualValue = iNorm, msg = "Boundary Curve Flags")
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
               CALL FTAssertEqual(expectedValue = 0,actualValue = iNorm, msg = "connectivity")
!
!              --------------------
!              Boundary curve names
!              --------------------
!
               DO k = 1, 4 
                  bName = e % boundaryInfo % bCurveName(k)
                  DO i = 1, LEN_TRIM(bName) 
                     testName(i:i) = namesArray(i,k,j) 
                  END DO 
                  CALL FTAssert(test = namesArray(i,k,j) == c_null_char,msg = "Boundary name null termination") 
                  CALL FTAssert(test = testName(1:i-1) == bName,msg = "Boundary name") 
               END DO 
               
               j = j + 1
               CALL iterator % moveToNext()
            END DO 
            
            DEALLOCATE(bcurveFlags)         
            DEALLOCATE(connectivityArray)
            DEALLOCATE(boundaryPoints)
            DEALLOCATE(namesArray)   
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
               CALL FTAssertEqual(expectedValue = 0, actualValue = iNorm, msg = "Edge Info")
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
         CALL HML_ReleaseProject(cPtr = projCPtr, errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,    &
                            actualValue   = flag, &
                            msg           = "Error flag calling HML_ReleaseProject 2")
         
      END SUBROUTINE testAPIWithControlFile
      
   END Module LibTestModule
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE BasicCollectionTests
         USE FTValueDictionaryClass
         USE FTLinkedListClass
         USE InteropUtilitiesModule
         USE ContainerInterfaceActions
         USE TestSuiteManagerClass
         USE FTAssertions
         IMPLICIT NONE  
         
         TYPE(c_ptr)                                       :: cPtrToDict, cPtrToList
         TYPE(c_ptr)                                       :: cPtrToList2, cPtrToDict2
         TYPE(c_ptr)                                       :: cPtrToDict3, cPtrToDict4
         TYPE( FTValueDictionary ), POINTER                :: dict, dict2, dict3, dict4, dict5
         TYPE(FTLinkedList), POINTER                       :: list, list2
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH)           :: str, fKey
         CHARACTER(KIND=c_char), DIMENSION(:), ALLOCATABLE :: cKey, cVal
         INTEGER                                           :: flag
         CLASS(FTObject), POINTER                          :: obj
         REAL(KIND=C_DOUBLE)                               :: array(3,4)
         CLASS(FTData)   , POINTER                         :: arrayData
         CHARACTER(LEN=1), POINTER                         :: encodedData(:)
         REAL(KIND=C_DOUBLE), ALLOCATABLE                  :: decodedArray(:,:)
         REAL(KIND(1.0d0))                                 :: er
!
!        -------------------
!        Create a dictionary
!        -------------------
!
         cPtrToDict = HML_AllocDictionary()
         CALL HML_InitDictionary(cPtr = cPtrToDict,errFlag = flag)
         CALL FTAssert(test = C_ASSOCIATED(cPtrToDict),msg = "Associated cPtr to dictionary")
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_InitDictionary")
!
!        -------------------
!        Add a key and value
!        -------------------
!
         fKey = "key"
         str  = "value"
         cKey = f_to_c_string(f_string = fKey)
         cVal = f_to_c_string(f_string = str )

         CALL HML_AddDictKeyAndValue(cPtrToDict = cPtrToDict, &
                                     cKey       = cKey,       &
                                     cValue     = cVal,       &
                                     errFlag    = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_AddDictKeyAndValue")
         
         CALL ptrToDictionary(cPtr = cPtrToDict,dict = dict,errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "ptrToDictionary")
         
         CALL FTAssert(test = ASSOCIATED(dict), msg = "Dictionary from cPtr to Dictionary")
         CALL FTAssertEqual(expectedValue = dict % stringValueForKey(key = fKey, requestedLength = 5), &
                            actualValue = str, &
                            msg = "stringValueForKey")
!
!        ------------
!        Add an array
!        ------------
!
         array      = 3.14d0
         array(2,3) = 7.28d0
         CALL HML_AddArrayToDict(array = array, &
                                 N = 3,M = 4,   &
                                 cPtrToDict = cPtrToDict, &
                                 errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_AddArrayToDict")
         CALL FTAssert(test = dict % containsKey(key = "data"), msg = "Dictionary contains data")
         
         obj         => dict % objectForKey(key = "data")
         arrayData   => dataFromObject(obj)
         encodedData => arrayData % storedData()
         CALL FTAssert(test = ASSOCIATED(encodedData),msg = "Encoded data is in dictionary")         
         CALL DECODE(enc = encodedData, N = 3, M = 4, arrayOut = decodedArray)
         er = MAXVAL(decodedArray-array)
         CALL FTAssertEqual(expectedValue = 0.0d0, actualValue = er, tol = 1.0d-10, &
                            msg = "Difference in output and input arrays")
!
!        -------------
!        Create a list
!        -------------
!
         cPtrToList = HML_AllocList()
         CALL FTAssert(test = C_ASSOCIATED(cPtrToList),msg = "Associated cPtr to list")
         CALL HML_InitList(cPtr = cPtrToList,errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_InitList")
!
!        ----------------
!        Add dict to list
!        ----------------
!
         CALL HML_AddDictToList(cPtrToDictToAdd = cPtrToDict, &
                                cPtrToList      = cPtrToList, &
                                errFlag         = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_AddDictToList")
!
!        ----------------------------------
!        Create another list and dictionary
!        ----------------------------------
!
         cPtrToList2 = HML_AllocList()
         CALL ptrToList(cPtr = cPtrToList2,list = list2,errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "ptrToList 2")
         CALL HML_InitList(cPtr = cPtrToList2,errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_InitList 2")
         
         cPtrToDict2 = HML_AllocDictionary()
         CALL HML_InitDictionary(cPtr = cPtrToDict2,errFlag = flag)
         CALL FTAssert(test = C_ASSOCIATED(cPtrToDict2),msg = "Associated cPtr to dictionary 2")
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_InitDictionary 2")
!
!        ------------------------
!        Add the list to the dict
!        ------------------------
!
         CALL HML_AddListToDict(cPtrToList = cPtrToList2, &
                                cPtrToDict = cPtrToDict2, &
                                errFlag    = flag)
         CALL FTAssertEqual(expectedValue = 0, actualValue = flag, msg = "HML_AddListToDict")
         CALL ptrToDictionary(cPtr = cPtrToDict2,dict = dict2,errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0, actualValue = flag, msg = "ptrToDictionary 2")
         CALL FTAssert(test = dict2 % containsKey(key = "LIST"),msg = "Dictionary has LIST key")
!
!        ----------------------------------------
!        Test adding a dictionary to a dictionary
!        ----------------------------------------
!
         cPtrToDict3 = HML_AllocDictionary()
         CALL FTAssert(test = C_ASSOCIATED(cPtrToDict3),msg = "Associated cPtr to dictionary 3")
         CALL HML_InitDictionary(cPtr = cPtrToDict3, errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_InitDictionary 3")
         CALL HML_AddDictKeyAndValue(cPtrToDict = cPtrToDict3, &
                                     cKey       = cKey,        &
                                     cValue     = cVal,        &
                                     errFlag    = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_AddDictKeyAndValue 2")
         CALL ptrToDictionary(cPtr = cPtrToDict3, dict = dict3,errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0, actualValue = flag, msg = "ptrToDictionary 3")
         CALL FTAssert(test = dict3 % containsKey(fKey),msg = "Dict3 contains key")
         CALL FTAssertEqual(expectedValue = dict3 % stringValueForKey(key = fKey, requestedLength = 5), &
                            actualValue   = str, &
                            msg           = "stringValueForKey 2")
         
         cPtrToDict4 = HML_AllocDictionary()
         CALL FTAssert(test = C_ASSOCIATED(cPtrToDict4),msg = "Associated cPtr to dictionary 4")
         CALL HML_InitDictionary(cPtr = cPtrToDict4, errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_InitDictionary 4")
         
         cKey = f_to_c_string(f_string = "DICT")
         CALL HML_AddDictForKey(cPtrToDict      = cPtrToDict4, &
                                cPtrToDictToAdd = cPtrToDict3, &
                                key             = cKey,        &
                                errFlag         = flag)
         CALL FTAssertEqual(expectedValue = 0, actualValue = flag, msg = "HML_AddDictForKey")
         CALL ptrToDictionary(cPtr = cPtrToDict4, dict = dict4, errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0, actualValue = flag, msg = "ptrToDictionary 4")
         CALL FTAssert(test = dict4 % containsKey("DICT"), msg = "Dict4 contains key DICT")
         obj => dict4 % objectForKey(key = "DICT")
         dict5 => valueDictionaryFromObject(obj)
         CALL FTAssert(test = dict5 % containsKey(fKey),msg = "Dict5 contains key")
         CALL FTAssertEqual(expectedValue = dict5 % stringValueForKey(key = fKey, requestedLength = 5), &
                            actualValue   = str, &
                            msg           = "stringValueForKey 5")
!
!        ---------------
!        Close out stuff
!        ---------------
         CALL HML_ReleaseDictionary(cPtr = cPtrToDict,errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_ReleaseDictionary")
         CALL HML_ReleaseList(cPtr = cPtrToList,errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_ReleaseList")
!
         CALL HML_ReleaseList(cPtr = cPtrToList2,errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_ReleaseList 2")
         CALL HML_ReleaseDictionary(cPtr = cPtrToDict2,errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_ReleaseDictiona 2")

         CALL HML_ReleaseDictionary(cPtr = cPtrToDict3, errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_ReleaseDictiona 3")

         CALL HML_ReleaseDictionary(cPtr = cPtrToDict4, errFlag = flag)
         CALL FTAssertEqual(expectedValue = 0,actualValue = flag, msg = "HML_ReleaseDictiona 4")
                  
      END SUBROUTINE BasicCollectionTests

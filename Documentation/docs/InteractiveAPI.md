#Fortran Library API

HOHQMesh can be accessed as a library to allow it to be manipulated interactively. At this point, interactivity is limited to run parameters, and not the model.

Library procedures are of the form HML_* (for HOHQMesh Library) and have the BIND(C) attribute so they can be accessed by anything that can deal with C. All procedures (except for the NewProject procedure) take a pointer to a project and either operate on it or inquire it for data. All return an error flag.

## Contents

1. [HOHQMesh API](#HOHQMesh)
2. [FTObjectLibrary API](#FTOL)

#HOHQMesh Library API<a name="HOHQMesh"></a>

 Constants currently defined as

	INTEGER, PARAMETER :: HML_ERROR_NONE = 0  ! Everything is OK
	INTEGER, PARAMETER :: HML_ERROR_MULTIPLE_REFERENCES = 1 ! Object has multiple references and cannot be deallocated
	INTEGER, PARAMETER :: HML_ERROR_DEALLOCATION = 2  ! Problem during deallocation
	INTEGER, PARAMETER :: HML_ERROR_NOT_A_PROJECT = 3  ! c_Ptr cannot be converted to a project pointer
    INTEGER, PARAMETER :: HML_ERROR_MEMORY_SIZE = 4  ! Array was allocated with the wrong size
    INTEGER, PARAMETER :: HML_ERROR_NO_OBJECT_FOR_REQUEST = 5  ! Trying to access a null pointer
    INTEGER, PARAMETER :: HML_ERROR_NULL_POINTER = 6  ! c_ptr is null
    INTEGER, PARAMETER :: HML_ERROR_STRING_TRUNCATED = 7  ! a string has been truncated
    INTEGER, PARAMETER :: HML_ERROR_NOT_A_DICT = 8  ! a c_ptr doesn't resolve to a FTDictionary
    INTEGER, PARAMETER :: HML_ERROR_NOT_A_LIST = 9 ! a c_ptr doesn't resolve to a FTLinkedList

All actions check to make sure that they are passed a proper 'MeshProject' object through the c_ptr.

Example usage (in fortran)

         projCPtr = HML_AllocProject()
         
         CALL HML_InitWithControlFile(cPtr      = projCPtr,            &
                                      cFileName = controlFileNameCStr, &
                                      errFlag   = flag)
         
         CALL HML_GenerateMesh(cPtr = projCPtr, errFlag = flag)
         
         PRINT *, HML_NumberOfNodes(projCPtr, flag),  &
                  HML_NumberOfEdges(projCPtr, flag),  &
                  HML_NumberOfElements(projCPtr, flag)
         
         CALL HML_WriteMesh(cPtr = projCPtr, errFlag = flag )
         CALL HML_WritePlotFile(cPtr = projCPtr, errFlag = flag )
         
         CALL HML_ReleaseProject(cPtr = projCPtr, errFlag = flag)
         
The errorFlag should be checked at each stage to make sure it is safe to continue.
## Actions
- **Project Creation:** Allocates memory for and returns a c_ptr to a project

		FUNCTION HML_AllocProject() BIND(C) RESULT(cPtr)
			TYPE(c_ptr)      :: cPtr

- **Project Destruction:** Deletes the project and all memory associated with it.


		SUBROUTINE HML_ReleaseProject(cPtr, errFlag)  BIND(C)
			TYPE(c_ptr)      :: cPtr
			INTEGER          :: errFlag
	Call this when done with a project. Forgetting to do so keeps all the memory around and may then leak. Rule: if `HML_AllocProject` is called there must be a corresponding call to `HML_ReleaseProject`.

- **Initializing a project** 

		SUBROUTINE HML_InitWithControlFile(cPtr, cFileName, errFlag)
			TYPE(c_ptr)      				       :: cPtr
			CHARACTER(KIND=c_char), DIMENSION(*)   :: cFileName
			INTEGER          				       :: errFlag

	The argument cFileName is a cString for the control file path.
- **Writing a mesh file**

		SUBROUTINE HML_WriteMesh(cPtr, errFlag)   BIND(C)
			TYPE(c_ptr) :: cPtr
			INTEGER     :: errFlag

	Writes a mesh file in the format and path as specified in the control file.
	
- **Writing a plot file**


		SUBROUTINE HML_WritePlotFile(cPtr, errFlag)   BIND(C)
			TYPE(c_ptr) :: cPtr
			INTEGER     :: errFlag

	Writes a plot file in tecplot format at the path as specified in the control file.
- **Running the test suite**

		SUBROUTINE HML_RunTests(cPathToRepo, numberOfFailedTests) BIND(C)
	      CHARACTER(KIND=c_char), DIMENSION(*) :: cPathToRepo
	      INTEGER(C_INT)                       :: numberOfFailedTests

	`cPathToRepo` is the path to the HOHQMesh directory in cString format.
- **Overriding the polynomial order**

		SUBROUTINE HML_SetPolynomialOrder(cPtr, p, errFlag)  BIND(C)
         TYPE(c_ptr)    :: cPtr
         INTEGER(C_INT) :: p
         INTEGER(C_INT) :: errFlag
	This procedure sets the polynomial order, p, for the mesh. If the mesh has already been generated with a different polynomial order, the element boundary values are re-computed at the new order.

## Accessors
Accessors are used to get and set project parameters and mesh results, e.g. to be able to create a Trixi native mesh format, or plotting information without passing through a mesh file.

- **Default string length**

		INTEGER(C_INT) FUNCTION DefaultCharacterLength() BIND(C)
	
	For string allocation outside of HOHQMesh.
- **Boundary name string length**

		INTEGER(C_INT) FUNCTION BoundaryNameLength() BIND(C)

	For string allocation outside of HOHQMesh
- **Number of nodes in a project's mesh**

		INTEGER FUNCTION HML_NumberOfNodes(cPtr, errFlag)   BIND(C)
		
- **Number of elements in a project's mesh**

		INTEGER FUNCTION HML_NumberOfElements(cPtr, errFlag)   BIND(C)
- **Number of edges in a project's mesh**

		INTEGER FUNCTION HML_NumberOfEdges(cPtr, errFlag)   BIND(C)
- **Array of node locations**

		SUBROUTINE HML_NodeLocations(cPtr, locationsArray, N, errFlag)  BIND(C)  
         	TYPE(c_ptr)          :: cPtr
        	INTEGER(C_INT)       :: N
        	REAL(KIND=C_DOUBLE)  :: locationsArray(3,N)
        	INTEGER(C_INT)       :: errFlag
	Allocate the `locationsArray` using `HML_NumberOfNodes` for N to determine the size and pass to the procedure.

- **Getting the polynomial order**

		SUBROUTINE HML_PolynomialOrder(cPtr, p, errFlag)
	      TYPE(c_ptr)    :: cPtr
         INTEGER(C_INT)  :: p, errFlag

	Inquiry function to see what polynomial order the project has defined.

- **Array of edge connectivty**

		SUBROUTINE HML_2DElementConnectivity(cPtr, connectivityArray, N, errFlag)   BIND(C)
			TYPE(c_ptr)     :: cPtr
			INTEGER (C_INT) :: N
			INTEGER(C_INT)  :: connectivityArray(6,N)
        	INTEGER(C_INT)  :: errFlag
	
	Allocate the `connectivityArray` using `HML_NumberOfElements` for N to determine the size and pass to the procedure.
	
	The procedure the six items for each edge needed for the ISM-v2 mesh file format:
	
           connectivityArray(1,j) = start node id
           connectivityArray(2,j) = end node id
           connectivityArray(3,j) = left element id
           connectivityArray(4,j) = right element id (or 0, if a boundary edge)
           connectivityArray(5,j) = element side for left element
           connectivityArray(6,j) = element side for right element signed for direction (or 0 for boundary edge)

- **Array of element connectivity**
	
		SUBROUTINE HML_2DElementConnectivity(cPtr, connectivityArray, N, errFlag)   BIND(C)
			TYPE(c_ptr)          :: cPtr
			INTEGER              :: N
			REAL(KIND=C_DOUBLE)  :: connectivityArray(4,N)
        	INTEGER(C_INT)       :: errFlag

	Allocate the `connectivityArray` using `HML_NumberOfEdges ` for N to determine the size and pass to the procedure.The procedure fills connectivityArray(4,j) with node IDs of the four corners of element j
- **Array of element edge points**

		SUBROUTINE HML_2DElementBoundaryPoints(cPtr, boundaryPoints, p, N, errFlag)    BIND(C)
         TYPE(c_ptr)       :: cPtr
         INTEGER(C_INT)    :: N, p
         REAL(C_DOUBLE)    :: boundaryPoints(3,p+1,4,N)
         INTEGER(C_INT)    :: errFlag
For each element, e, and each edge in the element, k, `boundaryPoints` contains the three xyz values at the p+1 Chebyshev points, along side k where $p$ is the polynomial order. Use `HML_PolynomialOrder` to get p.

- **Array of boundary curve flags**

		SUBROUTINE HML_2DElementEdgeFlag(cPtr, curveFlag, N, errFlag)    BIND(C)
         TYPE(c_ptr)       :: cPtr
         INTEGER(C_INT)    :: N
         INTEGER(C_INT)    :: curveFlag(4,N)
         INTEGER(C_INT)    :: errFlag
	Allocate the `curveFlag` array using `HML_NumberOfElements` for N to determine the size and pass to the procedure.The procedure fills curveFlag(k,j), k = 1,...,4 with either a 0 (side k is a straight line) or a 1 (side k is not a straight line).
	
- **Array of element boundary names**

	TBD
#FTObjectLibrary Library API<a name="FTOL"></a>

- **Create a new Dictionary**

		   FUNCTION HML_AllocDictionary() BIND(C) RESULT(cPtr)
		      TYPE( FTValueDictionary ), POINTER :: dict
		      TYPE(c_ptr)                        :: cPtr
- **Initialize a Dictionary**

		   SUBROUTINE HML_InitDictionary(cPtr, errFlag) BIND(C)
		      TYPE(c_ptr)    :: cPtr
		      INTEGER(C_INT) :: errFlag
- **Destruct a Dictionary**

		   SUBROUTINE HML_ReleaseDictionary(cPtr, errFlag)   BIND(C)
		      TYPE(c_ptr)                   :: cPtr
		      INTEGER(C_INT), INTENT(OUT)   :: errFlag
- **Add a key and value to a Dictionary**

		   SUBROUTINE HML_AddDictKeyAndValue(cPtrToDict, cKey, cValue, errFlag)  
		      TYPE(c_ptr)                          :: cPtrToDict
		      CHARACTER(KIND=c_char), DIMENSION(*) :: cKey, cValue
		      INTEGER(C_INT), INTENT(OUT)          :: errFlag
- **Add a Dictionary to another Dictionary**

		   SUBROUTINE HML_AddDictForKey(cPtrToDict, cPtrToDictToAdd, key, errFlag)  
		      TYPE(c_ptr)                          :: cPtrToDict,cPtrToDictToAdd
		      CHARACTER(KIND=c_char), DIMENSION(*) :: key
		      INTEGER(C_INT), INTENT(OUT)          :: errFlag
- **Add a two-dimensional array to a Dictionary**

		   SUBROUTINE HML_AddArrayToDict(array, N, M, cPtrToDict, errFlag)  
		      INTEGER(C_INT)                       :: N, M
		      REAL(KIND=C_DOUBLE)                  :: array(N,M)
		      TYPE(c_ptr)                          :: cPtrToDict
		      INTEGER(C_INT), INTENT(OUT)          :: errFlag
- **Create a new linked list**

		   FUNCTION HML_AllocList() BIND(C) RESULT(cPtr)
		      IMPLICIT NONE
		      TYPE( FTLinkedList ), POINTER :: list
		      TYPE(c_ptr)                   :: cPtr
- **Initialize a linked list**

		   SUBROUTINE HML_InitList(cPtr, errFlag) BIND(C)
		      TYPE(c_ptr)                          :: cPtr
		      INTEGER(C_INT)                       :: errFlag
- **Destruct a linked list**

		   SUBROUTINE HML_ReleaseList(cPtr, errFlag)   BIND(C)
		      TYPE(c_ptr)                   :: cPtr
		      INTEGER(C_INT), INTENT(OUT)   :: errFlag
- **Add a linked list to a Dictionary**

		   SUBROUTINE HML_AddListToDict( cPtrToList, cPtrToDict, errFlag)  
		      TYPE(c_ptr)                 :: cPtrToList,cPtrToDict
		      INTEGER(C_INT), INTENT(OUT) :: errFlag
- **Add a Dictionary to a Linked List**

		   SUBROUTINE HML_AddDictToList( cPtrToDictToAdd, cPtrToList, errFlag)  
		      TYPE(c_ptr)                          :: cPtrToList,cPtrToDictToAdd
		      INTEGER(C_INT), INTENT(OUT)          :: errFlag

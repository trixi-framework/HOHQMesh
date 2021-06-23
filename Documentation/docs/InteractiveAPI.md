#HOHQMesh Library API

All operations are to return an error flag. Constants currently defined as

	INTEGER, PARAMETER :: HML_ERROR_NONE = 0 
	INTEGER, PARAMETER :: HML_ERROR_MULTIPLE_REFERENCES = 1
	INTEGER, PARAMETER :: HML_ERROR_DEALLOCATION = 2  
	INTEGER, PARAMETER :: HML_ERROR_NOT_A_PROJECT = 3  
    INTEGER, PARAMETER :: HML_ERROR_MEMORY_SIZE = 4  
    INTEGER, PARAMETER :: HML_ERROR_NO_OBJECT_FOR_REQUEST = 5  

All actions check to make sure that they are passed a proper 'MeshProject' object through the c_ptr.

Example usage (in fortran)

         projCPtr = HML_NewProject()
         
         CALL HML_InitWithControlFile(cPtr     = projCPtr, &
                                      fileName = controlFileName, &
                                      errFlag  = flag)
         CALL StopOnError(flag)
         
         CALL HML_GenerateMesh(cPtr = projCPtr, errFlag = flag)
         CALL StopOnError(flag)
         
         PRINT *, HML_NumberOfNodes(projCPtr),  &
                  HML_NumberOfEdges(projCPtr),  &
                  HML_NumberOfElements(projCPtr)
         
         CALL HML_WriteMesh(cPtr = projCPtr, errFlag = flag )
         CALL StopOnError(flag)
         CALL HML_WritePlotFile(cPtr = projCPtr, errFlag = flag )
         CALL StopOnError(flag)
         
         CALL HML_CloseProject(cPtr = projCPtr, errFlag = flag)
         PRINT *, "errorFlag = ",flag


## Actions
- **Project Creation:** Allocates memory for and returns a c_ptr to a project

		FUNCTION HML_NewProject() BIND(C) RESULT(cPtr)
			TYPE(c_ptr)      :: cPtr

- **Project Destruction:** Deletes the project and all memory associated with it.


		SUBROUTINE HML_CloseProject(cPtr, errFlag)  BIND(C)
			TYPE(c_ptr)      :: cPtr
			INTEGER          :: errFlag
	Call this when done with a project. Forgetting to do so keeps all the memory around.

- **Initializing a project.** 

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
 		
## Accessors
Accessors are used to get and set project parameters and mesh results, e.g. to be able to create a Trixi native mesh format, or plotting information without passing through a mesh file.

- **Default string length**

		INTEGER(C_INT) FUNCTION DefaultCharacterLength() BIND(C)
	
	For string allocation
- **Boundary name string length**

		INTEGER(C_INT) FUNCTION BoundaryNameLength() BIND(C)

	For string allocation
- **Number of nodes in a project's mesh**

		INTEGER FUNCTION HML_NumberOfNodes(cPtr)   BIND(C)
		
- **Number of elements in a project's mesh**

		INTEGER FUNCTION HML_NumberOfElements(cPtr)   BIND(C)
- **Number of edges in a project's mesh**

		INTEGER FUNCTION HML_NumberOfEdges(cPtr)   BIND(C)
- **Array of node locations**

		SUBROUTINE HML_NodeLocations(cPtr, locationsArray, N, errFlag)  BIND(C)  
         	TYPE(c_ptr)          :: cPtr
        	INTEGER(C_INT)       :: N
        	REAL(KIND=C_DOUBLE)  :: locationsArray(3,N)
        	INTEGER(C_INT)       :: errFlag
- **Array of edge connectivty**

		SUBROUTINE HML_2DElementConnectivity(cPtr, connectivityArray, N, errFlag)   BIND(C)
			TYPE(c_ptr)     :: cPtr
			INTEGER (C_INT) :: N
			INTEGER(C_INT)  :: connectivityArray(6,N)
        	INTEGER(C_INT)  :: errFlag
	Fills the six items for each edge needed for the ISM-v2 mesh file format:
	
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

	Fills connectivityArray(4,j) with node IDs of the four corners of element j
- **Array of element edge curves**

- **Array of element boundary names**

## Setters

Setters would be used for setting parameters, as a way to override values set in a control file. 

- **Set the polynomial order of the boundaries**

		SUBROUTINE HML_SetPolynomialOrder(cPtr, n, errFlag)  BIND(C)

## Utility Functions

	SUBROUTINE StopOnError(errorFlag)  ! ERROR STOP if errorFlag /= 0
	   
	LOGICAL FUNCTION IsMeshProjectPtr(projectPtr) ! Checks integrity of C_F_POINTER conversion

#=
! HQM Fortran routines wrapped:
!
!      +FUNCTION   HML_AllocProject() BIND(C) RESULT(cPtr)
!      +SUBROUTINE HML_ReleaseProject(cPtr, errFlag)   BIND(C)
!      +SUBROUTINE HML_InitWithControlFile(cPtr, cFileName, errFlag) BIND(C)
!       SUBROUTINE HML_InitWithDictionary(cPtr, cPtrToDict, errFlag) BIND(C)
!
!      +SUBROUTINE HML_GenerateMesh(cPtr, errFlag)   BIND(C)
!      +SUBROUTINE HML_WriteMesh(cPtr, errFlag)   BIND(C)
!      +SUBROUTINE HML_WritePlotFile(cPtr, errFlag)   BIND(C)
!       FUNCTION HML_MeshFileFormat(self)  RESULT(fileFormat)
!
!      +INTEGER(C_INT) FUNCTION HML_DefaultCharacterLength() BIND(C)
!      +INTEGER(C_INT) FUNCTION HML_BoundaryNameLength() BIND(C)
!      +INTEGER(C_INT) FUNCTION HML_NumberOfNodes(cPtr, errFlag)   BIND(C)
!      +INTEGER(C_INT) FUNCTION HML_NumberOfElements(cPtr, errFlag)   BIND(C)
!      +INTEGER(C_INT) FUNCTION HML_NumberOfEdges(cPtr, errFlag)   BIND(C)
!
!      +SUBROUTINE HML_SetMeshFileName( cPtr, cFileName, errFlag)  BIND(c)
!      +SUBROUTINE HML_SetPlotFileName( cPtr, cFileName, errFlag)  BIND(c)
!       SUBROUTINE HML_MeshFileName(cPtr, nameAsCString, strBuf, errFlag) BIND(C)
!       SUBROUTINE HML_PlotFileName(cPtr, nameAsCString, strBuf, errFlag) BIND(C)
!      +SUBROUTINE HML_SetMeshFileFormat( cPtr, cString, errFlag)  
!
!      +SUBROUTINE HML_SetPolynomialOrder(cPtr, n, errFlag)  BIND(C)
!      +SUBROUTINE HML_PolynomialOrder(cPtr, p, errFlag)
!
!       +SUBROUTINE HML_NodeLocations(cPtr, locationsArray, N, errFlag)  BIND(C)
!       +SUBROUTINE HML_2DElementConnectivity(cPtr, connectivityArray, N, errFlag)    BIND(C)
!       SUBROUTINE HML_2DElementBoundaryNames(cPtr, namesArray, N, errFlag)    BIND(C)
!       SUBROUTINE HML_2DElementBoundaryPoints(cPtr, boundaryPoints, p, N, errFlag)    BIND(C)
!       SUBROUTINE HML_2DElementEdgeFlag(cPtr, curveFlag, N, errFlag)    BIND(C)
!       +SUBROUTINE HML_2DEdgeConnectivity(cPtr, connectivityArray, N, errFlag)  BIND(C)
!
! FTOL Functions wrapped
!
!      +FUNCTION   HML_AllocDictionary() BIND(C) RESULT(cPtr)
!      +FUNCTION   HML_AllocList() BIND(C) RESULT(cPtr)
!
!      +SUBROUTINE HML_InitDictionary(cPtr, errFlag) BIND(C)
!      +SUBROUTINE HML_ReleaseDictionary(cPtr, errFlag)   BIND(C)
!
!      +SUBROUTINE HML_InitList(cPtr, errFlag) BIND(C)
!      +SUBROUTINE HML_ReleaseList(cPtr, errFlag)   BIND(C)
!
!      +SUBROUTINE HML_AddDictKeyAndValue(cPtrToDict, cKey, cValue, errFlag)  
!      +SUBROUTINE HML_AddDictForKey(cPtrToDict, cPtrToDictToAdd, key, errFlag)  
!      +SUBROUTINE HML_AddDictToList( cPtrToDictToAdd, cPtrToList, errFlag)  
!      +SUBROUTINE HML_AddListToDict( cPtrToList, cPtrToDict, errFlag) 
!
!      +SUBROUTINE HML_AddArrayToDict(array, N, M, cPtrToDict, errFlag)  

=#
using Libdl: dlext

# Platform-independent library name
lib_path() = joinpath(".", "HOHQMeshLib."*dlext)

errorMessages = ["Object has multiple references and will not be deallocated",
                 "An error has occured during dallocation",
                 "The object passed is not a Mesh Project",
                 "The storage passed is not sufficiently large",
                 "No object is available yet for the request",
                 "The pointer being passed is NULL",
                 "The string has been truncated",
                 "The Cptr being passed is not a dictionary",
                 "The Cptr being passed is not a list"]

#
#----------------------------------------------------------------------------------
#
"""
    HML_DefaultCharacterLength

Inquiry function for the defined length of the Fortran strings
"""
function HML_DefaultCharacterLength()
    return ccall((:hml_defaultcharacterlength,lib_path()),Cint,())
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_BoundaryNameLength

Inquiry function for the defined length of the Fortran strings used for boundary names
"""
function HML_BoundaryNameLength()
    return ccall((:hml_boundarynamelength,lib_path()),Cint,())
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_AllocProject

Returns a Cptr to a new HOHQMesh project
"""
function HML_AllocProject()
    return ccall((:hml_allocproject,lib_path()),Ptr{Cvoid},())
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_CloseProject

Destruct the project
"""
function HML_ReleaseProject(proj)
    erp::Cint = 0
    ccall((:hml_releaseproject,lib_path()),
                  Cvoid,
                  (Ref{Ptr{Cvoid}},Ref{Cint}),
                  proj,erp)
    if erp > 0
        error(errorMessages[erp])
    end
end
#
#----------------------------------------------------------------------------------
#
"""
HML_InitWithDictionary(proj, dict::dict)

    Initializer that constructs a project from a Julia dictionary
"""
function HML_InitWithDictionary(proj, dict::Dict)
    erp::Cint = 0
    # Convert Julia dictionary to FTOL dictionary

    # ccall((:hml_initwithdictionary,lib_path()),
    #       Cvoid,
    #       (Ref{Ptr{Cvoid}},Ref{Ptr{Cvoid}},Ref{Cint}), 
    #       proj,projDict,erp)
    if erp > 0
        error(errorMessages[erp])
    end

end
#
#----------------------------------------------------------------------------------
#
"""
    HML_InitWithControlFile(proj, controlFilePath::String)

Initializer that constructs a project from a control file
"""
function HML_InitWithControlFile(proj, controlFilePath::String)
    erp::Cint = 0

    ccall((:hml_initwithcontrolfile,lib_path()),
          Cvoid,
          (Ref{Ptr{Cvoid}},Cstring,Ref{Cint}), 
          proj,controlFilePath,erp)
    if erp > 0
        error(errorMessages[erp])
    end

end
#
#----------------------------------------------------------------------------------
#
"""
    HML_GenerateMesh(proj)

Generate mesh with rules specified in the project
"""
function HML_GenerateMesh(proj)
    erp::Cint = 0

    ccall((:hml_generatemesh,lib_path()),
          Cvoid,
          (Ref{Ptr{Cvoid}},Ref{Cint}), proj, erp)
    if erp > 0
        error(errorMessages[erp])
    end
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_WriteMesh(proj)

Write the mesh associated with the project
"""
function HML_WriteMesh(proj)
    erp::Cint = 0

    ccall((:hml_writemesh,lib_path()),
          Cvoid,
          (Ref{Ptr{Cvoid}},Ref{Cint}), proj, erp)
    if erp > 0
        error(errorMessages[erp])
    end

end
#
#----------------------------------------------------------------------------------
#
"""
    HML_WriteMesh(proj, path::String)

Write the mesh associated with the project to the specified path
"""
function HML_WriteMesh(proj, path::AbstractString)
    HML_SetMeshFileName(proj,String)
    HML_WriteMesh(proj)
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_WritePlotFile(proj)

Write the plot file for the mesh associated with the project
"""
function HML_WritePlotFile(proj)
    erp::Cint = 0

    ccall((:hml_writeplotfile,lib_path()),
          Cvoid,
          (Ref{Ptr{Cvoid}},Ref{Cint}), proj, erp)
    if erp > 0
        error(errorMessages[erp])
    end

end
#
#----------------------------------------------------------------------------------
#
"""
    HML_WritePlotFile(proj, path::AbstractString)

Write the plot file for the mesh associated with the project
    to the specified path
"""
function HML_WritePlotFile(proj, path::AbstractString)
    HML_SetPLotFileName(proj,path)
    HML_WritePlotFile(proj)
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_SetMeshFileName

Set the mesh file name for the project
"""
function HML_SetMeshFileName(proj,fileName::AbstractString)
    erp::Cint = 0
    ccall((:hml_setmeshfilename,lib_path()),
          Cvoid,
          (Ref{Ptr{Cvoid}},Cstring,Ref{Cint}), 
          proj,fileName,erp)
    if erp > 0
        error(errorMessages[erp])
    end
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_SetMeshFileFormat

Set the format in which the mesh file will be written
"""
function HML_SetMeshFileFormat(proj,format::AbstractString)
    erp::Cint = 0
    ccall((:hml_setmeshfileformat,lib_path()),
          Cvoid,
          (Ref{Ptr{Cvoid}},Cstring,Ref{Cint}), 
          proj,format,erp)
    if erp > 0
        error(errorMessages[erp])
    end
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_NumberOfNodes(proj)

Returns the number of nodes in the project's mesh.
"""
function HML_NumberOfNodes(proj)
    erp::Cint    = 0
    nNodes::Cint = ccall((:hml_numberofnodes,lib_path()),
                    Cint,
                    (Ref{Ptr{Cvoid}},Ref{Cint}),
                    proj,erp)
    if erp > 0
        error(errorMessages[erp])
    end
    return nNodes      
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_NumberOfElements(proj)

Returns the number of elements in the project's mesh.
"""
function HML_NumberOfElements(proj)
    erp::Cint    = 0
    nEl::Cint = ccall((:hml_numberofelements,lib_path()),
                    Cint,
                    (Ref{Ptr{Cvoid}},Ref{Cint}),
                    proj,erp)
    if erp > 0
        error(errorMessages[erp])
    end
    return nEl      
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_NumberOfEdges(proj)

Returns the number of edges in the project's mesh.
"""
function HML_NumberOfEdges(proj)
    erp::Cint    = 0
    nEdges::Cint = ccall((:hml_numberofedges,lib_path()),
                    Cint,
                    (Ref{Ptr{Cvoid}},Ref{Cint}),
                    proj,erp)
    if erp > 0
        error(errorMessages[erp])
    end
    return nEdges      
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_SetPolynomialOrder(proj,order::Int64)

Set the polynomial order of the curved elements.
"""
function HML_SetPolynomialOrder(proj,order::Int64)
    erp::Cint = 0
    ccall((:hml_setpolynomialorder,lib_path()),
          Cvoid,
          (Ref{Ptr{Cvoid}},Ref{Cint},Ref{Cint}), 
          proj,order,erp)
    if erp > 0
        error(errorMessages[erp])
    end
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_SetSetPolynomialOrder(proj)

Set the polynomial order of the curved elements.
"""
function HML_PolynomialOrder(proj)
    erp::Cint = 0
    p::Cint   = 0
    ccall((:hml_polynomialorder,lib_path()),
          Cvoid,
          (Ref{Ptr{Cvoid}},Ref{Cint},Ref{Cint}), 
          proj,p,erp)
    if erp > 0
        error(errorMessages[erp])
    end
    return p
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_NodeLocations(proj)

 Fill an array of dimension (3,numberOfNodes) with the physical space node locations (x,y,z)
"""
function HML_NodeLocations(proj)
    erp::Cint = 0
    n::Int = HML_NumberOfNodes(proj)
    if n >0
        nodes = zeros(Cdouble,3,n)
        ccall((:hml_nodelocations,lib_path()),
        Cvoid,
        (Ref{Ptr{Cvoid}},Ref{Cdouble},Ref{Cint},Ref{Cint}), 
         proj,nodes,n,erp)
        if erp > 0
            error(errorMessages[erp])
        end
        return nodes
    end
    return nothing
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_2DEdgeConnectivity(proj)

Fill an array of dimension (6,numberOfEdges) with the connectivity 
needed for the ISM-v2 mesh file format. For edge j,
        connectivityArray(1,j) = start node id
        connectivityArray(2,j) = end node id
        connectivityArray(3,j) = left element id
        connectivityArray(4,j) = right element id (or 0, if a boundary edge)
        connectivityArray(5,j) = element side for left element
        connectivityArray(6,j) = element side for right element signed for direction (or 0 for boundary edge)
    """
function HML_2DEdgeConnectivity(proj)
    erp::Cint = 0
    n::Int = HML_NumberOfEdges(proj)
    if n >0
        arry = zeros(Cint,6,n)
        ccall((:hml_2dedgeconnectivity,lib_path()),
        Cvoid,
        (Ref{Ptr{Cvoid}},Ref{Cint},Ref{Cint},Ref{Cint}), 
         proj,arry,n,erp)
        if erp > 0
            error(errorMessages[erp])
        end
        return arry
    end
    return nothing
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_2DElementConnectivity

Fill an array, dimension (4,N), for a 2D quad element with the node IDs 
for the four corners for each of the N elements
"""
function HML_2DElementConnectivity(proj)
    erp::Cint = 0
    n::Int = HML_NumberOfElements(proj)
    if n >0
        arry = zeros(Cint,4,n)
        ccall((:hml_2delementconnectivity,lib_path()),
        Cvoid,
        (Ref{Ptr{Cvoid}},Ref{Cint},Ref{Cint},Ref{Cint}), 
         proj,arry,n,erp)
        if erp > 0
            error(errorMessages[erp])
        end
        return arry
    end
    return nothing
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_2DElementEdgeFlag(proj)

 Fill an array, dimension (4,N), for a 2D quad element, where 
 N is the number of elements. The value (k,e) is either 
    0  if side k of element e is a straight line OR
    1  if side k of element e is defined by some other curve
"""
function HML_2DElementEdgeFlag(proj)
    erp::Cint = 0
    n::Int = HML_NumberOfElements(proj)
    if n >0
        arry = zeros(Cint,4,n)
        ccall((:hml_2delementedgeflag,lib_path()),
        Cvoid,
        (Ref{Ptr{Cvoid}},Ref{Cint},Ref{Cint},Ref{Cint}), 
         proj,arry,n,erp)
        if erp > 0
            error(errorMessages[erp])
        end
        return arry
    end
    return nothing
end
#
#----------------------------------------------------------------------------------
#
"""
    HML_2DElementBoundaryPoints(proj)

Fill an array, dimension (3,p+1,4,N), for a 2D quad element, where 
  N is the number of elements and p is the polynomial order.
"""
function HML_2DElementBoundaryPoints(proj)
    erp::Cint = 0
    N::Int = HML_NumberOfNodes(proj)
    p::Int = HML_PolynomialOrder(proj)

    if N >0
        boundaryPoints = zeros(Cdouble,3,p+1,4,N)
        ccall((:hml_2delementboundarypoints,lib_path()),
        Cvoid,
        (Ref{Ptr{Cvoid}},Ref{Cdouble},Ref{Cint},Ref{Cint},Ref{Cint}), 
         proj,boundaryPoints,p,N,erp)
        if erp > 0
            error(errorMessages[erp])
        end
        return nodes
    end
    return nothing
end
#
#----------------------------------------------------------------------------------
#
#                           FTOBJECTLIBRARY ACTIONS
#
#----------------------------------------------------------------------------------
#
"""
    HML_AllocDictionary

Create a new, uninitialized FTDictionary
"""
function HML_AllocDictionary()
    return ccall((:hml_allocdictionary,lib_path()),Ptr{Cvoid},())
end
"""
    HML_AllocDictionary

Create a new, uninitialized FTLinkedList
"""
function HML_AllocList()
    return ccall((:hml_alloclist,lib_path()),Ptr{Cvoid},())
end
"""
    HML_InitDictionary

Initialize the FTDictionary referenced as a Cptr.
"""
function HML_InitDictionary(dict)
    erp::Cint = 0
    ccall((:hml_initdictionary,lib_path()),
          Cvoid,
          (Ref{Ptr{Cvoid}},Ref{Cint}), 
          dict,erp)
    if erp > 0
        error(errorMessages[erp])
    end
end
"""
    HML_CloseDictionary

Destruct the FTDictionary
"""
function HML_ReleaseDictionary(dict)
    erp::Cint = 0
    ccall((:hml_closedictionary,lib_path()),
                  Cvoid,
                  (Ref{Ptr{Cvoid}},Ref{Cint}),
                  dict,erp)
    if erp > 0
        error(errorMessages[erp])
    end
end
"""
    HML_InitList

Initialize the FTLinkedList referenced as a Cptr.
"""
function HML_InitList(list)
    erp::Cint = 0
    ccall((:hml_initlist,lib_path()),
          Cvoid,
          (Ref{Ptr{Cvoid}},Ref{Cint}), 
          list,erp)
    if erp > 0
        error(errorMessages[erp])
    end
end
"""
    HML_CloseList

Destruct the FTLinkedList
"""
function HML_ReleaseList(list)
    erp::Cint = 0
    ccall((:hml_closelist,lib_path()),
                  Cvoid,
                  (Ref{Ptr{Cvoid}},Ref{Cint}),
                  list,erp)
    if erp > 0
        error(errorMessages[erp])
    end
end
"""
HML_AddDictKeyAndValue(dict, key::String, value::String)

Add a value, encoded as a string, for a given key to the FTDictionary.
"""
function HML_AddDictKeyAndValue(dict, key::String, value::String)
    erp::Cint = 0
    ccall((:hml_adddictkeyandvalue,lib_path()),
          Cvoid,
          (Ref{Ptr{Cvoid}},Cstring,Cstring,Ref{Cint}), 
          dict,key,value,erp)
    if erp > 0
        error(errorMessages[erp])
    end
end
"""
    HML_AddDictForKey(cPtrToDict, cPtrToDictToAdd, key::String)

Add a dictionary, encoded as a Cptr, for a given key to the FTDictionary.
"""
function HML_AddDictForKey(cPtrToDict, cPtrToDictToAdd, key::String)
    erp::Cint = 0
    ccall((:hml_adddictforkey,lib_path()),
          Cvoid,
          (Ref{Ptr{Cvoid}},Ref{Ptr{Cvoid}},Cstring,Ref{Cint}), 
          cPtrToDict,cPtrToDictToAdd,key,erp)
    if erp > 0
        error(errorMessages[erp])
    end
end
"""
    HML_AddDictToList(cPtrToDict, cPtrToDictToAdd, key::String)

Add a dictionary, encoded as a Cptr, for a given key to the FTLinkedList.
"""
function HML_AddDictToList(cPtrToDict, list)
    erp::Cint = 0
    ccall((:hml_adddicttolist,lib_path()),
          Cvoid,
          (Ref{Ptr{Cvoid}},Ref{Ptr{Cvoid}},Ref{Cint}), 
          cPtrToDict,list,erp)
    if erp > 0
        error(errorMessages[erp])
    end
end
"""
    HML_AddListToDict(cPtrToDict, cPtrToDictToAdd, key::String)

Add a dictionary, encoded as a Cptr, to a FTLinkedList, also a Cptr.
"""
function HML_AddListToDict( list, cPtrToDict)
    erp::Cint = 0
    ccall((:hml_addlisttodict,lib_path()),
          Cvoid,
          (Ref{Ptr{Cvoid}},Ref{Ptr{Cvoid}},Ref{Cint}), 
          list,cPtrToDict,erp)
    if erp > 0
        error(errorMessages[erp])
    end
end
"""
    HML_AddArrayToDict(array::Array{Float64,Float64}, n::Int64, m::Int64, cPtrToDict)

Add a dictionary, encoded as a Cptr, to a FTLinkedList, also a Cptr.
"""
function HML_AddArrayToDict( array::Matrix{Float64}, n::Int64, m::Int64, cPtrToDict)
    erp::Cint = 0
    ccall((:hml_addarraytodict,lib_path()),
          Cvoid,
          (Ptr{Matrix{Float64}},Ref{Cint},Ref{Cint},Ref{Ptr{Cvoid}},Ref{Cint}), 
          array,n,m,cPtrToDict,erp)
    if erp > 0
        error(errorMessages[erp])
    end
end
"""
    HML_PrintDictionary(cPtrToDict)

Print out the dictionary. For debugging, basically.
"""
function HML_PrintDictionary(cPtrToDict)
    erp::Cint = 0
    ccall((:hml_printdict,lib_path()),
          Cvoid,
          (Ref{Ptr{Cvoid}},Ref{Cint}), 
          cPtrToDict,erp)
    if erp > 0
        error(errorMessages[erp])
    end
end

function exerciseDict()
    d = HML_AllocDictionary()
    HML_InitDictionary(d)
    HML_AddDictKeyAndValue(d,"key1","vvalue")
    HML_PrintDictionary(d)
end

function exerciseMesher()
    p = HML_AllocProject()
    HML_InitWithControlFile(p,"CavityRamp.control")
    HML_GenerateMesh(p)
    # HML_WriteMesh(p)
    #nodes = HML_NodeLocations(p)
    n = HML_NumberOfNodes(p)
    println(n)
    m = HML_NumberOfElements(p)
    println(m)
    con = HML_2DElementConnectivity(p)
    #display(con)
    HML_ReleaseProject(p)
end

#=
! HQM Fortran routines wrapped:
!
!      FUNCTION   HML_AllocProject() BIND(C) RESULT(cPtr)
!      SUBROUTINE HML_CloseProject(cPtr, errFlag)   BIND(C)
!      SUBROUTINE HML_InitWithControlFile(cPtr, cFileName, errFlag) BIND(C)
!      SUBROUTINE HML_InitWithDictionary(cPtr, cPtrToDict, errFlag) BIND(C)
!      SUBROUTINE HML_GenerateMesh(cPtr, errFlag)   BIND(C)
!      SUBROUTINE HML_WriteMesh(cPtr, errFlag)   BIND(C)
!      SUBROUTINE HML_WritePlotFile(cPtr, errFlag)   BIND(C)
!      FUNCTION HML_MeshFileFormat(self)  RESULT(fileFormat)
!
!      INTEGER(C_INT) FUNCTION HML_DefaultCharacterLength() BIND(C)
!      INTEGER(C_INT) FUNCTION HML_BoundaryNameLength() BIND(C)
!      INTEGER(C_INT) FUNCTION HML_NumberOfNodes(cPtr, errFlag)   BIND(C)
!      INTEGER(C_INT) FUNCTION HML_NumberOfElements(cPtr, errFlag)   BIND(C)
!      INTEGER(C_INT) FUNCTION HML_NumberOfEdges(cPtr, errFlag)   BIND(C)
!
!      SUBROUTINE HML_SetMeshFileName( cPtr, cFileName, errFlag)  BIND(c)
!      SUBROUTINE HML_SetPlotFileName( cPtr, cFileName, errFlag)  BIND(c)
!      SUBROUTINE HML_MeshFileName(cPtr, nameAsCString, strBuf, errFlag) BIND(C)
!      SUBROUTINE HML_PlotFileName(cPtr, nameAsCString, strBuf, errFlag) BIND(C)
!      SUBROUTINE HML_SetMeshFileFormat( cPtr, cString, errFlag)  
!      SUBROUTINE HML_NodeLocations(cPtr, locationsArray, N, errFlag)  BIND(C)
!      SUBROUTINE HML_SetPolynomialOrder(cPtr, n, errFlag)  BIND(C)
!      SUBROUTINE HML_PolynomialOrder(cPtr, p, errFlag)  
!      SUBROUTINE HML_2DElementConnectivity(cPtr, connectivityArray, N, errFlag)    BIND(C)
!      SUBROUTINE HML_2DElementBoundaryNames(cPtr, namesArray, N, errFlag)    BIND(C)
!      SUBROUTINE HML_2DElementBoundaryPoints(cPtr, boundaryPoints, p, N, errFlag)    BIND(C)
!      SUBROUTINE HML_2DElementEdgeFlag(cPtr, curveFlag, N, errFlag)    BIND(C)
!      SUBROUTINE HML_2DEdgeConnectivity(cPtr, connectivityArray, N, errFlag)  BIND(C)
!
! FTOL Functions wrapped
!
!      FUNCTION   HML_AllocDictionary() BIND(C) RESULT(cPtr)
!      FUNCTION   HML_AllocList() BIND(C) RESULT(cPtr)
!
!      SUBROUTINE HML_InitDictionary(cPtr, errFlag) BIND(C)
!      SUBROUTINE HML_CloseDictionary(cPtr, errFlag)   BIND(C)
!
!      SUBROUTINE HML_InitList(cPtr, errFlag) BIND(C)
!      SUBROUTINE HML_CloseList(cPtr, errFlag)   BIND(C)
!
!      SUBROUTINE HML_AddDictKeyAndValue(cPtrToDict, cKey, cValue, errFlag)  
!      SUBROUTINE HML_AddDictForKey(cPtrToDict, cPtrToDictToAdd, key, errFlag)  
!      SUBROUTINE HML_AddDictToList( cPtrToDictToAdd, cPtrToList, errFlag)  
!      SUBROUTINE HML_AddListToDict( cPtrToList, cPtrToDict, errFlag) 
!
!      SUBROUTINE HML_AddArrayToDict(array, N, M, cPtrToDict, errFlag)  

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

"""
    HML_DefaultCharacterLength

Inquiry function for the defined length of the Fortran strings
"""
function HML_DefaultCharacterLength()
    return ccall((:hml_defaultcharacterlength,lib_path()),Cint,())
end
"""
    HML_BoundaryNameLength

Inquiry function for the defined length of the Fortran strings used for boundary names
"""
function HML_BoundaryNameLength()
    return ccall((:hml_boundarynamelength,lib_path()),Cint,())
end
"""
    HML_AllocProject

Returns a Cptr to a new HOHQMesh project
"""
function HML_AllocProject()
    return ccall((:hml_allocproject,lib_path()),Ptr{Cvoid},())
end
"""
    HML_CloseProject

Destruct the project
"""
function HMLReleaseProject(proj)
    erp::Cint = 0
    ccall((:hml_closeproject,lib_path()),
                  Cvoid,
                  (Ref{Ptr{Cvoid}},Ref{Cint}),
                  proj,erp)
    if erp > 0
        error(errorMessages[erp])
    end
end
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
# FTOBJECTLIBRARY ACTIONS
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

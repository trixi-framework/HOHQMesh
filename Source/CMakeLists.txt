set(HOHQMESH "HOHQMesh")
set(HOHQMESH_LIB "hohqmesh")

file(GLOB_RECURSE HOHQMESH_SRC ${CMAKE_CURRENT_SOURCE_DIR} "*.f90")

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)
            
add_executable(${HOHQMESH} ${HOHQMESH_SRC})
target_link_libraries(${HOHQMESH} "${FTOBJECTLIBRARY_LIBRARY}")

install(TARGETS ${HOHQMESH})

# Note: Separate library build disabled in https://github.com/trixi-framework/HOHQMesh/pull/123
#       since it caused issues on macOS
#
# add_library(${HOHQMESH_LIB} STATIC 
#             ${HOHQMESH_SRC})
# 
# Install libhohqmesh.a
# INSTALL(TARGETS ${HOHQMESH_LIB} 
#         ARCHIVE DESTINATION lib
#         LIBRARY DESTINATION lib)
#
# Add .mod files to include/ directory 
# install(DIRECTORY ${CMAKE_BINARY_DIR}/include DESTINATION .)

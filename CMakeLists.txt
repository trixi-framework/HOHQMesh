cmake_minimum_required(VERSION 3.5)

set(FFLAGS "-cpp -O" CACHE STRING "Fortran compiler flags")

message("---------------  cmake START -------------------")
message("-- FC:                   ${FC}")
message("-- FFLAGS:               ${FFLAGS}")
message("-- CMAKE_INSTALL_PREFIX: ${CMAKE_INSTALL_PREFIX}")
message("------------------------------------------------")

set(CMAKE_Fortran_COMPILER_INIT ${FC})
set(CMAKE_Fortran_FLAGS "${FFLAGS} ${CMAKE_Fortran_FLAGS}")

find_package(FTObjectLibrary REQUIRED)

include_directories("${FTOBJECTLIBRARY_INCLUDE_DIRS}")

project(HOHQMesh Fortran)

if(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    message(FATAL_ERROR "Fortran compiler does not support F90")
endif(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

add_subdirectory(${CMAKE_SOURCE_DIR}/Source)

message("-- HOHQMESH -------------  cmake DONE --------------------")

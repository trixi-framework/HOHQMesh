CMAKE_MINIMUM_REQUIRED(VERSION 3.0.2)

SET(FFLAGS "-cpp -O" CACHE STRING "Fortran compiler flags")

MESSAGE("---------------  cmake START -------------------")
MESSAGE("-- FC:                   ${FC}")
MESSAGE("-- FFLAGS:               ${FFLAGS}")
MESSAGE("-- CMAKE_INSTALL_PREFIX: ${CMAKE_INSTALL_PREFIX}")
MESSAGE("------------------------------------------------")

SET(CMAKE_Fortran_COMPILER_INIT ${FC})
SET(CMAKE_Fortran_FLAGS "${FFLAGS} ${CMAKE_Fortran_FLAGS}")

FIND_PACKAGE(FTObjectLibrary REQUIRED)

INCLUDE_DIRECTORIES("${FTOBJECTLIBRARY_INCLUDE_DIRS}")

PROJECT(HOHQMesh Fortran)
SET(VERSION 1.0.2-pre)
SET(CMAKE_VERBOSE_MAKEFILE ON)

IF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    MESSAGE(FATAL_ERROR "Fortran compiler does not support F90")
ENDIF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

ADD_SUBDIRECTORY(${CMAKE_SOURCE_DIR}/Source)

message("-- HOHQMESH -------------  cmake DONE --------------------")

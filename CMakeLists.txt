cmake_minimum_required (VERSION 2.6)
project(PARF)
enable_language(Fortran)
if(NOT DEFINED ${CMAKE_BUILD_TYPE})
   set(CMAKE_BUILD_TYPE RELEASE)
endif()
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
set(CMAKE_C_FLAGS_RELEASE "-Wall -pg --static")
set(CMAKE_C_FLAGS_DEBUG "-Wall -g -pg --static")
set(PAR none)
message("Fortran compiler: " ${Fortran_COMPILER_NAME})
if(Fortran_COMPILER_NAME MATCHES "gfortran")
   message(${CMAKE_BUILD_TYPE})
   set(CMAKE_Fortran_FLAGS_RELEASE "-fno-range-check -pg --static -O3")
   set(CMAKE_Fortran_FLAGS_DEBUG "-fno-range-check -g -pg --static -O0")
elseif (Fortran_COMPILER_NAME MATCHES "ifort")
   set(CMAKE_Fortran_FLAGS_RELEASE "-pg -CB -traceback --static -O3")
   set(CMAKE_Fortran_FLAGS_DEBUG "-g -pg -CB -traceback --static -O0")
elseif (Fortran_COMPILER_NAME MATCHES "mpif90")
   set(CMAKE_C_FLAGS_RELEASE "-Wall -pg")
   set(CMAKE_C_FLAGS_DEBUG "-Wall -g -pg")
   set(CMAKE_Fortran_FLAGS_RELEASE "-fno-range-check -g -pg -O3")
   set(CMAKE_Fortran_FLAGS_DEBUG "-fno-range-check -g -pg -O0")
   set(PAR mpi)
endif()
set(FSOURCES "bitvectors.f90"
             "bootstraps.f90"
             "forests.f90"
             "graphics.f90"
             "importances.f90"
             "instancesets.f90"
             "main.f90"
             "options.f90"
             "prototypes.f90"
             "trees.f90"
             "utilities.f90")
set(CSOURCES "support.c")
add_executable(parf ${FSOURCES} ${CSOURCES} "parallel/${PAR}.f90")

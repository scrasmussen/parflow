cmake_minimum_required (VERSION 3.5.2)

### Build Flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELWITHDEBINFO} -fbacktrace")
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -fbacktrace -fcheck=all" )
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELWITHDEBINFO} -traceback")
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -traceback -check all")
endif()

### Find and Include ESMF
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules/")
if (NOT TARGET ESMF)
  find_package(ESMF MODULE REQUIRED)
  if (NOT ESMF_FOUND)
    message(FATAL_ERROR "ESMF library not found. Please set environment variable ESMFMKFILE.")
  endif (NOT ESMF_FOUND)
endif (NOT TARGET ESMF)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

### Library Files
list(APPEND pf_nuopc_files
  src/pf_nuopc_cap.F90
  src/pf_nuopc_fields.F90
  src/pf_nuopc_flags.F90
)

### New Library: parflow_nuopc
add_library(parflow_nuopc STATIC ${pf_nuopc_files})
add_dependencies(parflow_nuopc pfsimulator)
include_directories(parflow_nuopc include)
target_link_libraries(parflow_nuopc PUBLIC ESMF)
target_link_libraries(parflow_nuopc PUBLIC pfsimulator)

### Install Library
install(TARGETS parflow_nuopc DESTINATION lib)
install(FILES ${CMAKE_BINARY_DIR}/include/parflow_nuopc.mod DESTINATION include)

### Add NUOPC Test Application
if (CMAKE_BUILD_TYPE MATCHES "Debug")
  add_subdirectory (test)
endif()

cmake_minimum_required (VERSION 3.5.2)

### Build Flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELWITHDEBINFO} -g -fbacktrace")
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -g -fbacktrace -O0 -fcheck=bounds -ffpe-trap=invalid,zero,overflow,underflow" )
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELWITHDEBINFO} -g -traceback")
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -g -traceback -O0 -check -check noarg_temp_created -check nopointer -fpe0 -ftrapuv -link_mpi=dbg")
endif()

### Find and Include ESMF
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules/")
if (NOT TARGET ESMF)
  find_package(ESMF MODULE REQUIRED)
endif (NOT TARGET ESMF)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

### Library Files
list(APPEND pf_nuopc_files
  pf_nuopc_cap.F90
  pf_nuopc_fields.F90
  pf_nuopc_flags.F90
)

### New Library: parflow_nuopc
add_library(parflow_nuopc STATIC ${pf_nuopc_files})
add_dependencies(parflow_nuopc pfsimulator)
target_link_libraries(parflow_nuopc PUBLIC ESMF)
target_link_libraries(parflow_nuopc PUBLIC pfsimulator)

### Install Library
install(TARGETS parflow_nuopc DESTINATION lib)
install(DIRECTORY ${CMAKE_BINARY_DIR}/include/ DESTINATION include)

### Add Test Application
if (NOT CMAKE_BUILD_TYPE MATCHES "Release")
  add_subdirectory (test)
endif()

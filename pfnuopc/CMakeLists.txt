cmake_minimum_required (VERSION 3.5.2)

### Find and Include ESMF
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules/")
if (NOT TARGET ESMF)
  find_package(ESMF MODULE REQUIRED)
endif (NOT TARGET ESMF)

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

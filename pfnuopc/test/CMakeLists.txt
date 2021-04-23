cmake_minimum_required (VERSION 3.5.2)

### nuopc test application files
list(APPEND pf_nuopc_test_files
  pf_nuopc_test_app.F90
  pf_nuopc_test_drv.F90
  pf_nuopc_test_lnd.F90
)

### nuopc test application
add_executable(parflow_nuopc_test ${pf_nuopc_test_files})
add_dependencies(parflow_nuopc_test parflow_nuopc)
target_include_directories(parflow_nuopc_test PUBLIC "${CMAKE_CURRENT_BINARY_DIR}/..")
target_link_libraries(parflow_nuopc_test parflow_nuopc)
target_link_libraries(parflow_nuopc_test ESMF)
install(TARGETS parflow_nuopc_test DESTINATION bin)

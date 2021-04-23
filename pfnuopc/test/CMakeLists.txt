cmake_minimum_required (VERSION 3.5.2)

### parflow_nuopc_test files
list(APPEND pf_nuopc_test_files
  pf_nuopc_test_app.F90
  pf_nuopc_test_drv.F90
  pf_nuopc_test_lnd.F90
)

### parflow_nuopc_test application
add_executable(parflow-nuopc-test ${pf_nuopc_test_files})
add_dependencies(parflow-nuopc-test parflow_nuopc)
target_include_directories(parflow-nuopc-test PUBLIC "${CMAKE_BINARY_DIR}/include")
target_link_libraries(parflow-nuopc-test parflow_nuopc)
target_link_libraries(parflow-nuopc-test ESMF)
install(TARGETS parflow-nuopc-test DESTINATION bin)

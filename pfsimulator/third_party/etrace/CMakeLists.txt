
if (${PARFLOW_ENABLE_ETRACE})
  set(SRC_FILES ptrace.c)

  add_library(etrace STATIC ${SRC_FILES})

  install(TARGETS etrace EXPORT ParFlow DESTINATION lib)
  install(FILES etrace DESTINATION bin)
endif (${PARFLOW_ENABLE_ETRACE})






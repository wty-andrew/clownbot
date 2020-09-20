set(MAKE_LISP_SCRIPT "@(CMAKE_CURRENT_SOURCE_DIR)/scripts/make_lisp_script")

function(add_lisp_script filename system_name entry_point)
  if(NOT ${ARGC} EQUAL 3)
    message(FATAL_ERROR "add_lisp_script can only have 3 arguments")
  endif()

  set(output_dir ${CATKIN_DEVEL_PREFIX}/${CATKIN_PACKAGE_BIN_DESTINATION})
  set(script ${output_dir}/${filename})
  set(target _roslisp_${filename})

  file(MAKE_DIRECTORY ${output_dir})

  add_custom_command(
    OUTPUT ${script}
    COMMAND ${MAKE_LISP_SCRIPT} ${PROJECT_NAME} ${system_name} ${entry_point} ${script}
    DEPENDS ${MAKE_LISP_SCRIPT}
    COMMENT "Generating lisp test script ${output_dir}/${filename}"
    VERBATIM)
  add_custom_target(${target} ALL
    DEPENDS ${script})

  install(PROGRAMS ${script} DESTINATION ${CATKIN_PACKAGE_BIN_DESTINATION})
endfunction()


_generate_function_if_testing_is_disabled("catkin_add_lisptests")

function(catkin_add_lisptests filename system_name entry_point)
  if(NOT ${ARGC} EQUAL 3)
    message(FATAL_ERROR "catkin_add_lisptests can only have 3 arguments")
  endif()

  cmake_parse_arguments(ARG "" "TIMEOUT;WORKING_DIRECTORY" "" ${ARGN})
  if(ARG_TIMEOUT)
    message(WARNING "TIMEOUT argument to catkin_add_lisptests() is ignored")
  endif()

  add_lisp_script(${filename} ${system_name} ${entry_point})

  # TODO: figure out how to share the three duplicate variables with add_lisp_script
  set(output_dir ${CATKIN_DEVEL_PREFIX}/${CATKIN_PACKAGE_BIN_DESTINATION})
  set(script ${output_dir}/${filename})
  set(target _roslisp_${filename})
  set(test_output_dir ${CATKIN_TEST_RESULTS_DIR}/${PROJECT_NAME})
  set(xunit_filename lisptests-${filename}.xml)
  add_dependencies(tests ${target})

  file(MAKE_DIRECTORY ${test_output_dir})

  catkin_run_tests_target("lisptests" ${target} ${xunit_filename}
    COMMAND "${script} --gtest_output=xml:${test_output_dir}/${xunit_filename}"
    DEPENDENCIES ${target}
    WORKING_DIRECTORY ${ARG_WORKING_DIRECTORY})
endfunction()

---
title: Testing Setup
---

It seems the `roslisp_testing` package mentioned in [ros wiki](http://wiki.ros.org/roslisp_support/Tutorials/UnitTestingwithRT) no longer exist anymore. However, the wiki page provide detailed enough information so we can roll our own solution, here are some notes taken while trying to replicate the functionality.

<!--truncate-->

## Integration with Rostest

In order to make our own test compliant with rostest, we need to convert the test result into google test compatible format, which actually uses the JUnit XML format[^1].

The [wiki][1] page already provide the structure templates and the function signature of the entry point

```lisp
(defstruct gtestfailure
  type ;; string
  message ;; string
  )

(defstruct gtestcase
  classname ;; string
  name ;; string
  time ;; float
  failures ;; list of gtestfailure
  )

(defstruct gtestsuite
  name ;; string
  time ;; float
  testcases ;; list of gtestcase
  sysout ;; string
  syserr ;; string
  )

(defun run-suites-write-gtest-file (posix-args suite-fun transform-fun)
  ...)
```

To fill in the `run-suites-write-gtest-file`, we need a function to convert the structures to something like below

```xml
<testsuites name="AllTests" ...>
  <testsuite name="test_case_name" ...>
    <testcase name="test_name" ...>
      <failure message="..."/>
      <failure message="..."/>
      <failure message="..."/>
    </testcase>
  </testsuite>
</testsuites>
```

and a function to find out xml output path from `posix-args` (rostest will pass it through the `--gtest_output` flag).

`suite-fun` and `tranform-fun` are customized functions for the testing framework being used
* `suite-fun` runs the tests and returns a test result object
* `tranform-fun` converts the test result object into the `gtestsuite` structure

A minimal implementation for `run-suites-write-gtest-file` would be

```lisp
(defun run-suites-write-gtest-file (posix-args suite-fun transform-fun)
  (let* ((output-path (find-gtest-output-path posix-args)
         (test-results (funcall suite-fun))
         (gtest-results (funcall transform-fun test-results))
    (with-open-file (*standard-output* output-path :direction :output
                                                   :if-exists :supersede)
      (gtest->xml gtest-results))))))
```

For a practical example, suppose we want to run it with [lisp-unit][2], `suite-fun` will be the function to run the tests: `(lambda () (lisp-unit:run-tests :all :my-package))`, `transform-fun` can be implemented by converting the following objects

| lisp-unit         | gtest          |
|-------------------|----------------|
| `failure`         | `gtestfailure` |
| `test-result`     | `gtestcase`    |
| `test-results-db` | `gtestsuite`   |

the one-to-one correspondence is not a coincidence since [lisp-unit][2] claimed to be inspired by JUnit, the `transform-fun` happened to be quite trivial in this case.

## Executable Script

The ros wiki page on [organizing files for roslisp][3] have thorough information on creating lisp executable scripts, the easiest way is to use the cmake function

```cmake title="catkin_ws/src/my_package/CMakeLists.txt"
# the function arguments are output, system_name, and entry_point respectively
add_lisp_executable(my_node my-system my-package:my-func)
```

which should generate the following script

```bash title="catkin_ws/devel/lib/my_package/my_node" {2}
#!/usr/bin/env sh
"true"; exec /usr/bin/env sbcl --noinform --load /opt/ros/melodic/share/roslisp/scripts/roslisp-sbcl-init --script "$0" "$@"

(ros-load:load-system "my_package" "my-system")
(my-package:my-func)
```

The script will first be run by shell and then `sbcl`, the highlighted line shows a clever trick, `"true"` and the semicolon are both valid syntax for `sh` and `sbcl`, `sbcl` ignores shebang by default so the entire script is a valid lisp script (`"true"` just evaluates to a string which does no harm).

Another trick found from [roswell][4] uses block comments, in addition, the mode line will inform emacs to choose the correct mode for editing

```bash {2}
#!/usr/bin/env sh
#|-*- mode:lisp -*-|#
#|
exec /usr/bin/env sbcl --noinform --script "$0" "$@"
|#
```

Now back to the lisp executable script, notice there's only a single entry function that takes no arguments, so a wrapper function is required if we are going to run `run-suites-write-gtest-file`. The other problem is `sbcl` does not load `.sbclrc` when running with `--script` option (according to [this discussion][5] on stackoverflow), we have to load `quicklisp` ourselves in order to use external libraries.

Here's the final modified template for lisp executable script

```bash
#!/usr/bin/env sh
#|-*- mode:lisp -*-|#
#|
exec /usr/bin/env sbcl --noinform \
                       --load $(rospack find roslisp)/scripts/roslisp-sbcl-init \
                       --load $HOME/quicklisp/setup.lisp \
                       --script "$0" "$@"
|#

(ros-load:load-system "my_package" "my-system")
(my-package:my-func)
```

To automate the script generation process, the `add_lisp_executable` function (see [roslisp-extras.cmake.em][6]) is copied and named `add_lisp_script` in the project.

## Integration with Catkin

It would be great if our lisp test can be invoked automatically when running the `catkin_make run_tests` command, this is done in python and C++ through the [catkin_add_nosetests][7] and [catkin_add_gmock][8] cmake function, they use the internal function `catkin_run_tests_target` to add in the dependency.

Combined with the `add_lisp_script` from the above section, here's the new `catkin_add_lisptests` cmake function

```cmake
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
```

It takes the same arguments as `add_lisp_executable`, uses rostest to run the generated executable.

The whole implementation mentioned in this page can be found in the [clownbot_common][9] package. We should now have a testing setup that integrates well with ROS.

[1]: http://wiki.ros.org/roslisp_support/Tutorials/UnitTestingwithRT#Other_test_frameworks
[2]: https://github.com/OdonataResearchLLC/lisp-unit
[3]: http://wiki.ros.org/roslisp/Tutorials/OrganizingFiles
[4]: https://github.com/roswell/roswell
[5]: https://stackoverflow.com/questions/9229526
[6]: https://github.com/ros/roslisp/blob/master/cmake/roslisp-extras.cmake.em
[7]: https://github.com/ros/catkin/blob/noetic-devel/cmake/test/nosetests.cmake
[8]: https://github.com/ros/catkin/blob/noetic-devel/cmake/test/gtest.cmake
[9]: https://github.com/wty-andrew/clownbot/tree/master/clownbot_common/testing

[^1]: See [here](https://github.com/google/googletest/blob/master/googletest/docs/advanced.md#generating-an-xml-report) and [here](https://llg.cubic.org/docs/junit/) for detailed description of the JUnit XML reporting format

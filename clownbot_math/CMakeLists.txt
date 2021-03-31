cmake_minimum_required(VERSION 3.0.2)
project(clownbot_math)

find_package(catkin REQUIRED COMPONENTS
  clownbot_common
)

catkin_package()

if(CATKIN_ENABLE_TESTING)
  find_package(rostest REQUIRED)

  catkin_add_lisptests(test-vector clownbot-math-tests clownbot-math-tests/vector:test-vector)
endif()

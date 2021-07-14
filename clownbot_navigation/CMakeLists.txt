cmake_minimum_required(VERSION 3.0.2)
project(clownbot_navigation)

find_package(catkin REQUIRED COMPONENTS
  clownbot_common
  roslisp
)

catkin_package()

if(CATKIN_ENABLE_TESTING)
  find_package(rostest REQUIRED)
endif()

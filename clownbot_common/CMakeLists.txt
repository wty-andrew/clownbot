cmake_minimum_required(VERSION 3.0.2)
project(clownbot_common)

find_package(catkin REQUIRED COMPONENTS
  roslisp
)

catkin_package(
  CFG_EXTRAS clownbot-extras.cmake
)

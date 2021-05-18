cmake_minimum_required(VERSION 3.0.2)
project(clownbot_gazebo)

find_package(catkin REQUIRED COMPONENTS
  gazebo_plugins
  gazebo_ros
)

find_package(gazebo REQUIRED)

catkin_package(
#  INCLUDE_DIRS include
#  LIBRARIES clownbot_gazebo
  CATKIN_DEPENDS gazebo_ros gazebo_plugins
#  DEPENDS system_lib
)

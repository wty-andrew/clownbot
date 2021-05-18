cmake_minimum_required(VERSION 3.0.2)
project(clownbot_description)

find_package(catkin REQUIRED COMPONENTS
  urdf
  xacro
)

catkin_package(
#  INCLUDE_DIRS include
#  LIBRARIES clownbot_description
#  CATKIN_DEPENDS urdf xacro
#  DEPENDS system_lib
)

install(DIRECTORY launch meshes urdf
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION}
)

#!/bin/bash
set -e

source "/opt/ros/$ROS_DISTRO/setup.bash"
source "$CATKIN_WS/devel/setup.bash"

exec "$@"

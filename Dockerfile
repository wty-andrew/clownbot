ARG ROS_DISTRO=noetic
FROM osrf/ros:${ROS_DISTRO}-desktop-full

ARG ROS_DISTRO
ENV CATKIN_WS=/root/catkin_ws

SHELL ["/bin/bash", "-c"]

# Update/upgrade system packages
RUN apt-get update && apt-get -y upgrade

# Setup lisp
RUN curl -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp \
 && sbcl --quit --load /tmp/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
 && rm /tmp/quicklisp.lisp

# Install Dependencies
RUN sbcl --quit --eval '(ql:quickload "lisp-unit")'

# Create ROS workspace
RUN source /opt/ros/${ROS_DISTRO}/setup.bash \
 && mkdir -p ${CATKIN_WS}/src && cd $_ \
 && catkin_init_workspace

WORKDIR ${CATKIN_WS}/src

# Install ROS packages (copy directory, git clone, etc ...)
COPY . clownbot

# Build workspace
WORKDIR ${CATKIN_WS}
RUN source /opt/ros/${ROS_DISTRO}/setup.bash \
 && catkin_make

RUN echo "source /opt/ros/${ROS_DISTRO}/setup.bash" >> /root/.bashrc
RUN echo "source ${CATKIN_WS}/devel/setup.bash" >> /root/.bashrc

COPY entrypoint.sh /
ENTRYPOINT ["/entrypoint.sh"]

CMD ["bash"]

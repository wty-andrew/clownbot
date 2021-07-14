(uiop:define-package #:clownbot-navigation/navigator-node
  (:use #:cl #:roslisp #:cl-transforms-stamped
        #:clownbot-navigation/utils
        #:clownbot-navigation/planner
        #:clownbot-navigation/pure-pursuit
        #:clownbot-navigation/ros))

(in-package #:clownbot-navigation/navigator-node)

(defvar *goal-sub* nil)

(defun init-goal-sub ()
  (setf *goal-sub* (subscribe "move_base_simple/goal" "geometry_msgs/PoseStamped"
                              #'start-navigation)))

(defun start-navigation (pose-stamped-msg)
  (navigate (pose-stamped->pose (from-msg pose-stamped-msg))))

(defun send-zero-velocity ()
  (send-velocity-command (make-2d-twist 0 0)))

(defun navigate (goal &optional (timeout 20.0))
  (let ((deadline (+ (ros-time) timeout))
        (global-plan (make-global-plan (lookup-robot-pose) goal)))
    (unless global-plan
      (ros-warn (navigate) "No global plan found.")
      (return-from navigate))
    (loop-at-most-every (/ 1 *controller-frequency*)
      (when (> (ros-time) deadline)
        (ros-warn (navigate) "Failed due to timeout")
        (send-zero-velocity)
        (return))
      (let* ((robot-pose (lookup-robot-pose)) ; global frame
             (transform (transform-inv (pose->transform robot-pose)))
             (pruned-plan (or (drop-while (lambda (pose)
                                            (< (v-dist (origin robot-pose)
                                                       (origin pose))
                                               *lookahead-dist*))
                                          global-plan)
                              (last global-plan))))
        (setf global-plan pruned-plan)
        (let* ((transformed-plan (mapcar (lambda (pose)
                                           (transform-pose transform pose))
                                         global-plan))
               (target (first transformed-plan)))
          (when (and (null (cdr transformed-plan))
                     (< (v-dist (origin target) (make-identity-vector))
                        *xy-tolerance*))
            (when (< (abs (yaw (orientation target))) *yaw-tolerance*)
              (ros-info (navigate) "Goal Reached")
              (send-zero-velocity)
              (return))
            ;; pure rotation
            )
          (publish-carrot target)
          (multiple-value-bind (twist local-plan) (compute-velocity target)
            (send-velocity-command twist)
            (publish-local-plan local-plan)))))))

(defun init ()
  (init-tf)
  (init-publishers)
  (init-goal-sub))

(roslisp:def-ros-node navigator () (:spin t)
  "Start controller action server"
  (init))

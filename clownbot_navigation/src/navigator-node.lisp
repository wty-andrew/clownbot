(uiop:define-package #:clownbot-navigation/navigator-node
  (:use #:cl #:roslisp #:cl-utils #:cl-transforms-stamped
        #:clownbot-navigation/utils
        #:clownbot-navigation/planner
        #:clownbot-navigation/pure-pursuit
        #:clownbot-navigation/ros))

(in-package #:clownbot-navigation/navigator-node)

(defvar *goal-sub* nil)

;; TODO: replace with actionlib
(defun init-goal-sub ()
  (setf *goal-sub* (subscribe "move_base_simple/goal" "geometry_msgs/PoseStamped"
                              #'start-navigation)))

(defun start-navigation (pose-stamped-msg)
  (navigate (pose-stamped->pose (from-msg pose-stamped-msg))))

(defun send-zero-velocity ()
  (send-velocity-command (make-2d-twist 0 0)))

(defun navigate (goal &optional (timeout 30.0))
  (flet ((terminate (state message)
           (ecase state
             (:success (ros-info (navigator) "~A" message))
             (:failure (ros-warn (navigator) "~A" message)))
           (send-zero-velocity)
           (return-from navigate)))
    (let ((deadline (+ (ros-time) timeout))
          (global-plan (make-global-plan (lookup-robot-pose) goal)))
      (unless global-plan
        (terminate :failure "No global plan found."))
      (publish-global-plan global-plan)
      (setf *global-plan* global-plan)
      (loop-at-most-every (/ 1 *controller-frequency*)
        (when (> (ros-time) deadline)
          (terminate :failure "Failed due to timeout"))
        (let ((robot-pose (lookup-robot-pose)))
          (when (and (< (distance robot-pose goal) *xy-tolerance*)
                     (< (abs (angular-dist (get-yaw (orientation robot-pose))
                                           (get-yaw (orientation goal))))
                        *yaw-tolerance*))
            (terminate :success "Goal Reached"))
          (multiple-value-bind (twist local-plan carrot)
              (compute-velocity robot-pose)
            (send-velocity-command twist)
            (publish-carrot carrot)
            (publish-global-plan *global-plan*)
            (publish-local-plan local-plan)))))))

(defun init ()
  (init-tf)
  (init-publishers)
  (init-goal-sub))

;; (roslisp:def-ros-node navigator () (:spin t)
;;   "Start controller action server"
;;   (init))

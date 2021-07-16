(uiop:define-package #:clownbot-navigation/ros
  (:use #:cl #:roslisp #:cl-tf #:navfn-srv
        #:clownbot-navigation/utils
        #:clownbot-navigation/planner
        #:clownbot-navigation/pure-pursuit)
  (:export #:*transformer*
           #:*global-frame*
           #:*robot-base-frame*
           #:*controller-frequency*
           #:init-tf
           #:init-publishers
           #:lookup-robot-pose
           #:send-velocity-command
           #:publish-global-plan
           #:publish-local-plan
           #:publish-carrot))

(in-package #:clownbot-navigation/ros)

(defvar *transformer* nil)
(defvar *tf-default-timeout* 3.0)

(defvar *cmd-vel-pub* nil)
(defvar *global-plan-pub* nil)
(defvar *local-plan-pub* nil)
(defvar *carrot-pub* nil)
(defvar *carrot-pub* nil)

(defvar *global-frame* "map")
(defvar *robot-base-frame* "base_link")

(defvar *controller-frequency* 5
  "The rate to run the control loop and publish velocity command")

(defmethod to-msg ((data twist))
  (make-msg "geometry_msgs/Twist"
            :linear (to-msg (translation data))
            :angular (to-msg (rotation data))))

;; publishers
(defun init-cmd-vel-pub ()
  (unless *cmd-vel-pub*
    (setf *cmd-vel-pub* (advertise "cmd_vel" "geometry_msgs/Twist"))))

(defun get-cmd-vel-pub ()
  (or *cmd-vel-pub* (init-cmd-vel-pub)))

(defun init-global-plan-pub ()
  (unless *global-plan-pub*
    (setf *global-plan-pub* (advertise "~global_plan" "nav_msgs/Path"))))

(defun get-global-plan-pub ()
  (or *global-plan-pub* (init-global-plan-pub)))

(defun init-local-plan-pub ()
  (unless *local-plan-pub*
    (setf *local-plan-pub* (advertise "~local_plan" "geometry_msgs/PoseArray"))))

(defun get-local-plan-pub ()
  (or *local-plan-pub* (init-local-plan-pub)))

(defun init-carrot-pub ()
  (unless *carrot-pub*
    (setf *carrot-pub* (advertise "current_goal" "geometry_msgs/PoseStamped"))))

(defun get-carrot-pub ()
  (or *carrot-pub* (init-carrot-pub)))

;; initializations
(defun init-tf ()
  (setf *global-frame* (get-param "~global_frame" "map")
        *robot-base-frame* (get-param "~robot-base-frame" "base_link"))
  (unless *transformer*
    (setf *transformer* (make-instance 'transform-listener)))
  (wait-for-transform *transformer*
                      :source-frame *global-frame*
                      :target-frame *robot-base-frame*
                      :timeout *tf-default-timeout*))

(defun init-publishers ()
  (init-cmd-vel-pub)
  (init-global-plan-pub)
  (init-local-plan-pub)
  (init-carrot-pub))

;; high level commands
(defun lookup-robot-pose ()
  "Return the current robot pose in global frame"
  (pose-stamped->pose
   (transform-pose
    *transformer*
    :target-frame *global-frame*
    :pose (pose->pose-stamped *robot-base-frame*
                              0.0
                              (make-identity-pose)))))

(defun send-velocity-command (twist)
  "Publish the given twist as velocity command"
  (publish (get-cmd-vel-pub) (to-msg twist)))

(defun publish-global-plan (plan)
  (publish (get-global-plan-pub)
           (make-message "nav_msgs/Path"
                         (:frame_id :header) *global-frame*
                         :poses (map 'vector #'to-msg plan))))

(defun publish-local-plan (plan)
  (publish (get-local-plan-pub)
           (make-message "geometry_msgs/PoseArray"
                         (:frame_id :header) *robot-base-frame*
                         :poses (map 'vector #'to-msg plan))))

(defun publish-carrot (pose)
  "Publish the current following target"
  (publish (get-carrot-pub) (to-msg (pose->pose-stamped *robot-base-frame* 0 pose))))

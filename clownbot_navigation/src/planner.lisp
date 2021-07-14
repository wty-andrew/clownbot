(uiop:define-package #:clownbot-navigation/planner
  (:use #:cl #:roslisp #:cl-transforms-stamped #:navfn-srv)
  (:export #:make-global-plan))

(in-package #:clownbot-navigation/planner)

;; TODO: replace with own implementation
(defvar *global-frame* "map")

(defun make-global-plan (start-pose goal-pose)
  "Return global plan as a list of poses in global frame"
  (flet ((pose->pose-stamped-msg (pose)
           (to-msg (pose->pose-stamped *global-frame* 0.0 pose))))
    (with-slots (plan_found path)
        (call-service "planner_node/make_plan"
                      "navfn/MakeNavPlan"
                      (make-request "navfn/MakeNavPlan"
                                    :start (pose->pose-stamped-msg start-pose)
                                    :goal (pose->pose-stamped-msg goal-pose)))
      (when (= plan_found 1)
        (map 'list (lambda (pose-stamped)
                     (pose-stamped->pose (from-msg pose-stamped)))
             path)))))

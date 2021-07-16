(uiop:define-package #:clownbot-navigation/pure-pursuit
  (:use #:cl #:cl-utils #:cl-transforms
        #:clownbot-navigation/utils)
  (:export #:*xy-tolerance*
           #:*yaw-tolerance*
           #:*desired-linear-vel*
           #:*lookahead-dist*
           #:*max-angular-vel*
           #:*global-plan*
           #:prune-plan
           #:compute-local-plan
           #:compute-velocity))

(in-package #:clownbot-navigation/pure-pursuit)

(defvar *xy-tolerance* 0.05
  "Allowed position error in meters")
(defvar *yaw-tolerance* 0.6
  "Allowed orientation error in randians")

(defparameter *desired-linear-vel* 0.4)
(defparameter *lookahead-dist* 0.5)
(defparameter *max-angular-vel* 0.5)
(defparameter *rotate-to-heading-min-angle* 0.785) ; ~45 degrees

(defvar *global-plan* nil)

(defun prune-plan (init-pose plan)
  "Return a new plan that removes all poses prior to the closest pose to
init-pose on plan"
  (let ((index 0)
        (min-dist most-positive-single-float))
    (loop for pose in plan and i from 0
          for curr-dist = (distance init-pose pose)
          when (<= curr-dist min-dist)
            do (setf index i
                     min-dist curr-dist)
          finally (return (nthcdr index plan)))))

(defun motion-model (pose linear-vel angular-vel dt)
  "Calculate next pose based on differential drive robot model"
  (with-slots (origin orientation) pose
    (let ((x (x origin))
          (y (y origin))
          (theta (get-yaw orientation)))
      (if (< (abs angular-vel) 1e-6)
          (let ((ds (* linear-vel dt)))
            (make-2d-pose (+ x (* ds (cos theta))) (+ y (* ds (sin theta))) theta))
          (let* ((radius (/ linear-vel angular-vel))
                 (new-theta (normalize-angle (+ theta (* angular-vel dt))))
                 (new-x (+ x (* radius (- (sin new-theta) (sin theta)))))
                 (new-y (- y (* radius (- (cos new-theta) (cos theta))))))
            (make-2d-pose new-x new-y new-theta))))))

(defun compute-local-plan (v w limit &optional (dt 0.2))
  "Return predicted future poses"
  (if (zerop v) ; pure rotation
      (loop for angle from 0.0 below (abs limit) by 0.5 ; ~ 30 deg
            collect (make-2d-pose 0 0 (copy-sign angle limit)))
      (let ((max-poses (ceiling (abs (/ pi w dt))))) ; (pi * radius) / v = pi / w
        (loop with pose = (make-identity-pose)
              for n from 0
              while (and (< (v-norm (origin pose)) limit)
                         (< n max-poses))
              collect pose into poses
              do (setf pose (motion-model pose v w dt))
              finally (return poses)))))

(defun find-lookahead-pose (current-pose path &optional (lookahead-dist *lookahead-dist*))
  "Return the temporary goal on path which should be at least lookahead-dist far
away from current pose"
  (find-if (lambda (pose) (> (distance current-pose pose) lookahead-dist)) path))

(defun compute-velocity (robot-pose)
  "Return the calculated velocity and correspond local plan"
  (assert *global-plan*)
  (let* ((pruned-plan (prune-plan robot-pose *global-plan*))
         (lookahead-pose (find-lookahead-pose robot-pose pruned-plan))
         (carrot-pose (transform (transform-inv (pose->transform robot-pose))
                                 (or lookahead-pose (car (last pruned-plan))))))
    (setf *global-plan* pruned-plan)
    (with-slots ((position origin) orientation) carrot-pose
      (let* ((chord-length (v-norm position))
             (theta (normalize-angle (get-yaw orientation)))
             (large-angular-dist-p (> (abs theta) *rotate-to-heading-min-angle*))
             (should-rotate-in-place-p (or (and lookahead-pose large-angular-dist-p)
                                           (< chord-length *xy-tolerance*))))
        (condlet ((should-rotate-in-place-p (v 0)
                                            (w (copy-sign *max-angular-vel* theta))
                                            (limit theta))
                  (t (v *desired-linear-vel*)
                     (w (* *desired-linear-vel* (/ (* 2 (y position)) (expt chord-length 2))))
                     (limit chord-length)))
          (let ((ratio (/ w *max-angular-vel*)))
            (when (> ratio 1)
              (setf v (/ v ratio)
                    w (/ w ratio))))
          (setf v (* v (/ chord-length *lookahead-dist*))) ; slow down when approaching goal
          (values (make-2d-twist v w)
                  (compute-local-plan v w limit)
                  carrot-pose))))))

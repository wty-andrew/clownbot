(uiop:define-package #:clownbot-navigation/pure-pursuit
  (:use #:cl #:cl-transforms
        #:clownbot-navigation/utils)
  (:export #:*xy-tolerance*
           #:*yaw-tolerance*
           #:*desired-linear-vel*
           #:*lookahead-dist*
           #:*max-angular-vel*
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
(defparameter *max-angular-vel* 0.3)

(defun prune-plan (plan &optional (lookahead-dist *lookahead-dist*))
  "Returns a new plan that removes the first few poses that are within
lookahead-dist, if all poses are within the distance, keep only the last pose"
  (or (member-if (lambda (pose)
                   (> (v-norm (origin pose)) lookahead-dist))
                 plan)
      (last plan)))

(defun motion-model (pose linear-vel angular-vel dt)
  "Calculate next pose based on differential drive robot model"
  (with-slots (origin orientation) pose
    (let ((x (x origin))
          (y (y origin))
          (theta (yaw orientation)))
      (if (< (abs angular-vel) 1e-6)
          (let ((ds (* linear-vel dt)))
            (make-2d-pose (+ x (* ds (cos theta))) (+ y (* ds (sin theta))) theta))
          (let* ((radius (/ linear-vel angular-vel))
                 (new-theta (normalize-angle (+ theta (* angular-vel dt))))
                 (new-x (+ x (* radius (- (sin new-theta) (sin theta)))))
                 (new-y (- y (* radius (- (cos new-theta) (cos theta))))))
            (make-2d-pose new-x new-y new-theta))))))

(defun compute-local-plan (v w range &optional (dt 0.2))
  "Return predicted future poses within range by applying motion model"
  (let ((max-poses (ceiling (abs (/ pi w dt))))) ; (pi * radius) / v = pi / w
    (loop with pose = (make-identity-pose)
          for n from 0
          while (and (< (v-norm (origin pose)) range)
                     (< n max-poses))
          collect pose into poses
          do (setf pose (motion-model pose v w dt))
          finally (return poses))))

(defun compute-velocity (target)
  "Return the velocity and correspond local plan"
  (let* ((target-position (origin target))
         (chord-length (v-norm target-position))
         (x (x target-position))
         (y (y target-position))
         (theta (normalize-angle (yaw (orientation target)))))
    (if (> x 0) ; robot is always facing x direction in its own frame
        (let ((v *desired-linear-vel*)
              (w (* *desired-linear-vel* (/ (* 2.0 y) (expt chord-length 2)))))
          (values (make-2d-twist v w)
                  (compute-local-plan v w chord-length)))
        (let ((sign (sign theta)))
          (values (make-2d-twist 0 (* sign *max-angular-vel*))
                  (loop for angle from 0 below (abs theta) by 0.5 ; ~ 30 deg
                        collect (make-2d-pose 0 0 (* angle sign))))))))

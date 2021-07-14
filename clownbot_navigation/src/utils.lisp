(uiop:define-package #:clownbot-navigation/utils
  (:use #:cl #:roslisp #:cl-transforms)
  (:export #:drop-while
           #:sign
           #:degree->radian #:radian->degree
           #:angular-dist #:yaw
           #:make-2d-twist))

(in-package #:clownbot-navigation/utils)

;; general utils
(defun drop-while (pred list)
  (cond ((null list) nil)
        ((funcall pred (car list)) (drop-while pred (cdr list)))
        (t list)))

;; math utils
(defun sign (num)
  (cond ((plusp num) 1)
        ((zerop num) 0)
        (t -1)))

(defun degree->radian (degree)
  (/ (* degree pi) 180))

(defun radian->degree (rad)
  (/ (* rad 180) pi))

;; supplement to cl-transforms
(defun angular-dist (from to)
  "Return the smaller difference between two angles"
  (normalize-angle (- to from)))

(defun yaw (quaternion)
  "Return the yaw angle of the given quaternion"
  (with-slots (x y z w) quaternion
    (atan (* 2 (+ (* x y) (* w z)))
          (- (+ (* w w) (* x x)) (+ (* y y) (* z z))))))

(defun make-2d-twist (linear-vel angular-vel)
  (make-twist (make-3d-vector linear-vel 0 0)
              (make-3d-vector 0 0 angular-vel)))

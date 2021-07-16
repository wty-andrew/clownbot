(uiop:define-package #:clownbot-navigation/utils
  (:use #:cl #:roslisp #:cl-transforms)
  (:export #:sign #:copy-sign
           #:degree->radian #:radian->degree
           #:angular-dist
           #:make-2d-twist
           #:distance))

(in-package #:clownbot-navigation/utils)

;; math utils
(defun sign (num)
  (cond ((plusp num) 1)
        ((zerop num) 0)
        (t -1)))

(defun copy-sign (x y)
  (* x (sign y)))

(defun degree->radian (degree)
  (/ (* degree pi) 180))

(defun radian->degree (rad)
  (/ (* rad 180) pi))

;; supplement to cl-transforms
(defun angular-dist (from to)
  "Return the smaller difference between two angles"
  (normalize-angle (- to from)))

(defun make-2d-twist (linear-vel angular-vel)
  (make-twist (make-3d-vector linear-vel 0 0)
              (make-3d-vector 0 0 angular-vel)))

(defgeneric distance (a b)
  (:documentation "Return the distance between two objects")
  (:method ((v1 3d-vector) (v2 3d-vector))
    (v-dist v1 v2))
  (:method ((p1 pose) (p2 pose))
    (v-dist (origin p1) (origin p2))))

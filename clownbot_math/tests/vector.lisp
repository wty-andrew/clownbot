(uiop:define-package #:clownbot-math-tests/vector
  (:use #:cl #:lisp-unit #:clownbot-math/vector)
  (:shadowing-import-from #:clownbot-math/vector #:norm)
  (:import-from #:clownbot-testing
                #:lisp-unit->gtest)
  (:export #:test-vector))

(in-package #:clownbot-math-tests/vector)

(defun test-vector ()
  (lisp-unit->gtest :clownbot-math-tests/vector))

(define-test test-vector-creation
  (assert-float-equal #(1 2 3) (make-vector 3 :data '(1 2 3))))

(define-test test-vector-copy
  (let ((v (make-vector 3 :data '(1 2 3))))
    (assert-float-equal v (copy-vector v))))

(define-test test-vector-addition
  (let ((v1 (make-vector 3 :data '(1 2 3)))
        (v2 (make-vector 3 :data '(4 5 6))))
    (assert-float-equal (make-vector 3 :data '(5 7 9)) (v+ v1 v2))
    (assert-float-equal (make-vector 3 :data '(6 9 12)) (v+ v1 v1 v2))))

(define-test test-vector-subtraction
  (let ((v1 (make-vector 3 :data '(4 5 6)))
        (v2 (make-vector 3 :data '(1 2 3))))
    (assert-float-equal (make-vector 3 :data '(3 3 3)) (v- v1 v2))
    (assert-float-equal (make-vector 3 :data '(2 1 0)) (v- v1 v2 v2))))

(define-test test-vector-scalar-addtion
  (let ((v (make-vector 3 :data '(1 2 3))))
    (assert-float-equal (make-vector 3 :data '(11 12 13)) (.+ v 10))))

(define-test test-vector-scalar-subtraction
  (let ((v (make-vector 3 :data '(1 2 3))))
    (assert-float-equal (make-vector 3 :data '(-9 -8 -7)) (.- v 10))))

(define-test test-vector-scalar-multiplication
  (let ((v (make-vector 3 :data '(1 2 3))))
    (assert-float-equal (make-vector 3 :data '(10 20 30)) (.* v 10))))

(define-test test-vector-scalar-division
  (let ((v (make-vector 3 :data '(1 2 3))))
    (assert-float-equal (make-vector 3 :data '(0.1 0.2 0.3)) (./ v 10))))

(define-test test-vector-dot-product
  (let ((v1 (make-vector 3 :data '(1 2 3)))
        (v2 (make-vector 3 :data '(4 5 6))))
    (assert-number-equal 32 (dot v1 v2))))

(define-test test-vector-cross-product
  (let ((v1 (make-vector 3 :data '(1 2 3)))
        (v2 (make-vector 3 :data '(4 5 6))))
    (assert-float-equal (make-vector 3 :data '(-3 6 -3)) (cross v1 v2))))

(define-test test-vector-norm
  (let ((v (make-vector 3 :data '(1 2 3))))
    (assert-float-equal (sqrt 14) (norm v))))

(define-test test-vector-normalization
  (let ((v (make-vector 2 :data '(3 4))))
    (assert-float-equal (make-vector 2 :data '(0.6 0.8)) (normalize v))
    (assert-float-equal 1.0 (norm (normalize v)))))

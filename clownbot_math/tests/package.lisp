(uiop:define-package #:clownbot-math-tests
  (:use #:cl #:lisp-unit
        #:clownbot-math-tests/vector)
  (:export #:main))

(in-package #:clownbot-math-tests)

(defun main ()
  (lisp-unit:run-tests :all :clownbot-math-tests/vector))

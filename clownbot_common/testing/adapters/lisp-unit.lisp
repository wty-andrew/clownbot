(uiop:define-package #:clownbot-common/testing/adapters/lisp-unit
  (:use #:cl #:lisp-unit #:clownbot-common/testing/core)
  (:export #:lisp-unit->gtest))

(in-package #:clownbot-common/testing/adapters/lisp-unit)

(defun normalize-string (str)
  "Remove the line breaks and `|` characters, replace double quotes with back ticks in string."
  (let ((alist '((#\" . #\`) (#\Newline . "") (#\| . ""))))
    (string-trim " " (with-output-to-string (s)
                       (loop for c across str
                             do (write-string (string (or (cdr (assoc c alist)) c)) s))))))

(defun failure->gtestfailure (failure)
  "Transform a failure object into a gtestfailure structure."
  (make-gtestfailure :message (normalize-string (with-output-to-string (s)
                                                  (lisp-unit:print-failures failure s)))))
(defun error->gtesterror (err)
  "Transform a exerr object into a gtesterror structure."
  (make-gtesterror :message (normalize-string (format nil "~a" err))))

(defun test-result->gtestcase (result)
  "Transform a test-result object into a gtestcase structure."
  (make-gtestcase :name (string-downcase (string (lisp-unit::name result)))
                  :time (float (lisp-unit::run-time-s result))
                  :failures (mapcar #'failure->gtestfailure (lisp-unit::fail result))
                  :errors (when (lisp-unit::exerr result)
                            (list (error->gtesterror (lisp-unit::exerr result))))))

(defun test-results-db->gtest (test-results-db)
  "Transform a test-results-db object into a gtestsuite structure."
  (let ((testcases (loop for test-result being the hash-value of (lisp-unit::database test-results-db)
                         collect (test-result->gtestcase test-result))))
    (make-gtestsuite :name "All Tests"
                     :time (apply #'+ (mapcar #'gtestcase-time testcases))
                     :testcases testcases)))

(defun make-keyword (name)
  (if (keywordp name)
      name
      (intern (string-upcase name) "KEYWORD")))

(defun lisp-unit->gtest (package)
  (run-test->gtest (lambda () (lisp-unit:run-tests :all (make-keyword package)))
                   #'test-results-db->gtest))

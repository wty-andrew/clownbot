(uiop:define-package #:clownbot-common/testing/core
  (:use #:cl)
  (:shadow #:error)
  (:export #:make-gtestfailure #:gtestfailure-type #:gtestfailure-message
           #:make-gtesterror #:gtesterror-type #:gtesterror-message
           #:make-gtestcase #:gtestcase-classname #:gtestcase-name
           #:gtestcase-time #:gtestcase-failures #:gtestcase-errors
           #:make-gtestsuite #:gtestsuite-name #:gtestsuite-tests #:gtestsuite-time
           #:gtestsuite-testcases #:gtestsuite-sysout #:gtestsuite-syserr
           #:run-test->gtest))

(in-package #:clownbot-common/testing/core)

(defvar *tab-width* 2)
(defvar *indent-level* 0)

(defun format-indent (stream format-str &rest args)
  "Same as format function but adds extra spaces deteremined by *tab-width* and *indent-level*."
  (format stream "~&~,,v@a" (* *tab-width* *indent-level*) (apply #'format nil format-str args)))

(defun prologue ()
  (format t "<?xml version=\"1.0\" encoding=\"utf-8\"?>"))

(defun cdata (text)
  (format t "~&<![CDATA[~%~a~%]]>" text))

(defmacro def-tag (tag)
  "Defines a macro that prints xml node string."
  `(defmacro ,tag (attrs &body body)
     `(progn
        (format-indent t "<~(~a~)~:[ ~{~(~a~)=\"~a\"~^ ~}~;~]>"
                       ',',tag ,(null attrs) (list ,@attrs))
        ,(if (null body)
             `(format t "</~(~a~)>~%" ',',tag)
             `(progn
                (let ((*indent-level* (1+ *indent-level*)))
                  ,@body)
                (format-indent t "</~(~a~)>~%" ',',tag))))))

(defmacro def-tags (&rest tags)
  `(progn ,@(mapcar (lambda (tag) `(def-tag ,tag)) tags)))

(def-tags testsuite testcase error failure system-out system-err)

(defstruct gtestfailure
  "Test failure container."
  (type "" :type string)
  (message "" :type string))

(defstruct gtesterror
  "Test error container."
  (type "" :type string)
  (message "" :type string))

(defstruct gtestcase
  "Test case container."
  (classname "" :type string)
  (name "" :type string)
  (time 0.0 :type float)
  (failures '() :type list)
  (errors '() :type list))

(defstruct gtestsuite
  "Test suite container."
  (name "" :type string)
  (tests 0 :type integer)
  (time 0.0 :type float)
  (testcases '() :type list)
  (sysout "" :type string)
  (syserr "" :type string))

(defun gtest->xml (suite)
  "Print the xml report to *standard-output* from a gtestsuite structure."
  (flet ((sum-fields (key sequence)
           (apply #'+ (mapcar (lambda (item) (length (funcall key item)))
                              sequence))))
    (let ((testcases (gtestsuite-testcases suite)))
      (prologue)
      (testsuite (:tests (length testcases)
                  :errors (sum-fields #'gtestcase-errors testcases)
                  :failures (sum-fields #'gtestcase-failures testcases)
                  :name (gtestsuite-name suite)
                  :time (gtestsuite-time suite))
        (dolist (testcase testcases)
          (testcase (:classname (gtestcase-classname testcase)
                     :name (gtestcase-name testcase)
                     :time (gtestcase-time testcase))
            (dolist (testfailure (gtestcase-failures testcase))
              (failure (:message (gtestfailure-message testfailure)
                        :type (gtestfailure-type testfailure))))
            (dolist (testerror (gtestcase-errors testcase))
              (error (:message (gtesterror-message testerror)
                      :type (gtesterror-type testerror))))))
        (system-out nil (cdata (gtestsuite-sysout suite)))
        (system-err nil (cdata (gtestsuite-syserr suite)))))))

(defun find-gtest-output-path (args)
  "Find the output path from the `--gtest_output` flag in args."
  (when (consp args)
    (let* ((str (car args))
           (p (search "--gtest_output=xml:" str)))
      (if (and p (zerop p))
          (subseq str 19)
          (find-gtest-output-path (cdr args))))))

(defun run-suites-write-gtest-file (output-path suite-fun transform-fun)
  "Writes the xml report to output-path, suite-fun should generate a report that can be turned into
  a gtestsuite by transform-fun."
  (let* ((sysout (make-string-output-stream))
         (syserr (make-string-output-stream))
         (*standard-output* (make-broadcast-stream *standard-output* sysout))
         (*error-output* (make-broadcast-stream *error-output* syserr))
         (test-results (funcall suite-fun))
         (gtest-results (funcall transform-fun test-results)))
    (setf (gtestsuite-sysout gtest-results) (get-output-stream-string sysout)
          (gtestsuite-syserr gtest-results) (get-output-stream-string syserr))
    (with-open-file (*standard-output* output-path :direction :output :if-exists :supersede)
      (gtest->xml gtest-results))))

(defun run-test->gtest (test-fun transform-fun)
  "Run the test-fun, if the gtest output argument is found from the command line argument, convert
  the result into xml report. This is intended to be used with rostest."
  (let ((output-path (find-gtest-output-path sb-ext:*posix-argv*)))
    (if output-path
        (run-suites-write-gtest-file output-path test-fun transform-fun)
        (funcall test-fun))))

(uiop:define-package #:clownbot-common/testing/package
  (:nicknames #:clownbot-testing)
  (:use #:cl)
  (:use-reexport #:clownbot-common/testing/core
                 #:clownbot-common/testing/adapters/all))

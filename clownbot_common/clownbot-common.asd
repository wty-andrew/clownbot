(defsystem clownbot-common
  :class :package-inferred-system
  :depends-on ("clownbot-common/testing/package"))

(register-system-packages "clownbot-common/testing/package" '(:clownbot-testing))

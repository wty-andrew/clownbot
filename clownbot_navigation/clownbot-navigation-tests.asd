(defsystem clownbot-navigation-tests
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("clownbot-common" "clownbot-navigation-tests/package")
  :perform (asdf:test-op (op c) (uiop:symbol-call :clownbot-navigation-tests '#:main)))

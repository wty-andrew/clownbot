(defsystem clownbot-math-tests
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("clownbot-common" "clownbot-math-tests/package")
  :perform (asdf:test-op (op c) (uiop:symbol-call :clownbot-math-tests '#:main)))

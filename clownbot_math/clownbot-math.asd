(defsystem clownbot-math
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("clownbot-math/package")
  :in-order-to ((asdf:test-op (asdf:test-op "clownbot-math-tests"))))

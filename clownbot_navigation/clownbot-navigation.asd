(defsystem clownbot-navigation
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("roslisp"
               "nav_msgs-msg"
               "geometry_msgs-msg"
               "navfn-srv"
               "cl-utils"
               "cl-tf"
               "clownbot-navigation/package")
  :in-order-to ((asdf:test-op (asdf:test-op "clownbot-navigation-tests"))))

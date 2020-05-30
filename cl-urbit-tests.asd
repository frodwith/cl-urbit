(defsystem cl-urbit-tests
  :author "Paul Driver <frodwith@gmail.com>"
  :license "MIT"
  :depends-on (:cl-urbit :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "tests")
                 (:file "syntax")
                 (:file "math")
                 (:file "zig")
                 (:file "mug")
                 (:file "data")
                 (:file "common")
                 (:file "ideal")
                 (:file "speed")
                 (:file "nock"))))
  :perform (test-op (o s)
    (uiop:symbol-call '#:urbit/tests '#:test-urbit)))

(defsystem cl-urbit-tests
  :author "Paul Driver <frodwith@gmail.com>"
  :license "MIT"
  :depends-on (:cl-urbit :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "tests")
                 (:file "math")
                 (:file "mug"))))
  :perform (test-op (o s)
    (uiop:symbol-call '#:urbit/tests '#:test-urbit)))

(defsystem cl-urbit-test
  :author "Paul Driver <frodwith@gmail.com>"
  :license "MIT"
  :depends-on (:cl-urbit
               :rove)
  :components ((:module "t"
                :serial t
                :components
                ((:file "noun")))))

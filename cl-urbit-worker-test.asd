(defsystem cl-urbit-worker-test
  :author "Paul Driver <frodwith@gmail.com>"
  :license "MIT"
  :depends-on (:cl-urbit-worker
               :rove)
  :components ((:module "t"
                :serial t
                :components
                ((:file "noun")))))

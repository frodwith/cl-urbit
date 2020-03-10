(defsystem cl-urbit-test
  :author "Paul Driver <frodwith@gmail.com>"
  :license "MIT"
  :depends-on (:cl-urbit
               :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "t"
                :serial t
                :components
                ((:file "util")
                 (:test-file "syntax")
                 (:test-file "interner")
                 (:test-file "noun")
                 (:test-file "axis-map")
                 (:test-file "kernels")
                 (:test-file "chunker")
                 (:test-file "basic-nock"))))
  :perform (test-op :after (op c)
    (funcall (intern #.(string :run) :prove) c)))

(defsystem cl-urbit-worker
  :author "Paul Driver <frodwith@gmail.com>"
  :maintainer "Paul Driver <frodwith@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/frodwith/cl-urbit-worker"
  :version "0.1"
  ; for future reference
  ; :depends-on (:local-time)
  :components ((:module "src"
                :serial t
                :components
                ((:file "error")
                 (:file "noun"))))
                 ;(:file "nock"))))
  :description "nock runtime and urbit worker process (http://urbit.org)"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-urbit-worker-test))))

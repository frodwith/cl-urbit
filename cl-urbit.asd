(defsystem cl-urbit
  :author "Paul Driver <frodwith@gmail.com>"
  :maintainer "Paul Driver <frodwith@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/frodwith/cl-urbit"
  :version "0.1"
  :depends-on (:cl-murmurhash)
  :components ((:module "src"
                :serial t
                :components
                ((:file "error")
                 (:file "atom")
                 (:file "cell")
                 (:file "noun")
                 (:file "math")
                 (:file "mug")
                 (:file "formula")
                 (:file "equality")
                 (:file "data/constant-atom")
                 (:file "data/constant-cell")
                 (:file "context")
                 (:file "data/slimcell")
                 (:file "data/bigatom")
                 (:file "data/fixnum"))))
  :description "nock runtime and urbit worker process (http://urbit.org)"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-urbit-test))))

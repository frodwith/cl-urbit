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
                 (:file "mug")
                 (:file "pairset")
                 (:file "equality")
                 (:file "data/bigatom")
                 (:file "data/fixnum")
                 (:file "data/slimcell"))))
                 ;(:file "nock"))))
  :description "nock runtime and urbit worker process (http://urbit.org)"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-urbit-test))))

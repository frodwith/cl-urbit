(defsystem cl-urbit
  :author "Paul Driver <frodwith@gmail.com>"
  :maintainer "Paul Driver <frodwith@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/frodwith/cl-urbit"
  :version "0.1"
  :depends-on 
  (:cl-murmurhash :named-readtables :cl-intbytes :trivial-bit-streams)
  :components ((:module "src"
                :serial t
                :components
                ((:file "math")
                 (:file "syntax")
                 (:file "data")
                 (:file "common")
                 (:file "data/fixnum")
                 (:file "data/bignum")
                 (:file "data/cons")
                 (:file "mug")
                 (:file "ideal")
                 (:file "data/iatom")
                 (:file "data/icell")
                 (:file "serial")
                 (:file "jets")
                 (:file "equality")
                 (:file "nock")
                 )))
  :description "nock runtime and urbit worker process (http://urbit.org)"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "cl-urbit-tests"))))

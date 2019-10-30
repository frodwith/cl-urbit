(defsystem cl-urbit
  :author "Paul Driver <frodwith@gmail.com>"
  :maintainer "Paul Driver <frodwith@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/frodwith/cl-urbit"
  :version "0.1"
  :depends-on (:cl-murmurhash :named-readtables)
  :components ((:module "src"
                :serial t
                :components
                ((:file "packages")
                 (:file "util")
                 (:file "error")
                 (:file "math")
                 (:file "atom")
                 (:file "cell")
                 (:file "noun")
                 (:file "meta")
                 (:file "mug")
                 (:file "unique")
                 (:file "equality")
                 (:file "context")
                 (:file "data/bigatom")
                 (:file "data/constant-atom")
                 (:file "axis-map")
                 (:file "data/constant-cell")
                 (:file "data/core")
                 (:file "data/slimcell")
;                 (:file "syntax")
;                 (:file "formula")
;                 (:file "kernels")
;                 (:file "data/fixnum")
;                 (:file "chunker")
;                 (:file "compiler"))))
                                    )))
  :description "nock runtime and urbit worker process (http://urbit.org)"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-urbit-test))))

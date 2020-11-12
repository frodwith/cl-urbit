(defsystem "cl-urbit"
  :author      "Paul Driver <frodwith@gmail.com>"
  :maintainer  "Paul Driver <frodwith@gmail.com>"
  :license     "MIT"
  :homepage    "https://github.com/frodwith/cl-urbit"
  :version     "0.1"
  :description "urbit tools in common lisp (http://urbit.org)"
  :long-description
  #.(uiop:read-file-string
      (uiop:subpathname *load-pathname* "README.md"))
  :depends-on ("cl-urbit/hepl"
               "cl-urbit/lars")
  :in-order-to ((test-op (test-op "cl-urbit/base"))))

(defsystem "cl-urbit/base"
  :description "nock/hoon runtime"
  :depends-on ("alexandria"
               "named-readtables"
               "trivial-bit-streams"
               "cl-murmurhash"
               "cl-intbytes")
  :in-order-to ((test-op (test-op "cl-urbit/base/test")))
  :components
  ((:module "nock"
            :serial t
            :components
            ((:file "math")
             (:file "axis")
             (:file "zig")
             (:file "data")
             (:file "common")
             (:file "mug")
             (:file "ideal")
             (:file "world")
             (:file "cell-meta")
             (:file "bignum-meta")
             (:file "data/fixnum")
             (:file "data/bignum")
             (:file "data/cons")
             (:file "data/slimcell")
             (:file "data/slimatom")
             (:file "data/core")
             (:file "data/iatom")
             (:file "data/icell")
             (:file "cord")
             (:file "jets")
             (:file "equality")
             (:file "nock")))
   (:module "hoon"
            :depends-on ("nock")
            :serial t
            :components
            ((:file "cache")
             (:file "syntax")
             (:file "hints")
             (:file "tape")
             (:file "serial")
             (:file "k141")
             (:file "ivory")))))

(defsystem "cl-urbit/base/test"
  :depends-on ("cl-urbit/base" "fiveam")
  :components ((:module "t"
                :serial t
                :components
                ((:file "tests")
                 (:file "syntax")
                 (:file "math")
                 (:file "axis")
                 (:file "convert")
                 (:file "zig")
                 (:file "mug")
                 (:file "data")
                 (:file "common")
                 (:file "ideal")
                 (:file "speed")
                 (:file "nock"))))
  :perform (test-op (o s)
             (uiop:symbol-call '#:urbit/tests '#:test-urbit)))

(defsystem "cl-urbit/urcrypt"
  :description "bindings to liburcrypt"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "secure-random")
  :in-order-to ((test-op (test-op "cl-urbit/urcrypt/test")))
  :components ((:module "urcrypt"
                :serial t
                :components
                ((:file "package")
                 (:cffi-grovel-file "grovel")
                 (:file "urcrypt")))))

(defsystem "cl-urbit/urcrypt/test"
  :description "test the liburcrypt bindings"
  :depends-on ("fiveam" "cl-urbit/urcrypt")
  :components ((:file "t/urcrypt"))
  :perform (test-op (o s)
             (uiop:symbol-call '#:urbit/urcrypt/test '#:test-urcrypt)))

(defsystem "cl-urbit/hepl"
  :description "hoon REPL"
  :depends-on ("cl-urbit/base" "ironclad" "unix-opts")
  :build-operation program-op
  :build-pathname "bin/hepl"
  :entry-point "urbit/hepl/main:entry"
  :components
  ((module "hepl"
           :serial t
           :components
           ((:file "jets")
            (:file "main")))
   (:static-file "ivory.pill")))

(defsystem "cl-urbit/lars"
  :description "urbit worker process"
  :depends-on ("cl-urbit/base" "cl-urbit/urcrypt" "trivial-timeout")
  :build-operation program-op
  :build-pathname "bin/lars"
  :entry-point "urbit/lars/main:entry"
  :components
  ((module "lars"
           :serial t
           :components
           ((:file "jets")
            (:file "newt")
            (:file "main")))))

(defpackage #:urbit/tests/equality
  (:use #:cl #:fiveam #:named-readtables #:urbit/tests 
        #:urbit/hoon/syntax #:urbit/nock/equality #:urbit/nock/mug))

(in-package #:urbit/tests/equality)
(in-readtable hoon)

(def-suite equality-tests
           :description "test noun equality"
           :in all-tests)

(in-suite equality-tests)

; there were no tests for equality when i found a bug, so the bug-case
; is reproduced here.
; TODO: test equality more thoroughly.

(test mugcell=unmugcell
  (let ((a [0 0 0])
        (b [0 0 0]))
    (mug a)
    (is (same a b))))

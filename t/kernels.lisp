(defpackage urbit/tests/kernels
  (:use :cl :prove :urbit/syntax :urbit/tests/util)
  (:import-from :urbit/cell :head)
  (:import-from :urbit/kernels :root :static :child :hook :hooks
                :kernel-name :kernel-parent :kernel-parent-axis
                :root-kernel-constant))

(in-package :urbit/tests/kernels)
(enable-brackets)

(plan 13)

(let* ((k141  (root
                :k141 141
                (hooks `((:foo . ,(lambda (x) x))
                         (:bar . ,(lambda (x)
                                    (declare (ignore x))
                                    42))))))
       (k141c [[1 141] 141])
       (one   (static
                :one k141
                (hooks `((:baz . ,(lambda (x)
                                    (head x)))))))
       (onec  [[1 0] k141c])
       (add   (child :add 7 one))
       (addc  [[0 0] 0 onec]))
  (subtest "root"
    (is (kernel-name k141) :k141)
    (is (kernel-parent k141) nil)
    (is (kernel-parent-axis k141) nil)
    (is (root-kernel-constant k141) 141))
  (subtest "child"
    (is (kernel-name one) :one)
    (is (kernel-parent one) k141 "parent")
    (is (kernel-parent-axis one) 3))
  (subtest "grandchild"
    (is (kernel-name add) :add)
    (is (kernel-parent add) one "parent")
    (is (kernel-parent-axis add) 7))
  (subtest "hooks"
    (is (hook :foo add addc) k141c)
    (is (hook :bar add addc) 42)
    (is-same (hook :baz add addc) [1 0])))

(finalize)

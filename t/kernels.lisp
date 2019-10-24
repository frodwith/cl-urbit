(defpackage #:urbit/tests/kernels
  (:use :cl :prove :urbit/syntax :urbit/tests/util)
  (:import-from :urbit/cell :head)
  (:import-from :urbit/kernels :root :static :child :hook :hooks
                :kernel-name :kernel-parent :kernel-parent-axis
                :root-kernel-constant :warm-root :warm-child
                :stencil :stencil-parent :check-stencil))

(in-package :urbit/tests/kernels)
(enable-brackets)

(plan 3)

(let* ((root-core [[1 141] 141])
       (one-core [[1 0] root-core])
       (add-core [[0 0] 0 one-core]))
  (subtest "pure"
    (let* ((root  (root
                    :k141 141
                    (hooks `((:foo . ,(lambda (x) x))
                             (:bar . ,(lambda (x)
                                        (declare (ignore x))
                                        42))))))
           (one   (static
                    :one root
                    (hooks `((:baz . ,(lambda (x)
                                        (head x)))))))
           (add   (child :add 7 one)))
      (subtest "root"
        (is (kernel-name root) :k141)
        (is (kernel-parent root) nil)
        (is (kernel-parent-axis root) nil)
        (is (root-kernel-constant root) 141))
      (subtest "child"
        (is (kernel-name one) :one)
        (is (kernel-parent one) root "parent")
        (is (kernel-parent-axis one) 3))
      (subtest "grandchild"
        (is (kernel-name add) :add)
        (is (kernel-parent add) one "parent")
        (is (kernel-parent-axis add) 7))
      (subtest "hooks"
        (is (hook :foo add add-core) root-core)
        (is (hook :bar add add-core) 42)
        (is-same (hook :baz add add-core) [1 0]))))
  (let* ((root-table (make-warm-table))
         (root-hooks '((:foo . (0 1))
                       (:bar . (1 42))))
         (one-hooks  '((:baz . (0 2))))
         (root (warm-root root-table :k141 141 root-hooks))
         (one  (warm-child :one 3 root one-hooks))
         (add  (warm-child :add 7 one)))
    (subtest "warm"
      (subtest "identity"
        (isnt (warm-root root-table :k141 141) root "hookless")
        (is (warm-root root-table :k141 141 root-hooks) root "hooked")
        (isnt (warm-child :one 3 root) one "hookless")
        (is (warm-child :one 3 root one-hooks) one "hooked")
        (is (warm-child :add 7 one) add "add"))
      (subtest "hooks"
        (let ((add-kernel (kernel-node-kernel add)))
          (is (hook :foo add-kernel add-core) root-core)
          (is (hook :bar add-kernel add-core) 42)
          (is-same (hook :baz add-kernel add-core) [1 0]))))
    (subtest "stencils"
      (let ((root-stencil (stencil root root-core))
            (one-stencil (stencil one one-core))
            (add-stencil (stencil add add-core)))
        (subtest "parents"
          (is (stencil-parent add-stencil) one-stencil)
          (is (stencil-parent one-stencil) root-stencil)
          (is (stencil-parent root-stencil) nil))
        (subtest "check"
          (subtest "root"
            (ok (check-stencil root-stencil root-core))
            (no (check-stencil root-stencil one-core))
            (no (check-stencil root-stencil add-core))
            (ok (check-stencil root-stencil [[1 141] 141]))
            (no (check-stencil root-stencil [[0 141] 141]))
            (no (check-stencil root-stencil [[1 141] 140]))))))))

(finalize)

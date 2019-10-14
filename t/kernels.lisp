(defpackage #:urbit/tests/kernels
  (:use :cl :prove :urbit/syntax :urbit/tests/util)
  (:import-from :urbit/cell :head)
  (:import-from :urbit/context :with-context :make-context :kernel-node-kernel
                :find-root-kernel-node :find-child-kernel-node)
  (:import-from :urbit/kernels :root :static :child :hook :hooks
                :kernel-name :kernel-parent :kernel-parent-axis
                :root-kernel-constant))

(in-package :urbit/tests/kernels)
(enable-brackets)

(plan 2)

(let* ((root-core [[1 141] 141])
       (one-core [[1 0] root-core])
       (add-core [[0 0] 0 one-core]))
  (subtest "no context"
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
  (subtest "in context"
    (with-context (make-context)
      (let* ((root-hooks '((:foo . (0 1))
                           (:bar . (1 42))))
             (root (find-root-kernel-node :k141 141 root-hooks))
             (one-hooks '((:baz . (0 2))))
             (one  (find-child-kernel-node :one 3 root one-hooks))
             (add  (find-child-kernel-node :add 7 one)))
        (subtest "identity"
          (isnt (find-root-kernel-node :k141 141) root "hookless")
          (is (find-root-kernel-node :k141 141 root-hooks) root "hooked")
          (isnt (find-child-kernel-node :one 3 root) one "hookless")
          (is (find-child-kernel-node :one 3 root one-hooks) one "hooked")
          (is (find-child-kernel-node :add 7 one) add "add"))
        (subtest "hooks"
          (let ((add-kernel (kernel-node-kernel add)))
            (is (hook :foo add-kernel add-core) root-core)
            (is (hook :bar add-kernel add-core) 42)
            (is-same (hook :baz add-kernel add-core) [1 0])))))))

(finalize)

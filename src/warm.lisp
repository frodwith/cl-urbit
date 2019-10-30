(defpackage #:urbit/warm-tree
  (:use :cl)
  (:import-from :urbit/noun :noun)
  (:import-from :urbit/util :cache-hash)
  (:import-from :urbit/interner :unique :unique-head)
  (:import-from :urbit/compiler :formula) 
  (:import-from :urbit/kernel :hooks :root :static :child :static-kernel)
  (:import-from :urbit/warm-data :make-warm-node :warm-node-kernel
                :warm-node-children)
  (:export :warm-root :warm-child))

; warm-tree uses the compiler, which uses warm-data
; but the compiler does not depend on warm-tree
;  arrgghh...  but doesn't it, via context??! the code it generates clearly
;  needs to be able to call context-root, which needs to call warm-root...

; i was dodging this via the formula generic before.

(in-package :urbit/warm-tree)

; hooks is a list of (keyword nock-val) pairs
;   nock-val is something you can pass to (noun)
;               that compares correctly under (equal)
;   i.e. a list like '(9 2 0 1)
(defun process-hooks (pairs)
  (hooks 
    (mapcar
      (lambda (p)
        (cons (car p)
              (formula (noun (cdr p)))))
      pairs)))

(defun warm-root (table name constant &optional hooks)
  (cache-hash (cons (cons name constant) hooks) table 
    (make-warm-node (root name constant (process-hooks hooks)))))

(defun warm-child (name axis parent-node &optional hooks)
  (cache-hash (cons (cons name axis) hooks) (warm-node-children parent-node)
    (let ((parent (warm-node-kernel parent-node))
          (phooks (process-hooks hooks)))
      (make-warm-node
        (if (and (= axis 3) (typep parent 'static-kernel))
            (static name parent phooks)
            (child name axis parent phooks))))))

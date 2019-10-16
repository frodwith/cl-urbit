(defpackage #:urbit/context
  (:use :cl)
  (:import-from :urbit/util :cache-hash)
  (:import-from :urbit/noun :noun)
  (:import-from :urbit/atom :atomp :learn-integer :to-integer)
  (:import-from :urbit/cell :learn-constant-cell :get-constant-cell :head :tail)
  (:import-from :urbit/kernels :kernel :static-kernel :hooks :root :static :child)
  (:import-from :urbit/formula :formula)
  (:import-from :urbit/data/constant-cell :make-constant-cell)
  (:import-from :urbit/data/constant-atom :make-constant-atom :constant-atom))

(in-package :urbit/context)

(defparameter *context* nil)

(defmacro with-context (c &body body)
  `(let ((*context* ,c))
     ,@body))

(defstruct (context (:constructor cons-context (atoms cells roots)))
  (atoms nil :type hash-table)
  (cells nil :type hash-table)
  (warm  nil :type hash-table))

; weakness is SBCL-only

(defun make-context ()
  (cons-context 
    (make-hash-table :test 'eql :weakness :value)
    (make-hash-table :test 'equal :weakness :value)
    (make-hash-table :test 'equal :weakness :value)))

(defun intern-atom (i &optional mug)
  (if (typep i 'fixnum)
      i
      (cache-hash i (context-atoms *context*) (make-constant-atom i mug))))

; call with INTERNED head and tail
(defun intern-cell (head tail &optional mug)
  (cache-hash (cons head tail) (context-cells *context*)
    (make-constant-cell head tail mug)))

; XX: use noun/sum?
; also, do we have to intern left and right every time?
; can't we do something with mug/equality?
; maybe not - equality is still traversal?
; seems like there are some possible hashtable lookups avoided by doing
; mug/equality.
(defun intern-noun (n &optional mug)
  (if (atomp n)
      (if (typep n 'constant-atom)
          n
          (let* ((i (to-integer n))
                 (k (intern-atom i mug)))
            (learn-integer n (to-integer k))
            k))
      (let ((cc (get-constant-cell n)))
        (or cc
            (let* ((head (intern-noun (head n)))
                   (tail (intern-noun (tail n)))
                   (k (intern-cell head tail mug)))
              (learn-constant-cell n k)
              k)))))

(defstruct (warm-node
             (:constructor cons-warm (kernel parent children stencils)))
  (kernel nil :type kernel)
  (parent nil :type (or null warm-node))
  (children nil :type hash-table)
  (stencils nil :type hash-table))

(defun make-warm-node (kernel parent)
  (cons-warm kernel parent
             (make-hash-table :test 'equal :weakness :value)
             (make-hash-table :test 'equal :weakness :value)))

; hooks is a list of (keyword nock-val) pairs
;   nock-val is something you can pass to (noun)
;               that compares correctly under (equal)
;   i.e. a list like '(9 2 0 1)
(defun process-hooks (pairs)
  (hooks (mapcar (lambda (p) (cons (car p) (formula (noun (cdr p)))))
                 pairs)))

(defun warm-root (name constant &optional hooks)
  (cache-hash (cons (cons name constant) hooks) (context-warm *context*)
    (make-warm-node (root name constant (process-hooks hooks)))))

(defun warm-child (name axis parent-node &optional hooks)
  (cache-hash (cons (cons name axis) hooks) (warm-node-children parent-node)
    (let ((parent (warm-node-kernel parent-node))
          (phooks (process-hooks hooks)))
      (make-warm-node
        (if (and (= axis 3) (typep parent 'static-kernel))
            (static name parent phooks)
            (child name axis parent phooks))))))

(defstruct stencil ((:constructor make-stencil (kernel noun parent)))
  (node nil :type warm-node)
  (noun nil :type constant-cell)
  (parent nil :type (or null stencil)))

(defun find-stencil (node core)
  (let ((kernel (warm-node-kernel node)))
    (multiple-value-bind (noun &optional parent)
      (etypecase kernel
        (static-kernel (intern-noun core))
        (child-kernel  (values (battery core)
                               (stencil (parent-core kernel core)))))
      (cache-hash (if parent (cons noun parent) noun)
                  (warm-node-stencils node)
                  (make-stencil node noun parent)))))

(defun check-inner (stencil core)
  (let ((cached (cached-stencil core)))
    (if cached
        (eq cached stencil)
        (let ((kernel (warm-node-kernel (stencil-node stencil)))
              (noun   (stencil-noun stencil)))
          (and (etypecase kernel
                 (static-kernel (same noun core))
                 (child-kernel
                   (and (same noun (head core))
                        (check-inner (stencil-parent stencil)
                                     (parent-core kernel core)))))
               (progn
                 (learn-stencil core stencil)
                 t))))))

(defun check-stencil (stencil core)
  (handler-case (check-inner stencil core)
    (exit (e) nil)))

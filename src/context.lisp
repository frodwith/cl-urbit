(defpackage #:urbit/context
  (:use :cl)
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
  (roots nil :type hash-table))

; weakness is SBCL-only

(defun make-context ()
  (cons-context 
    (make-hash-table :test 'eql :weakness :value)
    (make-hash-table :test 'equal :weakness :value)
    (make-hash-table :test 'equal :weakness :value)))

(defun intern-atom (i &optional mug)
  (if (typep i 'fixnum)
      i
      (let* ((table (context-atoms *context*))
             (got   (gethash i table)))
        (or got
            (setf (gethash i table)
                  (make-constant-atom i mug))))))

; call with INTERNED head and tail
(defun intern-cell (head tail &optional mug)
  (let* ((table (context-cells *context*))
         (key   (cons head tail))
         (got   (gethash key table)))
    (or got
        (setf (gethash key table)
              (make-constant-cell head tail mug)))))

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

; hooks is a list of (keyword nock-val) pairs
;   nock-val is something you can pass to (noun)
;               that compares correctly under (equal)
;   i.e. a list like '(9 2 0 1)
(defun process-hooks (pairs)
  (let ((foo (mapcar (lambda (p)
                       (cons (car p)
                             (formula (noun (cdr p)))))
                     pairs)))
    (hooks foo)))

(defstruct (kernel-node (:constructor make-kernel-node (kernel)))
  (kernel nil :type kernel)
  (table nil :type (or null hash-table)))

(defun kernel-node-children (node)
  (or (kernel-node-table node)
      (setf (kernel-node-table node)
            (make-hash-table :test 'equal :weakness :value))))

(defun find-root-kernel-node (name constant &optional hooks)
  (let ((table (context-roots *context*))
        (key (cons (cons name constant) hooks)))
    (or (gethash key table)
        (setf (gethash key table) 
              (make-kernel-node (root name constant (process-hooks hooks)))))))

(defun find-child-kernel-node (name axis parent-node &optional hooks)
  (let ((table (kernel-node-children parent-node))
        (key (cons (cons name axis) hooks)))
    (or (gethash key table)
        (let ((parent (kernel-node-kernel parent-node))
              (phooks (process-hooks hooks)))
          (setf (gethash key table)
                (make-kernel-node 
                  (if (and (= axis 3) (typep parent 'static-kernel))
                      (static name parent phooks)
                      (child name axis parent phooks))))))))

;(defstruct stencil (kernel nil :type kernel))
;
;(defstruct (full-stencil 
;             (:include stencil)
;             (:constructor full (kernel core)))
;  (core nil :type constant-cell))
;
;(defstruct (gap-stencil 
;             (:include stencil)
;             (:constructor gap (kernel battery parent)))
;  (battery nil :type constant-cell)
;  (parent nil :type stencil))
;(defun cut-stencil (kernel core)
;  (etypecase kernel
;    (static-kernel
;      (full kernel (intern-noun core)))
;    (child-kernel 
;      (gap kernel (battery core)
;           (cut-stencil (child-kernel-parent kernel)
;                        (frag core (child-kernel-axis kernel)))))) 

; XX TODO: if stencils are interned, they can be compared directly for
; equality. "tree of stencils" interner??
; if we do that, they get their own file.

; what does this mean? i don't want something like intern noun, since that isn't
; necessary - we won't build up complex structures, we will intern "one layer"
; at a time. this implies a structure like...
;   do we do drivers by name, or kernel structure? kernel structure implies a
;   depency on the kelvin, which is good, and on the hooks...  which is probably
;   also good, honestly.
;   a kernel-tree-node is a pair of kernel and a (weak) hashtable of children.
;   you lookup a kernel by supplying the relevant fields for building it. these
;   are hashed and looked up in the children table.
;
;   stencils are managed in the same manner

; what does this buy us?
;   1) super fast comparison (hashtable lookups, arm prologues)
;   2) better memory usage for stencils

; kernels should also be interned by contexts, that just makes sense


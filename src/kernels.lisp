(defpackage #:urbit/kernels
  (:use :cl)
  (:import-from :urbit/noun :noun)
  (:import-from :urbit/equality :same)
  (:import-from :urbit/util :cache-hash)
  (:import-from :urbit/error :exit :oops)
  (:import-from :urbit/noun :frag :noun)
  (:import-from :urbit/cell :head)
  (:import-from :urbit/formula :formula :battery))

(in-package :urbit/kernels)

(defstruct kernel 
  (name nil :type keyword)
  (hooks nil :type (or null hash-table)))

;; Section 1: "pure" kernels
;; abstract, nested cores with hooks 

(defgeneric kernel-parent (k))
(defgeneric kernel-parent-axis (k))

(defstruct (root-kernel
             (:include kernel)
             (:constructor root (name constant &optional hooks)))
  (constant nil :type integer))

(defmethod kernel-parent ((r root-kernel))
  nil)

(defmethod kernel-parent-axis ((r root-kernel))
  nil)

(deftype static-kernel () '(or root-kernel static-child-kernel))

(defstruct (static-child-kernel
             (:include kernel)
             (:constructor static (name parent &optional hooks)))
  (parent nil :type static-kernel))

(defmethod kernel-parent ((s static-child-kernel))
  (static-child-kernel-parent s))

(defmethod kernel-parent-axis ((s static-child-kernel))
  3)

(defstruct (child-kernel
             (:include kernel)
             (:constructor child (name axis parent &optional hooks)))
  (parent nil :type kernel)
  (axis nil :type integer))

(defmethod kernel-parent ((c child-kernel))
  (child-kernel-parent c))

(defmethod kernel-parent-axis ((c child-kernel))
  (child-kernel-axis c))

(defun parent-core (kernel core)
  (frag core (kernel-parent-axis kernel)))

; list of (keyword . function)
(defun hooks (pairs)
  (loop with m = (make-hash-table :test 'eq)
        for pair in pairs
        do (setf (gethash (car pair) m) (cdr pair))
        finally (return m)))

(defun hook (name kernel core)
  (let* ((hooks (kernel-hooks kernel))
         (hook (and hooks (gethash name hooks))))
    (if hook
        (funcall hook core)
        (typecase kernel
          (root-kernel (error 'oops))
          (otherwise (hook name (kernel-parent kernel)
                           (parent-core kernel core)))))))

;; Section 2: Kernel tree
;; find the canonical instance of a kernel
;; primary client in context.lisp

; weakness is SBCL-only
(defun make-warm-table ()
  (make-hash-table :test 'equal :weakness :value))

(defstruct (warm-node
             (:constructor cons-warm (kernel parent children stencils)))
  (kernel nil :type kernel)
  (parent nil :type (or null warm-node))
  (children nil :type hash-table)
  (stencils nil :type hash-table))

(defun make-warm-node (kernel &optional parent)
  (cons-warm kernel parent
             (make-warm-table)
             (make-warm-table)))

; hooks is a list of (keyword nock-val) pairs
;   nock-val is something you can pass to (noun)
;               that compares correctly under (equal)
;   i.e. a list like '(9 2 0 1)
(defun process-hooks (pairs)
  (hooks (mapcar (lambda (p) (cons (car p) (formula (noun (cdr p)))))
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

;; Section 3: Stencils
;; a "pattern of batteries" particular to a warm tree.
;; designed to be stored on a core.

(defgeneric cached-stencil (core))
(defmethod cached-stencil (obj) 
  nil)

(defgeneric learn-stencil (core stencil))
(defmethod learn-stencil (obj stencil)
  nil)

(defun stencil (node core)
  (let ((kernel (warm-node-kernel node)))
    (multiple-value-bind (noun parent)
      (etypecase kernel
        (static-kernel (intern-noun core))
        (child-kernel  (values (battery core)
                               (stencil (warm-node-parent node)
                                        (parent-core kernel core)))))
      (cache-hash (if parent (cons noun parent) noun)
                  (warm-node-stencils node)
                  (make-stencil node noun parent)))))

(defun check-inner (stencil core)
  (let ((cached (cached-stencil core)))
    (if cached
        (eq cached stencil)
        (let ((kernel (warm-node-kernel (stencil-node stencil)))
              (noun   (stencil-noun stencil)))
          (when (etypecase kernel
                  (static-kernel (same noun core))
                  (child-kernel
                    (and (same noun (head core))
                         (check-inner (stencil-parent stencil)
                                      (parent-core kernel core)))))
              (learn-stencil core stencil)
              t)))))

(defun check-stencil (stencil core)
  (handler-case (check-inner stencil core)
    (exit () nil)))

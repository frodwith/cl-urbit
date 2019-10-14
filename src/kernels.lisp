(defpackage #:urbit/kernels
  (:use :cl)
  (:import-from :urbit/error :oops)
  (:import-from :urbit/noun :frag :noun))

(in-package :urbit/kernels)

(defstruct kernel 
  (name nil :type keyword)
  (hooks nil :type (or null hash-table)))

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
        (hook name
              (or (kernel-parent kernel)
                  (error 'oops))
              (frag core (or (kernel-parent-axis kernel)
                             (error 'oops)))))))

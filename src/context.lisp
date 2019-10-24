(defpackage #:urbit/context
  (:use :cl :urbit/interner)
  (:import-from :urbit/kernels :warm-root :make-warm-table)
  (:export :make-context :with-context :context-intern :context-warm))

(in-package :urbit/context)

(defparameter *context* nil)

(defmacro with-context (c &body body)
  `(let ((*context* ,c))
     ,@body))

(defstruct (context (:constructor cons-context (interner warm-tree)))
  (interner nil :type noun-interner)
  (warm-tree nil :type hash-table)) 

(defun make-context ()
  (cons-context (make-noun-interner) (make-warm-table)))

(defun context-intern (noun &optional mug)
  (intern-with (context-interner *context*) noun mug))

(defun context-warm (root-name root-constant &optional hooks)
  (warm-root (context-warm-tree *context*) root-name root-constant hooks))

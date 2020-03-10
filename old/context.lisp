(in-package #:urbit/context)

(defparameter *context* nil)

(defmacro with-context (c &body body)
  `(let ((*context* ,c))
     ,@body))

(defstruct (context (:constructor cons-context (interner warm-tree)))
  (interner nil :type noun-interner)
  (warm-tree nil :type hash-table)) 

(defun make-context ()
  (cons-context (make-noun-interner) (make-warm-table)))

(defun unique-cons (head tail &optional mug)
  (hash-cons (context-interner *context*) head tail mug))

(defun unique-integer (i &optional mug)
  (find-integer (context-interner *context*) i mug))

(defun context-warm (root-name root-constant &optional hooks)
  (warm-root (context-warm-tree *context*) root-name root-constant hooks))

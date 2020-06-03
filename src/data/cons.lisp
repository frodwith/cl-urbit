(defpackage #:urbit/data/cons
  (:use #:cl #:urbit/cell-meta))

(in-package #:urbit/data/cons)

; use conses as dumb cells, store metadata in a hashtable

(defparameter *cons-meta* (make-hash-table :test 'eq :weakness :key))

(defun cons-meta (c)
  (the cell-meta (gethash c *cons-meta*)))

(defun (setf cons-meta) (val c)
  (declare (cell-meta val))
  (setf (gethash c *cons-meta*) val))

(define-cell-methods cons car cdr cons-meta)

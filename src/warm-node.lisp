(defpackage #:urbit/warm-node
  (:use :cl)
  (:import-from :urbit/kernel :kernel))

(in-package :urbit/warm-node)

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

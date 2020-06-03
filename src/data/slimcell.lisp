(defpackage #:urbit/data/slimcell
  (:use #:cl #:urbit/cell-meta))

(in-package #:urbit/data/slimcell)

; dumb cells with a slot for their metadata

(defstruct (slimcell (:constructor slim-cons (head tail)))
  head
  tail
  (meta nil :type cell-meta))

(define-cell-methods slimcell slimcell-head slimcell-tail slimcell-meta)

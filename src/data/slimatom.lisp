(defpackage #:urbit/data/slimatom
  (:use #:cl #:urbit/data #:urbit/bignum-meta)
  (:export #:slim-malt))

(in-package #:urbit/data/slimatom)

(defstruct slimatom
  (num nil :type bignum)
  (meta nil :type bignum-meta))

(define-atom-methods slimatom slimatom-num slimatom-meta)

(defun slim-malt (a &optional mug)
  (etypecase a
    (fixnum a)
    (bignum (make-slimatom :num a :meta mug))))

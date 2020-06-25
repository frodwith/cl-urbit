(defpackage #:urbit/data/slimatom
  (:use #:cl #:urbit/data #:urbit/bignum-meta)
  (:export #:slim-malt #:make-slimatom))

(in-package #:urbit/data/slimatom)

(defstruct (slimatom (:constructor make-slimatom (num &optional meta)))
  (num nil :type bignum)
  (meta nil :type bignum-meta))

(define-atom-methods slimatom slimatom-num slimatom-meta)

(defun slim-malt (a &optional mug)
  (etypecase a
    (fixnum a)
    (bignum (make-slimatom a mug))))

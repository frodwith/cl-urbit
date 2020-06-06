(defpackage #:urbit/data/bignum
  (:use #:cl #:urbit/data #:urbit/bignum-meta))

(in-package #:urbit/data/bignum)

(defparameter *bignum-meta* (make-hash-table :test 'eq :weakness :key))

(defun meta (a)
  (declare (bignum a))
  (the bignum-meta (gethash a *bignum-meta*)))

(defun (setf meta) (val a)
  (declare (bignum a) (bignum-meta val))
  (setf (gethash a *bignum-meta*) val))

(defun bignum-num (a)
  (declare (bignum a))
  a)

(defun (setf bignum-num) (val a)
  (declare (bignum val) (ignore a))
  val)

(define-atom-methods bignum bignum-num meta)

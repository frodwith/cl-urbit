(defpackage #:urbit/data/bignum
  (:use #:cl #:urbit/data)
  (:import-from #:urbit/mug #:mug)
  (:import-from #:urbit/ideal #:iatom #:iatom-mug))

(in-package #:urbit/data/bignum)

(defparameter *bignum-meta* (make-hash-table :test 'eq :weakness :key))

(defun meta (i)
  (gethash i *bignum-meta*))

(defun (setf meta) (val i)
  (setf (gethash i *bignum-meta*) val))

(defmethod deep ((i bignum))
  nil)

(defmethod cached-mug ((i bignum))
  (let ((m (meta i)))
    (etypecase m
      (null nil)
      (mug m)
      (iatom (iatom-mug m)))))

(defmethod (setf cached-mug) (val (i bignum))
  (declare (mug val))
  (let ((m (meta i)))
    (when (null m)
      (setf (meta i) val))))

(defmethod cached-ideal ((i bignum))
  (let ((m (meta i)))
    (and (typep m 'iatom) m)))

(defmethod (setf cached-ideal) ((val iatom) (i bignum))
  (setf (meta i) val))

(defmethod cl-integer ((i bignum))
  i)

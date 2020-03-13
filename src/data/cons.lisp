(defpackage #:urbit/data/cons
  (:use #:cl #:urbit/data #:urbit/mug)
  (:import-from #:urbit/mug #:mug)
  (:import-from #:urbit/ideal #:icell #:icell-mug))

(in-package #:urbit/data/cons)

(defparameter *cons-meta* (make-hash-table :test 'eq :weakness :key))

(defun meta (c)
  (gethash c *cons-meta*))

(defun (setf meta) (val c)
  (setf (gethash c *cons-meta*) val))

(defmethod deep ((p cons))
  t)

(defmethod cached-mug ((c cons))
  (let ((m (meta c)))
    (etypecase m
      (null nil)
      (mug m)
      (icell (icell-mug m)))))

(defmethod (setf cached-mug) (val (c cons))
  (declare (mug val))
  (let ((m (meta c)))
    (when (null m)
      (setf (meta c) val))))

(defmethod cached-ideal ((c cons))
  (let ((m (meta c)))
    (and (typep m 'icell) m)))

(defmethod (setf cached-ideal) ((val icell) (c cons))
  (setf (meta c) val))

(defmethod head ((c cons))
  (car c))

(defmethod (setf head) (val (c cons))
  (setf (car c) val))

(defmethod tail ((c cons))
  (cdr c))

(defmethod (setf tail) (val (c cons))
  (setf (cdr c) val))

; TODO: cached-speed

(defpackage #:urbit/data/cons
  (:use #:cl #:urbit/data #:urbit/mug #:urbit/ideal)
  (:import-from #:alexandria #:when-let))

(in-package #:urbit/data/cons)

(defparameter *cons-meta* (make-hash-table :test 'eq :weakness :key))

(deftype cons-meta ()
  '(or null mug core (cons mug core) icell))

(defun meta (c)
  (the cons-meta (gethash c *cons-meta*)))

(defun (setf meta) (val c)
  (declare (cons-meta val))
  (setf (gethash c *cons-meta*) val))

(defmethod deep ((p cons))
  t)

(defmethod cached-mug ((c cons))
  (let ((m (meta c)))
    (etypecase m
      (null nil)
      (mug m)
      (core nil)
      (cons (car m))
      (icell (icell-mug m)))))

(defmethod (setf cached-mug) (val (c cons))
  (declare (mug val))
  (let ((m (meta c)))
    (typecase m
      (null (setf (meta c) val))
      (core (setf (meta c) (cons val m))))))

(defmethod cached-ideal ((c cons))
  (let ((m (meta c)))
    (when (typep m 'icell) m)))

(defmethod (setf cached-ideal) ((val icell) (c cons))
  (let ((m (meta c)))
    (when-let (spd (typecase m
                     (core m)
                     (cons (cdr m))))
      (setf (cached-speed val) spd)))
  (setf (meta c) val)
  (setf (car c) (icell-head val))
  (setf (cdr c) (icell-tail val)))

(defmethod head ((c cons))
  (car c))

(defmethod (setf head) (val (c cons))
  (when (typep (meta c) '(or null mug))
    (setf (car c) val))
  val)

(defmethod tail ((c cons))
  (cdr c))

(defmethod (setf tail) (val (c cons))
  (unless (typep (meta c) 'icell)
    (setf (cdr c) val))
  val)

(defmethod (setf cached-battery) ((val icell) (c cons))
  (rplaca c val)
  val)

; assumption: if we have a cached speed, our head is already ideal

(defmethod cached-battery ((c cons))
  (let ((m (meta c)))
    (typecase m
      (icell (icell-head m))
      ((or cons core) (car c)))))

(defmethod cached-speed ((c cons))
  (let ((m (meta c)))
    (typecase m
      (core m)
      (cons (cdr m))
      (icell (icell-speed m)))))

(defmethod (setf cached-speed) (val (c cons))
  (let ((m (meta c)))
    (typecase m
      ((or null core) (setf (meta c) val))
      (mug (setf (meta c) (cons m val)))
      (cons (rplacd m val))
      (icell (setf (icell-speed m) val))))
  val)

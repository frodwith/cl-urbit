(defpackage #:urbit/data/core
  (:use #:cl #:urbit/data #:urbit/mug #:urbit/ideal)
  (:export #:core #:core-cons))

(in-package #:urbit/data/core)

(defstruct (core (:constructor core-cons (battery payload speed meta)))
  (battery nil :type icell :read-only t)
  payload
  (speed nil :type core-speed)
  (meta nil :type (or null mug icell)))

(defmethod deep ((c core))
  t)

(defmethod cached-mug ((c core))
  (let ((m (core-meta c)))
    (typecase m
      (mug m)
      (icell (icell-mug m)))))

(defmethod (setf cached-mug) (val (c core))
  (unless (core-meta c)
    (setf (core-meta c) val)))

(defmethod cached-ideal ((c core))
  (let ((m (core-meta c)))
    (typecase m
      (icell m))))

(defmethod (setf cached-ideal) (val (c core))
  (unless (typep (core-meta c) 'icell)
    (setf (core-payload c) (icell-tail val))
    (let ((spd (core-speed c)))
      (if (speed-valid spd)
          (setf (icell-speed val) spd)
          (setf (core-speed c) (icell-speed val))))))

(defmethod head ((c core))
  (core-battery c))

(defmethod tail ((c core))
  (core-payload c))

(defmethod (setf tail) (val (c core))
  (unless (typep (core-meta c) 'icell)
    (setf (core-payload c) val))
  val)

(defmethod cached-battery ((c core))
  (core-battery c))

(defmethod cached-speed ((c core))
  (core-speed c))

(defmethod (setf cached-speed) (val (c core))
  (setf (core-speed c) val)
  (let ((m (core-meta c)))
    (typecase m
      (icell (setf (icell-speed m) val)))))

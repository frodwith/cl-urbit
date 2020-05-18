(defpackage #:urbit/data/icell
  (:use #:cl #:urbit/data #:urbit/ideal))

(in-package #:urbit/data/icell)

(defmethod deep ((p icell))
  t)

(defmethod head ((a icell))
  (icell-head a))

(defmethod unique-head ((a icell))
  (icell-head a))

(defmethod tail ((a icell))
  (icell-tail a))

(defmethod cached-mug ((a icell))
  (icell-mug a))

(defmethod cached-ideal ((a icell))
  a)

(defmethod cached-speed ((c icell))
  (icell-speed c))

(defmethod (setf cached-speed) (val (c icell))
  (setf (icell-speed c) val))

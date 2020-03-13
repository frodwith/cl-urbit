(defpackage #:urbit/data/fixnum
  (:use #:cl #:urbit/data))

(in-package #:urbit/data/fixnum)

(defmethod deep ((i fixnum))
  nil)

(defmethod cached-mug ((i fixnum))
  nil)

(defmethod cached-ideal ((i fixnum))
  i)

(defmethod cl-integer ((i fixnum))
  i)

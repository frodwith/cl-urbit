(defpackage #:urbit/nock/data/fixnum
  (:use #:cl #:urbit/nock/data)
  (:import-from #:urbit/nock/mug #:murmug))

(in-package #:urbit/nock/data/fixnum)

(defmethod deep ((i fixnum))
  nil)

(defconstant +smallmug-limit+ 4096)
(defparameter +smallmugs+
  (loop with a = (make-array +smallmug-limit+)
        for i below +smallmug-limit+
        do (setf (aref a i) (murmug i))
        finally (return a)))

(defmethod cached-mug ((i fixnum))
  (when (< i +smallmug-limit+) (aref +smallmugs+ i)))

(defmethod cached-ideal ((i fixnum))
  i)

(defmethod cl-integer ((i fixnum))
  i)

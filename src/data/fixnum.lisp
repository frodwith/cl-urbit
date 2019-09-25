(defpackage urbit/data/fixnum
  (:use :cl)
  (:import-from :urbit/mug :murmug :compute-mug)
  (:import-from :urbit/atom :atomp :to-integer)
  (:import-from :urbit/equality :atom=))

(in-package :urbit/data/fixnum)

(defmethod atomp ((a fixnum))
  t)

(defmethod to-integer ((a fixnum))
  a)

(defmethod compute-mug ((a fixnum))
  (murmug a))

(defmethod atom= ((a fixnum) (b fixnum))
  (= a b))

(defmethod atom= ((a fixnum) (b t))
  nil)

(defmethod atom= ((a t) (b fixnum))
  nil)

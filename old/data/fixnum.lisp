(defpackage urbit/data/fixnum
  (:use :cl)
  (:import-from :urbit/mug :murmug :compute-mug)
  (:import-from :urbit/interner :unique)
  (:import-from :urbit/noun :to-noun)
  (:import-from :urbit/atom :atomp :bump :to-integer)
  (:import-from :urbit/equality :atom=))

(in-package :urbit/data/fixnum)

(defmethod atomp ((a fixnum))
  t)

(defmethod to-integer ((a fixnum))
  a)

(defmethod unique ((a fixnum))
  a)

(defmethod bump ((a fixnum))
  (to-noun (1+ a)))

(defmethod compute-mug ((a fixnum))
  (murmug a))

(defmethod atom= ((a fixnum) (b fixnum))
  (= a b))

(defmethod atom= ((a fixnum) (b t))
  nil)

(defmethod atom= ((a t) (b fixnum))
  nil)

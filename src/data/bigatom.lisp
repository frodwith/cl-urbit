(defpackage #:urbit/data/bigatom
  (:use :cl)
  (:import-from :urbit/noun :to-noun)
  (:import-from :urbit/mug :cached-mug :compute-mug :murmug :learn-mug)
  (:import-from :urbit/atom :atomp :bump :to-integer :learn-integer)
  (:import-from :urbit/equality :teach :atom=))

(in-package :urbit/data/bigatom)

(defstruct (bigatom (:constructor make-bigatom (num)))
  (num nil :type bignum)
  (mug nil :type (or null (unsigned-byte 31))))

(defmethod atomp ((a bigatom))
  t)

(defmethod to-integer ((a bigatom))
  (bigatom-num a))

(defmethod bump ((a bigatom))
  (make-bigatom (1+ (bigatom-num a))))

(defmethod learn-integer ((a bigatom) i)
  (setf (bigatom-num a) i))

(defmethod learn-mug ((a bigatom) m)
  (setf (bigatom-mug a) m))

(defmethod cached-mug ((a bigatom))
  (bigatom-mug a))

(defmethod compute-mug ((a bigatom))
  (setf (bigatom-mug a) (murmug (bigatom-num a))))

(defmethod atom= ((a bigatom) (b bigatom))
  (when (= (bigatom-num a) (bigatom-num b))
    (setf (bigatom-num b) (bigatom-num a))
    (if (bigatom-mug a)
        (setf (bigatom-mug b) (bigatom-mug a))
        (when (bigatom-mug b)
          (setf (bigatom-mug a) (bigatom-mug b))))
    t))

(defmethod teach ((a bigatom) (b t))
  (learn-integer b (bigatom-num a))
  (when (bigatom-mug a)
    (learn-mug b (bigatom-mug a))))

(defmethod unify ((a bigatom) (b bigatom))
  (setf (bigatom-num b) (bigatom-num a))
  (if (bigatom-mug a)
      (setf (bigatom-mug b) (bigatom-mug a))
      (when (bigatom-mug b)
        (setf (bigatom-mug a) (bigatom-mug b)))))

(defmethod to-noun ((a bignum))
  (make-bigatom a))

(defmethod print-object ((a bigatom) out)
  (write (bigatom-num a)))

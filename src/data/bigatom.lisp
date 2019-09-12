(defpackage urbit/data/bigatom
 (:use :cl)
 (:import-from :urbit/noun :to-noun)
 (:import-from :urbit/mug :cached-mug :compute-mug :murmug :learn-mug)
 (:import-from :urbit/atom :atomp :to-integer :learn-integer)
 (:import-from :urbit/equality :teach :atom=))

(in-package :urbit/data/bigatom)

(defclass bigatom ()
  ((num :initarg :num
        :accessor bnum
        :type bignum)
   (mug :initform nil
        :accessor bmug
        :type (unsigned-byte 31)))
  (:documentation "wrapping around bignum to cache mug"))

(defmethod atomp ((a bigatom))
 t)

(defmethod to-integer ((a bigatom))
 (bnum a))

(defmethod learn-integer ((a bigatom) (i integer))
 (setf (bnum a) i))

(defmethod learn-mug ((a bigatom) (m fixnum))
 (setf (bmug a) m))

(defmethod cached-mug ((a bigatom))
 (bmug a))

(defmethod compute-mug ((a bigatom))
 (setf (bmug a) (murmug (bnum a))))

(defmethod atom= ((a bigatom) (b bigatom))
 (when (= (bnum a) (bnum b))
  (setf (bnum b) (bnum a))
  (if (bmug a)
   (setf (bmug b) (bmug a))
   (when (bmug b)
    (setf (bmug a) (bmug b))))
  t))

(defmethod teach ((a bigatom) (b t))
 (learn-integer b (bnum a))
 (when (bmug a)
  (learn-mug b (bmug a))))
 
(defmethod unify ((a bigatom) (b bigatom))
 (setf (bnum b) (bnum a))
 (if (bmug a)
  (setf (bmug b) (bmug a))
  (when (bmug b)
   (setf (bmug a) (bmug b)))))

(defmethod to-noun ((a bignum))
 (make-instance 'bigatom :num a))

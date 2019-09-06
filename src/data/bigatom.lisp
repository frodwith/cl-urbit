(defpackage cl-urbit/data/bigatom
 (:use :cl)
 (:import-from :cl-urbit/unify :teach)
 (:import-from :cl-urbit/mug :murmug :compute-mug :cached-mug :learn-mug)
 (:import-from :cl-urbit/atom :atomp :atom= :to-integer :learn-integer))

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

(defmethod cached-mug ((a bigatom))
 (bmug a))

(defmethod compute-mug ((a bigatom))
 (setf (bmug a) (murmug (bnum a))))

(defmethod atom= ((a bigatom) (b bigatom))
 (when (= (bnum a) (bnum b))
  (setf (bnum b) (bnum a))
  (if (bmug a)
   (setf (bmug b) (bmug a))
   (if (bmug b)
    (setf (bmug a) (bmug b))))))

(defmethod teach ((a bigatom) (b t))
 (learn-integer b (bnum a))
 (when (bmug a)
  (learn-mug b (bmug a))))
 
(defmethod unify ((a bigatom) (b bigatom))
 (setf (bnum b) (bnum a))
 (if (bmug a)
  (setf (bmug b) (bmug a))
  (when (bmub b)
   (setf (bmug a) (bmug b)))))

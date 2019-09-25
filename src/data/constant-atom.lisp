(defpackage #:urbit/data/constant-atom
  (:use :cl)
  (:import-from :urbit/error :oops)
  (:import-from :urbit/mug :cached-mug :murmug :learn-mug)
  (:import-from :urbit/atom :atomp :to-integer :learn-integer)
  (:import-from :urbit/equality :teach :atom= :unify))

(in-package :urbit/data/constant-atom)

(defclass constant-atom ()
  ((num :initarg :num
        :reader  cnum
        :type bignum)
   (mug :initarg :mug
        :reader   cmug
        :type (unsigned-byte 31)))
  (:documentation "wrapping around bignum to cache mug"))

(defmethod atomp ((a constant-atom))
  t)

(defmethod to-integer ((a constant-atom))
  (cnum a))

(defmethod cached-mug ((a constant-atom))
  (cmug a))

(defmethod atom= ((a constant-atom) (b constant-atom))
  (eq a b))

(defmethod teach ((a constant-atom) (b t))
  (learn-integer b (cnum a))
  (learn-mug b (cmug a)))

(defmethod unify ((a constant-atom) (b constant-atom))
  (error 'oops))

(defun make-constant-atom (big &optional mug)
  (make-instance 'constant-atom :num big :mug (or mug (murmug big))))

(defmethod print-object ((a constant-atom) out)
  (write (cnum a)))

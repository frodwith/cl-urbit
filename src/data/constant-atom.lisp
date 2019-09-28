(defpackage #:urbit/data/constant-atom
  (:use :cl)
  (:import-from :urbit/error :oops)
  (:import-from :urbit/mug :cached-mug :murmug :learn-mug)
  (:import-from :urbit/atom :atomp :to-integer :learn-integer)
  (:import-from :urbit/equality :teach :atom= :unify)
  (:import-from :urbit/data/bigatom :make-bigatom))

(in-package :urbit/data/constant-atom)

(defstruct (constant-atom (:constructor make-constant-atom (num mug))
                          (:print-object print-constant-atom))
  (num nil :type bignum)
  (mug nil :type (or null (unsigned-byte 31))))

(defmethod atomp ((a constant-atom))
  t)

(defmethod to-integer ((a constant-atom))
  (constant-atom-num a))

(defmethod bump ((a constant-atom))
  (make-bigatom (1+ (constant-atom-num a))))

(defmethod compute-mug ((a constant-atom))
  (setf (constant-atom-mug a) (murmug (constant-atom-num a))))

(defmethod cached-mug ((a constant-atom))
  (constant-atom-mug a))

(defmethod atom= ((a constant-atom) (b constant-atom))
  (eq a b))

(defmethod teach ((a constant-atom) (b t))
  (learn-integer b (constant-atom-num a))
  (let ((m (constant-atom-mug a)))
    (when m (learn-mug b m))))

(defmethod unify ((a constant-atom) (b constant-atom))
  (error 'oops))

(defun print-constant-atom (a out)
  (write (constant-atom-num a)))

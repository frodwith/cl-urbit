(defpackage cl-urbit/noun
 (:use :cl :cl-urbit/error)
 (:import-from :murmurhash :murmurhash)
 (:export :noun :cellp :atomp :head :tail :mug))

(in-package cl-urbit/noun)

(defgeneric atomp (a))
(defgeneric cellp (a))
(defgeneric head (a))
(defgeneric tail (a))
(defgeneric mug (a))

(defun nounp (a) (or (atomp a) (cellp a)))
(deftype noun () `(satisfies nounp))
(deftype cell () `(satisfies cellp))
(deftype natom () `(satisfies atomp))

; most things are neither atoms nor cells
(defmethod cellp ((a t)) nil)
(defmethod atomp ((a t)) nil)

; these all work on regular lisp numbers, not atoms.
; (so you have to unpack bigatoms, in particular)
(defun mask (bits)
 (1- (ash 1 bits)))

(defun low-bits (bits of)
 (logand of (mask bits)))

(defun end (bloq n a)
 (low-bits (ash n bloq) a))

(defun met (bloq a)
 (let ((bits (integer-length a)))
  (if (zerop bloq)
   a
   (let* ((shift (ash bits bloq))
          (trunc (ash a (- shift)))
          (left  (low-bits shift a)))
     (if (zerop left) trunc (1+ trunc))))))

(defun mix (a b)
 (logxor a b))

(defun lsh (bloq n a)
 (ash a (ash n bloq)))

(defun rsh (bloq n a)
 (ash a (- (ash n bloq))))

(defun murmug (key)
 (loop for syd upfrom #xcafebabe
       for haz = (murmurhash key :seed syd)
       for ham = (mix (ash haz -31) (low-bits 31 haz))
       unless (zerop ham) return ham))

; fixnums are atoms
(defmethod atomp ((a fixnum)) t)
(defmethod head ((a fixnum)) (error 'exit))
(defmethod tail ((a fixnum)) (error 'exit))
(defmethod mug ((a fixnum)) (murmug a))

(defclass bigatom ()
  ((num :initarg :num
        :accessor bnum
        :type bignum)
   (mug :initform 0
        :accessor bmug
        :type (unsigned-byte 31)))
  (:documentation "wrapping around bignum to cache mug"))

(defmethod atomp ((a bigatom)) t)
(defmethod head ((a bigatom)) (error 'exit))
(defmethod tail ((a bigatom)) (error 'exit))
(defmethod mug ((a bigatom))
 (let ((got (bmug a)))
  (if (not (zerop got))
   got
   (let ((calculated (murmug (bnum a))))
    (setf (bmug a) calculated)
    calculated))))

(defclass dynamic-cell ()
  ((head :initarg :head
         :accessor dhead
         :type noun)
   (tail :initarg :tail
         :accessor dtail
         :type noun)
   (mug :initarg :mug
        :initform 0
        :accessor dmug
        :type (unsigned-byte 31)))
  (:documentation "pair of nouns with cached mug"))

(defun mug-both (a b)
  (murmug (mix a (mix #x7fffffff b))))

(defmethod cellp ((a dynamic-cell)) t)
(defmethod head ((a dynamic-cell)) (dhead a))
(defmethod tail ((a dynamic-cell)) (dtail a))
(defmethod mug ((a dynamic-cell))
 (let ((m (dmug a)))
   (if (not (zerop m))
    m
    (let* ((ma (mug (dhead a)))
           (mb (mug (dtail a)))
           (mc (mug-both ma mb)))
     (setf (dmug a) mc)
     mc))))

(defgeneric to-noun (a))

(defmethod to-noun ((a cons))
 (let* ((head (car a))
        (tail (cdr a))
        (here (to-noun head)))
  (if (null tail)
   here
   (make-instance 'dynamic-cell :head here :tail (to-noun tail)))))

(defmethod to-noun ((a fixnum)) a)
(defmethod to-noun ((a bignum))
 (make-instance 'bigatom :num a))

(define-condition no-noun-coercion (oops) ())
(defmethod to-noun ((a t))
 (if (nounp a)
  a
  (error 'no-noun-coercion)))

(defun noun (&rest args)
 (to-noun args))

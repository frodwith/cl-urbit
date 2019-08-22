(defpackage cl-urbit-worker/noun
 (:use :cl :cl-urbit-worker/error)
 (:export :noun :cellp :atomp :head :tail :mug))

(in-package cl-urbit-worker/noun)

(defgeneric cellp (a))
(defgeneric head (a))
(defgeneric tail (a))
(defgeneric atomp (a))
(defgeneric mug (a))

(defun nounp (a) (or (atomp a) (cellp a)))

(deftype noun () `(satisfies nounp))
(deftype cell () `(satisfies cellp))
(deftype natom () `(satisfies atomp))

(defmethod cellp ((a t)) nil)
(defmethod atomp ((a t)) nil)

(defmethod atomp ((a integer)) t)
(defmethod head ((o integer)) (error 'exit))
(defmethod tail ((o integer)) (error 'exit))
(defmethod mug ((a integer))
 ;; fixme: non-zero murmer3
 42)

(defun mug-both (mug-a mug-b)
 ;; fixme: combine mugs
 42)

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
        :type fixnum))
  (:documentation "A regular old, nothing special pair of two nouns"))

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
     m))))

(defun dcons (a b)
 (make-instance 'dynamic-cell :head a :tail b))

(defgeneric to-noun (a))

(defmethod to-noun ((a cons))
 (let* ((head (car a))
        (tail (cdr a))
        (here (to-noun head)))
  (if (null tail)
   here
   (dcons here (to-noun tail)))))

(defmethod to-noun ((a integer)) a)

(define-condition no-noun-coercion (oops) ())
(defmethod noun ((a t))
 (if (nounp a)
  a
  (error 'no-noun-coercion)))

(defun noun (&rest args)
 (to-noun args))

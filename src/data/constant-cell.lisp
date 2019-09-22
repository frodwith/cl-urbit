(defpackage urbit/data/constant-cell
 (:use :cl)
 (:import-from :urbit/mug :mug :cached-mug :compute-mug :murmug-two)
 (:import-from :urbit/cell :cellp :head :tail 
               :get-constant-cell :learn-constant-cell)
 (:import-from :urbit/equality :teach))

(in-package :urbit/data/constant-cell)

(defclass slimcell ()
  ((head :initarg :head
         :accessor shead
         :type noun)
   (tail :initarg :tail
         :accessor stail
         :type noun)
   (meta :initarg :mug
         :initform nil
         :accessor smeta)))

(defclass constant-cell ()
 ((head :initarg :head
        :reader chead
        :type noun)
  (tail :initarg :tail
        :reader ctail
        :type noun)
  (mug  :initarg :mug
        :reader cmug
        :type (unsigned-byte 31))))

(defun make-constant-cell (head tail)
 (make-instance 'constant-cell
  :head head
  :tail tail
  :mug (murmug-two (mug head) (mug tail))))

(defmethod cellp ((a constant-cell))
 t)

(defmethod head ((a constant-cell))
 (chead a))

(defmethod tail ((a constant-cell))
 (ctail a))

(defmethod cached-mug ((a constant-cell))
 (cmug a))

(defmethod compute-mug ((a constant-cell))
 (cmug a))

(defmethod teach ((a constant-cell) (b t))
 (learn-constant-cell b a))

(defmethod get-constant-cell ((a constant-cell))
 a)

(defpackage urbit/data/slimcell
 (:use :cl)
 (:import-from :urbit/noun :to-noun)
 (:import-from :urbit/mug :cached-mug :compute-mug :murmug :learn-mug :mug-cell)
 (:import-from :urbit/cell :cellp :head :tail :learn-head :learn-tail)
 (:import-from :urbit/equality :teach))

(in-package :urbit/data/slimcell)

(defclass slimcell ()
  ((head :initarg :head
         :accessor dhead
         :type noun)
   (tail :initarg :tail
         :accessor dtail
         :type noun)
   (mug :initarg :mug
        :initform nil
        :accessor dmug
        :type (unsigned-byte 31))))

(defmethod cellp ((a slimcell))
 t)

(defmethod head ((a slimcell))
 (dhead a))

(defmethod tail ((a slimcell))
 (dtail a))

(defmethod cached-mug ((a slimcell))
 (dmug a))

(defmethod compute-mug ((a slimcell))
 (setf (dmug a) (mug-cell a)))

(defmethod learn-mug ((a slimcell) (mug fixnum))
 (setf (dmug a) mug))

(defmethod learn-head ((a slimcell) (head t))
 (setf (dhead a) head))

(defmethod learn-tail ((a slimcell) (tail t))
 (setf (dtail a) tail))

(defmethod teach ((a slimcell) (b t))
 (learn-head b (dhead a))
 (learn-tail b (dtail a))
 (when (dmug a)
  (learn-mug b (dmug a))))

(defmethod unify ((a slimcell) (b slimcell))
 (setf (dhead b) (dhead a))
 (setf (dtail b) (dtail a))
 (if (dmug a)
  (setf (dmug b) (dmug a))
  (when (dmug b)
   (setf (dmug a) (dmug b)))))

(defmethod to-noun ((a cons))
 (let* ((head (car a))
        (tail (cdr a))
        (here (to-noun head)))
  (if (null tail)
	 here
	 (make-instance 'slimcell :head here :tail (to-noun tail)))))

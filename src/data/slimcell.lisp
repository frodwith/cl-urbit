(defpackage urbit/data/slimcell
 (:use :cl)
 (:import-from :urbit/noun :to-noun)
 (:import-from :urbit/equality :teach)
 (:import-from :urbit/cell :cellp :head :tail :learn-head :learn-tail)
 (:import-from :urbit/mug :cached-mug :compute-mug :murmug :learn-mug :mug-cell)
 (:import-from :urbit/data/constant-cell :constant-cell))

(in-package :urbit/data/slimcell)

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

(defmethod cellp ((a slimcell))
 t)

(defmethod head ((a slimcell))
 (shead a))

(defmethod tail ((a slimcell))
 (stail a))

(defmethod cached-mug ((a slimcell))
 (let ((m (smeta a)))
  (cond ((null m) nil)
        ((typep m 'fixnum) m)
        (t (cached-mug m)))))

(defmethod compute-mug ((a slimcell))
 (let ((m (smeta a)))
  (cond ((null m) (setf (smeta a) (mug-cell a)))
        ((typep m 'fixnum) nil)
        (t (compute-mug m)))))

(defmethod learn-mug ((a slimcell) (mug fixnum))
 (let ((m (smeta a)))
  (cond ((null m) (setf (smeta a) mug))
        ((typep m 'fixnum) nil)
        (t (learn-mug m mug)))))

(defmethod learn-head ((a slimcell) (head t))
 (setf (shead a) head))

(defmethod learn-tail ((a slimcell) (tail t))
 (setf (stail a) tail))

(defmethod learn-constant-cell ((a slimcell) (k constant-cell))
 (setf (smeta a) k))

(defmethod get-constant-cell ((a slimcell))
 (let ((m (smeta a)))
  (if (or (null m) (typep m 'fixnum))
   nil
   (get-constant-cell m))))

(defun teach-meta (a b)
 (let ((m (smeta a)))
  (cond ((null m) nil)
        ((typep m 'fixnum) (learn-mug b m))
        (t (teach m b)))))

(defmethod teach ((a slimcell) (b t))
 (learn-head b (shead a))
 (learn-tail b (stail a))
 (teach-meta a b))

(defmethod unify ((a slimcell) (b slimcell))
 (setf (shead b) (shead a))
 (setf (stail b) (stail a))
 (teach-meta a b)
 (teach-meta b a))

(defmethod to-noun ((a cons))
 (let* ((head (car a))
        (tail (cdr a))
        (here (to-noun head)))
  (if (null tail)
	 here
	 (make-instance 'slimcell :head here :tail (to-noun tail)))))

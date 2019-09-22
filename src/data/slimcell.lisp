(defpackage urbit/data/slimcell
 (:use :cl)
 (:import-from :urbit/noun :to-noun)
 (:import-from :urbit/equality :teach)
 (:import-from :urbit/formula :formula)
 (:import-from :urbit/context :intern-cell)
 (:import-from :urbit/cell :cellp :head :tail :learn-head :learn-tail)
 (:import-from :urbit/mug :cached-mug :compute-mug :murmug :learn-mug :mug-cell)
 (:import-from :urbit/data/constant-cell :constant-cell :chead :ctail :cmug :code))

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

(defun in (a &optional mug)
 (let ((i (intern-cell (shead a) (stail a) mug)))
  (setf (shead a) (chead i))
  (setf (stail a) (ctail i))
  (setf (smeta a) i)))

(defmethod formula ((a slimcell))
 (code
  (let ((m (smeta a)))
   (etypecase m
    (null (in a))
    (fixnum (in a m))
    (constant-cell m)))))

(defmethod cached-mug ((a slimcell))
 (let ((m (smeta a)))
  (etypecase m
   (null nil)
   (fixnum m)
   (constant-cell (cmug m)))))

(defmethod compute-mug ((a slimcell))
 (let ((m (smeta a)))
  (etypecase m
   (null (setf (smeta a) (mug-cell a)))
   (fixnum nil)
   (constant-cell (cmug m)))))

(defmethod learn-mug ((a slimcell) (mug fixnum))
 (let ((m (smeta a)))
  (etypecase m
   (null (setf (smeta a) mug))
   (fixnum nil)
   (constant-cell nil))))

(defmethod learn-head ((a slimcell) (head t))
 (setf (shead a) head))

(defmethod learn-tail ((a slimcell) (tail t))
 (setf (stail a) tail))

(defmethod learn-constant-cell ((a slimcell) (k constant-cell))
 (setf (smeta a) k))

(defmethod get-constant-cell ((a slimcell))
 (let ((m (smeta a)))
  (etypecase m
   (null nil)
   (fixnum nil)
   (constant-cell m))))

(defun teach-meta (a b)
 (let ((m (smeta a)))
  (etypecase m
   (null nil)
   (fixnum (learn-mug b m))
   (constant-cell (teach m b)))))

(defmethod teach ((a slimcell) (b t))
 (learn-head b (shead a))
 (learn-tail b (stail a))
 (teach-meta a b))

(defmethod unify ((a slimcell) (b slimcell))
 (setf (shead b) (shead a))
 (setf (stail b) (stail a))
 (teach-meta a b)
 (teach-meta b a))

(defun scons (head tail)
 (make-instance 'slimcell :head head :tail tail))

(defmethod to-noun ((a cons))
 (let* ((head (car a))
        (tail (cdr a))
        (here (to-noun head)))
  (if (null tail)
	 here
   (scons here (to-noun tail)))))

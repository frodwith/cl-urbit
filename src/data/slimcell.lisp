(defpackage urbit/data/slimcell
  (:use :cl)
  (:import-from :urbit/noun :to-noun :noun)
  (:import-from :urbit/equality :teach)
  (:import-from :urbit/formula :formula)
  (:import-from :urbit/context :intern-noun)
  (:import-from :urbit/cell :cellp :head :tail :learn-head :learn-tail :print-cell)
  (:import-from :urbit/mug :cached-mug :compute-mug :murmug :learn-mug :mug-cell)
  (:import-from :urbit/data/constant-cell :constant-cell :chead :ctail :cmug))

(in-package :urbit/data/slimcell)

(defstruct (slimcell (:constructor scons (head tail))
                     (:print-object print-slimcell))
  (head nil :type noun)
  (tail nil :type noun)
  (meta nil))

(defmethod cellp ((a slimcell))
  t)

(defmethod head ((a slimcell))
  (slimcell-head a))

(defmethod tail ((a slimcell))
  (slimcell-tail a))

(defun in (a &optional mug)
  (let ((i (intern-noun a mug)))
    (setf (slimcell-head a) (chead i))
    (setf (slimcell-tail a) (ctail i))
    (setf (slimcell-meta a) i)))

(defmethod formula ((a slimcell))
  (formula
    (let ((m (slimcell-meta a)))
      (etypecase m
        (null (in a))
        (fixnum (in a m))
        (constant-cell m)))))

(defmethod cached-mug ((a slimcell))
  (let ((m (slimcell-meta a)))
    (etypecase m
      (null nil)
      (fixnum m)
      (constant-cell (cmug m)))))

(defmethod compute-mug ((a slimcell))
  (let ((m (slimcell-meta a)))
    (etypecase m
      (null (setf (slimcell-meta a) (mug-cell a)))
      (fixnum nil)
      (constant-cell (cmug m)))))

(defmethod learn-mug ((a slimcell) (mug fixnum))
  (let ((m (slimcell-meta a)))
    (etypecase m
      (null (setf (slimcell-meta a) mug))
      (fixnum nil)
      (constant-cell nil))))

(defmethod learn-head ((a slimcell) (head t))
  (setf (slimcell-head a) head))

(defmethod learn-tail ((a slimcell) (tail t))
  (setf (slimcell-tail a) tail))

(defmethod learn-constant-cell ((a slimcell) (k constant-cell))
  (setf (slimcell-meta a) k))

(defmethod get-constant-cell ((a slimcell))
  (let ((m (slimcell-meta a)))
    (etypecase m
      (null nil)
      (fixnum nil)
      (constant-cell m))))

(defun teach-meta (a b)
  (let ((m (slimcell-meta a)))
    (etypecase m
      (null nil)
      (fixnum (learn-mug b m))
      (constant-cell (teach m b)))))

(defmethod teach ((a slimcell) (b t))
  (learn-head b (slimcell-head a))
  (learn-tail b (slimcell-tail a))
  (teach-meta a b))

(defmethod unify ((a slimcell) (b slimcell))
  (setf (slimcell-head b) (slimcell-head a))
  (setf (slimcell-tail b) (slimcell-tail a))
  (teach-meta a b)
  (teach-meta b a))

(defmethod to-noun ((a cons))
  (let* ((head (car a))
         (tail (cdr a))
         (here (to-noun head)))
    (if (null tail)
        here
        (scons here (to-noun tail)))))

(defun print-slimcell (a out)
  (print-cell (slimcell-head a) (slimcell-tail a) out))

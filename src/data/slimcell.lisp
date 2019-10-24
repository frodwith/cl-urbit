(defpackage urbit/data/slimcell
  (:use :cl)
  (:import-from :urbit/noun :to-noun :noun)
  (:import-from :urbit/equality :teach)
  (:import-from :urbit/formula :formula)
  (:import-from :urbit/context :context-intern)
  (:import-from :urbit/cell :cellp :head :tail :learn-head :learn-tail
                :constant-head :learn-core :print-cell :slot-etypecase)
  (:import-from :urbit/mug :cached-mug :compute-mug :murmug :learn-mug :mug
                :mug-cell)
  (:import-from :urbit/data/core :core :core-head :core-tail :make-core)
  (:import-from :urbit/data/constant-cell :constant-cell
                :constant-cell-head :constant-cell-tail))

(in-package :urbit/data/slimcell)

(defstruct (slimcell (:constructor scons (head tail))
                     (:print-object print-slimcell))
  (head nil :type noun)
  (tail nil :type noun)
  (meta nil :type (or null mug core constant-cell)))

(defmethod cellp ((a slimcell))
  t)

(defmethod head ((a slimcell))
  (slimcell-head a))

(defmethod tail ((a slimcell))
  (slimcell-tail a))

(defmacro smeta (slimcell (name) &body forms)
  `(slot-etypecase ,slimcell slimcell-meta (,name) ,@forms))

(defmethod formula ((a slimcell))
  (formula
    (smeta a (m)
      (constant-cell m)
      (null (setf (slimcell-meta a) (intern-noun a)))
      (mug  (setf (slimcell-meta a) (intern-noun a m)))
      (core (setf (slimcell-meta a) (intern-noun a (cached-mug m)))))))

(defmethod cached-mug ((a slimcell))
  (smeta a (m)
    (null nil)
    (mug m)
    ((or core constant-cell) (cached-mug m))))

(defmethod compute-mug ((a slimcell))
  (smeta a (m)
    (null (setf (slimcell-meta a) (mug-cell a)))
    (mug nil)
    ((or core constant-cell) (compute-mug m))))

(defmethod learn-mug ((a slimcell) mug)
  (smeta a (m)
    (null (setf (slimcell-meta a) mug))
    (mug nil)
    ((or core constant-cell) (learn-mug m mug))))

(defmethod learn-head ((a slimcell) head)
  (setf (slimcell-head a) head))

(defmethod learn-tail ((a slimcell) tail)
  (setf (slimcell-tail a) tail))

(defmethod learn-constant-cell ((a slimcell) (k constant-cell))
  (setf (slimcell-head a) (constant-cell-head k))
  (setf (slimcell-tail a) (constant-cell-tail k))
  (setf (slimcell-meta a) k))

(defmethod learn-core ((a slimcell) (c core))
  (smeta a (m)
    ((or core constant-cell) nil)
    ((or null mug) (setf (slimcell-head a) (core-head c))
                   (setf (slimcell-tail a) (core-tail c))
                   (setf (slimcell-meta a) c))))

(defun find-battery (a &optional mug)
  (let ((k (context-intern (slimcell-head a))))
    (setf (slimcell-head a) k)
    (setf (slimcell-meta a) (make-core k (slimcell-tail a) mug))
    k))

(defmethod constant-head ((a slimcell))
  (smeta a (m)
    (null (find-battery a))
    (mug (find-battery a m))
    ((or core constant-cell) (constant-head m))))

(defmethod get-constant-cell ((a slimcell))
  (smeta a (m)
    ((or null mug) nil)
    (core (get-constant-cell m))
    (constant-cell m)))

(defun teach-smeta (a b)
  (smeta a (m)
    (null nil)
    (mug (learn-mug b m))
    ((or core constant-cell) (teach m b))))

(defmethod teach ((a slimcell) b)
  (learn-head b (slimcell-head a))
  (learn-tail b (slimcell-tail a))
  (teach-smeta a b))

(defmethod unify ((a slimcell) (b slimcell))
  (setf (slimcell-head b) (slimcell-head a))
  (setf (slimcell-tail b) (slimcell-tail a))
  (teach-smeta a b)
  (teach-smeta b a))

(defmethod to-noun ((a cons))
  (let* ((head (car a))
         (tail (cdr a))
         (here (to-noun head)))
    (if (null tail)
        here
        (scons here (to-noun tail)))))

(defun print-slimcell (a out)
  (print-cell (slimcell-head a) (slimcell-tail a) out))

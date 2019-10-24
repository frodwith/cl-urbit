(defpackage #:urbit/data/core
  (:use :cl)
  (:import-from :urbit/noun :noun)
  (:import-from :urbit/equality :teach)
  (:import-from :urbit/formula :formula)
  (:import-from :urbit/context :intern-noun)
  (:import-from :urbit/cell :cellp :head :tail :learn-head :learn-tail
                :learn-core :print-cell :slot-etypecase)
  (:import-from :urbit/mug :cached-mug :compute-mug :murmug :learn-mug :mug
                :mug-cell)
  (:import-from :urbit/data/constant-cell :constant-cell 
                :constant-cell-head :constant-cell-tail))

(in-package :urbit/data/core)

(defstruct (core (:constructor make-core (head tail meta))
                 (:print-object print-core)) 
  (head nil :type constant-cell :read-only t)
  (tail nil :type noun)
  (meta nil :type (or null mug constant-cell)))

(defmethod cellp ((a core))
  t)

(defmethod head ((a core))
  (core-head a))

(defmethod constant-head ((a core))
  (core-head a))

(defmethod tail ((a core))
  (core-tail a))

(defmacro cmeta (core (name) &body forms)
  `(slot-etypecase ,core core-meta (,name) ,@forms))

(defmethod formula ((a core))
  (formula
    (cmeta a (m)
      (constant-cell m)
      (null (setf (core-meta a) (intern-noun a)))
      (mug  (setf (core-meta a) (intern-noun a m))))))

(defmethod cached-mug ((a core))
  (cmeta a (m)
    (null nil)
    (mug m)
    (constant-cell (cached-mug m))))

(defmethod compute-mug ((a core))
  (cmeta a (m)
    (null (setf (core-meta a) (mug-cell a)))
    (mug nil)
    (constant-cell (compute-mug m))))

(defmethod learn-mug ((a core) mug)
  (cmeta a (m)
    (null (setf (core-meta a) mug))
    (mug nil)
    (constant-cell (learn-mug m mug))))

(defmethod learn-tail ((a core) tail)
  (setf (core-tail a) tail))

(defmethod learn-constant-cell ((a core) (k constant-cell))
  (setf (core-tail a) (constant-cell-tail k))
  (setf (core-meta a) k))

(defmethod get-constant-cell ((a core))
  (cmeta a (m)
    ((or null mug) nil)
    (constant-cell m)))

(defun teach-cmeta (a b)
  (cmeta a (m) 
    (null nil)
    (mug (learn-mug b m))
    (constant-cell (teach m b))))

(defmethod teach ((a core) b)
  (learn-head b (core-head a))
  (learn-tail b (core-tail a))
  (learn-core b a)
  (teach-cmeta a b))

(defmethod unify ((a core) (b core))
  (setf (core-tail b) (core-tail a))
  (teach-cmeta a b)
  (teach-cmeta b a))

(defun print-core (a out)
  (print-cell (core-head a) (core-tail a) out))

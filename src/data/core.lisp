(in-package #:urbit/data/core)

(defstruct (core (:constructor make-core (head tail stencil meta))
                 (:print-object print-core)) 
  (head nil :type constant-cell :read-only t)
  (tail nil :type noun)
  (stencil nil :type stencil)
  (meta nil :type (or null mug constant-cell)))

(defmethod cellp ((a core))
  t)

(defmethod head ((a core))
  (core-head a))

(defmethod tail ((a core))
  (core-tail a))

(defmacro core-case (core (name) &body forms)
  `(meta-case (core-meta ,core) (,name) ,@forms))

(defmethod cached-unique ((a core))
  (core-case a (meta)
    (constant-cell meta)
    ((or null mug) nil)))

(defmethod compute-unique ((a core))
  (unique-cons (core-head core) 
               (unique (core-tail core))
               (cached-mug a)))

(defmethod learn-unique ((a core) (k constant-cell))
  (setf (core-tail a) (constant-cell-tail k))
  (unify-mug k a)
  (learn-stencil k (core-stencil a))
  (setf (core-meta a) k))

(defmethod unique-head ((a core))
  (core-head core))

(defmethod cached-mug ((a core))
  (core-case a (meta)
    (null nil)
    (mug meta)
    (constant-cell (cached-mug m))))

(defmethod compute-mug ((a core))
  (mug-cell a))

(defmethod learn-mug ((a core) (m fixnum))
  (core-case a (meta)
    (null (setf (core-meta a) m))
    (mug nil)
    (constant-cell (learn-mug meta m))))

(defmethod cached-stencil ((a core))
  (core-stencil a))

(defmethod learn-tail ((a core) tail)
  (setf (core-tail a) tail))

(defun teach-cmeta (a b)
  (core-case a (meta)
    (null nil)
    (mug (learn-mug b meta))
    (constant-cell (teach meta b))))

(defmethod teach ((a core) b)
  (learn-head b (core-head a))
  (learn-tail b (core-tail a))
  (learn-stencil b a)
  (teach-cmeta a b))

(defmethod unify ((a core) (b core))
  (teach-cmeta a b)
  (teach-cmeta b a)
  (setf (core-tail b) (core-tail a)))

(defun print-core (a out)
  (print-cell (core-head a) (core-tail a) out))

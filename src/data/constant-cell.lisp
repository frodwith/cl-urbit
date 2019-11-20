(in-package #:urbit/data/constant-cell)

(defstruct (battery-meta (:constructor make-battery-meta (discovery match)))
  (arms nil :type axis-map)
  (discovery nil); :type hashboard-entry)
  (match nil :type (or null match)))

(defstruct (nock-meta (:constructor make-nock-meta (form)))
  (form nil :type (or list symbol))
  (func nil :type (or null (function (noun) noun)))
  (battery nil :type (or null battery-meta)))

(deftype constant-noun () '(or fixnum constant-atom constant-cell))

(defstruct (constant-cell (:constructor make-constant-cell (head tail mug))
                          (:print-object print-constant-cell))
  (head nil :type constant-noun :read-only t)
  (tail nil :type constant-noun :read-only t)
  (mug  nil :type (or null mug))
  (nock nil :type (or null nock-meta))
  (gnosis nil :type gnosis))

(defmethod cellp ((a constant-cell))
  t)

(defmethod head ((a constant-cell))
  (constant-cell-head a))

(defmethod constant-head ((a constant-cell))
  (constant-cell-head a))

(defmethod tail ((a constant-cell))
  (constant-cell-tail a))

(defmethod compute-mug ((a constant-cell))
  (mug-cell a))

(defmethod cached-mug ((a constant-cell))
  (constant-cell-mug a))

(defmethod learn-mug ((a constant-cell) mug)
  (setf (constant-cell-mug a) mug))

(defmethod cached-unique ((a constant-cell))
  a)

(defmethod cached-essence ((a constant-cell))
  (constant-cell-gnosis a))

(defmethod compute-essence ((a constant-cell))
  ; fixme: shim
  t)

(defmethod learn-essence ((a constant-cell) essence)
  (setf (constant-cell-gnosis a) essence))

(defmethod teach ((a constant-cell) b)
  (when-let (m (constant-cell-mug a))
    (learn-mug b m))
  (when-let (e (constant-cell-gnosis a))
    (learn-essence b e)) 
  (learn-unique b a))

(defmethod unify ((a constant-cell) (b constant-cell))
  (error 'oops))

(defun print-constant-cell (a out)
  (print-cell (constant-cell-head a) (constant-cell-tail a) out))

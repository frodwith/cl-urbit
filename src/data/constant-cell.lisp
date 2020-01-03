(in-package #:urbit/data/constant-cell)

(defstruct (assumption (:constructor make-assumption ()))
  (is-valid t :type boolean))

(defun invalidate (assumption)
  (setf (assumption-is-valid assumption) nil))

(defstruct (battery-meta
             (:constructor make-battery-meta (stability discovery match)))
  (arms nil :type axis-map)
  (stability nil :type assumption)
  (matcher nil :type matcher)
  (discovery nil)); :type hashboard-entry))

(defun battery-meta-destabilize (meta)
  (setf (battery-meta-arms meta) nil)
  (invalidate (battery-meta-stability meta))
  (setf (battery-meta-stability meta) (make-assumption)))

(defstruct (nock-meta (:constructor make-nock-meta (form)))
  (form nil :type (or list symbol))
  (func nil :type (or null (function (noun) noun)))
  (battery nil :type (or null battery-meta)))

(defun battery-meta (nock)
  (declare (type nock-meta nock))
  (cache-field nock nock-meta-battery
    (make-battery-meta (make-assumption) nil nil)))

(defstruct (constant-cell (:constructor make-constant-cell (head tail mug))
                          (:print-object print-constant-cell))
  (head nil :type constant-noun :read-only t)
  (tail nil :type constant-noun :read-only t)
  (mug  nil :type (or null mug))
  (nock nil :type (or null nock-meta))
  (gnosis nil :type gnosis))

(deftype constant-noun () '(or constant-atom constant-cell))

(defun nock-meta (a)
  (declare (type constant-cell a))
  (cache-field a constant-cell-nock (make-nock-meta (compile-raw a))))

(defun constant-frag (k axis)
  (declare (type constant-noun k)
           (type constant-atom axis))
  (let ((axis (constant-atom-num axis)))
    (if (zerop axis)
        (error 'exit)
        (loop with c = k
              until (= axis 1)
              unless (typep c 'constant-cell)
              do (error 'exit)
              do (setq c (if (= 2 (cap axis))
                             (constant-cell-head c)
                             (constant-cell-tail c)))
              do (setq axis (mas axis))
              finally (return (the constant-noun c))))))

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

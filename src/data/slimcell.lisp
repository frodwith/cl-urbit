(in-package #:urbit/data/slimcell)

(defstruct (slimcore (:constructor make-slimcore (mug essence)))
  (essence nil :type essence)
  (mug nil :type (or null mug)))

(defstruct (slimcell (:constructor scons (head tail))
                     (:print-object print-slimcell))
  (head nil :type noun)
  (tail nil :type noun)
  (meta nil :type (or null mug slimcore constant-cell)))

(defmethod cellp ((a slimcell))
  t)

(defmethod head ((a slimcell))
  (slimcell-head a))

(defmethod tail ((a slimcell))
  (slimcell-tail a))

(defmacro slim-case (slimcell (name) &body clauses)
  `(meta-case (slimcell-meta ,slimcell) (,name) ,@clauses))

(defmethod cached-unique ((a slimcell))
  (slim-case a (meta)
    (constant-cell meta)
    ((or null mug slimcore) nil)))

(defmethod compute-unique ((a slimcell))
  (multiple-value-bind (head mug)
    (slim-case a (meta)
      (null (values nil nil))
      (mug (values nil meta))
      (slimcore (values (slimcell-head a) (slimcore-mug meta))))
    (unique-cons (or head (unique (slimcell-head a)))
                 (unique (slimcell-tail a))
                 mug)))

(defmethod learn-unique ((a slimcell) (k constant-cell))
  (setf (slimcell-head a) (constant-cell-head k))
  (setf (slimcell-tail a) (constant-cell-tail k))
  (unify-mug k a)
  (unify-stencil k a)
  (setf (slimcell-meta a) k))

(defmethod unique-head ((a slimcell))
  (slim-case a (meta)
    ((or null mug) (setf (slimcell-head a) (unique (slimcell-head a))))           
    ((or slimcore constant-cell) (slimcell-head a))))

(defmethod cached-mug ((a slimcell))
  (slim-case a (meta)
    (null nil)           
    (mug meta)
    (slimcore (slimcore-mug meta))
    (constant-cell (constant-cell-mug meta))))

(defmethod compute-mug ((a slimcell))
  (mug-cell a))

(defmethod learn-mug ((a slimcell) (m fixnum))
  (slim-case a (meta)
    (null (setf (slimcell-meta a) m))
    (mug nil)
    (slimcore (setf (slimcore-mug meta) m))
    (constant-cell (learn-mug meta m)))) 

(defmethod cached-essence ((a slimcell))
  (slim-case a (meta)
    ((or null mug) nil)
    (slimcore (slimcore-essence meta))
    (constant-cell (constant-cell-gnosis meta))))

(defmethod compute-essence ((a slimcell))
  ; FIXME: shim
  t)

(defmethod learn-essence ((a slimcell) essence)
  (slim-case a (meta)
    (null (setf (slimcell-meta a) (make-slimcore nil essence)))
    (mug (setf (slimcell-meta a) (make-slimcore meta essence)))
    (slimcore nil)
    (constant-cell (learn-essence meta essence))))
  
(defmethod learn-head ((a slimcell) head)
  (slim-case a (meta)
    ((or null mug) (setf (slimcell-head a) head))
    ((or slimcore constant-cell) nil)))

(defmethod learn-tail ((a slimcell) tail)
  (slim-case a (meta)
    (constant-cell nil)  
    ((or null mug slimcore) (setf (slimcell-head a) head))))

(defun teach-smeta (a b)
  (slim-case a (meta)
    (null nil)
    (mug (learn-mug b meta))
    (slimcore (when-let (m (slimcore-mug meta))
                (learn-mug b m))
              (learn-stencil b (slimcore-stencil meta)))
    (constant-cell (teach meta b))))

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

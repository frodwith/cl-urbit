(in-package #:urbit/data/bigatom)

(defstruct (bigatom (:constructor make-bigatom (num))
                    (:print-object print-bigatom))
  (num nil :type bignum)
  (meta nil :type (or null mug constant-atom)))

(defmacro bcase (bigatom (name) &body clauses)
  `(meta-case (bigatom-meta ,bigatom) (,name)
     ,@clauses))

(defmethod atomp ((a bigatom))
  t)

(defmethod to-integer ((a bigatom))
  (bigatom-num a))

(defmethod cached-unique ((a bigatom))
  (bcase a (meta)
    ((or null mug) nil)
    (constant-atom meta)))

(defmethod compute-unique ((a bigatom))
  (bcase a (meta)
    (null (unique-integer (bigatom-num a)))
    (mug (unique-integer (bigatom-num a) meta))))

(defmethod learn-unique ((a bigatom) (k constant-atom))
  (bcase a (meta)
    (null (setf (bigatom-meta a) k)
          (setf (bigatom-num a) (constant-atom-num k)))
    (mug (learn-mug k meta)
         (setf (bigatom-meta a) k)
         (setf (bigatom-num a) (constant-atom-num k)))
    (constant-atom nil)))

(defmethod learn-integer ((a bigatom) i)
  (bcase a (meta)
    ((or null mug) (setf (bigatom-num a) i))
    (constant-atom nil)))

(defmethod bump ((a bigatom))
  (make-bigatom (1+ (bigatom-num a))))

(defmethod cached-mug ((a bigatom))
  (bcase a (meta)
    (null nil)
    (mug meta)
    (constant-atom (constant-atom-mug meta))))

(defmethod compute-mug ((a bigatom))
  (murmug (bigatom-num a)))

(defmethod learn-mug ((a bigatom) (m fixnum))
  (bcase a (meta)
    (null (setf (bigatom-meta a) m))
    (mug nil)
    (constant-atom (learn-mug meta m))))

(defmethod atom= ((a bigatom) (b bigatom))
  (when (= (bigatom-num a) (bigatom-num b))
    (setf (bigatom-num b) (bigatom-num a))
    (if (bigatom-mug a)
        (setf (bigatom-mug b) (bigatom-mug a))
        (when (bigatom-mug b)
          (setf (bigatom-mug a) (bigatom-mug b))))
    t))

(defmethod teach ((a bigatom) b)
  (bcase a (meta)
    (null nil)
    (mug (learn-mug b (bigatom-mug a)))
    (constant-atom (teach meta b)))
  (learn-integer b (bigatom-num a)))

(defmethod to-noun ((a bignum))
  (make-bigatom a))

(defun print-bigatom (a out)
  (write (bigatom-num a) :stream out))

(in-package #:urbit/data/constant-atom)

(defstruct (constant-bigatom (:constructor make-constant-bigatom (num mug))
                            (:print-object print-constant-bigatom))
  (num nil :type bignum :read-only t)
  (mug nil :type (or null mug)))

(defmethod atomp ((a constant-bigatom))
  t)

(defmethod to-integer ((a constant-bigatom))
  (constant-bigatom-num a))

(defmethod bump ((a constant-bigatom))
  (make-bigatom (1+ (constant-bigatom-num a))))

(defmethod compute-mug ((a constant-bigatom))
  (setf (constant-bigatom-mug a) (murmug (constant-bigatom-num a))))

(defmethod cached-mug ((a constant-bigatom))
  (constant-bigatom-mug a))

(defmethod atom= ((a constant-bigatom) (b constant-bigatom))
  (eq a b))

(defmethod teach ((a constant-bigatom) (b t))
  (learn-integer b (constant-bigatom-num a))
  (let ((m (constant-bigatom-mug a)))
    (when m (learn-mug b m))))

(defmethod unify ((a constant-bigatom) (b constant-bigatom))
  (error 'oops))

(defun print-constant-bigatom (a out)
  (write (constant-bigatom-num a) :stream out))

(deftype constant-atom () '(or fixnum constant-bigatom))

(defun constant-atom-num (k)
  (declare (type constant-atom k))
  (the integer
       (etypecase k
         (fixnum k)
         (constant-bigatom (constant-bigatom-num k)))))

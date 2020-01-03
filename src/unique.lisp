(in-package #:urbit/unique)

(defnoun-meta unique)

(defgeneric unique-head (obj)
  (:method (obj) (if (cellp obj)
                     (error 'oops)
                     (error 'exit))))

(defstruct (noun-interner (:constructor cons-interner (atoms cells)))
  (atoms nil :type hash-table)
  (cells nil :type hash-table))

; weakness is SBCL-only
(defun make-atom-table ()
  (make-hash-table :test 'eql :weakness :value))

(defun make-cell-table ()
  (make-hash-table :test 'equal :weakness :value))

(defun make-noun-interner ()
  (cons-interner (make-atom-table) (make-cell-table)))

(defun find-bignum (interner big &optional mug)
  (declare (type bignum big))
  (the constant-bigatom
       (cache-hash big (noun-interner-atoms interner)
                   ;to muffle the style-warning
                   ;(declare #+sbcl (sb-ext:muffle-conditions style-warning))
                   (make-constant-bigatom big mug))))

; call with unique (already internered) head and tail
(defun hash-cons (interner head tail &optional mug)
  (declare (type constant-noun head)
           (type constant-noun tail))
  (the constant-cell
       (cache-hash (cons head tail) (noun-interner-cells interner)
                   (make-constant-cell head tail mug))))

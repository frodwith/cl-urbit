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

(defun find-integer (interner i &optional mug)
  (cache-hash i (noun-interner-atoms interner)
    ;to muffle the style-warning
    ;(declare #+sbcl (sb-ext:muffle-conditions style-warning))
    (make-constant-atom i mug)))

; call with unique (already internered) head and tail
(defun hash-cons (interner head tail &optional mug)
  (cache-hash (cons head tail) (noun-interner-cells interner)
    (make-constant-cell head tail mug)))

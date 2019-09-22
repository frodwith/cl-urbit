(defpackage urbit/context
 (:use :cl)
 (:import-from urbit/atom :atomp :learn-integer :to-integer)
 (:import-from urbit/cell :learn-constant-cell :get-constant-cell :head :tail)
 (:import-from urbit/data/constant-cell :make-constant-cell)
 (:import-from urbit/data/constant-atom :make-constant-atom :constant-atom))

(in-package :urbit/context)

(defun make-atom-table ()
 ; SBCL only (weakness)
 (make-hash-table :test 'eql :weakness :value))

(defun make-cell-table ()
 ; SBCL only (weakness)
 (make-hash-table :test 'equal :weakness :value))

(defclass context ()
 ((cell-table 
   :reader   cells
   :initform (make-cell-table))
  (atom-table
   :reader   atoms
   :initform (make-atom-table))))

(defparameter *context* nil)

(defmacro with-context (c &rest body)
 `(let ((*context* ,c))
   ,@body))

(defun make-context ()
 (make-instance 'context))

(defun intern-atom (i &optional mug)
 (if (typep i 'fixnum)
  i
  (let* ((table (atoms *context*))
         (got   (gethash i table)))
   (or got
    (setf (gethash i table)
     (make-constant-atom i mug))))))

(defun intern-cell (head tail &optional mug)
 (let* ((table (cells *context*))
        (key   (cons head tail))
        (got   (gethash key table)))
  (or got
   (setf (gethash key table)
    (make-constant-cell head tail mug)))))

(defun intern-noun (n)
 (if (atomp n)
  (if (typep n 'constant-atom)
   n
   (let* ((i (to-integer n))
          (k (intern-atom i)))
    (learn-integer n (to-integer k))
    k))
  (let ((cc (get-constant-cell n)))
   (or cc
    (let* ((head (intern-noun (head n)))
           (tail (intern-noun (tail n)))
           (k (intern-cell head tail)))
     (learn-constant-cell n k)
     k)))))

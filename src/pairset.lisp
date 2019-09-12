; set of pairs of nouns

(defpackage cl-urbit/pairset
 (:import-from :cl-urbit/noun :mug)
 (:import-from :cl-urbit/mug :mug :murmug-two)
 (:export :make-pairset :put :contains))

(in-package cl-urbit/pairset)

(defun pair-eq (a b)
 (and (eq (car a) (car b))
      (eq (cdr a) (cdr b))))

(defun make-pairset ()
 (make-hash-table :test 'pair-eq))

(defun pairset-hash (a)
 (mug-both (mug (car a)) (mug (cdr a))))

(defun has-pair (table a b)
 (gethash (cons a b) table))

(defun have-pair (table a b)
 (setf (gethash (cons a b) table) t))

; SBCL only
(define-hashtable-test pair-eq pairset-hash)

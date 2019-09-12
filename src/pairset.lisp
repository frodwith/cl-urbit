(defpackage urbit/pairset
 (:use :cl)
 (:import-from :urbit/mug :mug :murmug-two))

(in-package :urbit/pairset)

; set of pairs of nouns
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

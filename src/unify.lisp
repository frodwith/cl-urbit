(defpackage cl-urbit/unify
 (:use :cl))

(in-package cl-urbit/unify)

; tell an equal object everything you know
; return value is ignored
(defgeneric teach (a b))
(defmethod teach ((a t) (b t))
 nil)

; copy slots and pick a representative
(defgeneric unify (a b))
(defmethod unify ((a t) (b t))
 (teach a b)
 (teach b a)
 a)

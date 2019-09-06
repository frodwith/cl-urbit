(defpackage cl-urbit/noun/atom
 (:use :cl)
 (:import-from cl-urbit/unify :unify))

(in-package :cl-urbit/noun/atom)

(defgeneric atomp (a))
(defmethod atomp ((a t))
 nil)

(defgeneric to-integer (a))
(defmethod to-integer ((a t))
 (error 'oops))

(defgeneric atom= (a b))

; implementations are encouraged to add specializations, this is the fallback
(defmethod atom= ((a t) (b t))
 (when (= (to-integer a) (to-integer b))
  (unify a b)))

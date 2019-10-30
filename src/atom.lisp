(in-package #:urbit/atom)

(defgeneric atomp (a)
  (:method (obj) nil))

(deftype natom () `(satisfies atomp))

; to-integer isn't a nock operation, so it just oopses if you don't define it
(defgeneric to-integer (a))
(defmethod to-integer ((a t))
  (error 'oops))

(defgeneric bump (a))

(defgeneric learn-integer (a i))
(defmethod learn-integer ((a t) (i integer))
  nil)

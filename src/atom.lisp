(defpackage #:urbit/atom
  (:use :cl)
  (:import-from :urbit/error :oops))

(in-package :urbit/atom)

(defgeneric atomp (a))
(defmethod atomp ((a t))
  nil)

(deftype natom () `(satisfies atomp))

; to-integer isn't a nock operation, so it just oopses if you don't define it
(defgeneric to-integer (a))
(defmethod to-integer ((a t))
  (error 'oops))

(defgeneric learn-integer (a i))
(defmethod learn-integer ((a t) (i integer))
  nil)

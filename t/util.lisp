(defpackage #:urbit/tests/util
  (:use :cl :prove)
  (:import-from :urbit/equality :same)
  (:export :no :is= :isnt= :is-eq :isnt-eq :is-same :isnt-same))

(in-package :urbit/tests/util)

(defmacro no (test &rest args)
  `(ok (not ,test) ,@args))

(defmacro defalias (name target test)
  `(defmacro ,name (&rest args) 
     `(,,target ,@args :test #',,test)))

(defalias is= 'is '=)
(defalias isnt= 'isnt '=)
(defalias is-eq 'is 'eq)
(defalias isnt-eq 'isnt 'eq)
(defalias is-same 'is 'same)
(defalias isnt-same 'isnt 'same)

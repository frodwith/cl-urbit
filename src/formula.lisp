(defpackage #:urbit/formula
  (:use :cl)
  (:import-from :urbit/error :exit))

(in-package :urbit/formula)

(defun crash (subject)
  (declare (ignore subject))
  (error 'exit))

(defgeneric formula (a))
(defmethod formula ((a t))
  #'crash)

(defun nock (subject formula)
  (funcall (formula formula) subject))

(defgeneric battery (core))
(defmethod battery (core)
  (error 'exit))

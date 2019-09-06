(defpackage cl-urbit/cell
 (:import-from :cl-urbit/atom :atomp)
 (:use :cl))

(in-package cl-urbit/cell)

(defgeneric cellp (a))
(defmethod cellp ((a t)) 
 nil)

(defun not-cell (a)
 (error (if (atomp a) 'exit 'oops)))

(defgeneric head (a))
(defmethod head ((a t))
 (not-cell a))

(defgeneric tail (a))
(defmethod tail ((a t))
 (not-cell a))

(defgeneric learn-head (a))
(defmethod learn-head ((a t))
 nil)

(defgeneric learn-tail (a))
(defmethod learn-tail ((a t))
 nil)

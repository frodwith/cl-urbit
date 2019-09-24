(defpackage urbit/cell
  (:use :cl)
  (:import-from :urbit/atom :atomp)
  (:import-from :urbit/error :exit :oops))

(in-package :urbit/cell)

(defgeneric cellp (a))
(defmethod cellp ((a t))
  nil)

(deftype cell () `(satisfies cellp))

; trying to fragment an atom is defined in nock as !! (exit)
(defun bad-cell (a)
  (if (atomp a)
      (error 'exit)
      (error 'oops)))

(defgeneric head (a))
(defmethod head ((a t))
  (bad-cell a))

(defgeneric tail (a))
(defmethod tail ((a t))
  (bad-cell a))

(defgeneric learn-head (a hed))
(defmethod learn-head ((a t) (hed t))
  nil)

(defgeneric learn-tail (a tal))
(defmethod learn-tail ((a t) (tl t))
  nil)

(defgeneric get-constant-cell (a))
(defmethod get-constant-cell ((a t))
  nil)

(defgeneric learn-constant-cell (a k))
(defmethod learn-constant-cell ((a t) (k t))
  nil)

(defparameter *print-cell-tail* nil)

(defun print-cell (head tail &optional out)
  (unless *print-cell-tail* (write-char #\[ out))
  (let ((*print-cell-tail* nil))
    (prin1 head out))
  (write-char #\space out)
  (let ((*print-cell-tail* t))
    (prin1 tail out))
  (unless *print-cell-tail* (write-char #\] out)))

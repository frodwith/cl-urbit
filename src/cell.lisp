(defpackage #:urbit/cell
  (:use :cl)
  (:import-from :urbit/atom :atomp :bump)
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

; fallback defined here because we need cellp
(defmethod bump ((a t))
  (if (cellp a)
      (error 'exit)
      (error 'oops)))

(defgeneric head (a))
(defmethod head ((a t))
  (bad-cell a))

; unconditionally returned interned head (core batteries)
(defgeneric constant-head (a))
(defmethod constant-head ((a t))
  (bad-cell a))

; unconditionally returned interned version of this cell
(defgeneric constant-cell (a))
(defmethod constant-cell (a)
  (bad-cell a))

(defgeneric cached-constant-cell (a))
(defmethod cached-constant-cell (a)
  nil)

(defgeneric learn-constant-cell (a k))
(defmethod learn-constant-cell (a k)
  nil)


(defgeneric tail (a))
(defmethod tail ((a t))
  (bad-cell a))

(defgeneric learn-head (a hed))
(defmethod learn-head ((a t) (hed t))
  nil)

(defgeneric learn-tail (a tal))
(defmethod learn-tail ((a t) (tl t))
  nil)

(defgeneric learn-core (a core))
(defmethod learn-core (a core)
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

(defmacro slot-etypecase (obj accessor (name) &body forms)
  `(let ((,name (,accessor ,obj)))
     (etypecase ,name
       ,@forms)))

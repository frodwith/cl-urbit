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
  (exit)
  (oops)))

(defgeneric head (a))
(defmethod head ((a t))
 (bad-cell a))

(defgeneric tail (a))
(defmethod tail ((a t))
 (bad-cell a))

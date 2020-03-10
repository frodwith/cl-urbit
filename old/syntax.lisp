;;;; [a b c] -> (noun a b c)
(defpackage #:urbit/syntax
  (:use :cl :named-readtables)
  (:import-from :urbit/noun :noun)
  (:export :brackets :enable-brackets))

(in-package :urbit/syntax)

(defun read-right (stream char)
  (declare (ignore stream char))
  (error "] is not valid by itself"))

(defun read-left (stream char)
  (declare (ignore char))
  `(noun ,@(read-delimited-list #\] stream)))

(defreadtable brackets
  (:merge :standard)
  (:macro-char #\[ #'read-left)
  (:macro-char #\] #'read-right))

(defmacro enable-brackets ()
  '(in-readtable brackets))

(defpackage #:urbit/syntax
  (:use #:cl #:named-readtables)
  (:export #:brackets #:enable-brackets #:arity-error))

(in-package #:urbit/syntax)

(defun read-right (stream char)
  (declare (ignore stream char))
  (error "] is not valid by itself"))

(define-condition arity-error (error) (contents))

(defun read-left (stream char)
  (declare (ignore char))
  (let ((contents (read-delimited-list #\] stream)))
    (if (or (null contents) (null (cdr contents)))
        (error 'arity-error :contents contents)
        (loop for pair on contents
              if (null (cddr pair))
              return ``(,,@leading ,,(car pair) . ,,(cadr pair))
              else
              collect (car pair) into leading))))

(defreadtable brackets
  (:merge :standard)
  (:macro-char #\[ #'read-left)
  (:macro-char #\] #'read-right))

(defmacro enable-brackets ()
  '(in-readtable brackets))

(defpackage #:urbit/hoon/syntax
  (:use #:cl #:named-readtables #:urbit/nock/cord)
  (:export #:brackets #:cords #:all
           #:enable-brackets #:enable-cords #:enable-syntax
           #:arity-error #:cord-error))

(in-package #:urbit/hoon/syntax)

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

(define-condition cord-error (error) (object))

(defun read-cord (stream char)
  (declare (ignore char))
  (let ((obj (read stream)))
    (if (typep obj 'symbol)
        (string->cord (string-downcase (symbol-name obj)))
        (error 'cord-error :object obj))))

(defreadtable brackets
  (:merge :standard)
  (:macro-char #\[ #'read-left)
  (:macro-char #\] #'read-right))

(defreadtable cords
  (:merge :standard)
  (:macro-char #\% #'read-cord))

(defreadtable all
  (:merge brackets cords))

(defmacro enable-brackets ()
  '(in-readtable brackets))

(defmacro enable-cords ()
  '(in-readtable cords))

(defmacro enable-syntax ()
  '(in-readtable all))

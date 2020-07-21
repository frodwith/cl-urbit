(defpackage #:urbit/convert
  (:use #:cl #:cl-intbytes #:urbit/data #:urbit/data/slimcell #:urbit/math)
  (:export #:string->tape #:tape->string
           #:string->cord #:cord->string
           #:cord->tape #:tape->cord))

(in-package #:urbit/convert)

(deftype cord () '(integer 0))

(defun string->tape (str &key (cell-fn #'slim-cons))
  (loop for tap = 0 then (funcall cell-fn c tap)
        for i from (1- (length str)) downto 0
        for c = (char-code (char str i))
        finally (return tap)))

(defun tape->string (tape)
  (loop with v = (make-array 100
                             :element-type 'character
                             :fill-pointer 0
                             :adjustable t)
        for n = tape then (tail n)
        while (deep n)
        for c = (code-char (head n))
        do (vector-push-extend c v)
        finally (return (coerce v 'string))))

(defun string->cord (s)
  (declare (string s))
  (loop with len = (length s)
        with oct = (make-array len)
        for c across s
        for i below len
        do (setf (aref oct i) (char-code c))
        finally (return (octets->uint oct len))))

(defun cord->string (a)
  (declare (cord a))
  (loop with len = (met 3 a)
        with str = (make-string len)
        for o across (int->octets a len)
        for i below len
        do (setf (schar str i) (code-char o))
        finally (return str)))

(defun cord->tape (a)
  (declare (cord a))
  (loop with len = (met 3 a)
        with vec = (int->octets a len)
        for tape = 0 then (cons c tape)
        for i from (1- len) downto 0
        for c = (aref vec i)
        finally (return tape)))

(defun tape->cord (tape)
  (loop with oct = (make-array 10 :adjustable t :fill-pointer 0
                               :element-type '(unsigned-byte 8))
        for n = tape then (tail n)
        while (deep n)
        for c = (end 0 8 (head n))
        do (vector-push-extend c oct)
        finally (return (octets->uint oct (length oct)))))

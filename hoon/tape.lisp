(defpackage #:urbit/hoon/tape
  (:use #:cl #:urbit/intbytes #:urbit/nock/cord #:urbit/nock/data)
  (:import-from #:urbit/nock/math #:met #:end)
  (:import-from #:urbit/nock/data/slimcell #:slim-cons)
  (:export #:string->tape #:tape->string #:cord->tape #:tape->cord))

(in-package #:urbit/hoon/tape)

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


(defun cord->tape (a)
  (declare (cord a))
  (loop with len = (met 3 a)
        with vec = (int->bytes a len)
        for tape = 0 then (slim-cons c tape)
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
        finally (return (the cord (values (bytes->int oct (length oct)))))))

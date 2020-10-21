(defpackage #:urbit/nock/cord
  (:use #:cl #:cl-intbytes)
  (:import-from #:urbit/nock/math #:met)
  (:export #:cord #:string->cord #:cord->string))

(in-package #:urbit/nock/cord)

; though cords are a hoon concept, we have "human-friendly" code
; for e.g. printing jet labels that needs to know about cords.

(deftype cord () '(integer 0))

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

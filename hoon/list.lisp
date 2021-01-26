(defpackage #:urbit/hoon/list
  (:use #:cl #:named-readtables #:urbit/nock/math
        #:urbit/nock/data #:urbit/hoon/syntax #:urbit/nock/common)
  (:export #:nlist->vector #:empty #:lent #:flop #:reap
           #:turn #:can #:rap #:rep))

(in-package #:urbit/hoon/list)
(in-readtable hoon)

; for lists, but also any $@(~ ...)
(defun empty (nlr)
  (unless (deep nlr)
    (prog1 t (?> (zerop (cl-integer nlr))))))

(defun lent (list)
  (loop for in = list then (tail in)
        until (empty in)
        counting t))

(defun flop (list)
  (loop for out = ~ then [item out]
        for in = list then (tail in)
        until (empty in)
        for item = (head in)
        finally (return out)))

(defun reap (times item)
  (loop for r = ~ then [item r]
        repeat times
        finally (return r)))

(defun nlist->vector (list)
  (loop with out = (make-array 10 :adjustable t :fill-pointer 0)
        for in = list then (tail in)
        until (empty in)
        for item = (head in)
        do (vector-push-extend item out)
        finally (return out)))

(defun turn (list fn)
  (loop with vec = (nlist->vector list)
        for out = ~ then [pro out]
        for i from (1- (length vec)) downto 0
        for pro = (funcall fn (aref vec i))
        finally (return out)))

(defun can (bloq list)
  (loop for out = 0 then (logior out (lsh bloq size (end bloq bloqs data)))
        for size = 0 then (+ size bloqs) 
        for in = list then (tail in)
        until (empty in)
        for pair = (head in)
        for bloqs = (cl-integer (head pair))
        for data = (cl-integer (tail pair))
        finally (return out)))

(defun rap (bloq list)
  (loop for out = 0 then (cat bloq out (cl-integer i))
        for in = list then (tail in)
        until (empty in)
        for i = (head in)
        finally (return out)))

(defun rep (bloq list)
  (loop with size = (ash 1 bloq)
        for r = 0 then (logior r (ash (ldb (byte size 0) (cl-integer i)) c))
        for c = 0 then (+ c size)
        for in = list then (tail in)
        until (empty in)
        for i = (head in)
        finally (return r)))

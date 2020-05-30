(defpackage #:urbit/zig
  (:use #:cl #:urbit/math)
  (:export #:zig #:axis->zig #:zig->axis #:zig-common #:zig-sub-p
           #:zig-compile-fail))

(in-package #:urbit/zig)

; zigs are an internal axis representation as bit-vectors.
; they're more directly decomposable than integers.
; the bits (0 for head, 1 for tail) go shallow to deep from left to right,
; i.e. #*0010 is tail tail head tail, or (tail (head (tail (tail x))))

(deftype zig () 'simple-bit-vector)

(defun axis->zig (a)
  (declare (pint a))
  (the zig (loop with len = (- (integer-length a) 1)
                 with vec = (make-array len :element-type 'bit)
                 for i from 0 below len
                 for j downfrom (1- len)
                 for cap = (if (logbitp j a) 1 0)
                 do (setf (bit vec i) cap)
                 finally (return vec))))

(defun zig->axis (z)
  (declare (zig z))
  (the pint (loop for a = 1 then (+ (ash a 1) (bit z i))
                  for i below (length z)
                  finally (return a))))

(defun zig-common-head (a b len)
  (declare (zig a b) (uint len))
  (equal a (subseq b 0 len)))

(defun zig-common (a b)
  (declare (zig a b))
  (let ((alen (length a))
        (blen (length b)))
    (if (= alen blen)
        (when (equal a b) (values a #*))
        (if (< alen blen)
            (when (zig-common-head a b alen)
              (values a (subseq b alen)))
            (when (zig-common-head b a blen)
              (values b (subseq a blen)))))))

(defun zig-sub-p (a b)
  (declare (zig a b))
  (let ((alen (length a))
        (blen (length b)))
    (if (= alen blen)
        (equal a b)
        (and (< alen blen) (zig-common-head a b alen)))))

(defun zig-compile-fail (z subject head tail deep)
  (declare (zig z))
  (declare (symbol subject head tail deep))
  (let ((o (gensym))
        (b (gensym)))
    `(let ((,o ,subject))
       (block
         ,b
         ,@(loop for i below (length z)
                 collect `(unless (,deep ,o)
                            (return-from ,b (values nil ,(subseq z 0 i))))
                 collect `(setq ,o (,(if (= (bit z i) 0) head tail) ,o)))
         ,o))))

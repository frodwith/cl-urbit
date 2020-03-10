(defpackage #:urbit/math
  (:use #:cl)
  (:import-from #:murmurhash #:murmurhash)
  (:export #:met #:mix #:end #:lsh #:rsh
           #:mas #:tax #:pax
           #:mug #:murmug #:murmugs))

(in-package #:urbit/math)

; primitive operations on common lisp integers.

(declaim (optimize
           (compilation-speed 0)
           (debug 0)
           (safety 0)
           (space 0)
           (speed 3)))

(deftype uint () '(integer 0))

(defun mask (bits)
  (declare (uint bits))
  (the uint (1- (ash 1 bits))))

(defun low-bits (bits of)
  (declare (uint bits of))
  (the uint (logand of (mask bits))))

(defun met (b a)
  (declare (uint b a))
  (the uint
    (let ((bits (integer-length a)))
      (if (zerop b)
          bits
          (let ((full (ash bits (- b))))
            (if (> bits (ash full b))
              (1+ full)
              full))))))

(defun mix (a b)
  (declare (uint a b))
  (the uint (logxor a b)))

(defun end (b n a)
  (declare (uint b n a))
  (the uint (low-bits (ash n b) a)))

(defun lsh (b n a)
  (declare (uint b n a))
  (the uint (ash a (ash n b))))

(defun rsh (b n a)
  (declare (uint b n a))
  (the uint (ash a (- (ash n b)))))

; axis functions
; NOTE the argument type must be >= 2
(deftype decomposable-axis () '(integer 2))

(defun mas (a)
  (declare (decomposable-axis a))
  (the uint
    (let ((len (- (integer-length a) 2)))
      (logxor (ash 1 len) (low-bits len a)))))

(defun tax (a)
  (declare (decomposable-axis a))
  (the boolean (logbitp (- (integer-length a) 2) a)))

(defun pax (a)
  (declare (decomposable-axis a))
  (the list
    (loop for i downfrom (- (integer-length a) 2) to 0
          collecting (logbitp i a))))

; murmur3 hash computation
(deftype mug () '(unsigned-byte 31))

(defun murmug (a)
  (declare (uint a))
  (the mug
    (loop for syd upfrom #xcafebabe
          for haz = (murmurhash a :seed syd)
          for ham = (mix (rsh 0 31 haz) (end 0 31 haz))
          unless (zerop ham) return ham)))

(defun murmugs (a b)
  (declare (mug a b))
  (the mug (murmug (mix a (mix #x7fffffff b)))))

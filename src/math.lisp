(defpackage urbit/math
 (:use :cl)
 (:import-from :urbit/error :exit))

(in-package :urbit/math)

; these all work on regular lisp numbers, not atoms.
(defun mask (bits)
  (1- (ash 1 bits)))

(defun low-bits (bits of)
 (logand of (mask bits)))

(defun end (bloq n a)
 (low-bits (ash n bloq) a))

(defun met (bloq a)
 (let ((bits (integer-length a)))
  (if (zerop bloq)
   a
   (let* ((shift (ash bits bloq))
          (trunc (ash a (- shift)))
          (left  (low-bits shift a)))
     (if (zerop left) trunc (1+ trunc))))))

(defun mix (a b)
 (logxor a b))

(defun lsh (bloq n a)
 (ash a (ash n bloq)))

(defun rsh (bloq n a)
 (ash a (- (ash n bloq))))

(defun cap (a)
 (case a
  ((0 1) (error 'exit))
  (t (let ((len (- (integer-length a) 2)))
      (if (logbitp len a)
       3
       2)))))

(defun mas (a)
 (case a
  ((0 1) (error 'exit))
  (t (let ((len (- (integer-length a) 2)))
      (logxor (ash 1 len) (low-bits len a))))))

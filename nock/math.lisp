(defpackage #:urbit/nock/math
  (:use #:cl)
  (:export #:uint #:pint #:met #:mix #:con #:dis #:lsh #:rsh #:end #:cat #:cut
           #:bex #:dec #:add #:sub #:mul #:div #:dvr #:hmod #:hmax #:hmin
           #:+fixnum-bits+))

(in-package #:urbit/nock/math)

(defparameter +fixnum-bits+ (integer-length most-positive-fixnum))

; primitive operations on common lisp integers.

(declaim (optimize
           (speed 3)
           (space 1)
           (compilation-speed 0)
           (debug 0)
           (safety 0)))

(deftype uint () '(integer 0)) ; unsigned (including zero)
(deftype pint () '(integer 1)) ; positive integers

; helpers

; (mask 1) -> 1
; (mask 2) -> 11
(defun mask (bits)
  (declare (uint bits))
  (the uint (1- (ash 1 bits))))

(defun low-bits (bits of)
  (declare (uint bits of))
  (the uint (logand of (mask bits))))

; hoon math

; bloq sizing
; ===========
; bloqs represent byte-sizes and are powers of two, i.e.
; 0 bloqs are 2^0 (1) bits wide
; 3 bloqs are 2^3 (8) bits
; 5 bloqs are 32 bits

; mnemonic: measure
; how many bloqs (b) is atom a?
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

; bitwise (logxxx)
(defun mix (a b)
  (declare (uint a b))
  (the uint (logxor a b)))

(defun con (a b)
  (declare (uint a b))
  (the uint (logior a b)))

(defun dis (a b)
  (declare (uint a b))
  (the uint (logand a b)))

; logical shifts, with bloq sizes
(defun lsh (b n a)
  (declare (uint b n a))
  (the uint (ash a (ash n b))))

(defun rsh (b n a)
  (declare (uint b n a))
  (the uint (ash a (- (ash n b)))))

; last n bloqs (size b) of atom a
(defun end (b n a)
  (declare (uint b n a))
  (the uint (low-bits (ash n b) a)))

(defun cat (a b c)
  (declare (uint a b c))
  (the uint (logior b (lsh a (met a b) c))))

(defun cut (bloq from-end bloq-count atom)
  (declare (uint bloq from-end bloq-count atom))
  (ldb (byte (ash bloq-count bloq)
             (ash from-end bloq))
       atom))

; mnemomic: binary exponent
; 1 << a
(defun bex (a)
  (declare (uint a))
  (the uint (ash 1 a)))

; arithmetic

(defun dec (a)
  (declare (pint a))
  (1- a))

(defun add (a b)
  (declare (uint a))
  (the uint (+ a b)))

(defun sub (a b)
  (declare (uint a b))
  (the uint (- a b)))

(defun mul (a b)
  (declare (uint a b))
  (the uint (* a b)))

(defun div (a b)
  (declare (uint a))
  (declare (pint b))
  (the uint (truncate a b)))

(defun dvr (a b)
  (declare (uint a))
  (declare (pint b))
  (the (values uint uint) (truncate a b)))

; there are common lisp functions named "mod" "max" and "min" so these have an
; "h" prefixed to avoid shadowing headaches.

(defun hmod (a b)
  (declare (uint a))
  (declare (pint b))
  (multiple-value-bind (q r) (truncate a b)
    (declare (ignore q))
    (the uint r)))

(defun hmax (a b)
  (declare (uint a b))
  (if (> a b) a b))

(defun hmin (a b)
  (declare (uint a b))
  (if (< a b) a b))

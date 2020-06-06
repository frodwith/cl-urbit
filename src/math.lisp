(defpackage #:urbit/math
  (:use #:cl #:cl-intbytes)
  (:export #:met #:mix #:end #:lsh #:rsh
           #:peg #:mas #:tax #:pax #:axis-parts
           #:string->cord #:cord->string
           #:pint #:uint #:decomposable-axis))

(in-package #:urbit/math)

; primitive operations on common lisp integers.

(declaim (optimize
           (compilation-speed 0)
           (debug 0)
           (safety 0)
           (space 0)
           (speed 3)))

(deftype uint () '(integer 0))
(deftype pint () '(integer 1))

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

; cords
(defun string->cord (s)
  (declare (string s))
  (loop with len = (length s)
        with oct = (make-array len)
        for c across s
        for i below len
        do (setf (aref oct i) (char-code c))
        finally (return (octets->uint oct len))))

(defun cord->string (a)
  (declare (uint a))
  (loop with len = (met 3 a)
        with str = (make-string len)
        for o across (int->octets a len)
        for i below len
        do (setf (schar str i) (code-char o))
        finally (return str)))

; axis functions

(defun peg (a b)
  (declare (uint a b))
  (if (= 1 a)
      b
      (if (= 1 b)
          a
          (let* ((c (integer-length b))
                 (d (1- c))
                 (e (ash 1 d))
                 (f (- b e))
                 (g (ash a d)))
            (+ f g)))))

; NOTE the argument type must be >= 2
(deftype decomposable-axis () '(integer 2))

(defun mas (a)
  "strips the first path element from axis"
  (declare (decomposable-axis a))
  (the uint
       (let ((len (- (integer-length a) 2)))
         (logxor (ash 1 len) (low-bits len a)))))

(defun tax (a)
  "boolean - is axis in tail?"
  (declare (decomposable-axis a))
  (the boolean (logbitp (- (integer-length a) 2) a)))

(defun pax (a)
  "list of path elements (t=tail, nil=head) for axis"
  (declare (decomposable-axis a))
  (the list
       (loop for i downfrom (- (integer-length a) 2) to 0
             collecting (logbitp i a))))

(defun axis-parts (a id head tail)
  (declare (decomposable-axis a)
           (symbol id head tail))
  (loop for is-tail in (pax a)
        for part = (if is-tail tail head)
        for s = (list part id) then (list part s)
        finally (return s)))

(defpackage #:urbit/nock/axis
  (:use #:cl #:urbit/nock/math)
  (:export #:decomposable #:tax #:cap #:mas #:peg #:for-axis #:compile-axis
           #:dax #:axis #:axis-length #:axis-nth #:subaxis-p
           #:axle #:axle-mint #:axle-length #:axle-nth #:subaxle-p))

(in-package #:urbit/nock/axis)

(declaim (optimize
           (speed 3)
           (space 1)
           (compilation-speed 0)
           (debug 0)
           (safety 0)))

(deftype axis () 'pint)
(deftype decomposable () '(integer 2))

; for indicating how many path elements are in an axis
(deftype axis-length () 'uint)

(defun axis-length (a)
  (declare (axis a))
  (the axis-length (1- (integer-length a))))

(defmacro dax ((axis in-length) (first-bit out-length) &body forms)
  `(let* ((,out-length (1- ,in-length))
          (,first-bit (logbitp ,out-length ,axis)))
     ,@forms))

; is tail the first path element of a
(defun tax (a)
  (declare (decomposable a))
  (dax (a (axis-length a)) (top more)
    (the boolean top)))

; there is an actual hoon function with this name, and this is how it returns.
(defun cap (a)
  (declare (decomposable a))
  (if (tax a) 3 2))

; compose two axes (first go a, then go b)
(defun peg (a b)
  (declare (pint a)
           (uint b))
  (the uint
       (if (= 1 a)
           b
           (if (= 1 b)
               a
               (let* ((c (integer-length b))
                      (d (1- c))
                      (e (ash 1 d))
                      (f (- b e))
                      (g (ash a d)))
                 (+ f g))))))

; bind tail to successive boolean values (true for tail) of axis,
; executing forms in those bindings.
(defmacro for-axis ((tail axis) &body forms)
  (let ((a (gensym)) (i (gensym)))
    `(loop with ,a = (the decomposable ,axis)
           for ,i from (- (integer-length ,a) 2) downto 0
           for ,tail = (logbitp ,i ,a)  
           do (progn ,@forms))))

; 4 -> (head (head subject))
(defun compile-axis (a subject head tail)
  (declare (axis a)
           (symbol subject head tail))
  (let ((s subject))
    (for-axis (in-tail a)
      (setq s (list (if in-tail tail head) s)))
    s))

; an axle is an axis that should be accompanied by its (non-zero) length,
; meaning that its higher bits (or lack thereof, for the sential bit)
; are ignored. These are for use in tight loops, because boundary condtions
; are not checked and they are inconvenient.
(deftype axle () 'decomposable)
; axle lengths are never 0
(deftype axle-length () 'pint)

; this is like an array lookup: if you ask for an n outside the axle/axis,
; you will get an error.
(defun axle-nth (n a len)
  (declare (uint n) (axle a) (axle-length len))
  (logbitp (1- (- len n)) a))

(defun axis-nth (n a)
  (declare (uint n) (axle a))
  (axle-nth n a (axis-length a)))

; len is the length of short, more is the additional length of long
(defun subaxle-p (short long len more)
  (declare (axle short long) (axle-length len) (uint more))
  (= (ldb (byte len 0) short)
     (ldb (byte len more) long)))

(defun subaxis-p (short long)
  (declare (axis short long))
  (or (= 1 short)
      (= short long)
      (and (not (= 1 long))
           (let ((shel (axis-length short))
                 (lang (axis-length long)))
             (unless (> shel lang)
               (subaxle-p short long shel (- lang shel)))))))

(defun axle-mint (a len)
  (declare (axle a) (axle-length len))
  (the decomposable (dpb 1 (byte 1 len) (ldb (byte len 0) a))))

; remove the first path element from axis a
(defun mas (a)
  (declare (decomposable a))
  (let ((less (1- (axis-length a))))
    (if (zerop less)
        1
        (axle-mint a less))))

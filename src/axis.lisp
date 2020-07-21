(defpackage #:urbit/axis
  (:use #:cl #:urbit/math)
  (:export #:decomposable #:tax #:cap #:mas #:peg #:for-axis #:compile-axis))

(in-package #:urbit/axis)

(declaim (optimize
           (speed 3)
           (space 1)
           (compilation-speed 0)
           (debug 0)
           (safety 0)))

(deftype decomposable () '(integer 2))

; is tail the first path element of a
(defun tax (a)
  (declare (decomposable a))
  (the boolean (logbitp (- (integer-length a) 2) a)))

; there is an actual hoon function with this name, and this is how it returns.
(defun cap (a)
  (declare (decomposable a))
  (if (tax a) 3 2))

; remove the first path element from axis a
(defun mas (a)
  (declare (decomposable a))
  (the uint
       (let ((len (- (integer-length a) 2)))
         (logxor (ash 1 len) (end 0 len a)))))

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
(defun compile-axis (axis subject head tail)
  (declare (pint axis)
           (symbol subject head tail))
  (let ((s subject))
    (for-axis (in-tail axis)
      (setq s (list (if in-tail tail head) s)))
    s))

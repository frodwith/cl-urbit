(defpackage #:urbit/mug
  (:use #:cl #:urbit/data #:urbit/math #:urbit/dostack #:murmurhash)
  (:export #:mug #:murmug #:murmugs))

(in-package #:urbit/mug)

(deftype u32 () '(unsigned-byte 32))

(defun raw (int seed)
  (declare (uint int seed))
  (let ((*hash-size* 32))
    (the u32 (murmurhash int :seed seed))))

(deftype mug () '(unsigned-byte 31))

(defun murmug (a)
  "hash a common lisp integer"
  (declare (uint a))
  (the mug
       (loop for syd upfrom #xcafebabe
             for haz of-type u32 = (raw a syd)
             for ham of-type mug = (mix (rsh 0 31 haz) (end 0 31 haz))
             unless (zerop ham) return ham)))

(defun murmugs (a b)
  "compute a single mug from two smaller mugs (i.e. for cells)"
  (declare (mug a b))
  (the mug (murmug (mix a (mix #x7fffffff b)))))

(defun mug-atom (a)
  (let ((m (murmug (cl-integer a))))
    (setf (cached-mug a) m)
    m))

(defun mug-cell (cell)
  (dostack-accumulate
    (main (more (cons t cell))
          (give (head cell)))
    (give (n)
      (let ((c (cached-mug n)))
        (when c (take c))
        (unless (deep n) (take (mug-atom n)))
        (more (cons t n))
        (give (head n))))
    (take (r top)
      (if (car top)
          (let ((c (cdr top)))
            (setf (car top) nil)
            (setf (cdr top) (cons c r))
            (give (tail c)))
          (destructuring-bind (c . h) (cdr top)
            (let ((m (murmugs h r)))
              (less)
              (setf (cached-mug c) m)
              (take m)))))))

(defun mug (n)
  "get the mug (lazy hash) for a noun"
  (or (cached-mug n)
      (if (deep n)
          (mug-cell n)
          (mug-atom n))))

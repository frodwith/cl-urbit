(defpackage #:urbit/mug
  (:use #:cl #:urbit/data #:urbit/math #:urbit/util #:murmurhash)
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
  (give-take
    (main (more (cons t cell))
          (give (head cell)))
    (give (n)
      (let ((c (cached-mug n)))
        (when c (take c))
        (unless (deep n) (take (murmug (cl-integer n))))
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

;(flet ((more (n)
;;             (setq frame (cons 0 n))
;;             (push frame stack) 
;;           (retn (m)
;;             (pop stack)
;;             (setq accum m)
;;             (setq frame (car stack))) 
;;      (do () ((null frame) accum)
;;          (ecase (car frame)
;;            (0 (let* ((n (cdr frame))
;;                      (c (cached-mug n)))
;;                 (if c
;;                     (retn c)
;;                     (if (not (deep n))
;;                         (retn (mug-atom n))
;;                         (progn
;;                           (setf (car frame) 1)
;;                           (more (head n)))))))
;;            (1 (let ((n (cdr frame)))
;;                 (setf (car frame) 2)
;;                 (setf (cdr frame) (cons n accum))
;;                 (more (tail n))))
;;            (2 (destructuring-bind (n . m) (cdr frame)
;;                 (let ((m (murmugs m accum)))
;;                   (setf (cached-mug n) m)
;;                   (retn m)))))) 

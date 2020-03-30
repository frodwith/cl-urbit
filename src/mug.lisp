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

; could also do this with explicit continuation passing, no?

;(defun sum-cell-cont (cell atomic fast slow)
;  (labels ((give (n k)
;             (multiple-value-bind (answer deep) (funcall fast n)
;               (if answer
;                   (funcall k answer)
;                   (if deep
;                       (give (head n) 
;                             (lambda (ha)
;                               (give (tail n)
;                                     (lambda (ta)
;                                       (funcall k (funcall slow n ha ta))))))
;                       (funcall k (funcall atomic n)))))))
;    (give (head cell)
;          (lambda (ha)
;            (give (tail cell)
;                  (lambda (ta)
;                    (funcall slow cell ha ta)))))))

; when efficiency matters, don't use higher order functions e.g. mapcar. atomic,
; fast, and slow can be symbols and sum-cell can be a macro. capture (for ideal,
; etc) can be accomplished with flet.

; actually fuck that, a good inliner makes this difference moot and it's clearer
; as functions. write inline declarations it's not inlining well.

; this labels usage (with give/take) is reasonable. the impl relies on TCO
; anyway.

; sum is pretty expressive. should so a similar thing i guess for cell=.
; fast returning multiple values is cool.  cool.

(defun mug (n)
  (flet ((atomic (atom)
           (let ((mug (murmug (cl-integer atom))))
             (setf (cached-mug atom) mug)
             mug))
         (fast (noun)
           (or (cached-mug noun)
               (values nil (deep noun))))
         (slow (cell head-mug tail-mug)
           (let ((mug (murmugs head-mug tail-mug)))
             (setf (cached-mug cell) mug)
             mug)))
    (sum-noun n #'atomic #'fast #'slow)))

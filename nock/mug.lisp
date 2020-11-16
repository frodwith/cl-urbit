(defpackage #:urbit/nock/mug
  (:use #:cl #:murmurhash #:urbit/intbytes
        #:urbit/nock/data #:urbit/nock/math #:urbit/nock/common)
  (:export #:mug #:murmug #:murmugs))

; although "mug" is a hoon concept, having a lazily cached noun hash
; is a deep assumption of the nock runtime.

(in-package #:urbit/nock/mug)

(deftype u32 () '(unsigned-byte 32))

(defun raw (int seed)
  (declare (uint int) (u32 seed))
  (let ((*hash-size* 32))
    (the u32 (murmurhash (int->bytes int (byte-length int)) :seed seed))))

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

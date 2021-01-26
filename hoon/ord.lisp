(defpackage #:urbit/hoon/ord
  (:use #:cl #:urbit/nock/data #:urbit/nock/mug #:urbit/nock/equality)
  (:export #:dor #:gor #:mor))

(in-package #:urbit/hoon/ord)

(defun dor (a b)
  (labels
    ((rec (a b)
       (or (same a b)
           (if (deep a)
               (when (deep b)
                 (let ((ha (head a))
                       (hb (head b)))
                   (if (same ha hb)
                       (rec (tail a) (tail b))
                       (rec ha hb))))
               (or (deep b)
                   (< (cl-integer a) (cl-integer b)))))))
    (rec a b)))

(defun gor (a b)
  (let ((c (mug a))
        (d (mug b)))
    (if (= c d)
        (dor a b)
        (< c d))))

(defun mor (a b)
  (let ((c (murmug (mug a)))
        (d (murmug (mug b))))
    (if (= c d)
        (dor a b)
        (< c d))))

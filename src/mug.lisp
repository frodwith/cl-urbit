(defpackage #:urbit/mug
  (:use :cl)
  (:import-from :murmurhash :murmurhash)
  (:import-from :urbit/error :oops)
  (:import-from :urbit/noun :sum)
  (:import-from :urbit/atom :atomp)
  (:import-from :urbit/math :mix :end :rsh))

(in-package :urbit/mug)

(defgeneric cached-mug (a))
(defmethod cached-mug ((a t))
  nil)

(defgeneric compute-mug (a))
(defmethod compute-mug ((a t))
  (error 'oops))

(defgeneric learn-mug (a m))
(defmethod learn-mug ((a t) (m fixnum))
  nil)

(defun mug (a)
  (or (cached-mug a) (compute-mug a)))

(defun murmug (key)
  (loop for syd upfrom #xcafebabe
        for haz = (murmurhash key :seed syd)
        for ham = (mix (rsh 0 31 haz) (end 0 31 haz))
        unless (zerop ham) return ham))

(defun murmug-two (a b)
  (murmug (mix a (mix #x7fffffff b))))

(defun mug-cell-fast (a)
  (or (cached-mug a) (and (atomp a) (compute-mug a))))

(defun mug-cell (a)
  (sum a #'mug-cell-fast #'murmug-two))

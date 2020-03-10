(in-package #:urbit/mug)

(deftype mug () '(unsigned-byte 31))

(defnoun-meta mug)

; forcing to single value context (working around values too complex to check)
(defmacro one (&body body)
  `(nth-value 0 ,@body))

(defun murmug (key)
  (declare (type integer key))
  (the mug (loop for syd upfrom #xcafebabe
                 for haz = (murmurhash key :seed syd)
                 for ham = (mix (rsh 0 31 haz) (end 0 31 haz))
                 unless (zerop ham) return ham)))

(defun murmug-two (a b)
  (declare (type mug a b))
  (the mug (murmug (mix a (mix #x7fffffff b)))))

(defun mug-cell-fast (a)
  (declare (type noun a))
  (the (or null mug)
       (one (or (cached-mug a) (and (atomp a) (compute-mug a))))))

(defun mug-cell (a)
  (declare (type cell a))
  (the mug (one (sum a #'mug-cell-fast #'murmug-two))))

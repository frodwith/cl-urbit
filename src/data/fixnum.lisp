(defpackage cl-urbit/noun/fixnum
 (:use :cl)
 (:import-from :cl-urbit/mug :murmug)
 (:import-from :cl-urbit/noun :atomp :atom= :to-integer :compute-mug))

(defmethod atomp ((a fixnum))
 t)

(defmethod to-integer ((a fixnum))
 a)

(defmethod compute-mug ((a fixnum))
 (murmug a))

(defmethod atom= ((a fixnum) (b fixnum))
 (= a b))

; fixnums are only -ever- equal to other fixnums. other representations are
; verboten by this logic.
(defmethod atom= ((a fixnum) (b t))
 nil)

(defmethod atom= ((a t) (b fixnum))
 nil)

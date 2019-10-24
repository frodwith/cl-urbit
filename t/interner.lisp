(defpackage urbit/tests/interner
  (:use :cl :prove :urbit/tests/util :urbit/interner)
  (:import-from :urbit/noun :noun)
  (:import-from :urbit/cell :tail))

(in-package :urbit/tests/interner)

(defparameter *interner* (make-noun-interner))

(defun in (&rest args)
  (intern-noun *interner* (noun args)))

(plan 7)

(is-eq 1 (in 1))
(is-eq 42 (in 42))
(is-eq most-positive-fixnum (in most-positive-fixnum)) 
(is-eq (in #xdeadbeefcafebabefeefeefee)
       (in #xdeadbeefcafebabefeefeefee))
(is-eq (in 1 2) (in 1 2))
(is-eq (in 1 2 3 4 5) (in 1 2 3 4 5))
(is-eq (tail (in 1 2 3)) (in 2 3))

(finalize)

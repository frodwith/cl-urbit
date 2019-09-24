(defpackage urbit/tests/interner
  (:use :cl :prove :urbit/tests/util)
  (:import-from :urbit/noun :noun)
  (:import-from :urbit/cell :tail)
  (:import-from :urbit/context :make-context :with-context :intern-noun))

(in-package :urbit/tests/interner)

(defun in (&rest args)
  (intern-noun (noun args)))

(plan 7)

(with-context (make-context)
  (is-eq 1 (in 1))
  (is-eq 42 (in 42))
  (is-eq most-positive-fixnum (in most-positive-fixnum)) 
  (is-eq (in #xdeadbeefcafebabefeefeefee)
         (in #xdeadbeefcafebabefeefeefee))
  (is-eq (in 1 2) (in 1 2))
  (is-eq (in 1 2 3 4 5) (in 1 2 3 4 5))
  (is-eq (tail (in 1 2 3)) (in 2 3)))

(finalize)

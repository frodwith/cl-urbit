(defpackage urbit/tests/interner
  (:use :cl :rove)
  (:import-from :urbit/noun :noun)
  (:import-from :urbit/cell :tail)
  (:import-from :urbit/context :make-context :with-context :intern-noun))

(in-package :urbit/tests/interner)

(defun in (&rest args)
 (intern-noun (noun args)))

(deftest interner
 (with-context (make-context)
  (testing "fixnums"
   (ok (eq 1 (in 1)))
   (ok (eq 42 (in 42)))
   (ok (eq most-positive-fixnum (in most-positive-fixnum))))
  (testing "bigatoms"
   (ok (eq (in #xdeadbeefcafebabefeefeefee)
           (in #xdeadbeefcafebabefeefeefee))))
  (testing "cells"
   (ok (eq (in 1 2) (in 1 2))))
   (ok (eq (in 1 2 3 4 5) (in 1 2 3 4 5)))
   (ok (eq (tail (in 1 2 3)) (in 2 3)))))

(run-suite *package*)

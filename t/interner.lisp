(defpackage urbit/tests/interner
  (:use :cl :rove)
  (:import-from :urbit/context :make-context :with-context :intern-noun))

(in-package :urbit/tests/interner)

(deftest interner
 (with-context (make-context)
  (testing "fixnums"
   (ok (eq 1 (intern-noun 1)))
   (ok (eq 42 (intern-noun 42)))
   (ok (eq most-positive-fixnum (intern-noun most-positive-fixnum))))))

(run-suite *package*)

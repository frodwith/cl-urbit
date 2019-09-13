(defpackage urbit/tests/pairset
 (:use :cl :rove)
 (:import-from :urbit/noun :noun)
 (:import-from :urbit/pairset :make-pairset :has-pair :have-pair))

(in-package :urbit/tests/pairset)

(deftest pairset
 (let ((s (make-pairset)))
  (let ((a (noun 1 1))
        (b (noun 1 1)))
   (testing "zero"
    (ng (has-pair s a a))
    (ng (has-pair s b b))
    (ng (has-pair s b a))
    (ng (has-pair s a b)))

   (have-pair s a b)
   (testing "one"
    (ng (has-pair s a a))
    (ng (has-pair s b b))
    (ng (has-pair s b a))
    (ok (has-pair s a b)))

   (have-pair s b a)
   (testing "two"
    (ng (has-pair s a a))
    (ng (has-pair s b b))
    (ok (has-pair s b a))
    (ok (has-pair s a b)))

   (have-pair s a a)
   (testing "three"
    (ok (has-pair s a a))
    (ng (has-pair s b b))
    (ok (has-pair s b a))
    (ok (has-pair s a b)))

   (have-pair s b b)
   (testing "four"
    (ok (has-pair s a a))
    (ok (has-pair s b b))
    (ok (has-pair s b a))
    (ok (has-pair s a b))))))

(run-suite *package*)

(defpackage cl-urbit/tests/noun
  (:use :cl :rove :cl-urbit/noun)
  (:import-from :cl-urbit/error :exit))

(in-package :cl-urbit/tests/noun)

(deftest noun
 (testing "integers"
   (ok (= 1 (noun 1))))
 (testing "pair list"
  (let ((n (noun 1 2)))
   (ok (= 1 (head n)))
   (ok (= 2 (tail n)))))
 (testing "three list"
  (let* ((n (noun 1 2 3))
         (m (tail n)))
   (ok (= 1 (head n)))
   (ok (= 2 (head m)))
   (ok (= 3 (tail m)))))
 (testing "sub-list"
  (let* ((n (noun '(1 (2 3 4) 5)))
         (m (tail n))
         (o (head m))
         (p (tail o)))
   (ok (= 1 (head n)))
   (ok (= 2 (head o)))
   (ok (= 3 (head p)))
   (ok (= 4 (tail p)))
   (ok (= 5 (tail m))))))

(deftest cells
 (testing "head"
  (signals (head 1) 'exit)
  (ok (= 42 (head (noun 42 0)))))
 (testing "tail"
  (signals (tail 1) 'exit)
  (ok (= 42 (tail (noun 0 42))))))

(deftest atomp
 (testing "cells"
  (ng (atomp (noun 1 2))))
 (testing "atoms"
  (ok (atomp (noun 1)))))

(deftest cellp
 (testing "cells"
  (ok (cellp (noun 1 2))))
 (testing "atoms"
  (ng (cellp (noun 1)))))

(deftest mug
 (testing "atoms"
  (ok (= 1681410502 (mug (noun 42))))
  (ok (= 2046756072 (mug (noun 0))))
  (ok (= 1553444423 (mug (noun #xdeadbeefcafebabedeed)))))
 (testing "cells"
  (ok (= 1392748553 (mug (noun 0 42))))
  (ok (= 436876331 (mug (noun '(1 2 (3 4 (5 6 7) 8 9 10) 11 12)))))))

(run-suite *package*)

(defpackage cl-urbit-worker/tests/noun
  (:use :cl :rove :cl-urbit-worker/noun))

(in-package :cl-urbit-worker/tests/noun)

(deftest noun-test
 (testing "integers"
  (let ((n (noun 1)))
   (ok (= 1 n))
   (ok (atomp n))
   (ng (cellp n))))
 (testing "pair list"
  (let ((n (noun 1 2)))
   (ok (cellp n))
   (ng (atomp n))
   (ok (= 1 (head n)))
   (ok (= 2 (tail n)))))
 (testing "three list"
  (let* ((n (noun 1 2 3))
         (m (tail n)))
   (ok (cellp n))
   (ok (cellp m))
   (ok (= 1 (head n)))
   (ok (= 2 (head m)))
   (ok (= 3 (tail m)))))
 (testing "sub-list"
  (let* ((n (noun 1 '(2 3 4) 5))
         (m (tail n))
         (o (head m))
         (p (tail o)))
   (ok (= 1 (head n)))
   (ok (= 2 (head o)))
   (ok (= 3 (head p)))
   (ok (= 4 (tail p)))
   (ok (= 5 (tail m))))))

(run-suite *package*)

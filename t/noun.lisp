(defpackage urbit/tests/noun
  (:use :cl :rove)
  (:import-from :urbit/mug :mug)
  (:import-from :urbit/noun :noun)
  (:import-from :urbit/atom :atomp :to-integer)
  (:import-from :urbit/error :exit)
  (:import-from :urbit/cell :head :tail :cellp)
  (:import-from :urbit/equality :same))

(in-package :urbit/tests/noun)

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
 (testing "fixnums"
  (ok (atomp (noun 1))))
 (testing "bignums"
  (ok (atomp (noun #xdeadbeefcafebabedeed)))))

(deftest cellp
 (testing "cells"
  (ok (cellp (noun 1 2))))
 (testing "fixnums"
  (ng (cellp (noun 1))))
 (testing "bignums"
  (ng (cellp (noun #xdeadbeefcafebabedeed)))))

(deftest cellp
 (testing "cells"
  (ok (cellp (noun 1 2))))
 (testing "ms"
  (ng (cellp (noun 1)))))

(deftest mug
 (testing "atoms"
  (ok (= 1681410502 (mug (noun 42))))
  (ok (= 2046756072 (mug (noun 0))))
  (ok (= 1553444423 (mug (noun #xdeadbeefcafebabedeed)))))
 (testing "cells"
  (ok (= 1392748553 (mug (noun 0 42))))
  (ok (= 436876331 (mug (noun '(1 2 (3 4 (5 6 7) 8 9 10) 11 12)))))))

(deftest equality
 (testing "fixnums"
  (ok (same (noun 1) (noun 1)))
  (ng (same (noun 1) (noun 2))))
 (testing "bigatoms"
  (let ((a (noun #xdeadbeefcafebabedeed))
        (b (noun (parse-integer "deadbeefcafebabedeed" :radix 16))))
   (ng (eq (to-integer a) (to-integer b)))
   (ok (same a b))
   (ok (eq (to-integer a) (to-integer b)))
   (ng (same a (noun #xdeedbabecafebeefdead)))))
 (testing "cells"
  (let ((a (noun '((1 2) 3 4)))
        (b (noun '((1 2) 3 4)))
        (c (noun '((3 4) 1 2))))
   (ng (same a c))
   (ng (eq (head a) (head b)))
   (ng (eq (tail a) (tail b)))
   (ok (same a b))
   (ok (eq (head a) (head b)))
   (ok (eq (tail a) (tail b))))))

;; XX TODO: test pairset independently
;; do equality on a really big noun, manually test that it takes forever without
;; the escape hatch

(run-suite *package*)

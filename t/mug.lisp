(defpackage #:urbit/tests/mug
  (:use #:cl #:fiveam #:urbit/tests #:urbit/syntax #:urbit/mug))

(in-package #:urbit/tests/mug)

(def-suite mug-tests
           :description "test the lazy hash function, mug"
           :in all-tests)

(in-suite mug-tests)

(enable-brackets)

(test murmug-test
  (is (= 2046756072 (murmug 0)))
  (is (= 1174754992 (murmug 10)))
  (is (= 2140226851 (murmug #xdeadbeef))))

(test murmugs-test
  (is (= 1392748553 (murmugs (murmug 0) (murmug 42)))))

(test mug
  (is (= 1681410502 (mug 42)))
  (is (= 2046756072 (mug 0)))
  (is (= 1553444423 (mug #xdeadbeefcafebabedeed)))
  (is (= 1392748553 (mug [0 42])))
  (is (= 436876331 (mug [1 2 [3 4 [5 6 7] 8 9 10] 11 12]))))

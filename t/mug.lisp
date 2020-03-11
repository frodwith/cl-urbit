(defpackage #:urbit/tests/mug
  (:use #:cl #:fiveam #:urbit/tests #:urbit/mug))

(in-package #:urbit/tests/mug)

(def-suite mug-tests
           :description "test the lazy hash function, mug"
           :in all-tests)

(in-suite mug-tests)

(test murmug-test
  (is (= 2046756072 (murmug 0)))
  (is (= 1174754992 (murmug 10)))
  (is (= 2140226851 (murmug #xdeadbeef))))

(test murmugs-test
  (is (= 1392748553 (murmugs (murmug 0) (murmug 42)))))

; TODO - once there is at least one implementation of the data protocol,
;        test the mug function

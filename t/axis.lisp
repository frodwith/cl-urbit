(defpackage #:urbit/tests/axis
  (:use #:cl #:fiveam #:urbit/tests #:urbit/axis))

(in-package #:urbit/tests/axis)

(def-suite axis-tests
           :description "test the functions in urbit/axis"
           :in all-tests)

(in-suite axis-tests)

(test mas-test
  (is (= 1 (mas 2)))
  (is (= 1 (mas 3)))
  (is (= 2 (mas 4)))
  (is (= 3 (mas 5)))
  (is (= 2 (mas 6)))
  (is (= 3 (mas 7)))
  (is (= 4 (mas 8))))

(test tax-test
  (is-false (tax 2))
  (is-true (tax 3))
  (is-false (tax 4))
  (is-false (tax 5))
  (is-true (tax 6))
  (is-true (tax 7))
  (is-false (tax 8)))

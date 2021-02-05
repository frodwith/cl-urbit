(defpackage #:urbit/tests/axis
  (:use #:cl #:fiveam #:urbit/tests #:urbit/nock/axis))

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

(test axis-nth
  (is-false (axis-nth 0 2))
  (is-true  (axis-nth 0 3))
  (is-false (axis-nth 0 4))
  (is-false (axis-nth 0 5))
  (is-true  (axis-nth 1 5))
  (is-false (axis-nth 1 4))
  (is-false (axis-nth 1 6))
  (is-true  (axis-nth 0 6)))

(test subaxis-p-test
  (is-true  (subaxis-p 1 1))
  (is-true  (subaxis-p 2 2))
  (is-true  (subaxis-p 3 3))
  (is-true  (subaxis-p 1 2))
  (is-true  (subaxis-p 1 3))
  (is-true  (subaxis-p 1 4))
  (is-true  (subaxis-p 1 5))
  (is-true  (subaxis-p 2 2))
  (is-true  (subaxis-p 2 4))
  (is-true  (subaxis-p 2 5))
  (is-true  (subaxis-p 2 8))
  (is-false (subaxis-p 3 2))
  (is-false (subaxis-p 3 4))
  (is-false (subaxis-p 3 5))
  (is-false (subaxis-p 3 8))
  (is-false (subaxis-p 2 3))
  (is-false (subaxis-p 3 2))
  (is-false (subaxis-p 4 6))
  (is-false (subaxis-p 6 4)))

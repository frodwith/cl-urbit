(defpackage #:urbit/tests
  (:use #:cl #:fiveam #:urbit/control)
  (:export #:test-urbit #:all-tests))

(in-package #:urbit/tests)

(def-suite all-tests
           :description "all of cl-urbit's tests")

(in-suite all-tests)

(defun test-urbit ()
  (run! 'all-tests))

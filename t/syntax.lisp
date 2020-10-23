(defpackage #:urbit/tests/syntax
  (:use #:cl #:fiveam #:urbit/tests #:urbit/hoon/syntax #:named-readtables))

(in-package #:urbit/tests/syntax)

(def-suite syntax-tests
           :description "test urbit reader augmentations"
           :in all-tests)

(in-suite syntax-tests)

(enable-brackets)

(test edges
  (let ((*readtable* (find-readtable 'brackets)))
    (signals arity-error (read-from-string "[]"))
    (signals arity-error (read-from-string "[1]"))))

(test shallow-test
  (is (equal '(1 2 . 3) [1 2 3]))
  (is (equal '(40 2 . 42) [40 2 (+ 40 2)])))

(test deep-test
  (is (equal '(1 2 (3 4 . 5) . 6)
             (let ((a 5))
               [1 2 [3 (+ 2 2) a] 6]))))

(enable-cords)

(test cord-test
  (is (= 1953718630 %fast))
  (is (= 97 %a))
  (let ((*readtable* (find-readtable 'cords)))
    (signals cord-error (read-from-string "%\"\""))))

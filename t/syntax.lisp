(defpackage #:urbit/tests/syntax
  (:use #:cl #:fiveam #:named-readtables #:urbit/tests
        #:urbit/nock/cord #:urbit/nock/data/slimcell
        #:urbit/nock/equality #:urbit/hoon/syntax))

(in-package #:urbit/tests/syntax)

(def-suite syntax-tests
           :description "test urbit reader augmentations"
           :in all-tests)

(in-suite syntax-tests)
(in-readtable hoon)

(test edges
  (is (null []))
  (is (= 1 [1])))

(test shallow-test
  (is (same '(1 2 . 3) [1 2 3]))
  (is (same '(40 2 . 42) [40 2 (+ 40 2)])))

(test deep-test
  (is (same '(1 2 (3 4 . 5) . 6)
             (let ((a 5))
               [1 2 [3 (+ 2 2) a] 6]))))

(test cord-test
  (is (= 1953718630 %fast))
  (is (= 97 %a))
  (let ((*readtable* (find-readtable 'cord-readtable)))
    (signals error (read-from-string "%\"\""))))

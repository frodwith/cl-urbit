(defpackage #:urbit/tests/ideal
  (:use #:cl #:fiveam #:named-readtables
        #:urbit/tests #:urbit/hoon/syntax #:urbit/nock/data
        #:urbit/nock/ideal #:urbit/nock/world)
  (:shadowing-import-from #:fiveam #:!!))

(in-package #:urbit/tests/ideal)
(in-readtable hoon)

(def-suite ideal-tests
           :description "test the world/ideal system"
           :in all-tests)

(in-suite ideal-tests)

(test uniqueness
  (bottle
    (let* ((a [1 2 [[3 4 5] 6] 7 8])
           (b [1 2 [[3 4 5] 6] 7 8])
           (c [1 2 [[4 5] 6] 7 8]))
      (is (not (eq a b)))
      (let ((ai (find-ideal a))
            (bi (find-ideal b))
            (ci (find-ideal c)))
        (is (eq ai bi))
        (is (not (eq ai ci)))
        (is (typep ai 'icell))
        (is (eq ai (cached-ideal a)))
        (is (eq ai (cached-ideal b)))
        (is (eq (icell-head ai) (head a)))
        (is (eq (icell-tail ai) (tail b)))))))

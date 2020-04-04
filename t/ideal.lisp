(defpackage #:urbit/tests/ideal
  (:use #:cl #:fiveam #:urbit/tests #:urbit/syntax #:urbit/data #:urbit/ideal))

(in-package #:urbit/tests/ideal)

(def-suite ideal-tests
           :description "test the world/ideal system"
           :in all-tests)

(in-suite ideal-tests)

(enable-brackets)

(test uniqueness
  (let* ((w (make-world))
         (a [1 2 [[3 4 5] 6] 7 8])
         (b (copy-tree a)))
    (is (not (eq a b)))
    (let ((ai (find-ideal w a))
          (bi (find-ideal w b)))
      (is (eq ai bi))
      (is (typep ai 'icell))
      (is (eq ai (cached-ideal a)))
      (is (eq ai (cached-ideal b)))
      (is (eq (icell-head ai) (head a)))
      (is (eq (icell-tail ai) (tail b))))))

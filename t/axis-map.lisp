(defpackage #:urbit/tests/axis-map
  (:use :cl :prove :urbit/tests/util :urbit/axis-map))

(in-package :urbit/tests/axis-map)

(plan 5)

(let ((m nil))
  (subtest "empty"
    (no (lookup m 8))
    (no (value (left (left (left m)))))
    (no (lookup m 4))
    (no (value (left (left m))))
    (no (lookup m 6))
    (no (value (left (right m)))))

  (subtest "foo"
    (setf m (insert m 8 'foo))
    (is (lookup m 8) 'foo)
    (is (value (left (left (left m)))) 'foo)
    (no (lookup m 4))
    (no (value (left (left m))))
    (no (lookup m 6))
    (no (value (left (right m)))))

  (subtest "baz"
    (setf m (insert m 8 'baz))
    (is (lookup m 8) 'baz)
    (is (value (left (left (left m)))) 'baz)
    (no (lookup m 4))
    (no (value (left (left m))))
    (no (lookup m 6))
    (no (value (left (right m)))))

  (subtest "bar"
    (setf m (insert m 4 'bar))
    (is (lookup m 8) 'baz)
    (is (value (left (left (left m)))) 'baz)
    (is (lookup m 4) 'bar)
    (is (value (left (left m))) 'bar)
    (no (lookup m 6))
    (no (value (left (right m))))) 

  (subtest "all"
    (setf m (insert m 6 'foo))
    (is (lookup m 8) 'baz)
    (is (value (left (left (left m)))) 'baz)
    (is (lookup m 4) 'bar)
    (is (value (left (left m))) 'bar)
    (is (lookup m 6) 'foo)
    (is (value (left (right m))) 'foo)))

(finalize)

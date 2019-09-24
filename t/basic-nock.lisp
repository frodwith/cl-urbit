(defpackage urbit/tests/basic-nock
  (:use :cl :prove :urbit/tests/util)
  (:import-from urbit/noun :noun)
  (:import-from urbit/cell :head :tail)
  (:import-from urbit/formula :nock)
  (:import-from urbit/context :with-context :make-context))

(in-package :urbit/tests/basic-nock)

(plan 1)

(with-context (make-context)
  (subtest "flip"
    (let ((r (nock (noun 0 42) (noun '(0 3) 0 2))))
      (ok (= 42 (head r)))
      (ok (= 0 (tail r))))))

(finalize)

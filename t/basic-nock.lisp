(defpackage urbit/tests/basic-nock
  (:use :cl :prove :urbit/syntax :urbit/tests/util)
  (:import-from urbit/noun :noun)
  (:import-from urbit/cell :head :tail)
  (:import-from urbit/formula :nock)
  (:import-from urbit/context :with-context :make-context))

(in-package :urbit/tests/basic-nock)
(enable-brackets)

(plan 1)

(with-context (make-context)
  (subtest "flip"
    (let ((r (nock [0 42] [[0 3] 0 2])))
      (is= 42 (head r))
      (is= 0 (tail r)))))

(finalize)

(defpackage urbit/tests/basic-nock
  (:use :cl :rove)
  (:import-from urbit/noun :noun)
  (:import-from urbit/cell :head :tail)
  (:import-from urbit/formula :nock)
  (:import-from urbit/context :with-context :make-context))

(in-package :urbit/tests/basic-nock)

(deftest autocons
 (testing "flip"
  (with-context (make-context)
                              ; this is ugly fix with macro
   (let ((r (nock (noun 0 42) (noun '((0 3) 0 2)))))
    (ok (= 42 (head r)))
    (ok (= 0 (tail r)))))))

(run-suite *package*)

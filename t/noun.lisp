(defpackage cl-urbit-worker/tests/noun
  (:use :cl)
  (:import-from :cl-urbit-worker/noun :noun)
  (:import-from :rove :ok))

(in-package :urbit-serf/tests/nock)

(deftest noun-test
 (testing "noun function"
  (ok (= 1 (noun 1)))
  (ok (atomp (noun 1)))
  (ng (cellp (noun 1)))

  (ok (cellp (noun 1 2)))
  (ng (atomp (noun 1 2)))
  (ok (= 1 (head (noun 1 2))))
  (ok (= 2 (tail (noun 1 2))))

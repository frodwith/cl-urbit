(defpackage cl-urbit-worker/tests/basic-nock
  (:use :cl)
  (:import-from :cl-urbit-worker/noun :noun)
  (:import-from :cl-urbit-worker/nock :nock)
  (:import-from :rove :ok))

(in-package :urbit-serf/tests/nock)

(deftest test-1
 (testing "nock 1 operator"
  (ok (nock 0 (noun 1 0)))))

(defpackage cl-urbit/tests/basic-nock
  (:use :cl)
  (:import-from :cl-urbit/noun :noun)
  (:import-from :cl-urbit/nock :nock)
  (:import-from :rove :ok))

(in-package :cl-urbit/tests/basic-nock)

(deftest test-1
 (testing "nock 1 operator"
  (ok (nock 0 (noun 1 0)))))

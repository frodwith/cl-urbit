(defpackage #:urbit/tests/math
  (:use #:cl #:fiveam #:urbit/tests #:urbit/math))

(in-package #:urbit/tests/math)

(def-suite math-tests
           :description "test the primitive math in urbit/math"
           :in all-tests)

(in-suite math-tests)

(test met-test
  (is (= 4 (met 0 8)))
  (is (= 9 (met 0 256)))
  (is (= 1 (met 3 8)))
  (is (= 1 (met 3 #xff)))
  (is (= 2 (met 3 #xffff)))
  (is (= 3 (met 3 #x10000))))

(test mix-test
  (is (= #b101 (mix #b100 #b001)))
  (is (= #b00010110011000 (mix #b10010011001001 #b10000101010001))))

(test end-test
  (is (= #b0110 (end 0 4 #b1110110)))
  (is (= #b10 (end 1 1 #b1110110)))
  (is (= #b00101001 (end 3 1 #b11101101101111000101001)))
  (is (= #b1101111000101001 (end 3 2 #b11101101101111000101001))))

(test lsh-test
  (is (= 2 (lsh 0 1 1)))
  (is (= 4 (lsh 0 2 1)))
  (is (= 8 (lsh 0 3 1)))
  (is (= #x200 (lsh 3 1 2)))
  (is (= #x80000 (lsh 3 2 8)))
  (is (= #xa000000 (lsh 3 3 10))))

(test rsh-test
  (is (= 1 (rsh 0 1 2)))
  (is (= 1 (rsh 0 2 4)))
  (is (= 1 (rsh 0 3 8)))
  (is (= 2 (rsh 3 1 #x200)))
  (is (= 8 (rsh 3 2 #x80000)))
  (is (= 10 (rsh 3 3 #xa000000)))
  (is (= 10 (rsh 3 3 #xa111111))))


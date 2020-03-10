(defpackage #:urbit/tests/math
  (:use #:cl #:prove #:urbit/math))

(in-package #:urbit/tests/math)

(subtest "met"
  (is (met 0 8) 4)
  (is (met 0 256) 9)
  (is (met 3 8) 1)
  (is (met 3 #xff) 1)
  (is (met 3 #xffff) 2)
  (is (met 3 #x10000) 3))

(subtest "mix"
  (is (mix #b100
           #b001)
           #b101)
  (is (mix #b10010011001001
           #b10000101010001)
           #b00010110011000))

(subtest "end"
  (is (end 0 4 #b1110110) #b0110)
  (is (end 1 1 #b1110110) #b10)
  (is (end 3 1 #b11101101101111000101001) #b00101001)
  (is (end 3 2 #b11101101101111000101001) #b1101111000101001))

(subtest "lsh"
  (is (lsh 0 1 1) 2)
  (is (lsh 0 2 1) 4)
  (is (lsh 0 3 1) 8)
  (is (lsh 3 1 2) #x200)
  (is (lsh 3 2 8) #x80000)
  (is (lsh 3 3 10) #xa000000))

(subtest "rsh"
  (is (rsh 0 1 2) 1)
  (is (rsh 0 2 4) 1)
  (is (rsh 0 3 8) 1)
  (is (rsh 3 1 #x200) 2)
  (is (rsh 3 2 #x80000) 8)
  (is (rsh 3 3 #xa000000) 10)
  (is (rsh 3 3 #xa111111) 10))

(subtest "mas"
  (is (mas 2) 1)
  (is (mas 3) 1)
  (is (mas 4) 2)
  (is (mas 5) 3)
  (is (mas 6) 2)
  (is (mas 7) 3)
  (is (mas 8) 4))

(subtest "tax"
  (ok (not (tax 2)))
  (ok (tax 3))
  (ok (not (tax 4)))
  (ok (not (tax 5)))
  (ok (tax 6))
  (ok (tax 7))
  (ok (not (tax 8))))

(subtest "pax"
  (is (pax 2) '(nil))
  (is (pax 3) '(t))
  (is (pax 4) '(nil nil))
  (is (pax 5) '(nil t))
  (is (pax 6) '(t nil))
  (is (pax 7) '(t t))
  (is (pax 8) '(nil nil nil)))

(subtest "mug"
  (is (murmug 0) 2046756072)
  (is (murmug 10) 1174754992)
  (is (murmug #xdeadbeef) 2140226851)
  (is (murmugs (murmug 0) (murmug 42)) 1392748553))

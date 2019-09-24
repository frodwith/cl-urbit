(defpackage urbit/tests/noun
  (:use :cl :prove :urbit/tests/util)
  (:import-from :urbit/mug :mug)
  (:import-from :urbit/noun :noun)
  (:import-from :urbit/atom :atomp :to-integer)
  (:import-from :urbit/error :exit)
  (:import-from :urbit/cell :head :tail :cellp)
  (:import-from :urbit/equality :same))

(in-package :urbit/tests/noun)

(defun many (n item)
  (loop for x = item then (noun x x)
        repeat n
        finally (return x)))

(defun levels (seed n f)
  (loop for v = seed then (funcall f v)
        repeat n
        finally (return v)))

(defun bigdag ()
  (levels 0 50 (lambda (xs) (many 100 xs))))

(plan 6)

(subtest "noun"
  (subtest "integers"
    (is= 1 (noun 1)))
  (subtest "pair list"
    (let ((n (noun 1 2)))
      (is= 1 (head n))
      (is= 2 (tail n))))
  (subtest "three list"
    (let* ((n (noun 1 2 3))
           (m (tail n)))
      (is= 1 (head n))
      (is= 2 (head m))
      (is= 3 (tail m))))
  (subtest "sub-list"
    (let* ((n (noun '(1 (2 3 4) 5)))
           (m (tail n))
           (o (head m))
           (p (tail o)))
      (is= 1 (head n))
      (is= 2 (head o))
      (is= 3 (head p))
      (is= 4 (tail p))
      (is= 5 (tail m)))))

(subtest "cells"
  (subtest "head"
    (is-error (head 1) 'exit)
    (is= 42 (head (noun 42 0))))
  (subtest "tail"
    (is-error (tail 1) 'exit)
    (is= 42 (tail (noun 0 42)))))

(subtest "atomp"
  (no (atomp (noun 1 2)) "cells")
  (ok (atomp (noun 1)) "fixnums")
  (ok (atomp (noun #xdeadbeefcafebabedeed)) "bignums"))

(subtest "cellp"
  (ok (cellp (noun 1 2)) "cells")
  (no (cellp (noun 1)) "fixnums")
  (no (cellp (noun #xdeadbeefcafebabedeed))))

(subtest "mug"
 (subtest "atoms"
  (is= 1681410502 (mug (noun 42)))
  (is= 2046756072 (mug (noun 0)))
  (is= 1553444423 (mug (noun #xdeadbeefcafebabedeed))))
 (subtest "cells"
  (is= 1392748553 (mug (noun 0 42)))
  (is= 436876331 (mug (noun '(1 2 (3 4 (5 6 7) 8 9 10) 11 12))))))

(subtest "equality"
  (subtest "fixnums"
    (is-same (noun 1) (noun 1))
    (isnt-same (noun 1) (noun 2)))
  (subtest "bigatoms"
    (let ((a (noun #xdeadbeefcafebabedeed))
          (b (noun (parse-integer "deadbeefcafebabedeed" :radix 16))))
      (isnt-eq (to-integer a) (to-integer b))
      (is-same a b)
      (is-same (to-integer a) (to-integer b))
      (isnt-same a (noun #xdeedbabecafebeefdead))))
  (subtest "cells"
    (let ((a (noun '((1 2) 3 4)))
          (b (noun '((1 2) 3 4)))
          (c (noun '((3 4) 1 2))))
      (isnt-same a c)
      (isnt-eq (head a) (head b))
      (isnt-eq (tail a) (tail b))
      (is-same a b)
      (is-eq (head a) (head b))
      (is-eq (tail a) (tail b))))
  (let ((a (bigdag))
        (b (bigdag)))
    (is-same a b "bigdags")))

(finalize)

(defpackage #:urbit/tests/zig
  (:use #:cl #:fiveam #:urbit/tests #:urbit/nock/zig))

(in-package #:urbit/tests/zig)

(def-suite zig-tests
           :description "test functions on zigs (axis-as-bitvector)"
           :in all-tests)

(in-suite zig-tests)

(test axis->zig
  (is (equal #* (axis->zig 1)))
  (is (equal #*0 (axis->zig 2)))
  (is (equal #*1 (axis->zig 3)))
  (is (equal #*10 (axis->zig 6)))
  (is (equal #*000 (axis->zig 8)))
  (is (equal #*110 (axis->zig 14))))

(test zig->axis
  (is (equal 1 (zig->axis #*)))
  (is (equal 2 (zig->axis #*0)))
  (is (equal 3 (zig->axis #*1)))
  (is (equal 6 (zig->axis #*10)))
  (is (equal 8 (zig->axis #*000)))
  (is (equal 14  (zig->axis #*110))))

(test inverse
  (for-all ((r (gen-integer :min 1)))
    (is (equal r (zig->axis (axis->zig r))))))

(test sub-examples
  (is (zig-sub-p (axis->zig 8) (axis->zig 8)))
  (is (zig-sub-p (axis->zig 7) (axis->zig 15)))
  (is (not (zig-sub-p (axis->zig 15) (axis->zig 7))))
  (is (not (zig-sub-p (axis->zig 7) (axis->zig 12)))))

(test random-sub
  (for-all ((r (gen-integer :min 4)))
    (let* ((z (axis->zig r))
           (less (subseq z 0 (- (length z) 2))))
      (is (zig-sub-p less z))
      (is (not (zig-sub-p z less))))))

(test common-examples
  (is (zig-common (axis->zig 6) (axis->zig 12)))
  (is (zig-common (axis->zig 12) (axis->zig 6)))
  (is (not (zig-common (axis->zig 4) (axis->zig 6))))
  (is (not (zig-common (axis->zig 4) (axis->zig 5)))))

(test random-common
  (for-all ((r (gen-integer :min 2)))
    (let* ((z (axis->zig r))
           (fork (copy-seq z)))
      (setf (bit fork 0) (if (zerop (bit fork 0)) 1 0))
      (is (not (zig-common z fork))))))

(defmacro zcf (a (part fail) &body forms)
  (let ((bus (gensym)))
    `(let ((,bus '((1 . 2) 3 4 . 5)))
       (multiple-value-bind (,part ,fail)
         ,(zig-compile-fail (axis->zig a) bus 'car 'cdr 'consp) 
         ,@forms))))

(test compile-fail
  (zcf 2 (p f)
    (is (equal '(1 . 2) p))
    (is (null f)))
  (zcf 3 (p f)
    (is (equal '(3 4 . 5) p))
    (is (null f)))
  (zcf 6 (p f)
    (is (= 3 p))
    (is (null f)))
  (zcf 12 (p f)
    (is (null p))
    (is (equal #*10 f)))
  (zcf 26 (p f)
    (is (null p))
    (is (equal #*10 f))))

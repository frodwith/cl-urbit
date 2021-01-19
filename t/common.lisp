(defpackage #:urbit/tests/common
  (:use #:cl #:fiveam #:named-readtables
        #:urbit/tests #:urbit/hoon/syntax #:urbit/nock/common #:urbit/nock/data))

(in-package #:urbit/tests/common)
(in-readtable hoon)

(def-suite denoun-tests
           :description "test noun destructuring macros"
           :in all-tests)

(in-suite denoun-tests)

(test basic
  (decons (a b c) '(1 2 . 3)
    (is (= 1 a))
    (is (= 2 b))
    (is (= 3 c)))
  (signals cell-required
    (decons (a b c) [1 2] 
      (list a b c))))

(test ignored
  (is (= 42 (decons nil '(1 2 . 3) 42)))
  (decons (a nil) '(40 . 2)
    (is (= 40 a))))

(test require-atom
  (decons (nil @b) '(1 . 2)
    (is (= 2 b)))
  (signals atom-required
    (decons (a @b) '(1 2 . 3)
      (cons a b)))
  (decons (@ b) '(1 . 42)
    (is (= 42 b)))
  (signals atom-required
    (decons (@ b) '((1 . 2) . 42)
      (is (= 42 b))))
  (decons (@@a nil) '(1 . 2)
    (is (= 1 a)))
  (signals atom-required
    (decons (@@a nil) '((1 . 2) . 3)
      a)))

(test require-cell
  (decons (nil ^b nil) '(1 (2 . 3) . 4)
    (is (= (car b) 2))
    (is (= (cdr b) 3)))
  (decons (^ b) '((1 . 2) . 42)
    (is (= 42 b)))
  (signals cell-required
    (decons (^ b) '(1 . 42)
      (is (= 42 b))))
  (signals cell-required
    (decons (nil ^ nil) '(1 2 . 3)
      42)))

(test nested
  (decons (a ((b c) d e) f g) '(1 ((2 . 3) 4 . 5) 6 . 7)
    (is (= 1 a))
    (is (= 2 b))
    (is (= 3 c))
    (is (= 4 d))
    (is (= 5 e))
    (is (= 6 f))
    (is (= 7 g))))

(test deaxis
  (deaxis ((sam 6)) [%bat %sam %pay]
    (is (= %sam sam)))
  (deaxis ((bat 2) (sam 6) (con-sam 30)) [%bat %sam %cat %cam %coc]
    (is (= %bat bat)
        (= %sam sam)
        (= %cam con-sam))))

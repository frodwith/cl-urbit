(defpackage urbit/tests/basic-nock
  (:use :cl :prove :urbit/syntax :urbit/tests/util)
  (:import-from urbit/noun :noun)
  (:import-from urbit/error :exit)
  (:import-from urbit/cell :head :tail)
  (:import-from urbit/formula :nock)
  (:import-from urbit/data/constant-cell :constant-cell)
  (:import-from urbit/data/constant-atom :constant-atom)
  (:import-from urbit/context :with-context :make-context))

(in-package :urbit/tests/basic-nock)
(enable-brackets)

(plan 11)

(with-context (make-context)
  (subtest "autocons"
    (subtest "flip"
      (let ((r (nock [0 42] [[0 3] 0 2])))
        (is= (head r) 42)
        (is= (tail r) 0))))
  (subtest "fragment"
    (let ((trel [1 2 3]))
      (is= (nock trel [0 2]) 1)
      (is= (nock trel [0 6]) 2)
      (is= (nock trel [0 7]) 3)))
  (subtest "quote"
    (subtest "fixnums"
      (let ((r (nock 0 [1 42])))
        (is-type r 'fixnum)
        (is-same r [42])))
    (subtest "bignums"
      (let ((r (nock 0 [1 #xdeadbeefcafebabefeefeefee])))
        (is-type r 'constant-atom)
        (is-same r [#xdeadbeefcafebabefeefeefee])))
    (subtest "cells"
      (let ((r (nock 0 [1 0 42])))
        (is-type r 'constant-cell)
        (is-same r [0 42]))))
  (subtest "nock"
    (is= (nock [[0 2] 42 0] [2 [0 3] 0 2]) 42))
  (subtest "deep"
    (is= (nock 3 [3 0 1]) 1)
    (is= (nock [0 0] [3 0 1]) 0))
  (subtest "bump"
    (is= (nock 0 [4 0 1]) 1)
    (is= (nock 41 [4 0 1]) 42)
    (is-same (nock most-positive-fixnum [4 0 1]) [(1+ most-positive-fixnum)]))
  (subtest "same"
    (is= (nock [42 42] [5 [0 2] 0 3]) 0)
    (is= (nock [42 43] [5 [0 2] 0 3]) 1)
    (is= (nock 0 [5 [1 42] 1 42]) 0))
  (subtest "if"
    (subtest "basic"
      (let ((forty-or-two [6 [0 1] [1 40] 1 2]))
        (is= (nock 0 forty-or-two) 40)
        (is= (nock 1 forty-or-two) 2)
        (is-error (nock 2 forty-or-two) 'exit)
        (is-error (nock [0 0] forty-or-two) 'exit)))
    (subtest "funky"
      (let ((funky [6 [0 1] 1 1 42]))
        (is= (nock 1 funky) 42)
        (is-error (nock 0 funky) 'exit))))
  (subtest "comp"
    (is-same (nock [[1 2] 3] [7 [0 2] [0 3] 0 2]) [2 1]))
  (subtest "push"
    (is-same (nock 0 [8 [1 42] [0 3] 0 2]) [0 42]))
  (subtest "pull"
    (is= (nock 0 [9 2 1 [0 6] 42 0]) 42)))

(finalize)

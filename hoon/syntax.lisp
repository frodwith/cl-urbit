(defpackage #:urbit/hoon/syntax
  (:use #:cl #:named-readtables #:urbit/nock/cord #:urbit/nock/data/slimcell)
  (:export #:hoon #:~))

(in-package #:urbit/hoon/syntax)

(defreadtable hoon
  (:merge slim-brackets cord-readtable))

(defconstant ~ 0)

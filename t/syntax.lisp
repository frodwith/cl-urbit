(defpackage #:urbit/tests/syntax
  (:use :cl :prove :urbit/syntax :urbit/tests/util)
  (:import-from :urbit/cell :head :tail))

(in-package :urbit/tests/syntax)
(enable-brackets)

(plan 4)

(is= 42 (tail [0 42]) "tail")
(is= 0 (head (tail [42 0 42])) "head")
(is-same [0 42] (tail [42 0 42]) "tail same")
(let ((a [0 42]))
  (is-same [1 a] [1 0 42] "eval inside braces"))

(finalize)

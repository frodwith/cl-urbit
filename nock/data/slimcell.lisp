(defpackage #:urbit/nock/data/slimcell
  (:use #:cl #:urbit/nock/cell-meta)
  (:export #:slim-cons #:slim-tuple))

(in-package #:urbit/nock/data/slimcell)

; dumb cells with a slot for their metadata

(defstruct (slimcell (:constructor slim-cons (head tail)))
  head
  tail
  (meta nil :type cell-meta))

(define-cell-methods slimcell slimcell-head slimcell-tail slimcell-meta)

(defmacro slim-tuple (&rest elements)
  (labels ((rec (es)
             (if (null (cdr es))
                 (car es)
                 `(slim-cons ,(car es) ,(rec (cdr es))))))
    (and elements (rec elements))))

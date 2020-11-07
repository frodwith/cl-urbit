(defpackage #:urbit/nock/data/slimcell
  (:use #:cl #:urbit/nock/cell-meta #:named-readtables)
  (:export #:slim-cons #:slim-tuple #:slim-brackets))

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

(defreadtable slim-brackets
  (:merge :standard)
  (:macro-char #\] (lambda (stream char)
                     (declare (ignore stream char))
                     (error "unmatched ]")))
  (:macro-char #\[ (lambda (stream char)
                     (declare (ignore char))
                     `(slim-tuple ,@(read-delimited-list #\] stream)))))

(defpackage #:urbit/tests/chunker
  (:use :cl :prove)
  (:import-from :urbit/chunker :with-chunker :chunker-put))

(in-package :urbit/tests/chunker)

(plan 5)

(defparameter *out* nil)

(defun pushout (byte) (push byte *out*))

(defmacro is-chunked (expected-chunks &body forms)
  `(let ((*out* nil))
     (with-chunker #'pushout (out)
       ,@forms)
     (is *out* (quote ,expected-chunks))))

(is-chunked (255)
  (chunker-put out 3)
  (chunker-put out 3)
  (chunker-put out 3)
  (chunker-put out 3))

(is-chunked (252 253 254 255)
  (chunker-put out 255)
  (chunker-put out 254)
  (chunker-put out 253)
  (chunker-put out 252))

(is-chunked (#xde #xad #xbe #xef)
  (chunker-put out #xdeadbeef))

(is-chunked (#xf #x7b #x6a)
  (chunker-put out 42)    ; 101010
  (chunker-put out #xded) ; 110111101101
  (chunker-put out 1)     ; 1
  (chunker-put out 0)     ;
  (chunker-put out 0)     ;
  (chunker-put out 0)     ;
  (chunker-put out 1))    ; 1
  ; 11110111101101101010

(is-chunked (#x21 #x07 #x7A #xB6 #xFB #x78 #xB3 #xE9)
  (chunker-put out 233)        ; 11101001
  (chunker-put out 3)          ; 11
  (chunker-put out 556)        ; 1000101100
  (chunker-put out 3)          ; 11
  (chunker-put out #xdeadbed)  ; 1101111010101101101111101101
  (chunker-put out 1)          ; 1
  (chunker-put out 1056))      ; 10000100000
  ; 10000100000111011110101011011011111011011110001011001111101001

(finalize)

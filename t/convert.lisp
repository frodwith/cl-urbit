(defpackage #:urbit/tests/convert
  (:use #:cl #:fiveam #:urbit/tests
        #:urbit/nock/cord #:urbit/hoon/tape
        #:urbit/hoon/syntax #:urbit/nock/equality))

(in-package #:urbit/tests/convert)

(enable-syntax)

(def-suite convert-tests
           :description "test conversions between lisp data and nouns"
           :in all-tests)

(in-suite convert-tests)

(test cords
  (is (= 97 (string->cord "a")))
  (is (= 1953718630 (string->cord "fast")))
  (is (string= "dEtErMiNiStIc"
               (cord->string 7866321123923179247042781529444))))

(test tapes
  (is (same [102 111 111 0] (string->tape "foo")))
  (is (string= "foo" (tape->string [102 111 111 0]))))

(defun gen-non-null-ascii ()
  (code-char (1+ (random 127))))

(defun gen-ascii ()
  (code-char (random 128)))

(test property
  (for-all ((s (gen-string :elements #'gen-ascii)))
    (is (string= s (tape->string (string->tape s)))))
  ; nulls aren't preserved by cords
  (for-all ((s (gen-string :elements #'gen-non-null-ascii)))
    (is (string= s (cord->string (string->cord s))))))

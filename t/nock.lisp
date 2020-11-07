(defpackage #:urbit/tests/nock
  (:use #:cl #:fiveam #:named-readtables
        #:urbit/tests #:urbit/hoon/syntax #:urbit/nock/nock
        #:urbit/hoon/serial #:urbit/hoon/hints #:urbit/nock/common
        #:urbit/nock/jets #:urbit/hoon/k141 #:urbit/nock/equality
        #:urbit/nock/data #:urbit/nock/data/slimatom))

(in-package #:urbit/tests/nock)
(in-readtable hoon)

(def-suite nock-tests
           :description "test nock"
           :in all-tests)

(def-suite basic-nock-tests
           :description "test the t-shirt functionality (no jets) of nock"
           :in nock-tests)

(def-suite jet-tests
           :description "test jets"
           :in nock-tests)

(def-suite hint-tests
           :description "test various utility hints"
           :in nock-tests)

(def-suite soft-tests
           :description "test virtualization/scry"
           :in nock-tests)

(in-suite basic-nock-tests)

(test autocons
  (bottle
    (let ((r (nock [0 42] [[0 3] 0 2])))
      (is (= 42 (nock r [0 2])))
      (is (= 0 (nock r [0 3]))))))

(test fragment
  (bottle
    (let ((trel [1 2 3]))
      (is (= 1 (nock trel [0 2])))
      (is (= 2 (nock trel [0 6])))
      (is (= 3 (nock trel [0 7]))))))

(test quoted
  (bottle
    (is (same 42 (nock 0 [1 42])))
    (is (same #xdeadbeefcafebabefeefeefee
              (nock 0 [1 #xdeadbeefcafebabefeefeefee])))
    (is (same [0 42] (nock 0 [1 0 42])))))

(test nock
  (bottle
    (is (= 42 (nock [[0 2] 42 0] [2 [0 3] 0 2])))))

(test deep
  (bottle
    (is (= 1 (nock 3 [3 0 1])))
    (is (= 0 (nock [0 0] [3 0 1])))))

(test bump
  (bottle
    (is (= 1 (nock 0 [4 0 1])))
    (is (= 42 (nock 41 [4 0 1])))
    (is (same (nock most-positive-fixnum [4 0 1]) (1+ most-positive-fixnum)))))

(test same
  (bottle
    (is (= 0 (nock [42 42] [5 [0 2] 0 3])))
    (is (= 1 (nock [42 43] [5 [0 2] 0 3])))
    (is (= 0 (nock 0 [5 [1 42] 1 42])))))

(test basic-if
  (bottle
    (let ((forty-or-two [6 [0 1] [1 40] 1 2]))
      (is (= 40 (nock 0 forty-or-two)))
      (is (= 2 (nock 1 forty-or-two)))
      (signals exit (nock 2 forty-or-two))
      (signals exit (nock [0 0] forty-or-two)))))

(test funky-if
  (bottle
    (let ((funky [6 [0 1] 1 1 42]))
      (is (= 42 (nock 1 funky)))
      (signals exit (nock 0 funky)))))

(test compose
  (bottle
    (is (same [2 1] (nock [[1 2] 3] [7 [0 2] [0 3] 0 2])))))

(test tislus
  (bottle
    (is (same [0 42] (nock 0 [8 [1 42] [0 3] 0 2])))))

(test pull
  (bottle
    (is (= 42 (nock 0 [9 2 1 [0 6] 42 0])))))

(test edit
  (bottle
    (is (same [1 3 3] (nock [1 2 3] [10 [6 4 0 6] 0 1])))
    (let ((set-head [10 [2 1 42] 0 1]))
      (is (same [42 0] (nock [0 0] set-head)))
      (signals exit (nock 0 set-head)))
    (signals exit (nock 0 [10 [0 1 42] 0 1]))
    (is (= 42 (nock [0 0 0] [10 [1 1 42] 0 1])))
    (signals exit (nock 0 [10 [1 1 42] 0 0]))))

(test hint
  (bottle
    (is (= 42 (nock 42 [11 1 0 1])))
    (is (= 42 (nock 42 [11 [1 1 1] 0 1])))
    (signals exit (nock 42 [11 [1 0 0] 0 1]))))

(in-suite jet-tests)

;  =<  ack
;  =>  %kack
;  ~%  %kack  ~  ~
;  |%
;  ++  dec
;    ~/  %dec
;    |=  a=@
;    =|  i=@
;    |-
;    =+  n=+(i)
;    ?:  =(n a)
;      i
;    $(i n)
;  ++  ack
;    ~/  %ack
;    |=  [m=@ n=@]
;    ?~  m  +(n)
;    ?~  n  $(m (dec m), n 1)
;    $(m (dec m), n $(n (dec n)))
;  --
(define-symbol-macro +ackerman-source+ [7 [7 [1 %kack] 8 [1 [7 [8 [1 0] [1 8 [1 0] 8 [1 8 [4 0 6] 6 [5 [0 2] 0 62] [0 14] 9 2 10 [6 0 2] 0 3] 9 2 0 1] 0 1] 11 [%fast 1 %dec [0 7] 0] 0 1] 7 [8 [1 0 0] [1 6 [5 [1 0] 0 12] [4 0 13] 6 [5 [1 0] 0 13] [9 2 10 [6 [8 [9 4 0 7] 9 2 10 [6 0 28] 0 2] 1 1] 0 1] 9 2 10 [6 [8 [9 4 0 7] 9 2 10 [6 0 28] 0 2] 9 2 10 [13 8 [9 4 0 7] 9 2 10 [6 0 29] 0 2] 0 1] 0 1] 0 1] 11 [%fast 1 %ack [0 7] 0] 0 1] 11 [%fast 1 %kack [1 0] 0] 0 1] 9 5 0 1])

(defvar *mock-dec-calls*)

(defun mock-dec (sam)
  (incf *mock-dec-calls*)
  (let ((i (cl-integer sam)))
    (if (= 0 i)
      (error 'exit)
      (1- i))))

(defunary mock-dec+< #'mock-dec)
(defgate +mock-dec #'mock-dec+<)

(define-symbol-macro +ackerman-jets+
  (list
    (jet-root
      %kack %kack nil
      (~/ dec #'+mock-dec))))

(defun ack (n m)
  (nock [n m] [9 2 10 [6 0 1] +ackerman-source+]))

(defparameter +decs-per-call+ 26)

(test ackerman
  (let (pack
        (*mock-dec-calls* 0))
    ; no mention of jets
    (is (= 7 (bottle (ack 2 2))))
    (is (= 0 *mock-dec-calls*))
    ; jet tree is present, but no registrations - no increase
    (is (= 7 (in-world (load-world :jet-tree +ackerman-jets+) (ack 2 2))))
    (is (= 0 *mock-dec-calls*))
    ; turn fast hints on - this time the jet should fire 
    (is (= 7 (in-world (load-world :jet-tree +ackerman-jets+
                                   :hinter #'fast-hinter)
               (let ((a (ack 2 2)))
                 ; jam/cue to wash metadata
                 (setf pack (jam (save-jet-pack)))
                 a))))
    (is (= +decs-per-call+ *mock-dec-calls*))
    ; call in a new world with no jets - no increase
    (is (= 7 (bottle (ack 2 2))))
    (is (= +decs-per-call+ *mock-dec-calls*))
    ; with jets but no registrations, no increase
    (is (= 7 (in-world (load-world :jet-tree +ackerman-jets+) (ack 2 2))))
    (is (= +decs-per-call+ *mock-dec-calls*))
    ; supply the saved jet pack, the jets fire
    (is (= 7 (in-world (load-world :jet-tree +ackerman-jets+
                                   :jet-pack (cue-slim-from-int pack))
               (ack 2 2))))
    (is (= (* 2 +decs-per-call+) *mock-dec-calls*))))

(in-suite hint-tests)

(test slog
  (in-world (load-world :hinter #'slog-hinter)
    (let (c)
      (handler-bind ((slog #'(lambda (s) (setq c s))))
        (is (= 42 (nock 42 [11 [%slog 1 1 %leaf %h %e %l %l %o 0] 0 1]))))
      (is (typep c 'slog))
      (is (= 1 (slog-priority c)))
      (is (same [%leaf %h %e %l %l %o 0] (slog-tank c))))))

(test stack
  (multiple-value-bind (pro tax)
    (with-bug-trap
      (in-world (load-world :hinter #'stack-hinter)
        (nock 42 [11 [%hunk 1 5]
                  11 [%hand 1 4]
                  11 [%mean 1 3]
                  11 [%spot 1 2]
                  11 [%lose 1 1]
                  0 0])))
    (is (null pro))
    (loop for got in tax
          for expected in (list [%lose 1] [%spot 2] [%mean 3] [%hand 4] [%hunk 5])
          do (is (same expected got)))))

; !=
; =<  fib
; =~  %42
; ~%  %kern  ~  ~
; |%
; ++  version  +
; --
; ~%  %one  +  ~
; |%
; ++  dec
;   ~/  %dec
;   |=  a=@
;   ?:  =(0 a)  !!
;   =|  i=@
;   |-  ^-  @
;   =/  n  +(i)
;   ?:  =(n a)
;     i
;   $(i n)
; ::
; ++  add
;   ~/  %add
;   |=  [a=@ b=@]
;   ?:  =(b 0)  a
;   %=  $
;     a  +(a)
;     b  (dec b)
;   ==
; --
; ~%  %two  +  ~
; |%
; ++  fib
;   ~/  %fib
;   |=  n=@
;   ^-  @  ~+
;   ?:  =(0 n)  0
;   ?:  =(1 n)  1
;   =/  a  (dec n)
;   =/  b  (dec a)
;   %+  add
;     $(n a)
;   $(n b)
; --  ==

(define-symbol-macro +memo-source+
  [7 [7 [1 42] 7 [8 [1 0 3] 11 [%fast 1 %kern [1 0] 0] 0 1] 7 [8 [1 [7 [8 [1 0 0] [1 6 [5 [0 13] 1 0] [0 12] 9 2 10 [6 [4 0 12] 8 [9 5 0 7] 9 2 10 [6 0 29] 0 2] 0 1] 0 1] 11 [%fast 1 %add [0 7] 0] 0 1] 7 [8 [1 0] [1 6 [5 [1 0] 0 6] [0 0] 8 [1 0] 8 [1 8 [4 0 6] 6 [5 [0 2] 0 62] [0 14] 9 2 10 [6 0 2] 0 3] 9 2 0 1] 0 1] 11 [%fast 1 %dec [0 7] 0] 0 1] 11 [%fast 1 %one [0 3] 0] 0 1] 8 [1 7 [8 [1 0] [1 11 [%memo 1 0] 6 [5 [1 0] 0 6] [1 0] 6 [5 [1 1] 0 6] [1 1] 8 [8 [9 5 0 15] 9 2 10 [6 0 14] 0 2] 8 [8 [9 5 0 31] 9 2 10 [6 0 6] 0 2] 8 [9 4 0 63] 9 2 10 [6 [7 [0 3] 9 2 10 [6 0 6] 0 7] 7 [0 3] 9 2 10 [6 0 2] 0 7] 0 2] 0 1] 11 [%fast 1 %fib [0 7] 0] 0 1] 11 [%fast 1 %two [0 3] 0] 0 1] 9 2 0 1])

(defun memo-dec (a)
  (if (zerop a)
      (error 'exit)
      (1- a)))

(defunary memo-dec+< #'memo-dec)
(defgate +memo-dec #'memo-dec+<)

(defun memo-add (a b)
  (+ a b))

(defbinary memo-add+< #'memo-add)
(defgate +memo-add #'memo-add+<)

(defvar *memo-count*)

(defun memo-fib+< (sample)
  (declare (ignore sample))
  (incf *memo-count*)
  nil)

(defgate +memo-fib #'memo-fib+<)

(defparameter +memo-jets+
  (list
    (jet-root
      %kern 42 nil
      (layer one
        (~/ dec #'+memo-dec)
        (~/ add #'+memo-add)
        (layer two
          (~/ fib #'+memo-fib))))))

(test memo
  (in-world (load-world :hinter (compose-hinters #'memo-hinter #'fast-hinter)
                        :jet-tree +memo-jets+)
    (with-fresh-memos
      (let ((*memo-count* 0))
        (is (= 55 (nock 0 [9 2 10 [6 1 10] +memo-source+])))
        ; 177 without memoization
        (is (= 19 *memo-count*))))))

(test fast-parent
  (macrolet ((pfp (parent &body forms)
               `(multiple-value-bind (valid axis)
                  (urbit/hoon/hints::parse-fast-parent ,parent)
                  ,@forms)))
    (pfp [11 %slog 1 0]
      (is-true valid)
      (is (null axis)))
    (pfp [11 %slog 11 %slog 1 0]
      (is-true valid)
      (is (null axis)))
    (pfp [11 %slog 11 %slog 1 1]
      (is (not valid))
      (is (null axis)))
    (pfp [11 %slog 11 %foo 0 1]
      (is (not valid))
      (is (null axis)))
    (pfp [11 %slog 11 %foo 0 4]
      (is (not valid))
      (is (null axis)))
    (pfp [11 %slog 11 %foo 0 7]
      (is-true valid)
      (is (= 7 axis)))))

(test fast-name
  (macrolet ((p (&rest args) `(urbit/hoon/hints::parse-fast-name ,@args)))
    (is (= %foo (p %foo)))
    (is (= %barbazquux (p %barbazquux)))
    (is (= %foo42 (p [%foo 42])))
    (is (null (p [1 42 %foo])))
    (is (null (p [%foo 0 42])))))

(in-suite soft-tests)

(defmacro succeeds (expected got)
  (let ((key (gensym))
        (val (gensym)))
    `(multiple-value-bind (,key ,val) ,got
       (is (eq :success ,key))
       (is (same ,expected ,val)))))

(defmacro blocks (expected-tail got)
  (let ((key (gensym))
        (val (gensym)))
    `(multiple-value-bind (,key ,val) ,got
       (is (eq :block ,key))
       (is (same ,expected-tail (tail ,val))))))

(defmacro hunks (got)
  (let ((key (gensym))
        (val (gensym)))
    `(multiple-value-bind (,key ,val) ,got
       (declare (ignore ,val))
       (is (eq :error ,key)))))

(defmacro exits (&body forms)
  `(signals exit ,@forms))

(define-symbol-macro +wisher-source+ [12 [1 151 %atom 116 0] 0 1])

(defmacro layers ((&rest gates) &body forms)
  (labels ((rec (gates)
             (destructuring-bind (gate . more) gates
               (if (null more)
                   `(soft ,gate ,@forms)
                   `(soft ,gate
                      (multiple-value-bind (key val) ,(rec (cdr gates))
                        (ecase key
                          (:success val)
                          (:block (error 'need :sample val))
                          (:error
                            (setf *bug-stack* val)
                            (error 'exit)))))))))
    (rec gates)))

(defun scry (path)
  (nock path +wisher-source+))

(defun soft-test-djinn ()
  ; [~ ~] on empty path,
  ; block on paths starting with /b,
  ; !! crash on /c,
  ; else ``42
  [[6 [6 [3 0 13] [1 1] 1 0] [1 0 0] 6 [5 [0 26] 1 98] [1 0] 6 [5 [0 26] 1 99] [0 0] 1 0 0 42] 0 0])

(defun soft-cd ()
  [99 100])

(defun soft-bc ()
  [98 99 0])

(defun soft-abcd ()
  [97 98 99 100])

(test soft-top
  (bottle
    (exits (nock 0 [12 [1 0] 1 97 98 99 0]))))

(test soft-basic
  (bottle
    (let ((always42 [[1 0 0 42] 0 0]))
      (succeeds 42 (soft always42 (scry 0))))))

(test soft-crash
  (bottle
    (let ((crash [[0 0] 0 0]))
      (exits (soft crash (scry 0))))))

(test soft-full
  (bottle
    (let ((cd (soft-cd))
          (bc (soft-bc))
          (abcd (soft-abcd))
          (test (soft-test-djinn)))
      (hunks (soft test (scry 0)))
      (exits (soft test (scry cd)))
      (blocks bc (soft test (scry bc)))
      (succeeds 42 (soft test (scry abcd))))))

(test soft-nested
  (bottle
    (let ((pass-thru [[[1 0] [1 0] 12 [0 12] 0 13] [0 0] 0])
          (cd (soft-cd)) 
          (bc (soft-bc))
          (abcd (soft-abcd)) 
          (test (soft-test-djinn)))
      (succeeds 42 (layers (test pass-thru pass-thru) (scry abcd)))
      (blocks bc (layers (test pass-thru pass-thru) (scry bc)))
      (hunks (layers (test pass-thru pass-thru) (scry 0)))
      (exits (layers (test pass-thru pass-thru) (scry cd))))))

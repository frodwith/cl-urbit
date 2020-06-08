(defpackage #:urbit/tests/nock
  (:use #:cl #:fiveam #:urbit/tests #:urbit/syntax #:urbit/nock #:urbit/hints
        #:urbit/jets #:urbit/equality #:urbit/data))

(in-package #:urbit/tests/nock)

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

(in-suite basic-nock-tests)

(enable-syntax)

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
(defparameter +ackerman-source+ [7 [7 [1 %kack] 8 [1 [7 [8 [1 0] [1 8 [1 0] 8 [1 8 [4 0 6] 6 [5 [0 2] 0 62] [0 14] 9 2 10 [6 0 2] 0 3] 9 2 0 1] 0 1] 11 [%fast 1 %dec [0 7] 0] 0 1] 7 [8 [1 0 0] [1 6 [5 [1 0] 0 12] [4 0 13] 6 [5 [1 0] 0 13] [9 2 10 [6 [8 [9 4 0 7] 9 2 10 [6 0 28] 0 2] 1 1] 0 1] 9 2 10 [6 [8 [9 4 0 7] 9 2 10 [6 0 28] 0 2] 9 2 10 [13 8 [9 4 0 7] 9 2 10 [6 0 29] 0 2] 0 1] 0 1] 0 1] 11 [%fast 1 %ack [0 7] 0] 0 1] 11 [%fast 1 %kack [1 0] 0] 0 1] 9 5 0 1])

(defvar *mock-dec-calls*)
(defun mock-dec (sam)
  (incf *mock-dec-calls*)
  (let ((i (cl-integer sam)))
    (if (= 0 i)
      (error 'exit)
      (1- i))))

(defparameter +ackerman-jets+
  (list
    (root %kack %kack nil
          (gate %dec #'mock-dec))))

(defun ack (n m)
  (nock [n m] (copy-tree [9 2 10 [6 0 1] +ackerman-source+])))

(defparameter +decs-per-call+ 26)

; placeholder
(test ackerman
  (setf *mock-dec-calls* 0)
  (let (pack)
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
                 (setf pack (save-jet-pack))
                 a
                 ))))
    (is (= +decs-per-call+ *mock-dec-calls*))
    ; call in a new world with no jets - no increase
    (is (= 7 (bottle (ack 2 2))))
    (is (= +decs-per-call+ *mock-dec-calls*))
    ; with jets but no registrations, no increase
    (is (= 7 (in-world (load-world :jet-tree +ackerman-jets+) (ack 2 2))))
    (is (= +decs-per-call+ *mock-dec-calls*))
    ; supply the saved jet pack, the jets fire
    (is (= 7 (in-world (load-world :jet-tree +ackerman-jets+ :jet-pack pack)
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
  (handler-case
    (let ((stack
            (handler-case
              (in-world (load-world :hinter #'stack-hinter)
                (nock 42 [11 [%hunk 1 5]
                          11 [%hand 1 4]
                          11 [%mean 1 3]
                          11 [%spot 1 2]
                          11 [%lose 1 1]
                          0 0]))
              (exit (e) (exit-stack e)))))
      (is (equal '((%hunk . 5) (%hand . 4) (%mean . 3) (%spot . 2) (%lose . 1))
                 stack)))))

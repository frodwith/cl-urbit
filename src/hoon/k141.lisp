(defpackage #:urbit/hoon/k141
  (:use #:cl #:urbit/jets #:urbit/common #:urbit/syntax
        #:urbit/convert #:urbit/math #:urbit/data #:urbit/hints
        #:urbit/data/slimatom #:urbit/data/slimcell)
  (:export #:load-k141))

(in-package #:urbit/hoon/k141)

(enable-cords)

(defmacro exit-mean-leaf (str)
  (let* ((tape (string->tape str :cell-fn #'cons))
         (tank (cons %leaf tape)))
    `(exit-with (cons %mean (copy-tree ',tank)))))

(defmacro gfn (name pattern &body forms)
  (let ((sam (gensym)))
    `(gate ,name
           (lambda (,sam)
             (dedata ,pattern ,sam ,@forms)))))

(defmacro gcmp (name cmp)
  `(gfn ,name (@@a @@b) (loob (,cmp a b))))

(defmacro binmath-gate (name op)
  `(gfn ,name (@@a @@b) (slim-malt (,op a b))))

(defparameter +jets+
  (list
    (jet-root
      %k141 141 nil
      (jet-core
        %one 1 nil
        (gcmp %gte >=)
        (gcmp %gth >)
        (gcmp %lte <=)
        (gcmp %lth <)
        (binmath-gate %add +)
        (binmath-gate %div truncate)
        (binmath-gate %mul *)
        (binmath-gate %peg peg)    
        (gfn %cap (@@a) (cap a))
        (gfn %mas (@@a) (slim-malt (mas a)))
        (gfn %max (@@a @@b) (if (> a b) a b))
        (gfn %min (@@a @@b) (if (< a b) a b))
        (gfn %dec (@@n)
          (if (zerop n)
              (exit-mean-leaf "decrement-underflow")
              (slim-malt (1- n))))
        (gfn %sub (@a @@b)
          (cond ((= a b) 0)
                ((> b a) (exit-mean-leaf "subtract-underflow"))
                (t (slim-malt (- a b)))))
        (gfn %dvr (@@a @@b)
          (multiple-value-bind (q r) (truncate a b)
            (slim-cons (slim-malt q) (slim-malt r))))
        (gfn %mod (@@a @@b)
          (multiple-value-bind (q r) (truncate a b)
            (declare (ignore q))
            (slim-malt r)))))))

(defun k141-hinter (tag clue next)
  (when clue
    (case tag
      (%slog +handle-slog+)
      (%memo (handle-memo next))
      ((%hunk %hand %mean %lose %spot) (handle-stack tag)))))

(defun load-k141 (&optional fast-hints-enabled)
  (load-world :jet-tree +jets+
              :hinter (if fast-hints-enabled
                          (compose-hinters #'fast-hinter #'k141-hinter)
                          #'k141-hinter)))

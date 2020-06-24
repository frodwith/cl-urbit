(defpackage #:urbit/hoon/k141
  (:use #:cl #:urbit/jets #:urbit/common #:urbit/syntax #:urbit/mug
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

(defmacro m (form)
  `(slim-malt ,form))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defun symbol-cord (s)
    (string->cord (string-downcase (symbol-name s)))))

; math gates take atom arguments and return atoms
(defmacro math-gate (argc name &optional cord-name)
  (let* ((names (loop for i below argc collect (format nil "A~a" i)))
         (pats (mapcar (lambda (n) (intern (format nil "@@~a" n))) names)))
    `(gfn ,(or cord-name (symbol-cord name)) ,pats
       (slim-malt (,name ,@(mapcar #'intern names))))))

(defmacro raw-gate (argc name &optional cord-name)
  (let ((names (loop for i below argc collect (gensym))))
    `(gfn ,(or cord-name (symbol-cord name)) ,names
       (,name ,@names))))

; jets that don't really belong anywhere else in the runtime just live
; here for now - later it might make sense to group them into modules
(defun rip (b a)
  (declare (uint b a))
  (if (zerop a)
      0
      (let* ((size (ash 1 b))
             (len (integer-length a))
             (partial-bits (mod len size)))
          (loop with ipos = (- len partial-bits)
                with last-bits = (ldb (byte partial-bits ipos) a)
                for r = (if (> last-bits 0)
                            (slim-cons (m last-bits) 0)
                            0)
                then (slim-cons (m part) r)
                for pos from (- ipos size) downto 0 by size
                for part = (ldb (byte size pos) a)
                finally (return r)))))

(defun weld (a b)
  (loop with v = (loop with v = (make-array 100 :adjustable t
                                            :fill-pointer 0)
                       for l = a then (tail l)
                       while (deep l)
                       do (vector-push-extend (head l) v)
                       finally (if (zerop (cl-integer l))
                                   (return v)
                                   (error 'exit)))
        for r = b then (slim-cons (aref v i) r)
        for i from (1- (length v)) downto 0
        finally (return r)))

(defun muk (syd len key)
  (if (or (> (met 5 syd) 1)
          (> (met 0 len) 31)
          (> (met 3 key) len))
      (error 'exit)
      (let ((murmurhash:*hash-size* 32))
        (murmurhash:murmurhash key :seed syd))))

(defun slag (a b)
  (declare (uint a))
  (loop for n = b then (tail n)
        for i below a
        while (deep n)
        finally (return n)))

(defparameter +jets+
  (list
    (jet-root
      %k141 141 nil
      (jet-core
        %one 1 nil
        (math-gate 2 add)
        (gfn %dec (@@a)
          (if (zerop a)
              (exit-mean-leaf "decrement-underflow")
              (m (dec a))))
        (gfn %div (@@a @@b)
          (if (zerop b)
              (exit-mean-leaf "divide-by-zero")
              (m (div a b))))
        (gfn %dvr (@@a @@b)
          (if (zerop b)
              (exit-mean-leaf "divide-by-zero")
              (multiple-value-bind (q r) (dvr a b)
                (slim-cons (m q) (m r)))))
        (gcmp %gte >=)
        (gcmp %gth >)
        (gcmp %lte <=)
        (gcmp %lth <)
        (math-gate 2 hmax %max)
        (math-gate 2 hmin %min)
        (gfn %mod (@@a @@b)
          (if (zerop b)
              (error 'exit)
              (m (hmod a b))))
        (math-gate 2 mul)
        (gfn %sub (@@a @@b)
          (cond ((= a b) 0)
                ((> b a) (exit-mean-leaf "subtract-underflow"))
                (t (m (sub a b)))))
        (math-gate 1 cap)
        (math-gate 1 mas)
        (gfn %peg (@@a @@b)
          (if (zerop a)
              (error 'exit)
              (peg a b)))
        (jet-core
          %two 1 nil
          (raw-gate 2 slag)      ; 691
          (math-gate 1 bex)      ; 801
          (math-gate 3 end)      ; 825
          (math-gate 3 lsh)      ; 839
          (math-gate 2 met)      ; 845
          (gfn %rip (@@bloq @@a) ; 893
            (rip bloq a))
          (math-gate 3 rsh)      ; 900
          (math-gate 2 con)      ; 947
          (math-gate 2 mix)      ; 981
          (jet-core              ; 1001
            %muk 27
            (gate-driver
              (lambda (sample)
                (dedata (@@syd @@len @@key) sample
                  (muk syd len key)))))
          (raw-gate 1 mug)       ; 1059
          (raw-gate 2 weld)      ; 1910
          )))))

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

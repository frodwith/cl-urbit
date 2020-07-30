(defpackage #:urbit/hoon/k141
  (:use #:cl #:ironclad #:cl-intbytes
        #:urbit/jets #:urbit/common #:urbit/syntax #:urbit/mug #:urbit/cache
        #:urbit/convert #:urbit/math #:urbit/axis #:urbit/data #:urbit/hints
        #:urbit/nock #:urbit/equality #:urbit/serial #:urbit/world
        #:urbit/data/slimatom #:urbit/data/slimcell)
  (:import-from #:alexandria #:when-let #:when-let* #:if-let)
  (:export #:load-k141 #:flop))

(in-package #:urbit/hoon/k141)

(enable-cords)

(defmacro exit-mean-leaf (str)
  (let* ((tape (string->tape str :cell-fn #'cons))
         (tank (cons %leaf tape)))
    `(exit-with (cons %mean (copy-tree ',tank)))))

(defun fuzzy-gate-driver (sample-fn)
  (lambda (kernel parent-stencil ideal hooks)
    (declare (ignore parent-stencil hooks))
    (trap (fuzzy kernel ideal 1 (gate sample-fn)))))

(defun jet-fuzzy-gate (name sample-fn)
  (jet-core name 3 (fuzzy-gate-driver sample-fn)))

(defmacro gfn-impl (build name pattern &body forms)
  (let ((sam (gensym)))
    `(,build ,name
       (lambda (,sam)
         (dedata ,pattern ,sam ,@forms)))))

(defmacro gfn (name pattern &body forms)
  `(gfn-impl jet-deaf-gate ,name ,pattern ,@forms))

(defmacro gfn-fuzz (name pattern &body forms)
  `(gfn-impl jet-fuzzy-gate ,name ,pattern ,@forms) )

(defmacro gcmp (name cmp)
  `(gfn ,name (@@a @@b) (loob (,cmp a b))))

(defmacro m (form)
  `(slim-malt ,form))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defun symbol-cord (s)
    (string->cord (string-downcase (symbol-name s)))))

; math gates take atom arguments and return atoms
(defmacro math-gate (argc name &optional cord-name fuzzy)
  (let* ((names (loop for i below argc collect (format nil "A~a" i)))
         (pats (mapcar (lambda (n) (intern (format nil "@@~a" n))) names)))
    `(,(if fuzzy 'gfn-fuzz 'gfn)
       ,(or cord-name (symbol-cord name)) ,pats
       (slim-malt (,name ,@(mapcar #'intern names))))))

(defmacro raw-gate (argc name &optional cord-name fuzzy)
  (let ((names (loop for i below argc collect (gensym))))
    `(,(if fuzzy 'gfn-fuzz 'gfn)
       ,(or cord-name (symbol-cord name)) ,names
       (,name ,@names))))

(defmacro sig-or-die (noun)
  `(unless (zerop (cl-integer ,noun))
     (error 'exit)))

(defmacro nlist-for (cur while i list &body forms)
  `(loop for ,cur = ,list then (tail ,cur)
         while ,while
         for ,i = (head ,cur)
         do (progn ,@forms)))

; there are basically two ways to test for the end of a list in hoon,
; and both are idiomatic: test cell depth or test equality to 0. These
; are equivalent for the list type, but of course nock is untyped.
; When you send an improper list like [1 2 3] to a depth-checking loop,
; it treats the trailing 3 as the list terminator.  A zero-checking loop
; will instead crash. Jets have to care in order to avoid mismatches, so
; we provide two list-iteration macros:

(defmacro for-?^ ((var list) &body forms)
  (let ((s (gensym)))
    `(nlist-for ,s (deep ,s) ,var ,list ,@forms)))

(defmacro test-?~ (var)
  `(or (deep ,var)
       (progn
         (sig-or-die ,var)
         nil)))

(defmacro for-?~ ((var list) &body forms)
  (let ((s (gensym)))
    `(nlist-for ,s (test-?~ ,s) ,var ,list ,@forms)))

(defmacro stack-vector (init-size)
  `(make-array ,init-size :adjustable t :fill-pointer 0))

; jets that don't really belong anywhere else in the runtime just live
; here for now - later it might make sense to group them into modules

(defun cat (a b c)
  (declare (uint a b c))
  (logior b (lsh a (met a b) c)))

(defun can (bloq list)
  (declare (uint bloq))
  (let ((r 0) (size 0))
    (for-?~ (pair list)
      (dedata (@@bloqs @@data) pair
        (setq r (logior r (lsh bloq size (end bloq bloqs data))))
        (setq size (+ size bloqs))))
    r))

(defun rap (bloq list)
  (declare (uint bloq))
  (let ((r 0))
    (for-?~ (i list)
      (setq r (cat bloq r (cl-integer i))))
    r))

(defun rep (bloq list)
  (declare (uint bloq))
  (let ((size (ash 1 bloq))
        (c 0)
        (r 0))
    (for-?~ (i list)
      (setq r (logior r (ash (ldb (byte size 0) (cl-integer i)) c)))
      (setq c (+ c size)))
    r))

(defun cut (bloq from-end bloq-count atom)
  (declare (uint bloq from-end bloq-count atom))
  (ldb (byte (ash bloq-count bloq)
             (ash from-end bloq))
       atom))

(defun dor (a b)
  (labels ((rec (a b)
             (or (same a b)
                 (if (deep a)
                     (when (deep b)
                       (let ((ha (head a))
                             (hb (head b)))
                         (if (same ha hb)
                             (rec (tail a) (tail b))
                             (rec ha hb))))
                     (or (deep b)
                         (< (cl-integer a) (cl-integer b)))))))
    (loob (rec a b))))

(defun gor (a b)
  (let ((c (mug a))
        (d (mug b)))
    (if (= c d)
        (dor a b)
        (loob (< c d)))))

(defun mor (a b)
  (let ((c (murmug (mug a)))
        (d (murmug (mug b))))
    (if (= c d)
        (dor a b)
        (loob (< c d)))))

(defun rip (b a)
  (declare (uint b a))
  (if (zerop a)
      0
      (let* ((size (ash 1 b))
             (len (integer-length a))
             (partial-bits (mod len size))
             (malt (if (> size +fixnum-bits+) #'slim-malt #'identity)))
          (loop with ipos = (- len partial-bits)
                with last-bits = (ldb (byte partial-bits ipos) a)
                for r = (if (> last-bits 0)
                            (slim-cons (funcall malt last-bits) 0)
                            0)
                then (slim-cons (funcall malt part) r)
                for pos from (- ipos size) downto 0 by size
                for part = (ldb (byte size pos) a)
                finally (return r)))))

; when you need a random access to a list (like to iterate over it backwards)
; strict means it will crash if the terminator isn't 0
(defmacro nlist->vector (list &key (strict t) (size 10))
  `(let ((vec (stack-vector ,size)))
     (,(if strict 'for-?~ 'for-?^)
       (i ,list)
       (vector-push-extend i vec))
     vec))

(defun vector-onto-nlist (vector list)
  (loop for n = list then (slim-cons a n)
        for i from (1- (length vector)) downto 0
        for a = (aref vector i)
        finally (return n)))

(defun weld (a b)
  (vector-onto-nlist (nlist->vector a) b))

(defun muk (syd len key)
  (if (or (> (met 5 syd) 1)
          (> (met 0 len) 31)
          (> (met 3 key) len))
      (error 'exit)
      (let ((murmurhash:*hash-size* 32))
        (murmurhash:murmurhash key :seed syd))))

(defun flop (list)
  (let ((r 0))
    (for-?~ (i list)
      (setq r (slim-cons i r)))
    r))

(defun lent (list)
  (let ((len 0))
    (for-?~ (i list)
      (incf len))
    len))

(defun reap (times item)
  (declare (uint times))
  (loop for r = 0 then (slim-cons item r)
        repeat times
        finally (return r)))

(defun turn (list gate)
  (loop with slam = (make-slam gate)
        with vec = (nlist->vector list)
        for r = 0 then (slim-cons pro r)
        for i from (1- (length vec)) downto 0
        for pro = (funcall slam (aref vec i))
        finally (return r)))

(defun shal (len ruz)
  (declare (uint len ruz))
  (octets->uint
    (digest-sequence :sha512 (int->octets ruz len))
    64))

(defun shan (ruz)
  (declare (uint ruz))
  (octets->uint
    (reverse (digest-sequence :sha1 (int->octets ruz (met 3 ruz))))
    20))

(defun shay (len ruz)
  (declare (uint len ruz))
  (octets->uint
    (digest-sequence :sha256 (int->octets ruz len))
    32))

; "partial memoization" caches for the hoon compiler

; a version of the hoon compiler is specified as the battery of ut and an
; icell for ut's static context. these are weakrefs, so losing all references
; to a hoon compiler clears its caches.
(defun versioned-compiler-cache (table ut-battery ut-context size)
  (if-let (top (gethash ut-battery table))
    (or (gethash ut-context top)
        (setf (gethash ut-context top) (make-cache size 'same)))
    (let* ((top (make-hash-table :test 'eq :weakness :key)))
      (setf (gethash ut-battery table) top)
      (setf (gethash ut-context top) (make-cache size 'same)))))

; each cached compiler function gets its own urbit/cache
(defmacro define-compiler-cache (name size)
  (let ((table (intern (format nil "*~a-table*" name))))
    `(progn
       (defparameter ,table (make-hash-table :test 'eq :weakness :key))
       (defun ,name (ut-battery ut-context)
         (versioned-compiler-cache ,table ut-battery ut-context ,size)))))

(defun unpack-ut (ut-core)
  (let* ((upay (tail ut-core))
         (pen (get-ideal-cell (tail (tail upay)))))
    (values (head upay) pen (get-battery ut-core))))

(defun look-in (core battery-fn ut-battery ut-context cache-fn noun-key)
  (cache-lookup (funcall cache-fn ut-battery ut-context)
                noun-key
                (funcall battery-fn core)))

(defparameter +large-cache+ 1024)
(defparameter +small-cache+ 256)

; the cache sizes are arbitrary - tune them? i took a guess at which ones
; should be bigger, but the main idea is that you can size them independently,
; and they don't step on each others' values.
(define-compiler-cache nest-cache +large-cache+)
(defun nest-driver (kernel parent-stencil ideal hooks)
  ; would like to access the arm version rather than the formula version
  ; in these fallbacks. Maybe a proto-stencil object to be passed to the
  ; drivers that could hold onto the dispatch function, and make the
  ; signatures for a lot of these functions (resolve-hook, etc) nicer.
  ; then profiling would be more accurate, and we can implement different
  ; declarations for arms vs. formulas
  ; more aggressive optimizations and longer time spent for fast arms
  ; turn the optimizer down and go for compilation speed and space in formulas
  ; might even make sense to use an interpreter as a base tier
  ; like every formula starts interpreted, till its run 100 times, then
  ; we compile it with the optimizer turned down till it runs 10k times,
  ; then we turn the optimizer up.
  ; and fast arms always have the optimizer turned all the way up.
  ; that could cut down on our startup time.
  (let ((battery-fn (icell-function ideal)))
    (when-let (veth (resolve-hook %vet kernel parent-stencil hooks :skip 3))
      (trap
        (lambda (core)
          (let* ((nest-in (tail core))
                 (in-pay (tail nest-in))
                 (in-sam (head in-pay))
                 (seg (head in-sam))
                 (reg (head (tail in-sam)))
                 (nest (tail in-pay))
                 (nest-pay (tail nest))
                 (nest-sam (head nest-pay))
                 (ref (tail nest-sam))
                 (ut (tail nest-pay)))
            (when-let (vet (call-hook veth ut))
              (multiple-value-bind (sut but pen) (unpack-ut ut)
                (look-in core battery-fn but pen #'nest-cache
                         (slim-tuple seg reg vet sut ref))))))))))

(defun vet-sut-sam (kernel parent-stencil ideal hooks cache-fn)
  (let ((battery-fn (icell-function ideal)))
    (when-let (veth (resolve-hook %vet kernel parent-stencil hooks :skip 1))
      (trap
        (lambda (core)
          (let* ((pay (tail core))
                 (sam (head pay))
                 (ut (tail pay)))
            (when-let (vet (call-hook veth ut))
              (multiple-value-bind (sut but pen) (unpack-ut ut)
                (look-in core battery-fn but pen cache-fn
                         (slim-tuple vet sut sam))))))))))

(defun vrf-sut-sam (kernel parent-stencil ideal hooks cache-fn)
  (let ((battery-fn (icell-function ideal)))
    (when-let* ((veth (resolve-hook %vet kernel parent-stencil hooks :skip 1))
                (fabh (resolve-hook %fab kernel parent-stencil hooks :skip 1)))
      (trap
        (lambda (core)
          (let* ((pay (tail core))
                 (sam (head pay))
                 (ut (tail pay)))
            (when-let* ((vet (call-hook veth ut))
                        (fab (call-hook fabh ut)))
              (multiple-value-bind (sut but pen) (unpack-ut ut)
                (look-in core battery-fn but pen cache-fn
                         (slim-tuple vet fab sut sam))))))))))

(define-compiler-cache crop-cache +small-cache+)
(defun crop-driver (kernel parent-stencil ideal hooks)
  (vet-sut-sam kernel parent-stencil ideal hooks #'crop-cache))

(define-compiler-cache fish-cache +small-cache+)
(defun fish-driver (kernel parent-stencil ideal hooks)
  (vet-sut-sam kernel parent-stencil ideal hooks #'fish-cache))

(define-compiler-cache fond-cache +small-cache+)
(defun fond-driver (kernel parent-stencil ideal hooks)
  (vet-sut-sam kernel parent-stencil ideal hooks #'fond-cache))

(define-compiler-cache fuse-cache +small-cache+)
(defun fuse-driver (kernel parent-stencil ideal hooks)
  (vet-sut-sam kernel parent-stencil ideal hooks #'fuse-cache))

(define-compiler-cache mint-cache +large-cache+)
(defun mint-driver (kernel parent-stencil ideal hooks)
  (vrf-sut-sam kernel parent-stencil ideal hooks #'mint-cache))

(define-compiler-cache mull-cache +large-cache+)
(defun mull-driver (kernel parent-stencil ideal hooks)
  (vet-sut-sam kernel parent-stencil ideal hooks #'mull-cache))

(define-compiler-cache peek-cache +small-cache+)
(defun peek-driver (kernel parent-stencil ideal hooks)
  (vet-sut-sam kernel parent-stencil ideal hooks #'peek-cache))

(define-compiler-cache play-cache +small-cache+)
(defun play-driver (kernel parent-stencil ideal hooks)
  (vrf-sut-sam kernel parent-stencil ideal hooks #'play-cache))

(define-compiler-cache rest-cache +small-cache+)
(defun rest-driver (kernel parent-stencil ideal hooks)
  (vet-sut-sam kernel parent-stencil ideal hooks #'rest-cache))

; layer six jet functions

; adapted from
; https://rosettacode.org/wiki/Longest_common_subsequence#Common_Lisp
(defun loss (list-1 list-2)
  (let* ((array1 (nlist->vector list-1))
         (array2 (nlist->vector list-2))
         (l1 (length array1))
         (l2 (length array2))
         (results (make-array (list l1 l2) :initial-element nil)))
    (declare (dynamic-extent results))
    (labels ((lcs (start1 start2)
               ;; if either sequence is empty
               (if (or (eql start1 l1) (eql start2 l2))
                   '(0 . 0)
                   ;; otherwise, return any memoized value
                   (or (aref results start1 start2)
                       (setf (aref results start1 start2)
                             (if (same (aref array1 start1) (aref array2 start2))
                                 ;; if they start with the same element,
                                 ;; move forward in both sequences
                                 (destructuring-bind (seq . len)
                                   (lcs (1+ start1) (1+ start2))
                                   (cons
                                     (slim-cons (aref array1 start1) seq)
                                     (1+ len)))
                                 ;; otherwise, move ahead in each separately,
                                 ;; and return the better result.
                                 (let ((a (lcs (1+ start1) start2))
                                       (b (lcs start1 (1+ start2))))
                                   (if (> (cdr a) (cdr b))
                                       a
                                       b))))))))
      (car (lcs 0 0)))))

(defun leer (cord)
  (declare (uint cord))
  (loop with len = (integer-length cord)
        with out = (stack-vector 10)
        for i = 0 then (+ 8 j)
        for j = (loop for pos from i below len by 8
                      for c = (ldb (byte 8 pos) cord)
                      when (= 10 c) return pos
                      finally (return pos))
        for line = (ldb (byte (- j i) i) cord)
        do (vector-push-extend line out)
        until (>= j len)
        finally (return (vector-onto-nlist out 0))))

; the actual jet tree

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
        (gfn %cap (@@a)
          (case a
            ((0 1) (error 'exit))
            (t (cap a))))
        (gfn %mas (@@a)
          (case a
            ((0 1) (error 'exit))
            (t (mas a))))
        (gfn %peg (@@a @@b)
          (if (zerop a)
              (error 'exit)
              (peg a b)))
        (jet-core
          %two 1 nil
          (raw-gate 2 turn)
          (math-gate 1 bex)
          (gfn %can (@@bloq list)
            (m (can bloq list)))
          (math-gate 3 cat)
          (gfn %cut (@@a (@@b @@c) @@d)
            (m (cut a b c d)))
          (math-gate 3 end)
          (math-gate 3 lsh)
          (math-gate 2 met)
          (gfn %rap (@@bloq list)
            (rap bloq list))
          (gfn %rep (@@bloq list)
            (rep bloq list))
          (gfn %rip (@@bloq @@a)
            (rip bloq a))
          (math-gate 3 rsh)
          (math-gate 2 con)
          (math-gate 2 dis)
          (math-gate 2 mix)
          (raw-gate 1 mug)
          (raw-gate 2 dor)
          (raw-gate 2 gor)
          (raw-gate 2 mor)
          (raw-gate 1 flop)
          (raw-gate 1 lent)
          (gfn %reap (@@times item)
            (reap times item))
          (gfn %jam (n)
            (jam (get-ideal n)))
          (gfn %cue (@@a)
            (cue-slim-from-int a))
          (jet-core
            %muk 27
            (deaf-gate-driver
              (lambda (sample)
                (dedata (@@syd @@len @@key) sample
                  (muk syd len key)))))
          (raw-gate 2 weld)
          (jet-core
            %tri 1 nil
            (math-gate 2 shal)
            (math-gate 1 shan)
            (math-gate 2 shay)
            (jet-core
              %qua 1 nil
              (gfn %trip (@@cord)
                (cord->tape cord))
              (gfn %mink ((subject formula) scry)
                (multiple-value-bind (tag val)
                  (with-fresh-memos (soft scry (nock subject formula)))
                  (ecase tag
                    (:success (slim-cons 0 val))
                    (:block (slim-cons 1 (tail val)))
                    (:error (slim-cons 2 (flop val))))))
              (jet-core
                %mule 3
                (lambda (kernel parent-stencil ideal hooks)
                  (declare (ignore ideal))
                  ; with partial evaluation (or just a 0 sample?) the
                  ; mute gate could be produced outside the driver
                  (break)
                  (when-let
                    (muth (resolve-hook %mute kernel parent-stencil hooks))
                    (trap
                      (lambda (core)
                        (when-let (mute (call-hook muth core))
                          (let ((sample (head (tail core))))
                            (nullify-exit (slam mute sample)))))))))
              (jet-core
                %pen 1 nil
                (jet-core
                  %ut 7 nil
                  (jet-core %crop 3 #'crop-driver)
                  (jet-core %fish 3 #'fish-driver)
                  (jet-core %fond 3 #'fond-driver)
                  (jet-core %fuse 3 #'fuse-driver)
                  (jet-core %mint 3 #'mint-driver)
                  (jet-core %mull 3 #'mull-driver)
                  (jet-core %peek 3 #'peek-driver)
                  (jet-core %play 3 #'play-driver)
                  (jet-core %rest 3 #'rest-driver)
                  (jet-core
                    %nest 3 nil
                    (jet-core
                      %nest-in 3 nil
                      (jet-core %nest-dext 1 #'nest-driver))))
                (jet-core
                  %hex 1 nil
                  (jet-core
                    %loss 31
                    (deaf-gate-driver
                      (lambda (sample)
                        (dedata (hel hev) sample
                          (loss hel hev)))))
                  (jet-core
                    %leer 31
                    (deaf-gate-driver
                      (lambda (sample)
                        (leer (cl-integer sample))))))))))))))

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

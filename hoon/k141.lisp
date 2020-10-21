(defpackage #:urbit/hoon/k141
  (:use #:cl #:urbit/nock/common #:urbit/nock/axis #:urbit/nock/math
        #:urbit/nock/cord #:urbit/nock/mug #:urbit/nock/jets #:urbit/nock/data
        #:urbit/nock/nock #:urbit/nock/equality #:urbit/nock/world
        #:urbit/nock/data/slimatom #:urbit/nock/data/slimcell
        #:urbit/hoon/tape #:urbit/hoon/cache #:urbit/hoon/syntax
        #:urbit/hoon/serial #:urbit/hoon/hints)
  (:import-from #:alexandria #:when-let #:when-let* #:if-let)
  (:export #:defunary #:defbinary #:defgate #:load-k141
           #:hoon-jet-tree #:layer #:container #:leaf #:~/ 
           #:+add #:+dec #:+div #:+dvr #:+gte #:+gth #:+lte #:+lth
           #:+max #:+min #:+mod #:+mul #:+sub #:+cap #:+mas #:+peg
           #:+turn #:+bex #:+can #:+cat #:+cut #:+end #:+lsh #:+met
           #:+rap #:+rep #:+rip #:+rsh #:+con #:+dis #:+mix
           #:+mug #:+dor #:+gor #:+mor
           #:flop #:+flop #:+lent #:+reap #:+jam #:+cue #:+muk #:+weld
           #:+shal #:+shan #:+shay #:+trip #:+mink #:+mule
           #:+crop #:+fish #:+fond #:+fuse #:+mint
           #:+mull #:+peek #:+play #:+rest #:+nest
           #:+loss #:+leer))

(in-package #:urbit/hoon/k141)

(enable-cords)

; helpers for constructing jet trees and leaves for hoon runtimes

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

(defmacro sig-or-die (noun)
  `(unless (zerop (cl-integer ,noun))
     (error 'exit)))

(defmacro test-?~ (var)
  `(or (deep ,var)
       (progn
         (sig-or-die ,var)
         nil)))

(defmacro for-?~ ((var list) &body forms)
  (let ((s (gensym)))
    `(nlist-for ,s (test-?~ ,s) ,var ,list ,@forms)))

(defmacro nlist-for (cur while i list &body forms)
  `(loop for ,cur = ,list then (tail ,cur)
         while ,while
         for ,i = (head ,cur)
         do (progn ,@forms)))

; when you need a random access to a list (like to iterate over it backwards)
; strict means it will crash if the terminator isn't 0
(defmacro nlist->vector (list &key (strict t) (size 10))
  `(let ((vec (stack-vector ,size)))
     (,(if strict 'for-?~ 'for-?^)
       (i ,list)
       (vector-push-extend i vec))
     vec))

; typically hoon jet trees have a single root with no driver,
; and are named after the kelvin version
(defmacro hoon-jet-tree (kelvin &body children)
  (declare (uint kelvin))
  (let ((kname (string->cord (format nil "k~d" kelvin))))
    `(list
       (jet-root ,kname ,kelvin nil ,@children))))

; a container core (no driver)
(defmacro container (name parent &body children)
  `(jet-core ,(symbol-cord name) ,parent nil ,@children))

; a static container (hoon layer 1 etc)
(defmacro layer (name &body children)
  `(container ,name 1 ,@children))

; no children, named by a symbol, otherwise just jet-core
(defmacro leaf (name-symbol parent axis-fn)
  `(jet-core ,(symbol-cord name-symbol) ,parent ,axis-fn))

; leaf with registered parent at +>
(defmacro ~/ (name-symbol axis-fn)
  `(leaf ,name-symbol 3 ,axis-fn))

; common axis-fn (axis is whole battery)
(defun trap (core-fn)
  (lambda (axis)
    (when (= axis 1)
      core-fn)))

; common core-fn (throw away all but +<)
(defun sample (sample-fn)
  (lambda (core)
    (funcall sample-fn (head (tail core)))))

; trap + sample = (sample-fn -> axis-fn)
(defun gate (sample-fn)
  (trap (sample sample-fn)))

; defun wrapper with dedata
(defmacro defund (name bindings &body forms)
  (let ((arg (gensym)))
    `(defun ,name (,arg) (dedata ,bindings ,arg ,@forms))))

; just an alias
(defmacro @ (form)
  `(slim-malt ,form))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gensyms (n)
    (loop repeat n collecting (gensym)))
  (defun symbol-cord (sym)
    (string->cord (string-downcase (symbol-name sym)))))

; common pattern: extract tuple of n atoms, call a func, malt the result
(defmacro defmath (name arity fn)
  (let* ((names (loop for i below arity collecting (format nil "A~d" i)))
         (pats (loop for name in names
                     collecting (intern (format nil "@@~a" name))))
         (refs (loop for name in names
                     collecting (intern name))))
    `(defund ,name ,pats (@ (funcall ,fn ,@refs)))))

; math with 2 args
(defmacro defbinary (name fn)
  `(defmath ,name 2 ,fn))

; defun a deaf driver from an axis-fn
; deaf in the sense that it doesn't "hear" the extra jet info
(defmacro defdeaf (name axis-fn)
  `(defun ,name (kernel parent-stencil ideal hooks)
     (declare (ignore kernel parent-stencil ideal hooks))
     ,axis-fn))

(defmacro defgate (name sample-fn)
  `(defdeaf ,name (gate ,sample-fn)))

; conventions:
; name a function on the sample jetname+<
; name the core driver +jetname
(defbinary add+< #'add)
(defgate +add #'add+<)

; push a mean onto the stack and die
(defmacro exit-mean-leaf (str)
  (let* ((tape (string->tape str :cell-fn #'cons))
         (tank (cons %leaf tape)))
    `(exit-with (cons %mean (copy-tree ',tank)))))

(defund dec+< (@@a)
  (if (zerop a)
      (exit-mean-leaf "decrement-underflow")
      (@ (dec a))))
(defgate +dec #'dec+<)

(defund div+< (@@a @@b)
  (if (zerop b)
      (exit-mean-leaf "divide-by-zero")
      (@ (div a b))))
(defgate +div #'div+<)

(defund dvr+< (@@a @@b)
  (if (zerop b)
      (exit-mean-leaf "divide-by-zero")
      (multiple-value-bind (q r) (dvr a b)
        (slim-cons (@ q) (@ r)))))
(defgate +dvr #'dvr+<)

(defmacro defcmp (name fn)
  `(defund ,name (@@a @@b) (loob (funcall ,fn a b))))

(defcmp gte+< #'<=)
(defgate +gte #'gte+<)

(defcmp gth+< #'>)
(defgate +gth #'gth+<)

(defcmp lte+< #'<=)
(defgate +lte #'lte+<)

(defcmp lth+< #'<)
(defgate +lth #'lth+<)

; to avoid re-malting
(defund max+< (@a @b)
  (if (> (cl-integer b) (cl-integer a))
      b
      a))
(defgate +max #'max+<)

(defund min+< (@a @b)
  (if (< (cl-integer b) (cl-integer a))
      b
      a))
(defgate +min #'min+<)

(defund mod+< (@@a @@b)
  (if (zerop b)
      (error 'exit)
      (@ (hmod a b))))
(defgate +mod #'mod+<)

(defbinary mul+< #'mul)
(defgate +mul #'mul+<)

(defund sub+< (@@a @@b)
  (cond ((= a b) 0)
        ((> a b) (@ (sub a b)))
        (t (exit-mean-leaf "subtract-underflow"))))
(defgate +sub #'sub+<)

; no need to malt the result
(defund cap+< (@@a)
  (cap a))
(defgate +cap #'cap+<)

(defmacro defunary (name fn)
  `(defmath ,name 1 ,fn))

(defunary mas+< #'mas)
(defgate +mas #'mas+<)

(defbinary peg+< #'peg)
(defgate +peg #'peg+<)

; unpack sample as tuple and pass as individual arguments to wrapped fn
(defmacro defwrap (name arity fn)
  (let ((args (gensyms arity)))
    `(defund ,name ,args (funcall ,fn ,@args))))

(defmacro stack-vector (init-size)
  `(make-array ,init-size :adjustable t :fill-pointer 0))

(defun turn (list gate)
  (loop with slam = (make-slam gate)
        with vec = (nlist->vector list)
        for r = 0 then (slim-cons pro r)
        for i from (1- (length vec)) downto 0
        for pro = (funcall slam (aref vec i))
        finally (return r)))
(defwrap turn+< 2 #'turn)
(defgate +turn #'turn+<)

(defunary bex+< #'bex)
(defgate +bex #'bex+<)

; several functions take a bloq and a list and turn them into atoms
(defmacro defbloqlist (name fn)
  `(defund ,name (@@bloq list)
     (@ (funcall ,fn bloq list))))

(defun can (bloq list)
  (declare (uint bloq))
  (let ((r 0) (size 0))
    (for-?~ (pair list)
      (dedata (@@bloqs @@data) pair
        (setq r (logior r (lsh bloq size (end bloq bloqs data))))
        (setq size (+ size bloqs))))
    r))
(defbloqlist can+< #'can)
(defgate +can #'can+<)

(defun cat (a b c)
  (declare (uint a b c))
  (logior b (lsh a (met a b) c)))
(defmath cat+< 3 #'cat)
(defgate +cat #'cat+<)

(defun cut (bloq from-end bloq-count atom)
  (declare (uint bloq from-end bloq-count atom))
  (ldb (byte (ash bloq-count bloq)
             (ash from-end bloq))
       atom))
(defund cut+< (@@a (@@b @@c) @@d)
  (cut a b c d))
(defgate +cut #'cut+<)

(defmath end+< 3 #'end)
(defgate +end #'end+<)

(defmath lsh+< 3 #'lsh)
(defgate +lsh #'lsh+<)
(lsh+< '(3 1 . 255))

(defbinary met+< #'met)
(defgate +met #'met+<)

(defun rap (bloq list)
  (declare (uint bloq))
  (let ((r 0))
    (for-?~ (i list)
      (setq r (cat bloq r (cl-integer i))))
    r))
(defbloqlist rap+< #'rap)
(defgate +rap #'rap+<)

(defun rep (bloq list)
  (declare (uint bloq))
  (let ((size (ash 1 bloq))
        (c 0)
        (r 0))
    (for-?~ (i list)
      (setq r (logior r (ash (ldb (byte size 0) (cl-integer i)) c)))
      (setq c (+ c size)))
    r))
(defbloqlist rep+< #'rap)
(defgate +rep #'rap+<)

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
(defund rip+< (@@b @@a)
  (rip b a))
(defgate +rip #'rip+<)

(defmath rsh+< 3 #'rsh)
(defgate +rsh #'rsh+<)

(defbinary con+< #'con)
(defgate +con #'con+<)

(defbinary dis+< #'dis)
(defgate +dis #'dis+<)

(defbinary mix+< #'mix)
(defgate +mix #'mix+<)

(defwrap mug+< 1 #'mug)
(defgate +mug #'mug+<)

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
(defwrap dor+< 2 #'dor)
(defgate +dor #'dor+<)

(defun gor (a b)
  (let ((c (mug a))
        (d (mug b)))
    (if (= c d)
        (dor a b)
        (loob (< c d)))))
(defwrap gor+< 2 #'gor)
(defgate +gor #'gor+<)

(defun mor (a b)
  (let ((c (murmug (mug a)))
        (d (murmug (mug b))))
    (if (= c d)
        (dor a b)
        (loob (< c d)))))
(defwrap mor+< 2 #'mor)
(defgate +mor #'mor+<)

(defun flop (list)
  (let ((r 0))
    (for-?~ (i list)
      (setq r (slim-cons i r)))
    r))
(defwrap flop+< 1 #'flop)
(defgate +flop #'flop+<)

(defun lent (list)
  (let ((len 0))
    (for-?~ (i list)
      (incf len))
    len))
(defwrap lent+< 1 #'lent)
(defgate +lent #'lent+<)

(defun reap (times item)
  (declare (uint times))
  (loop for r = 0 then (slim-cons item r)
        repeat times
        finally (return r)))
(defund reap+< (@@times item)
  (reap times item))
(defgate +reap #'reap+<)

(defun jam+< (n)
  (jam (get-ideal n)))
(defgate +jam #'jam+<)

(defund cue+< (@@a)
  (cue-slim-from-int a))
(defgate +cue #'cue+<)

(defun muk (syd len key)
  (if (or (> (met 5 syd) 1)
          (> (met 0 len) 31)
          (> (met 3 key) len))
      (error 'exit)
      (let ((murmurhash:*hash-size* 32))
        (murmurhash:murmurhash key :seed syd))))
(defund muk+< (@@syd @@len @@key)
  ; no need to malt
  (muk syd len key))
(defgate +muk #'muk+<)


(defun vector-onto-nlist (vector list)
  (loop for n = list then (slim-cons a n)
        for i from (1- (length vector)) downto 0
        for a = (aref vector i)
        finally (return n)))

(defun weld (a b)
  (vector-onto-nlist (nlist->vector a) b))
(defwrap weld+< 2 #'weld)
(defgate +weld #'weld+<)

(defund trip+< (@@cord)
  (cord->tape cord))
(defgate +trip #'trip+<)

(defun mink (subject formula scry)
  (multiple-value-bind (tag val)
    (with-fresh-memos (soft scry (nock subject formula)))
    (ecase tag
      (:success (slim-cons 0 val))
      (:block (slim-cons 1 (tail val)))
      (:error (slim-cons 2 (flop val))))))
(defund mink+< ((subject formula) scry)
  (mink subject formula scry))
(defgate +mink #'mink+<)

(defun +mule (kernel parent-stencil ideal hooks)
  (declare (ignore ideal))
  ; with partial evaluation (or just a 0 sample?) the
  ; mute gate could be produced outside the driver
  (when-let (muth (resolve-hook %mute kernel parent-stencil hooks))
    (trap
      (lambda (core)
        (when-let (mute (call-hook muth core))
          (let ((sample (head (tail core))))
            (nullify-exit (slam mute sample))))))))
  
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

(defwrap loss+< 2 #'loss)
(defgate +loss #'loss+<)

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
(defund leer+< (@@cord)
  (leer cord))
(defgate +leer #'leer+<)

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

; each cached compiler function gets its own cache
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
(defun +nest (kernel parent-stencil ideal hooks)
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
(defun +crop (kernel parent-stencil ideal hooks)
  (vet-sut-sam kernel parent-stencil ideal hooks #'crop-cache))

(define-compiler-cache fish-cache +small-cache+)
(defun +fish (kernel parent-stencil ideal hooks)
  (vet-sut-sam kernel parent-stencil ideal hooks #'fish-cache))

(define-compiler-cache fond-cache +small-cache+)
(defun +fond (kernel parent-stencil ideal hooks)
  (vet-sut-sam kernel parent-stencil ideal hooks #'fond-cache))

(define-compiler-cache fuse-cache +small-cache+)
(defun +fuse (kernel parent-stencil ideal hooks)
  (vet-sut-sam kernel parent-stencil ideal hooks #'fuse-cache))

(define-compiler-cache mint-cache +large-cache+)
(defun +mint (kernel parent-stencil ideal hooks)
  (vrf-sut-sam kernel parent-stencil ideal hooks #'mint-cache))

(define-compiler-cache mull-cache +large-cache+)
(defun +mull (kernel parent-stencil ideal hooks)
  (vet-sut-sam kernel parent-stencil ideal hooks #'mull-cache))

(define-compiler-cache peek-cache +small-cache+)
(defun +peek (kernel parent-stencil ideal hooks)
  (vet-sut-sam kernel parent-stencil ideal hooks #'peek-cache))

(define-compiler-cache play-cache +small-cache+)
(defun +play (kernel parent-stencil ideal hooks)
  (vrf-sut-sam kernel parent-stencil ideal hooks #'play-cache))

(define-compiler-cache rest-cache +small-cache+)
(defun +rest (kernel parent-stencil ideal hooks)
  (vet-sut-sam kernel parent-stencil ideal hooks #'rest-cache))

(defun k141-hinter (tag clue next)
  (when clue
    (case tag
      (%slog +handle-slog+)
      (%memo (handle-memo next))
      ((%hunk %hand %mean %lose %spot) (handle-stack tag)))))

(defun load-k141 (jet-tree &optional (fast-hints-enabled t))
  (load-world :jet-tree jet-tree
              :hinter (if fast-hints-enabled
                          (compose-hinters #'fast-hinter #'k141-hinter)
                          #'k141-hinter)))

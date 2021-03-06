(defpackage #:urbit/hoon/k141
  (:use #:cl #:named-readtables
        #:urbit/nock/common #:urbit/nock/axis #:urbit/nock/math
        #:urbit/nock/cord #:urbit/nock/mug #:urbit/nock/jets #:urbit/nock/data
        #:urbit/nock/nock #:urbit/nock/equality #:urbit/nock/world
        #:urbit/nock/data/slimatom #:urbit/nock/data/slimcell
        #:urbit/hoon/tape #:urbit/hoon/cache #:urbit/hoon/syntax
        #:urbit/hoon/list #:urbit/hoon/ord #:urbit/hoon/nlr
        #:urbit/hoon/serial #:urbit/hoon/hints)
  (:import-from #:alexandria #:when-let #:when-let* #:if-let)
  (:export #:@ #:defunary #:defbinary #:defgate #:defund
           #:load-k141 #:debug-core
           #:hoon-jet-tree #:layer #:container #:leaf #:~/ 
           #:+add #:+dec #:+div #:+dvr #:+gte #:+gth #:+lte #:+lth
           #:+max #:+min #:+mod #:+mul #:+sub #:+cap #:+mas #:+peg
           #:+turn #:+bex #:+can #:+cat #:+cut #:+end #:+lsh #:+met
           #:+rap #:+rep #:+rip #:+rsh #:+con #:+dis #:+mix
           #:+mug #:+dor #:+gor #:+mor
           #:+flop #:+lent #:+reap #:+jam #:+cue #:+muk #:+weld
           #:+get-by #:+put-by #:+del-by
           #:+has-in #:+put-in #:+del-in
           #:+shal #:+shan #:+shay #:+trip #:+mink #:+mule
           #:+crop #:+fish #:+fond #:+fuse #:+mint
           #:+mull #:+peek #:+play #:+rest #:+nest
           #:+loss #:+leer #:+lore))

(in-package #:urbit/hoon/k141)
(in-readtable hoon)

; helpers for constructing jet trees and leaves for hoon runtimes

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
  (let ((arg (gensym "sample")))
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
  `(exit-with [%mean [1 %leaf (string->tape ,str)] 0]))

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

(defcmp gte+< #'>=)
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
  (?< (zerop b))
  (@ (hmod a b)))
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

(defwrap turn+< 2 (lambda (list gate) (turn list (make-slam gate))))
(defgate +turn #'turn+<)

(defunary bex+< #'bex)
(defgate +bex #'bex+<)

; several functions take a bloq and a list and turn them into atoms
(defmacro defbloqlist (name fn)
  `(defund ,name (@@bloq list)
     (@ (funcall ,fn bloq list))))

(defbloqlist can+< #'can)
(defgate +can #'can+<)

(defmath cat+< 3 #'cat)
(defgate +cat #'cat+<)

(defund cut+< (@@a (@@b @@c) @@d)
  (cut a b c d))
(defgate +cut #'cut+<)

(defmath end+< 3 #'end)
(defgate +end #'end+<)

(defmath lsh+< 3 #'lsh)
(defgate +lsh #'lsh+<)

(defbinary met+< #'met)
(defgate +met #'met+<)

(defbloqlist rap+< #'rap)
(defgate +rap #'rap+<)


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
                then [(funcall malt part) r]
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

(defmacro deford (fn +< +)
  `(progn
     (defwrap ,+< 2 (lambda (a b) (loob (,fn a b))))
     (defgate ,+ #',+<)))

(deford dor dor+< +dor)
(deford gor gor+< +gor)
(deford mor mor+< +mor)

(defwrap flop+< 1 #'flop)
(defgate +flop #'flop+<)

(defwrap lent+< 1 #'lent)
(defgate +lent #'lent+<)

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

(defmacro defax (name parts &body forms)
  (destructuring-bind (k p i h c) (loop repeat 5 collect (gensym))
    `(defun ,name (,k ,p ,i ,h)
       (declare (ignore ,k ,p ,i ,h))
       (trap
         (lambda (,c)
           (deaxis ,parts ,c ,@forms))))))

(defax +del-by ((a 30) (b 6))
  (del-by a b))

(defax +get-by ((a 30) (b 6))
  (get-by a b))

(defax +put-by ((a 30) (b 12) (c 13))
  (put-by a b c))

(defax +has-in ((a 30) (b 6))
  (loob (has-in a b)))

(defax +put-in ((a 30) (b 6))
  (put-in a b))

(defax +del-in ((a 30) (b 6))
  (del-in a b))

(defund trip+< (@@cord)
  (cord->tape cord))
(defgate +trip #'trip+<)

(defun mink (subject formula scry)
  (multiple-value-bind (tag val)
    (with-fresh-memos (soft scry (nock subject formula)))
    (ecase tag
      (:success [0 val])
      (:block [1 (tail val)])
      (:error (loop for tax = 0 then [i tax]
                    for i in val
                    finally (return [2 tax]))))))
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
        with out = (make-array 10 :adjustable t :fill-pointer 0)
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

; lore and leer are subtly incompatible wrt to embedded null bytes,
; but we haven't written a version of lore yet and it's going away.
(defgate +lore #'leer+<)

; "partial memoization" for the hoon compiler
; using the memo cache causes lots of cache interference
; performance is much better using a cache per stencil
; although this is an unbounded memory leak!
; TODO: some cleanup mechanism to empty the caches in between events
;       would mitigate that problem.
(defun partially-memoized-trap (battery extract &optional save-p)
  (trap
    (let ((cache (make-cache 1024 #'same))
          (compute (icell-function battery)))
      (lambda (core)
        (when-let (key (nullify-exit (funcall extract core)))
          (or (cache-get cache key)
              (let ((pro (funcall compute core)))
                (prog1 pro
                  ; save can be conditional (nest)
                  (when (or (null save-p)
                            (nullify-exit (funcall save-p core pro)))
                    (cache-put cache key pro))))))))))

; nest is weird in a couple respects
;   it's just the dext arm so it's nested in inner cores
;   it only memo-saves sometimes.
(defun +nest (kernel parent-stencil ideal hooks)
  (declare (ignore kernel parent-stencil hooks))
  (partially-memoized-trap
    ideal
    (lambda (core)
      (deaxis ((vet 4086) (sut 254) (ref 125)) core
        [vet sut ref]))
    (lambda (core pro)
      (case (cl-integer pro)
        (0 (deaxis ((reg 58)) core (empty reg)))
        (1 (deaxis ((seg 28)) core (empty seg)))))))

; everything else 1 gate deep, just extracts a key and saves unconditionally
; (defcap is short for "define decapitated")
(defmacro defcap (name &rest parts)
  `(defun ,name (kernel parent-stencil ideal hooks)
     (declare (ignore kernel parent-stencil hooks))
     (partially-memoized-trap
       ideal
       (lambda (core)
         (deaxis ,parts core
           [,@(mapcar #'car parts)])))))

(defcap +crop (vet 502) (sut 30) (ref 6))
(defcap +fish (vet 502) (sut 30) (ref 6))
(defcap +fond (vet 502) (sut 30) (way 12) (hyp 13))
(defcap +fuse (vet 502) (sut 30) (ref 6))
(defcap +mint (vrf 251) (sut 30) (gol 12) (gen 13))
(defcap +mull (vet 502) (sut 30) (gol 12) (dox 26) (gen 27))
(defcap +peek (vet 502) (sut 30) (way 12) (hyp 13))
(defcap +play (vrf 251) (sut 30) (gen 6))
(defcap +rest (vet 502) (sut 30) (gen 6))

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

; instruments a kernel-driver to fuzz all its arms
(defun debug-core (kernel-driver)
  (lambda (kernel parent-stencil ideal hooks)
    (let ((axis-driver
            (funcall kernel-driver kernel parent-stencil ideal hooks)))
      (lambda (axis)
        (let ((arm-driver (funcall axis-driver axis)))
          (fuzzy kernel ideal axis arm-driver))))))

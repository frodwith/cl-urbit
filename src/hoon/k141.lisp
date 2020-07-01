(defpackage #:urbit/hoon/k141
  (:use #:cl #:ironclad #:cl-intbytes
        #:urbit/jets #:urbit/common #:urbit/syntax #:urbit/mug
        #:urbit/convert #:urbit/math #:urbit/data #:urbit/hints
        #:urbit/nock #:urbit/equality #:urbit/serial #:urbit/world
        #:urbit/data/slimatom #:urbit/data/slimcell)
  (:import-from #:alexandria #:when-let #:when-let* #:if-let)
  (:export #:load-k141))

(in-package #:urbit/hoon/k141)

(enable-cords)

(defmacro exit-mean-leaf (str)
  (let* ((tape (string->tape str :cell-fn #'cons))
         (tank (cons %leaf tape)))
    `(exit-with (cons %mean (copy-tree ',tank)))))

(defmacro gfn (name pattern &body forms)
  (let ((sam (gensym)))
    `(jet-deaf-gate ,name
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

(defmacro nlist-end (list value)
  `(if (zerop (cl-integer ,list))
       (return ,value)
       (error 'exit)))

(defun nlist->vector (list)
  (loop with vec = (make-array 10 :adjustable t :fill-pointer 0)
        for n = list then (tail n)
        while (deep n)
        do (vector-push-extend (head n) vec)
        finally (nlist-end n vec)))

; jets that don't really belong anywhere else in the runtime just live
; here for now - later it might make sense to group them into modules

(defun can (bloq list)
  (declare (uint bloq))
  (loop for n = list then (tail n)
        while (deep n)
        for pair = (head n)
        for bloqs = (cl-integer (head pair))
        for data = (end bloq bloqs (cl-integer (tail pair)))
        for r = data then (logior (ash data size) r)
        summing bloqs into size
        finally (nlist-end n r)))

(defun cat (a b c)
  (declare (uint a b c))
  (logior b (lsh a (met a b) c)))

(defun rap (bloq list)
  (declare (uint bloq))
  (loop for n = list then (tail n)
        while (deep n)
        for i = (cl-integer (head n))
        for r = i then (cat bloq r i)
        finally (nlist-end n r)))

(defun rep (bloq list)
  (declare (uint bloq))
  (loop with size = (ash 1 bloq)
        for n = list then (tail n)
        while (deep n)
        for c upfrom 0 by size
        for i = (ldb (byte size 0) (cl-integer (head n)))
        for r = i then (logior r (ash i c))
        finally (nlist-end n r)))

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

(defun weld (a b)
  (loop with v = (nlist->vector a)
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

(defun flop (list)
  (loop for n = list then (tail n)
        for r = 0 then (slim-cons i r)
        while (deep n)
        for i = (head n)
        finally (nlist-end n r)))

(defun lent (list)
  (loop for i upfrom 0
        for n = list then (tail n)
        while (deep n)
        finally (nlist-end n i)))

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
        (setf (gethash ut-context top) (make-compiler-cache size)))
    (let* ((top (make-hash-table :test 'eq :weakness :key)))
      (setf (gethash ut-battery table) top)
      (setf (gethash ut-context top) (make-compiler-cache size)))))

; each cached compiler function gets its own versioned table,
; capped at a fixed number of entries, keyed on noun sameness
(defmacro define-compiler-cache (name size)
  (let ((table (intern (format nil "*~a-table*" name))))
    `(progn
       (defparameter ,table (make-hash-table :test 'eq :weakness :key))
       (defun ,name (ut-battery ut-context)
         (versioned-compiler-cache ,table ut-battery ut-context ,size)))))

; each cache is keyed by a noun key (get relevants pieces of subject)
; empty goes down to zero from the initial value, after which each put
; into the table is preceded by an eviction (clock algorithm)
(sb-ext:define-hash-table-test same mug)
(defstruct (compiler-cache (:constructor make-compiler-cache (empty)))
  (empty 0 :type (integer 0)) 
  (table (make-hash-table :test 'same) :type hash-table :read-only t)
  (clock nil :type list))

; we cycle around all the entries in the table, marking warm things cold
; until we find a cold thing, and evict it
(defun compiler-cache-evict (cache)
  (let ((table (compiler-cache-table cache)))
    (loop named outer 
          do (loop for ((key . node) . more) on (compiler-cache-clock cache)
                   do (if (car node)
                          (setf (car node) nil)
                          (progn
                            (setf (compiler-cache-clock cache) more)
                            (remhash key table)
                            (return-from outer))))
          do (setf (compiler-cache-clock cache) 
                   (loop for k being the hash-keys of table
                         using (hash-value v)
                         collect (cons k v))))))

(defun compiler-cache-lookup (cache key compute)
  (let ((table (compiler-cache-table cache)))
    (if-let (node (gethash key table))
      (progn ; the act of looking up a key causes it to be marked warm
        (setf (car node) t)
        (cdr node))
      (let ((value (funcall compute))
            (slots (compiler-cache-empty cache)))
        (if (zerop slots)
            (compiler-cache-evict cache)
            (setf (compiler-cache-empty cache) (1- slots)))
        (setf (gethash key table) (cons t value))
        value))))

(defun compute-gate (gate)
  (lambda ()
    (nock gate (head gate))))

(defun unpack-ut (ut-core)
  (let* ((upay (tail ut-core))
         (pen (get-ideal-cell (tail (tail upay)))))
    (values (head upay) pen (get-battery ut-core))))

(defun look-in (core ut-battery ut-context cache-fn noun-key)
  (compiler-cache-lookup (funcall cache-fn ut-battery ut-context)
    noun-key
    (compute-gate core)))

(defparameter +large-cache+ 1024)
(defparameter +small-cache+ 256)

; the cache sizes are arbitrary - tune them? i took a guess at which ones
; should be bigger, but the main idea is that you can size them independently,
; and they don't step on each others' values.
(define-compiler-cache nest-cache +large-cache+)
(defun nest-driver (kernel parent-stencil ideal hooks)
  (declare (ignore ideal))
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
              (look-in core but pen #'nest-cache
                       (slim-tuple seg reg vet sut ref)))))))))

(defun vet-sut-sam (kernel parent-stencil hooks cache-fn)
  (when-let (veth (resolve-hook %vet kernel parent-stencil hooks :skip 1))
    (trap
      (lambda (core)
        (let* ((pay (tail core))
               (sam (head pay))
               (ut (tail pay)))
          (when-let (vet (call-hook veth ut))
            (multiple-value-bind (sut but pen) (unpack-ut ut)
              (look-in core but pen cache-fn
                       (slim-tuple vet sut sam)))))))))

(defun vrf-sut-sam (kernel parent-stencil hooks cache-fn)
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
              (look-in core but pen cache-fn
                       (slim-tuple vet fab sut sam)))))))))

(define-compiler-cache crop-cache +small-cache+)
(defun crop-driver (kernel parent-stencil ideal hooks)
  (declare (ignore ideal))
  (vet-sut-sam kernel parent-stencil hooks #'crop-cache))

(define-compiler-cache fish-cache +small-cache+)
(defun fish-driver (kernel parent-stencil ideal hooks)
  (declare (ignore ideal))
  (vet-sut-sam kernel parent-stencil hooks #'fish-cache))

(define-compiler-cache fond-cache +small-cache+)
(defun fond-driver (kernel parent-stencil ideal hooks)
  (declare (ignore ideal))
  (vet-sut-sam kernel parent-stencil hooks #'fond-cache))

(define-compiler-cache fuse-cache +small-cache+)
(defun fuse-driver (kernel parent-stencil ideal hooks)
  (declare (ignore ideal))
  (vet-sut-sam kernel parent-stencil hooks #'fuse-cache))

(define-compiler-cache mint-cache +large-cache+)
(defun mint-driver (kernel parent-stencil ideal hooks)
  (declare (ignore ideal))
  (vrf-sut-sam kernel parent-stencil hooks #'mint-cache))

(define-compiler-cache mull-cache +large-cache+)
(defun mull-driver (kernel parent-stencil ideal hooks)
  (declare (ignore ideal))
  (vet-sut-sam kernel parent-stencil hooks #'mull-cache))

(define-compiler-cache peek-cache +small-cache+)
(defun peek-driver (kernel parent-stencil ideal hooks)
  (declare (ignore ideal))
  (vet-sut-sam kernel parent-stencil hooks #'peek-cache))

(define-compiler-cache play-cache +small-cache+)
(defun play-driver (kernel parent-stencil ideal hooks)
  (declare (ignore ideal))
  (vrf-sut-sam kernel parent-stencil hooks #'play-cache))

(define-compiler-cache rest-cache +small-cache+)
(defun rest-driver (kernel parent-stencil ideal hooks)
  (declare (ignore ideal))
  (vet-sut-sam kernel parent-stencil hooks #'rest-cache))

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
        (math-gate 1 cap)
        (math-gate 1 mas)
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
                    (:error (slim-cons 2 val)))))
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
                      (jet-core %nest-dext 1 #'nest-driver))))))))))))

(defun k141-hinter (tag clue next)
  (when clue
    (case tag
      (%slog +handle-slog+)
      (%memo (handle-memo next))
;      )))
      ((%hunk %hand %mean %lose %spot) (handle-stack tag)))))

(defun load-k141 (&optional fast-hints-enabled)
  (load-world :jet-tree +jets+
              :hinter (if fast-hints-enabled
                          (compose-hinters #'fast-hinter #'k141-hinter)
                          #'k141-hinter)))

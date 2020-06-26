(defpackage #:urbit/hoon/k141
  (:use #:cl #:ironclad #:cl-intbytes
        #:urbit/jets #:urbit/common #:urbit/syntax #:urbit/mug
        #:urbit/convert #:urbit/math #:urbit/data #:urbit/hints
        #:urbit/nock #:urbit/equality #:urbit/serial #:urbit/world
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
            (gate-driver
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
                (lambda (kernel parent-stencil hooks ideal)
                  (declare (ignore ideal))
                  (break)
                  (let ((mute (hook %mute kernel hooks parent-stencil)))
                    (lambda (axis)
                      (when (= axis 1)
                        (lambda (core)
                          (slam (funcall mute core)
                                (head (tail core))))))))))))))))

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

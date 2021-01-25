(defpackage #:urbit/nock/equality
  (:use #:cl #:urbit/nock/data #:urbit/nock/common #:urbit/nock/ideal)
  (:import-from #:alexandria #:if-let)
  (:export #:same))

(in-package #:urbit/nock/equality)

(defmacro neither-one-both (a b accessor
                         neither-form
                         ((one-a one-b one-value &optional flipped)
                          &body one-forms)
                         ((a-value b-value)
                          &body both-forms))
  (let ((an (gensym))
        (bn (gensym))
        (av (gensym))
        (bv (gensym))
        (one (gensym)))
    `(let* ((,an ,a)
            (,bn ,b)
            (,av (,accessor ,an))
            (,bv (,accessor ,bn)))
       (flet ((,one (,one-a ,one-b ,one-value ,@(when flipped (list flipped)))
                ,@one-forms))
         (if ,av
             (if ,bv
               (let ((,a-value ,av)
                     (,b-value ,bv))
                 ,@both-forms)
               (,one ,an ,bn ,av ,@(when flipped '(nil))))
             (if ,bv
               (,one ,bn ,an ,bv ,@(when flipped '(t)))
               ,neither-form))))))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf (macro-function 'nob) (macro-function 'neither-one-both)))

(defun copy-speed (a b)
  (nob a b valid-cached-speed
       nil
       ((a b as)
        (declare (ignore a))
        (setf (cached-speed b) as))
       ((as bs)
        (declare (ignore as bs))
        nil)))

; extra attention is paid to promoting ideals: without this assurance,
; we don't know that cells which have a cached speed necessarily have an
; icell head. Idealness is also highly desirable, so it's worth doing
; a little extra work during unification to preserve / enhance it.
(defmacro copy-part (part a b)
  (let ((aval (gensym))
        (bval (gensym))
        (apart (gensym))
        (bpart (gensym))
        (cideal (gensym)))
    `(let* ((,aval ,a)
            (,bval ,b)
            (,apart (,part ,aval)))
       (if (typep ,apart 'ideal)
           (setf (,part ,bval) ,apart)
           (let ((,bpart (,part ,bval)))
             (if (typep ,bpart 'ideal)
                 (setf (,part ,aval) ,bpart)
                 (let ((,cideal (or (cached-ideal ,apart)
                                    (cached-ideal ,bpart))))
                   (if ,cideal
                       (setf (,part ,aval) ,cideal (,part ,bval) ,cideal)
                       (setf (,part ,bval) ,apart)))))))))

(defun copy-parts (a b)
  (copy-part head a b)
  (copy-part tail a b))

; compare an ideal atom with a non-eql, non-ideal atom
(defun ideal-atom=mundane (i a)
  (if-let (m (cached-mug a))
    (ideal-atom=mugatom i a)
    (when (= (iint i) (cl-integer a))
          (setf (cached-ideal a) i)
          t)))

; compare and unify two atoms, ignoring the mug slot
(defun nomug-atom= (a b)
  (let ((ai (cl-integer a))
        (bi (cl-integer b)))
    (when (= ai bi)
      (setf (cl-integer b) ai)
      t)))

; compare a mugged atom with an unmugged atom
(defun mugatom=unmugatom (a b amug)
  (when (nomug-atom= a b)
    (setf (cached-mug b) amug)))

; compare an icell with a non-eq, non-ideal cell
(defun icell=mundane (i c)
  (if-let (m (cached-mug c))
    (and (= m (icell-mug i))
         (icell=mugcell i c))
    (icell=unmugcell i c)))

; compare an icell with a non-eq, non-ideal, unmugged cell
(defun icell=unmugcell (i c)
  (flet ((atomic (i a)
           (if-let (ai (cached-ideal a))
             (eql i ai)
             (ideal-atom=mundane i a)))
         (fast (i c)
           (if-let (ci (cached-ideal c))
             (shallow (eq i ci))
             (if-let (m (cached-mug c))
               (if (= m (icell-mug i))
                   (shallow (icell=mugcell i c))
                   :diff)
               :deep))))
    (when (cell= i c
            #'ideep #'deep
            #'icell-head #'head
            #'icell-tail #'tail
            #'atomic #'icell-copy #'fast)
      (icell-copy i c)
      t)))

; compare non-eq cells with equal mugs
(defun mugcell=mugcell (a b)
  (flet ((atomic (a b)
           (nob a b cached-ideal
                (nomug-atom= a b)
                ((a b ai)
                 (declare (ignore a))
                 (ideal-atom=mugatom ai b))
                ((ai bi) (eql ai bi))))
         (unify (a b)
           (copy-parts a b)
           (copy-speed a b))
         (fast (a b)
           (flet ((same-mug (a b)
                    (= (cached-mug a) (cached-mug b))))
             (nob a b cached-ideal
                  (if (same-mug a b) :deep :diff)
                  ((a b ai) (shallow (and (same-mug a b)
                                          (icell=mugcell ai b))))
                  ((ai bi) (shallow (eq ai bi)))))))
    (when (cell= a b
            #'deep #'deep
            #'head #'head
            #'tail #'tail
            #'atomic #'unify #'fast)
      (unify a b)
      t)))

; compare a two non-eq, nonideal cells (a mugged, b not).
(defun mugcell=unmugcell (a b amug)
  (flet ((atomic (a b)
           (nob a b cached-ideal
                (let ((am (cached-mug a))
                      (bm (cached-mug b)))
                  (if bm
                      (and (= bm am) (nomug-atom= a b))
                      (mugatom=unmugatom a b am)))
                ((a b ai)
                 (declare (ignore a))
                 (ideal-atom=mundane ai b))
                ((ai bi) (eql ai bi))))
         (unify (a b)
           (setf (cached-mug b) (cached-mug a))
           (copy-parts a b)
           (copy-speed a b))
         (fast (a b)
           (nob a b cached-ideal
                (if-let (m (cached-mug b))
                  (if (= m (cached-mug a))
                      :deep
                      :diff)
                  :deep)
                ((a b ai flipped)
                 (declare (ignore a))
                 (shallow
                   (if flipped ; means b is mugged
                       (icell=mugcell ai b)
                       (icell=mundane ai b))))
                ((ai bi) (shallow (eq ai bi))))))
    (when (cell= a b
           #'deep #'deep
           #'head #'head
           #'tail #'tail
           #'atomic #'unify #'fast)
      (setf (cached-mug b) amug)
      (copy-parts a b)
      (copy-speed a b)
      t)))

; compare two non-eq, non-ideal, unmugged cells
(defun unmugcell=unmugcell (a b)
  (flet ((atomic (a b)
           (nob a b cached-ideal
                (nob a b cached-mug
                     (nomug-atom= a b)
                     ((a b am) (mugatom=unmugatom a b am))
                     ((am bm) (and (= am bm) (nomug-atom= a b))))
                ((a b ai)
                 (declare (ignore a))
                 (ideal-atom=mundane ai b))
                ((ai bi) (eql ai bi))))
         (unify (a b)
           (nob a b cached-mug
                nil
                ((a b am)
                 (declare (ignore a))
                 (setf (cached-mug b) am))
                ((am bm)
                 (declare (ignore am bm))
                 nil))
           (copy-parts a b)
           (copy-speed a b))
         (fast (a b)
           (nob a b cached-ideal
                (nob a b cached-mug
                     :deep
                     ((a b am) (shallow (mugcell=unmugcell a b am)))
                     ((am bm) (shallow (and (= am bm)
                                            (mugcell=mugcell a b)))))
                ((a b ai)
                 (declare (ignore a))
                 (shallow (icell=mundane ai b)))
                ((ai bi) (shallow (eq ai bi))))))
    (when (cell= a b
            #'deep #'deep
            #'head #'head
            #'tail #'tail
            #'atomic #'unify #'fast)
      (copy-parts a b)
      (copy-speed a b)
      t)))

(defmacro deep= (adeep bdeep atoms cells)
  `(if ,adeep
       (when ,bdeep ,cells)
       (unless ,bdeep ,atoms)))

; compare an ideal with a non-ideal noun
(defun ideal=mundane (a b)
  (deep= (ideep a) (deep b)
         (ideal-atom=mundane a b)
         (icell=mundane a b)))

; compare non-eq non-ideal nouns with equal mugs
(defun mugged=mugged (a b)
  (deep= (deep a) (deep b)
         (nomug-atom= a b)
         (mugcell=mugcell a b)))

; compare two non-eq non-ideals, a mugged and b not
(defun mugged=unmugged (a b amug)
  (deep= (deep a) (deep b)
         (mugatom=unmugatom a b amug)
         (mugcell=unmugcell a b amug)))

; compare two non-eq, non-ideal, unmugged nouns
(defun unmugged=unmugged (a b)
  (deep= (deep a) (deep b)
         (nomug-atom= a b)
         (unmugcell=unmugcell a b)))

; compare two non-eql non-ideal nouns
(defun mundane=mundane (a b)
  (nob a b cached-mug
       (unmugged=unmugged a b)
       ((a b am)
        (mugged=unmugged a b am))
       ((am bm)
        (and (= am bm) (mugged=mugged a b)))))

; compare two nouns
(defun same (a b)
  (or (eql a b)
      (nob a b cached-ideal
           (mundane=mundane a b)
           ((a b ai)
            (declare (ignore a))
            (ideal=mundane ai b))
           ((ai bi)
            (when (eql ai bi)
              (when (ideep ai)
                (copy-speed a b))
              t)))))

(sb-ext:define-hash-table-test same urbit/nock/mug:mug)

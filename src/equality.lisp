(defpackage #:urbit/equality)

(defun copy-speed (a b)
  (let ((as (cached-speed a))
        (bs (cached-speed b)))
    (if as
        (unless bs (setf (cached-speed b) as))
        (when bs (setf (cached-speed a) bs)))))

(defun copy-parts (a b)
  (setf (head b) (head a))
  (setf (tail b) (tail a)))

; compare an iatom with a non-eql, non-ideal atom
(defun iatom=mundane (i a)
  (let ((m (cached-mug a)))
    (if m
        (iatom=mugatom i a) 
        (when (= (iatom-int i (cl-integer a)))
          (setf (cached-ideal a) i)
          t))))

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
           (or (eql i a)
               (if-let (ai (cached-ideal a))
                 (eql i ai)
                 (iatom=mundane i a))))
         (fast (i c)
           (if-let (ci (cached-ideal c))
             (shallow (eq i ci))
             (if-let (m (cached-mug c))
               (if (= m (icell-mug i))
                   (icell=mugcell i c)
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
           (or (eql a b)
               (let ((ai (cached-ideal a))
                     (bi (cached-ideal b)))
                 (if ai
                     (if bi
                         (eql ai bi)
                         (iatom=mugatom ai b))
                     (if bi
                         (iatom=mugatom bi a)
                         (nomug-atom= a b))))))
         (unify (a b)
           (copy-parts a b)
           (copy-speed a b))
         (fast (a b)
           (if (not (= (cached-mug a) (cached-mug b)))
               :diff
               (let ((ai (cached-ideal a))
                     (bi (cached-ideal b)))
                 (if ai
                     (shallow (if bi 
                                  (eq ai bi)
                                  (icell=mugcell ai b)))
                     (if bi
                         (shallow (icell=mugcell bi a))
                         :deep))))))
    (when (cell= a b
            #'deep #'deep
            #'head #'head
            #'tail #'tail
            #'atomic #'unify #'fast)
      (unify a b)
      t)))

(defun mugcell=unmugcell (a b am)
  (flet ((atomic (a b)
           (let ((ai (cached-ideal a))
                 (bi (cached-ideal b)))
             (if ai
                 (if bi
                     (eql ai bi)
                     (iatom=mundane ai b))
                 (if bi 
                     (iatom=mundane bi a)
                     (let ((am (cached-mug a))
                           (bm (cached-mug b)))
                       (if bm
                           (and (= bm am) (nomug-atom= a b))
                           (mugatom=unmugatom a b am)))))))
         (unify (a b)
           (setf (cached-mug b) (cached-mug a))
           (copy-parts a b)
           (copy-speed a b))
         (fast (a b)
           (let ((ai (cached-ideal a))
                 (bi (cached-ideal b)))
             (if ai
                 (shallow (if bi 
                              (eq ai bi)
                              (if-let (m (cached-mug b))
                                (and (= m (icell-mug ai))
                                     (icell=mugcell ai b))
                                (icell=unmugcell ai b))))
                 (if bi
                     (shallow (and (= (cached-mug a) (icell-mug bi))
                                   (icell=mugcell bi a)))
                     (if-let (m (cached-mug b))
                       (if (= m (cached-mug a))
                           :deep
                           :diff)))))))
    (when (cell= a b
           #'deep #'deep
           #'head #'head
           #'tail #'tail
           #'atomic #'unify #'fast)
      (setf (cached-mug b) am)
      (copy-parts a b)
      (copy-speed a b)
      t)))

; compare two non-eq, non-ideal, unmugged cells
(defun unmugcell=unmugcell (a b)
  (flet ((atomic (a b)
           (let ((ai (cached-ideal a))
                 (bi (cached-ideal b)))
             (if ai
                 (if bi
                     (eql ai bi)
                     (iatom=mundane ai b))
                 (if bi
                     (iatom=mundane bi a)
                     (let ((am (cached-mug a))
                           (bm (cached-mug b)))
                       (if am
                           (if bm
                               (and (= am bm) (nomug-atom= a b))
                               (mugatom=unmugatom a b am))
                           (if bm 
                               (mugatom=unmugatom b a bm)
                               (nomug-atom= a b))))))))
         (unify (a b)
           (let ((am (cached-mug a))
                 (bm (cached-mug b)))
             (if am
                 (unless bm (setf (cached-mug b) am))
                 (when bm (setf (cached-mug a) bm))))
           (copy-parts a b)
           (copy-speed a b))
         (fast (a b)
           (let ((ai (cached-ideal a))
                 (bi (cached-ideal b)))
             (if ai
                 (shallow
                   (if bi
                       (eq ai bi)
                       (icell=mundane ai b)))
                 (if bi
                     (shallow (icell=mundane bi a))
                     (let ((am (cached-mug a))
                           (bm (cached-mug b)))
                       (if am
                           (shallow
                             (if bm
                                 (and (= am bm) (mugcell=mugcell a b))
                                 (mugcell=unmugcell a b am)))
                           (if bm
                               (shallow (mugcell=unmugcell b a bm))
                               :deep))))))))
    (when (cell= a b
            #'deep #'deep
            #'head #'head
            #'tail #'tail
            #'atomic #'unify #'fast)
      (copy-parts a b)
      (copy-speed a b))))

(defmacro defselect= (name adeep bdeep atoms cells)
  (defun ,name (a b)
    (if (,adeep a)
        (when (,bdeep b) (,cells a b))
        (unless (,bdeep b) (,atoms a b)))))

; compare an ideal with a non-ideal noun
(defselect= ideal=mundane ideep deep iatom=mundane icell=mundane)

; compare non-eq nouns with equal mugs
(defselect= mugged=mugged deep deep nomug-atom= mugcell=mugcell)

(defun mugged=unmugged-atomic (a b)
  (mugatom=unmugatom a b (cached-mug a)))

(defun mugged=unmugged-cell (a b)
  (mugcell=unmugcell a b (cached-mug a)))

; compare two non-eq non-ideals, a mugged and b not
(defselect= mugged=unmugged deep deep mug=unmugged-atomic mug=unmugged-cell)

; compare two non-eq, non-ideal, unmugged nouns
(defselect= unmugged=unmugged deep deep nomug-atom= unmugcell=unmugcell)

; compare two non-eql non-ideal nouns
(defun mundane=mundane (a b)
  (let ((am (cached-mug a))
        (bm (cached-mug b)))
    (if am
        (if bm
            (and (= am bm)
                 (mugged=mugged a b))
            (mugged=unmugged a b))
        (if bm
            (mugged=unmugged b a)
            (unmugged=unmugged a b)))))

; compare two nouns
(defun same (a b)
  (or (eql a b)
      (let ((ai (cached-ideal a))
            (bi (cached-ideal b)))
        (if ai
            (if bi
                (when (eq ai bi)
                  (when (ideep ai) 
                    (copy-speed a b))
                  t)
                (ideal=mundane ai b))
            (if bi
                (ideal=mundane bi a)
                (mundane=mundane a b))))))

(in-package #:urbit/dashboard)

; a dashboard measures the speed of cores
(defstruct (dashboard (:constructor mkdash))
  (roots nil :type hash-table)
  (batteries nil :type hash-table))

(defun weak-vals ()
  (make-hash-table :test 'equal :weakness :value))

(defun make-dashboard (interner)
  (mkdash :interner interner
          :roots (weak-vals)
          :batteries (make-hash-table :test 'eq)))

(defstruct (dnode (:constructor mknode))
  (kernel nil :type kernel)
  (parent nil :type (or dashboard dnode))
  (children nil :type hash-table)
  (stencils nil :type hash-table))

(defun make-dnode (parent kernel)
  (mknode :kernel kernel
          :parent parent
          :children (weak-vals)
          :stencils (weak-vals)))

; since warm tables are weak and the cells themselves are weakly interned, we
; need to store references to registered batteries or they can be garbage
; collected (thus losing the registration information).
(defun save-battery (dashboard battery)
  (declare (type constant-cell battery))
  (setf (gethash battery (dashboard-batteries dashboard))
        battery))

; hooks is a list of (keyword nock-val) pairs
;   nock-val is something you can pass to (noun)
;               that compares correctly under (equal)
;   i.e. a list like '(9 2 0 1)
(defun process-hooks (pairs)
  (make-hooks
    (mapcar
      (lambda (p)
        (cons (car p)
              (formula (noun (cdr p)))))
      pairs)))

(defun find-root-dnode (roots name constant hooks)
  (cache-hash (cons (cons name constant) hooks) roots
    (make-dnode dash (make-root-kernel 
                       :name name
                       :constant constant
                       :hooks (process-hooks hooks)))))

(defun find-child-dnode (parent name axis hooks)
  (cache-hash (cons (cons name axis) hooks)
              (dnode-children parent-node)
    (let ((pkern (dnode-kernel parent))
          (phooks (process-hooks hooks)))
      (make-dnode parent
                  (if (and (= axis 1)
                           (typep parent 'static-kernel))
                    (make-static-child-kernel 
                      :name name
                      :parent parent
                      :hooks phooks)
                    (make-dynamic-child-kernel
                      :name name
                      :parent parent
                      :axis axis
                      :hooks hooks))))))

(defstruct stencil
  (node nil :type dnode)
  (noun nil :type constant-cell)
  (parent nil :type (or null stencil)))

(defun find-static-stencil (node unique-core parent-stencil)
  (cache-hash unique-core (dnode-stencils node)
    (make-stencil
      :node node
      :noun unique-core
      :parent parent-stencil)))

(defun find-dynamic-stencil (node unique-battery parent-stencil)
  (cache-hash (cons unique-battery parent-stencil)
              (dnode-stencils node)
    (make-stencil
      :node node
      :noun unique-battery
      :parent parent-stencil)))

(defun find-root-stencil (dash unique-core name hooks)
  (let* ((constant (to-integer (constant-cell-tail unique-core)))
         (node (find-root-dnode (dashboard-roots dash) name constant hooks)))
    (find-static-stencil node unique-core nil)))

(defun find-child-stencil (core name axis hooks)
  (let* ((parent-core (frag core axis))
         (parent-speed (essence parent-core)))
    (if (eq t parent-essence)
        (error 'exit)
        (let* ((parent-node (stencil-node parent-essence))
               (node (warm-child name axis parent-node hooks))
               (kernel (dnode-kernel node)))
          (etypecase kernel
            (static-child-kernel
              (warm-static-stencil node (unique core)))
            (dynamic-child-kernel
              (warm-dynamic-stencil node (unique-head core)
                                         parent-essence)))))))

(defun check-inner (stencil core)
  (if-let (essence (cached-essence core))
    (eq essence stencil)
    (let* ((noun (stencil-noun essence)) 
           (node (stencil-node essence))
           (kernel (dnode-kernel node))
           (match (etypecase kernel
                    (static-kernel
                      (same noun core))
                    (dynamic-child-kernel
                      (and (same noun (head core))
                           (check-inner (stencil-parent essence)
                                        (parent-core kernel core)))))))
      (when match
        (learn-essence core stencil)
        t))))

(defun check-stencil (stencil core)
  (handler-case (check-inner stencil core)
    (exit () nil)))

(defstruct slow-core
  (assumptions :type list)
  (edits :type list))

(deftype fast-core () 'stencil)
(deftype core-speed () '(or fast-core slow-core))

(defnoun-meta core-speed)

(defun measure (core)




(defun find-essence (unique-battery payload)
  (let* ((battery (battery-meta (nock-meta unique-battery)))
         (matcher (battery-meta-matcher battery)))
    (or (if (atomp payload)
            (match-root matcher (to-integer payload))
            (match-child matcher payload))
        (let ((axis (registered-axis matcher)))
          (if (null axis)
              (list (battery-meta-stability battery))
              (let ((essence (handler-case (essence (frag payload axis))
                               (oops () t)    ; non-cell object
                               (exit () t)))) ; nothing at axis
                (if (typep essence 'impossible)
                    essence
                    (cons (battery-meta-stability battery)
                          (etypecase essence
                            (strange essence)
                            (familiar nil))))))))))

(defun register-root (tree core name hooks)
  (if (or (atomp core)
          (atomp (head core))
          (cellp (tail core)))
      (error 'exit)
      (let* ((unique-core (unique core))
             (noun (constant-cell-head unique-core))
             (nock (constant-cell-nock-meta noun))
             (battery (nock-meta-battery-meta nock))
             (stencil (root-stencil table unique-core name hooks)))
        (setf (battery-meta-match battery)
              (add-root (battery-meta-match battery) stencil))
        (learn-essence core stencil)
        (battery-meta-destabilize battery)
        (save-battery tree noun))))

(defun register-child (tree core name axis hooks)
  (if (or (atomp core)
          (atomp (head core))
          (atomp (tail core)))
      (error 'exit)
      (let* ((noun (unique-head core))
             (battery (battery-meta (nock-meta noun)))
             (match (battery-meta-match battery))
             (stencil (child-stencil core name axis hooks)))
        (setf (battery-meta-match battery)
              (add-child (battery-meta-match battery) stencil))
        (learn-essence core stencil)
        (battery-meta-destabilize battery)
        (save-battery tree noun))))


;(deftype small-table (key-type value-type)
;  `(or null (cons ,key-type ,value-type) hash-table))
;
;(defun getsmall (small key)
;  (etypecase small
;    (null nil)
;    (cons (if (eq (car small) key) (cdr small)))
;    (hash-table (gethash key small))))
;
;(defsetf getsmall (key small-place) (value)
;  (let ((have (gensym)))
;    `(let ((,have (,small-place)))
;       (etypecase ,have
;         (null (setf ,small-place (cons ,key ,value)))
;         (cons ,(let ((table (gensym)))
;                  `(let ((,table (make-hash-table :test 'eq)))
;                     (setf (gethash (car ,have) table) (cdr ,have))
;                     (setf (gethash ,key ,table) ,value))))
;         (hash-table (setf (gethash ,key ,have) ,value))))))
;
;(defstruct (match (:constructor make-match (axis)))
;  (axis nil :type integer) ; axis within payload to find parent
;  (roots nil :type (small-table integer stencil))
;  (parents nil :type (small-table stencil stencil)))

;(defun match-add-root (match root-stencil)
;  (setf (getsmall (stencil-noun root-stencil) (match-roots match))
;        root-stencil))
;
;(defun match-add-child (match child-stencil)
;  (setf (getsmall (stencil-parent child-stencil) (match-parents match))
;        child-stencil))
;
;(defun match-root (match payload)
;  (getsmall payload (match-roots match)))
;
;(defun match-child (match payload)
;  (getsmall (essence (frag payload (match-axis match)))
;            (match-stencils match)))

;; XX: you can't have a battery that's both a root and a child, that violates
;; (at least in principle) the unique axis to parent constraint. therefore the
;; matcher type should be:
;; null (no matches for this battery)
;; single-parent axis + (cons stencil stencil)
;;    here, we can just check-stencil, don't have to call essence
;; multiple-parents axis + (hashtable)
;; single-root (cons integer stencil)
;; multiple-roots (hashtable)

  
;      defun constant-cell-match (calls constant-cell-battery from compiler??)
;                                (move constant-cell-battery to warm?)
;                                battery meta holds ref to match and
;                                discovery...


;  there's some confusion here - where's this other table?
;  the canonical place IS on the constant cell
;  there's a match or there isn't one
;  it's not a lazy field.
;  So when you're making the battery-meta object,
;  at that moment you need to look at your current registry. and then when you
;  register, you need to adjust the field on this object - if it was nil, it
;  needs to become not nil, and if it was a match object, it needs to be
;  adjusted to incorporate the new registration information.

;  what does that mean "look at your current registry?" doesn't that mean look
;  at the field on the constant cell??

;   seemingly at the "boot" of a context, all these batteries have nil matches,
;   and then you imperatively "register" everything in the registration history
;   (or run a bunch of fast hints, or whatever).

;   do we then write "register" next?

;  IMPORTANT NOTE: since we store registration on constant cells, and those are
;  weakly referenced by the interner, if we don't want to forget registration
;  information as soon as we garbage collect a no-longer-referenced battery, we
;  need to keep a set of registered batteries per context as strong gc roots
;   (eq hashtable on context)

;  actually, the tables in the warm tree have strong refererences,
;  since they maintain registration information (which we never want to forget)
;  and the stencil table has references to the batteries, so no problem
;  (no need for a separate set)

; registration is exactly the process of:
;  finding the right kernel object (find-[static|dynamic]-kernel)
;  asking for (and presumably storing) its stencil with a core 

; QUESTION: if i register the same noun-pattern with two different sets of
; hooks, what happens? two different names?

; the same question appears in different guise in hashboard.

; the broader question is: what if the jet registrations are ambiguous, and i
; could validly regard a core by more than one label?

; supposition: the priorities are inverted. the real identifying factor is the
; pattern of nouns. registrations mutate that information known about patterns.

; but that is precisely how spotting works...  registration mutates the spotting
; structure. but it also puts things in the stencils table in the warm node?

; we could simply disallow registering cores that already spot. what effect
; would this have on, say, changing the hooks in the ut fast hint in a running
; system? the battery which produced the resultant core would be different,
; since the hint would be different, no? different hints hopefully mean a
; different parent. so the hypothesis is: if we're trying to register a core
; with an already matching stencil, it's an error and we should not proceed.
; we could detect this error either by first spotting cores that are candidates
; for registration, or by checking for conflicts while traversing the match
; structure. seemingly, spotting first is easier.
; so at a registration site, it's something like: okay i made this core, let's
; try (essence core)...  okay, it came back with an essence. does it match what
; i was going to register? if so, silently continue. warn if not. or it didn't
; come back with an essence, and i go ahead and register it.

; actually maybe just spot first, and then don't proceed at all if it spots
; (just proceed silently). registration only happens to unspotted cores, then,
; so if you somehow change the hint you're out of luck. we could offer some kind
; of configurable restart - to proceed silently, audit the clue, or maybe even
; double-check hashboard.


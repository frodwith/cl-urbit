(in-package #:urbit/warm)

(defun make-warm-table ()
  (make-hash-table :test 'equal))

(defstruct (warm-node
             (:constructor cons-warm (kernel parent children stencils)))
  (kernel nil :type kernel)
  (parent nil :type (or null warm-node))
  (children nil :type hash-table)
  (stencils nil :type hash-table))

(defun make-warm-node (kernel &optional parent)
  (cons-warm kernel parent
             (make-warm-table)
             (make-warm-table)))

; hooks is a list of (keyword nock-val) pairs
;   nock-val is something you can pass to (noun)
;               that compares correctly under (equal)
;   i.e. a list like '(9 2 0 1)
(defun process-hooks (pairs)
  (hooks 
    (mapcar
      (lambda (p)
        (cons (car p)
              (formula (noun (cdr p)))))
      pairs)))

(defun warm-root (table name constant &optional hooks)
  (cache-hash (cons (cons name constant) hooks) table 
    (make-warm-node (root name constant (process-hooks hooks)))))

(defun warm-child (name axis parent-node &optional hooks)
  (cache-hash (cons (cons name axis) hooks) (warm-node-children parent-node)
    (let ((parent (warm-node-kernel parent-node))
          (phooks (process-hooks hooks)))
      (make-warm-node
        (if (and (= axis 3) (typep parent 'static-kernel))
            (static name parent phooks)
            (child name axis parent phooks))))))

(defstruct (stencil (:constructor cons-stencil (node noun parent)))
  (node nil :type warm-node)
  (noun nil :type constant-cell)
  (parent nil :type (or null stencil)))

(deftype gnosis () '(or boolean stencil))
(deftype essence () '(and (not null) gnosis))

(defnoun-meta essence)

(defun warm-node-static-stencil (node unique-core)
  (cache-hash unique-core (warm-node-stencils node)
    (cons-stencil node unique-core (warm-node-parent node))))

(defun warm-node-dynamic-stencil (node unique-battery parent-stencil)
  (cache-hash (cons unique-battery parent-stencil) (warm-node-stencils node)
    (cons-stencil node unique-battery parent-stencil)))

(defun check-inner (stencil core)
  (if-let (essence (cached-essence core))
    (eq essence stencil)
    (let* ((noun (stencil-noun essence)) 
           (node (stencil-node essence))
           (kernel (warm-node-kernel node))
           (match (etypecase kernel
                    (static-kernel
                      (same noun core))
                    (child-kernel
                      (and (same noun (head core))
                           (check-inner (stencil-parent essence)
                                        (parent-core kernel core)))))))
      (when match
        (learn-essence core stencil)
        t))))

(defun check-stencil (stencil core)
  (handler-case (check-inner stencil core)
    (exit () nil)))

(deftype small-table (key-type value-type)
  `(or null (cons ,key-type ,value-type) hash-table))

(defun getsmall (small key)
  (etypecase small
    (null nil)
    (cons (if (eq (car small) key) (cdr small)))
    (hash-table (gethash key small))))

(defsetf getsmall (key small-place) (value)
  (let ((have (gensym)))
    `(let ((,have (,small-place)))
       (etypecase ,have
         (null (setf ,small-place (cons ,key ,value)))
         (cons ,(let ((table (gensym)))
                  `(let ((,table (make-hash-table :test 'eq)))
                     (setf (gethash (car ,have) table) (cdr ,have))
                     (setf (gethash ,key ,table) ,value))))
         (hash-table (setf (gethash ,key ,have) ,value))))))

(defstruct (match (:constructor make-match (axis))) 
  (axis nil :type integer) ; axis within payload to find parent
  (roots nil :type (small-table integer stencil))
  (parents nil :type (small-table stencil stencil)))

(defun match-add-root (match root-stencil)
  (setf (getsmall (stencil-noun root-stencil) (match-roots match))
        root-stencil))

(defun match-add-child (match child-stencil)
  (setf (getsmall (stencil-parent child-stencil) (match-parents match))
        child-stencil))

(defun match-root (match payload)
  (getsmall payload (match-roots match)))

(defun match-child (match payload)
  (getsmall (essence (frag payload (match-axis match)))
            (match-stencils match)))

;; XX: you can't have a battery that's both a root and a child, that violates
;; (at least in principle) the unique axis to parent constraint. therefore the
;; matcher type should be:
;; null (no matches for this battery)
;; single-parent axis + (cons stencil stencil)
;;    here, we can just check-stencil, don't have to call essence
;; multiple-parents axis + (hashtable)
;; single-root (cons integer stencil)
;; multiple-roots (hashtable)

(defun register-root (table core name hooks)
  (if (or (atomp core)
          (atomp (head core))
          (cellp (tail core)))
      (error 'exit)
      (let* ((unique-core (unique core))
             (battery (constant-cell-head unique-core))
             (match (constant-cell-match battery))
             (stencil (root-stencil table unique-core name hooks)))
        (when (null match)
          (setf match (setf (constant-cell-match battery) (make-match 1))))
        (match-add-root match stencil)
        (learn-essence core stencil))))

(defun register-child (core name axis hooks)
  (if (or (atomp core)
          (atomp (head core))
          (atomp (tail core)))
      (error 'exit)
      (let* ((battery (unique-head core))
             (match (constant-cell-match battery)))
        (when (null match)
          (setf match (setf (constant-cell-match battery) (make-match axis))))
        (if (not (= axis (match-axis match)))
            (error 'exit)
            (let ((stencil (child-stencil core name axis hooks)))
              (match-add-child match stencil)
              (learn-essence core stencil))))))

  
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

(defun root-stencil (table unique-core name hooks)
  (let* ((constant (to-integer (constant-cell-tail unique-core)))
         (node (warm-root table name constant hooks)))
    (warm-node-static-stencil node unique-core)))

(defun child-stencil (core name axis hooks)
  (let* ((parent-core (frag core axis))
         (parent-essence (essence parent-core)))
    (if (eq t parent-essence)
        (error 'exit)
        (let* ((parent-node (stencil-node parent-essence))
               (node (warm-child name axis parent-node hooks))
               (kernel (warm-node-kernel node)))
          (etypecase kernel
            (static-kernel (warm-node-static-stencil node (unique core)))
            (child-kernel (warm-node-dynamic-stencil node (unique-head core)
                                                     parent-essence)))))))

(defun find-essence (unique-battery payload)
  (or (when-let (match (constant-cell-match unique-battery))
        (if (atomp payload)
            (match-root match payload)
            (match-child match payload)))
      t))

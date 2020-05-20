(defpackage #:urbit/ideal
  (:use #:cl #:urbit/data #:urbit/mug #:urbit/common)
  (:import-from #:urbit/math #:uint)
  (:import-from #:alexandria #:if-let #:when-let)
  (:export #:make-world #:valid-speed #:invalidate-battery
           #:world-stencils #:world-roots #:world-hinter #:world-stable
           #:kernel #:root-kernel #:child-kernel
           #:kernel-children #:kernel-driver #:kernel-name
           #:dynamic-kernel #:dynamic-kernel-axis
           #:stencil #:child-stencil #:child-stencil-parent
           #:stencil-hooks #:stencil-ideal #:stencil-kernel #:stencil-driver
           #:core #:fast #:slow #:slow-edits #:slow-assumptions #:make-slow 
           #:icell-battery #:battery #:battery-meter #:battery-unregistered
           #:battery-parent-axis #:battery-parents #:battery-roots
           #:imug #:iint 
           #:iatom #:iatom-mug #:iatom-int #:iatom=mugatom
           #:icell #:icell-head #:icell-tail
           #:icell-meta #:icell-mug #:icell-speed
           #:find-ideal #:find-cons
           #:get-ideal-atom #:get-ideal-cell #:get-ideal
           #:formula #:make-formula #:formula-func #:formula-form
           #:fat #:make-fat #:fat-formula
           #:icell=mugcell #:ideep #:icell-copy))

(in-package #:urbit/ideal)

; kernels and stencils are used by the jet system. they're defined here
; because ideals refer to them (via battery, core) and they refer to
; ideals, so they're circular concepts. the functions for manipulating
; them are in jets.lisp

(defstruct kernel
  (name nil :type uint :read-only t)
  (driver nil :type (or null function) :read-only t)
  (children (make-hash-table :test 'equal) :read-only t))

(defstruct (root-kernel
             (:include kernel)
             (:constructor root-kernel (constant name driver)))
  (constant nil :type uint :read-only t))

(defstruct (child-kernel 
             (:include kernel)
             (:constructor child-kernel (parent name driver)))
  (parent nil :type kernel :read-only t))

(defstruct (dynamic-kernel
             (:include child-kernel)
             (:constructor dynamic-kernel (parent name axis driver)))
  (axis nil :type uint :read-only t))

(defstruct (stencil (:constructor stencil (ideal hooks kernel driver)))
  (ideal nil :type ideal :read-only t) ; battery or static core (see kernel)
  (hooks nil :type ideal :read-only t) ; unprocessed hook list
  (kernel nil :type kernel :read-only t)
  (driver nil :type (or null function) :read-only t))

(defstruct (child-stencil
             (:include stencil)
             (:constructor child-stencil (parent ideal hooks kernel driver)))
  (parent nil :type stencil :read-only t))

; ideals - values which represent "ideal" noun values. while there may be many
; cell objects in memory with a head of 0 and a tail of 0, there is only one
; ideal [0 0]. It is often valuable to use an ideal as the key in an eq
; hashtable, and certain kinds of metadata (particularly compiled nock formulae
; / battery information) are stored directly on ideals because looking them up
; is in the interpreter hot code path. The data protocol has a CACHED-IDEAL
; accessor to give this system a place to cache its lookups.

; in particular, core speed is generally not accessed through the ideal
; (though this is possible) because ideal-finding is expensive (a hashtable
; lookup+) when not cached, and so not suitable for values with varying parts
; (i.e. the samples of gates)

(deftype assumption () '(cons boolean null))

(defun make-assumption ()
  (cons t nil))

(defun check-assumption (a)
  (car a))

(defun invalidate-assumption (a)
  (setf (car a) nil))

(defstruct (slow (:constructor make-slow (assumptions edits)))
  (assumptions nil :read-only t :type list)
  (edits nil :read-only t :type list))

(deftype fast () 'stencil)
(deftype core () '(or fast slow))

(defun valid-speed (core)
  (let ((s (cached-speed core)))
    (typecase s
      (null nil)
      (fast s)
      (slow (if (every #'check-assumption (slow-assumptions s))
                s
                (progn
                  (setf (cached-speed core) nil)
                  nil))))))

(defun invalidate-battery (b)
  (invalidate-assumption (battery-unregistered b))
  (setf (battery-unregistered b) (make-assumption)))

; icell metadata
; TODO: roots and parents could probably be faster in most cases than
; hash-tables, since quite often they contain a small number of entries.
(defstruct battery
  (unregistered (make-assumption) :type assumption)
  (parent-axis 0 :type uint)
  (roots (make-hash-table :test 'eql) :type hash-table)
  (parents (make-hash-table :test 'eq) :type hash-table)
  (meter nil :type (or null function)))

(defstruct (formula (:constructor make-formula (form)))
  (form nil :read-only t :type (or list symbol))
  (func nil :type (or null function)))

(defstruct fat
  (battery nil :type (or null battery))
  (formula nil :type (or null formula))
  (core nil :type (or null core)))

(defstruct (icell (:constructor icons (head tail mug))
                  (:print-object print-icell))
  (head nil :read-only t :type ideal)
  (tail nil :read-only t :type ideal)
  (mug nil :read-only t :type mug)
  (meta nil :type (or null formula battery core fat)))

(defstruct (iatom (:constructor make-iatom (int mug)))
  (int nil :read-only t :type bignum)
  (mug nil :read-only t :type mug))

(deftype ideal-atom () '(or fixnum iatom))
(deftype ideal () '(or ideal-atom icell))

(defun print-icell (c &optional out)
  (print-unreadable-object (c out :type t :identity t)
    (format out "~x" (icell-mug c))))
;    (labels ((recur (o tail)
;               (if (ideep o)
;                   (progn
;                     (unless tail (write-char #\[ out))
;                     (recur (icell-head o) nil)
;                     (write-char #\space out)
;                     (recur (icell-tail o) t)
;                     (unless tail (write-char #\] out)))
;                   (prin1 o out))))
;      (recur c nil))))

(defun icell-battery (c)
  (macrolet ((with-b (&body forms)
               `(let ((b (make-battery)))
                  ,@forms
                  b)))
    (let ((m (icell-meta c)))
      (typecase m
        (battery m)
        (fat (or (fat-battery m)
                 (with-b (setf (fat-battery m) b))))
        (t (with-b (setf (icell-meta c)
                         (etypecase m
                           (null b)
                           (core (make-fat :core m :battery b))
                           (formula (make-fat :formula m :battery b))))))))))

(defun icell-speed (c)
  (let ((m (icell-meta c)))
    (typecase m
      (core m)
      (fat (fat-core m)))))

(defun (setf icell-speed) (val c)
  (let ((m (icell-meta c)))
    (typecase m
      ((or null core) (setf (icell-meta c) val))
      (formula (setf (icell-meta c) (make-fat :formula m :core val)))
      (battery (setf (icell-meta c) (make-fat :battery m :core val)))
      (fat (setf (fat-core m) val)))))

(defun iint (i)
  (etypecase i
    (fixnum i)
    (iatom (iatom-int i))))

(defun imug (i)
  (declare (ideal i))
  (the mug
       (etypecase i
         (fixnum (murmug i))
         (iatom (iatom-mug i))
         (icell (icell-mug i)))))

(defun ideep (i)
  (declare (ideal i))
  (etypecase i
    (icell t)
    (ideal-atom nil)))

(defun iatom=mugatom (i a)
  (or (eql i a) ; takes care of actual fixnums (no cached mug)
      (and (not (cached-ideal a))
           (= (iatom-mug i) (cached-mug a))
           (= (iatom-int i) (cl-integer a))
           (progn
             (setf (cached-ideal a) i)
             t))))

(defun icell-copy (i c)
  (declare (icell i))
  (if-let (ispd (icell-speed i))
    (setf (cached-speed c) ispd)
    (when-let (cspd (valid-speed c))
      (setf (icell-speed i) cspd)))
  (setf (cached-ideal c) i))

; compare a non-eq icell and mugged cell with equal mugs
(defun icell=mugcell (i c)
  (declare (icell i))
  (if-let (ci (cached-ideal c))
    (eq i ci)
    (flet ((fast (i c)
             (if-let (ci (cached-ideal c))
               (shallow (eq i ci))
               (if (= (icell-mug i) (cached-mug c))
                   :deep
                   :diff))))
      (when (cell= i c
              #'ideep #'deep
              #'icell-head #'head
              #'icell-tail #'tail
              #'iatom=mugatom #'icell-copy #'fast)
        (icell-copy i c)))))

(defun cells-hash (c)
  (if (typep c 'icell)
      (icell-mug c)
      (mug c)))

(defun cells= (a b)
  (if (typep a 'icell)
      (if (typep b 'icell)
          (eq a b)
          (icell=mugcell a b))
      (icell=mugcell b a)))

(sb-ext:define-hash-table-test cells= cells-hash)

(defun atoms-hash (a)
  (if (typep a 'iatom)
      (iatom-mug a)
      (mug a)))

(defun atoms= (a b)
  (if (typep a 'iatom)
      (if (typep b 'iatom)
          (eq a b)
          (iatom=mugatom a b))
      (iatom=mugatom b a)))

(sb-ext:define-hash-table-test atoms= atoms-hash)

(defun ignore-all-hints (tag clue next)
  (declare (uint tag)
           ; clue is null for static hints, otherwise formula
           ((or null icell) clue)
           (icell next))
  (declare (ignore tag clue next))
  nil)

(defstruct world
  ; noun hash-consing, split into two tables because
  ; 1) smaller tables = faster lookups
  ; 2) distinguished groups = faster hash/comparison
  (atoms (make-hash-table :test 'atoms= :weakness :key) :read-only t)
  (cells (make-hash-table :test 'cells= :weakness :key) :read-only t)
  ; the whole deduplicated tree of installed kernels (see jets.lisp)
  (roots (make-hash-table :test 'equal) :read-only t)
  ; decisions about how to handle hints are per-world because ideals are
  (hinter #'ignore-all-hints :type function :read-only t)
  ; setting stable nil will cause slow speeds to check battery assumptions
  (stable t :type boolean :read-only t)
  ; stack/log of installed stencils, most recent first (again jets.lisp)
  (stencils nil :type list)) 

(defun hashed-ideal (table noun)
  (let ((found (gethash noun table)))
    (when found (setf (cached-ideal noun) found))
    found))

(defun create-iatom (atoms a)
  (let ((int (cl-integer a)))
    (etypecase int
      (fixnum (setf (cached-ideal a) int)
              int)
      (bignum (let ((big (make-iatom int (mug a))))
                (setf (cached-ideal a) big)
                (setf (gethash big atoms) big)
                big)))))

(defun create-icell (atoms cells mugged)
  (flet ((atomic (atom)
           (let ((iatom (create-iatom atoms atom)))
             (setf (cached-ideal atom) iatom)
             iatom))
         (fast (noun)
           (or (cached-ideal noun)
               (let ((d (deep noun)))
                 (values (hashed-ideal (if d cells atoms) noun) d))))
         (slow (cell head tail)
           (let ((icell (icons head tail (murmugs (imug head) (imug tail)))))
             (setf (cached-ideal cell) icell)
             (setf (gethash icell cells) icell)
             icell)))
    (sum-cell mugged #'atomic #'fast #'slow)))

(defun find-cons (world ihead itail)
  (find-ideal-cell world (cons ihead itail)))

(defun find-ideal-cell (world cell)
  (let ((cells (world-cells world)))
    (or (hashed-ideal cells cell)
        (create-icell (world-atoms world) cells cell))))

(defun find-ideal-atom (world a)
  (let ((atoms (world-atoms world)))
    (or (hashed-ideal atoms a)
        (create-iatom atoms a))))

(defun find-ideal (world noun)
  (if (deep noun)
      (find-ideal-cell world noun)
      (find-ideal-atom world noun)))

; world will not be evaluated unless no cached-ideal
(defmacro idealm (finder world noun)
  (let ((s (gensym)))
    `(let ((,s ,noun))
       (or (cached-ideal ,s)
           (,finder ,world ,s)))))

(defmacro get-ideal (world noun)
  `(idealm find-ideal ,world ,noun))

(defmacro get-ideal-atom (world a)
  `(idealm find-ideal-atom ,world ,a))

(defmacro get-ideal-cell (world cell)
  `(idealm find-ideal-cell ,world ,cell))

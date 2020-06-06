(defpackage #:urbit/ideal
  (:use #:cl #:urbit/data #:urbit/mug #:urbit/zig #:urbit/math #:urbit/common)
  (:import-from #:alexandria #:if-let #:when-let)
  (:export #:kernel #:kernel-name #:kernel-driver #:kernel-children
           #:root-kernel #:root-kernel-constant
           #:child-kernel #:child-kernel-parent
           #:dynamic-kernel #:dynamic-kernel-axis
           #:stencil #:stencil-ideal #:stencil-hooks
           #:stencil-kernel #:stencil-driver
           #:child-stencil #:child-stencil-parent
           #:assumption #:make-assumption #:assumption-valid
           #:core-speed #:speed-valid #:valid-cached-speed
           #:void #:mean #:fast #:spry #:slow #:slug #:stop
           #:make-slow #:slow-to #:slow-parent #:make-spry #:spry-valid
           #:match #:match-meter #:root-match #:child-match
           #:make-root-match #:root-match-constants
           #:make-child-match #:child-match-axis #:child-match-parents
           #:battery #:battery-stable #:battery-match
           #:formula #:make-formula #:formula-form #:formula-func
           #:ideal #:ideal-atom #:iatom #:make-iatom #:iatom-int #:iatom-mug
           #:icell #:icons #:icell-head #:icell-tail #:icell-mug #:icell-meta
           #:fat #:make-fat #:fat-formula
           #:icell-battery #:icell-speed #:iint #:imug #:ideep
           #:iatom=mugatom #:icell=mugcell #:icell-copy))

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
  (axis nil :type zig :read-only t))

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

; icell metadata
(defstruct assumption
  (valid t :type boolean))

(deftype core-speed ()
  '(or void   ; not a core (cell with atom head) 
       mean   ; battery unknown to jet system (most cores)
       fast   ; matches a stencil
       spry   ; child, fast parent
       slow   ; child, parent not fast
       slug   ; root, wrong constant
       stop)) ; payload wrong shape at zig

; we can never be fast til the parent is fast, so don't need an assumption
(defstruct (slow (:constructor make-slow (to parent)))
  (to nil :type zig :read-only t)
  (parent nil :type core-speed :read-only t))

; we're one measly registration away from being fast, maybe!
(defstruct (spry (:include slow)
                 (:constructor make-spry (to parent valid)))
  (valid nil :type assumption :read-only t))

(deftype fast () 'stencil)
(deftype mean () 'assumption)
(deftype slug () '(cons (eql :slug) assumption))
(deftype stop () 'zig)
(deftype void () '(eql :void))

(defun speed-valid (spd)
  (etypecase spd
    ((or void fast stop) t)
    (mean (assumption-valid spd))
    (spry (assumption-valid (spry-valid spd)))
    (slow (speed-valid (slow-parent spd)))
    (slug (assumption-valid (cdr spd)))))

(defun valid-cached-speed (core)
  (when-let (spd (cached-speed core))
    (if (speed-valid spd)
        spd
        (progn
          (setf (cached-speed core) nil)
          nil))))

(defstruct match
  (meter nil :type function))

(defstruct (root-match (:include match)
                       (:constructor make-root-match (constants meter)))
  (constants nil :type hash-table))

(defstruct (child-match (:include match)
                        (:constructor make-child-match (axis parents meter)))
  (axis nil :type zig)
  (parents nil :type hash-table))

(defstruct battery
  (stable (make-assumption) :type assumption)
  (match nil :type (or null match)))

(defstruct (formula (:constructor make-formula (form)))
  (form nil :read-only t :type (or list symbol))
  (func nil :type (or null function)))

(defstruct fat
  (battery nil :type (or null battery))
  (formula nil :type (or null formula))
  (speed nil :type (or null core-speed)))

(defstruct (icell (:constructor icons (head tail mug))
                  (:print-object print-icell))
  (head nil :read-only t :type ideal)
  (tail nil :read-only t :type ideal)
  (mug nil :read-only t :type mug)
  (meta nil :type (or null formula battery core-speed fat)))

(defstruct (iatom (:constructor make-iatom (int mug)))
  (int nil :read-only t :type bignum)
  (mug nil :read-only t :type mug))

(deftype ideal-atom () '(or fixnum iatom))
(deftype ideal () '(or ideal-atom icell))

(defparameter *pretty-print-icells* nil)

(defun print-icell (c &optional out)
  (print-unreadable-object (c out :type t :identity t)
    (if *pretty-print-icells*
        (labels ((recur (o tail)
                   (if (ideep o)
                       (progn
                         (unless tail (write-char #\[ out))
                         (recur (icell-head o) nil)
                         (write-char #\space out)
                         (recur (icell-tail o) t)
                         (unless tail (write-char #\] out)))
                       (prin1 o out))))
          (recur c nil))
        (format out "~x" (icell-mug c)))))

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
                           (core-speed (make-fat :speed m :battery b))
                           (formula (make-fat :formula m :battery b))))))))))

(defun icell-speed (c)
  (let ((m (icell-meta c)))
    (typecase m
      (core-speed m)
      (fat (fat-speed m)))))

(defun (setf icell-speed) (val c)
  (let ((m (icell-meta c)))
    (typecase m
      ((or null core-speed) (setf (icell-meta c) val))
      (formula (setf (icell-meta c) (make-fat :formula m :speed val)))
      (battery (setf (icell-meta c) (make-fat :battery m :speed val)))
      (fat (setf (fat-speed m) val)))))

(defun iint (i)
  (etypecase i
    (fixnum i)
    (iatom (iatom-int i))))

(defun imug (i)
  (declare (ideal i))
  (etypecase i
    (fixnum (murmug i))
    (iatom (iatom-mug i))
    (icell (icell-mug i))))

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
    (when-let (cspd (valid-cached-speed c))
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

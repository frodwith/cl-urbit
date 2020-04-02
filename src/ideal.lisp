(defpackage #:urbit/ideal
  (:use #:cl #:urbit/data #:urbit/mug #:urbit/control)
  (:export #:make-world #:ideal #:imug
           #:iatom #:iatom-mug #:iatom-int #:iatom=mugatom
           #:icell #:icell-mug #:icell-head #:icell-tail
           #:icell=mugcell #:ideep #:icell-copy))

(in-package #:urbit/ideal)

; TL;DR: Use MAKE-WORLD to create a uniqueness context, and IDEAL to find the
; ideal for any noun within that context.

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

; TODO: Full Unifying Equality
;       write data interface tests and pass them lisp objects
;       test this file (with no ideal data impl)
;       write an ideal data impl and test it
;       add battery/formula slots, write nock.lisp (nock/pull)

(defstruct (iatom (:constructor make-iatom (int mug)))
  (int nil :read-only t :type bignum)
  (mug nil :read-only t :type mug))

(defstruct (icell (:constructor icons (head tail mug)))
  (head nil :read-only t :type ideal)
  (tail nil :read-only t :type ideal)
  (mug nil :read-only t :type mug))

(deftype ideal () '(or fixnum iatom icell))

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
    ((or fixnum iatom) nil)))

(defun iatom=mugatom (i a)
  (or (eql i a)
      (and (not (cached-ideal a))
           (= (iatom-mug i) (cached-mug a))
           (= (iatom-int (cl-integer a)))
           (progn
             (setf (cached-ideal a) i)
             t))))

(defun icell-copy (i c)
  (declare (icell i))
  ; TODO: copy speed from c if we don't have and available
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

(defstruct (world (:constructor make-world ()))
  "the context in which uniqueness is ensured"
  ; split into two tables because
  ; 1) smaller tables = faster lookups
  ; 2) distinguished groups = faster hash/comparison
  (atoms (make-hash-table :test 'atoms= :weakness :key) :read-only t)
  (cells (make-hash-table :test 'cells= :weakness :key) :read-only t))

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

(defun ideal (world noun)
  (or (cached-ideal noun)
      (if (deep noun)
          (let ((cells (world-cells world)))
            (or (hashed-ideal cells noun)
                (create-icell (world-atoms world) cells noun)))
          (let ((atoms (world-atoms world)))
            (or (hashed-ideal atoms noun)
                (create-iatom atoms noun))))))

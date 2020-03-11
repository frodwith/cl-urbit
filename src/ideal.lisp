(defpackage #:urbit/ideal
  (:use #:cl #:urbit/data)
  (:export #:make-world #:ideal))

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
; (though this is possible) because ideal-finding is expensive when not cached,
; and so not suitable for values with varying parts (i.e. the samples of gates)

; TODO: write a data implementation (the lisp object interface maybe),
;       and test that we can deep/head/tail/cl-integer/mug on it. cool.
;       test this file (with no ideal noun impl)
;       write an ideal noun impl and test it
;       add battery/formula slots, write nock.lisp (nock/pull)

(defstruct icell
  (head :read-only t :type ideal)
  (tail :read-only t :type ideal)
  (mug :read-only t :type mug))

(defstruct iatom
  (int :read-only t :type bignum)
  (mug :read-only t :type mug))

(deftype ideal () '(or fixnum iatom icell))

(defun imug (i)
  (declare (ideal i))
  (the mug
       (etypecase i
         (fixnum (murmug i))
         (iatom (iatom-mug i))
         (icell (icell-mug i)))))

(defun iatom=mugged (i n)
  (declare (iatom i))
  (when (and (not deep n)
             (= (iatom-mug i) (mug n))
             (= (iatom-int i) (cl-integer n)))
    (setf (cached-ideal n) i)
    t))

(defstruct (icell=mugged-frame
             (:constructor icmf (waiting ideal noun))
             (:conc-name icmf)
             (waiting :type boolean)
             (ideal :type ideal :read-only t)
             (noun :read-only t)))

(defun icell=mugged (i n)
  (declare (icell i))
  (and (deep n)
       (= (mug n) (icell-mug i))
       (let* ((frame (icmf nil (icell-head i) (head n)))
              (stack (list frame
                           (icmf nil (icell-tail i) (tail n))
                           (icmf t i n)
                           nil)))
         (do () ((null frame) t)
             (let ((i (icmf-ideal frame))
                   (n (icmf-noun frame)))
               (if (icmf-waiting frame)
                   (progn (setf (cached-ideal n) i)
                          (pop stack)
                          (setq frame (car stack)))
                   (etypecase i
                     (fixnum (unless (= i n) (return nil)))
                     (iatom  (unless (iatom=mugged i n) (return nil)))
                     (icell  (unless (= (icell-mug i) (mug n)) (return nil))
                             (setf (icmf-waiting frame) t)
                             (push (icmf nil (icell-tail i) (tail n)) stack)
                             (setq frame (icmf nil (icell-head i) (head n)))
                             (push frame stack)))))))))

(defun ideal-hash (a)
  (typecase a
    (ideal (imug a))
    (t (mug a))))

(defun ideal= (a b)
  (typecase a
    (iatom (typecase b
             (iatom (eq a b))
             (icell nil)
             (t (iatom=mugged a b))))
    (icell (typecase b
             (iatom nil)
             (icell (eq a b))
             (t (icell=mugged a b))))
    (t (etypecase b
         (iatom (iatom=mugged b a))
         (icell (icell=mugged b a))))))

(define-hash-table-test ideal= ideal-hash)

(defun make-world ()
  "the context in which uniqueness is ensured"
  (make-hash-table :test 'ideal= :weakness :key))

(defun hashed-ideal (table noun)
  (let ((found (gethash noun table)))
    (when found (setf (cached-ideal noun) found))
    found))

(defun put-iatom (table a)
  (let ((i (make-iatom :int (cl-integer a) :mug (mug a))))
    (setf (cached-ideal a) i)
    (setf (gethash i table) i)
    i))

(defun create-ideal (table mugged)
  (if (not (deep mugged))
      (put-iatom table mugged)
      (let ((accum nil)
            (frame (cons 0 mugged))
            (stack (list frame nil)))
        (flet ((more (n)
                 (setq frame (cons 0 n))
                 (push frame stack))
               (retn (i)
                 (setq accum i)
                 (pop stack)
                 (setq frame (car stack))))
          (do ((null frame) accum)
              (ecase (car frame)
                ((0) (let* ((n (cdr frame))
                            (c (cached-ideal n)))
                       (if c
                           (retn c)
                           (let ((h (hashed-ideal table n)))
                             (if h
                                 (retn h)
                                 (if (not (deep n))
                                     (retn (put-iatom table n))
                                     (progn
                                       (setf (car frame) 1)
                                       (more (head n)))))))))
                ((1) (let ((n (cdr frame)))
                       (setf (car frame) 2)
                       (setf (cdr frame) (cons n accum))
                       (more (tail n))))
                ((2) (destructuring-bind (cdr frame) (n . ideal-head)
                       (let ((i (make-icell :head ideal-head
                                            :tail accum
                                            :mug (murmugs (imug ideal-head)
                                                          (imug accum)))))
                         (setf (cached-ideal n) i)
                         (setf (gethash i table) i)
                         (retn i))))))))))

(defun ideal (world noun)
  (or (cached-ideal noun)
      (hashed-ideal table noun)
      (create-ideal table noun)))

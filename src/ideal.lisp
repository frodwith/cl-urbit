(defpackage #:urbit/ideal
  (:use #:cl #:urbit/data #:urbit/mug)
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

(defun iatom=mugatom (i n)
  (declare (iatom i))
  (when (and (= (iatom-mug i) (mug n))
             (= (iatom-int i) (cl-integer n)))
    (setf (cached-ideal n) i)
    t))

(defstruct (icell=mugcell-frame
             (:constructor icmf (waiting ideal noun))
             (:conc-name icmf-))
  (waiting nil :type boolean)
  (ideal nil :type ideal :read-only t)
  (noun nil :read-only t))

(defun icell=mugcell (i n)
  (declare (icell i))
  (and (= (mug n) (icell-mug i))
       (let* ((frame (icmf nil (icell-head i) (head n)))
              (stack (list frame
                           (icmf nil (icell-tail i) (tail n))
                           (icmf t i n)
                           nil)))
         (flet ((next ()
                  (pop stack)
                  (setq frame (car stack))))
           (do () ((null frame) t)
               (let ((i (icmf-ideal frame))
                     (n (icmf-noun frame)))
                 (if (icmf-waiting frame)
                     (progn (setf (cached-ideal n) i)
                            (next))
                     (etypecase i
                       (fixnum (if (= i n)
                                   (next)
                                   (return nil)))
                       (iatom (if (and (not deep n) (iatom=mugatom i n))
                                  (next)
                                  (return nil)))
                       (icell (unless (= (icell-mug i) (mug n)) (return nil))
                              (setf (icmf-waiting frame) t)
                              (push (icmf nil (icell-tail i) (tail n)) stack)
                              (setq frame (icmf nil (icell-head i) (head n)))
                              (push frame stack))))))))))

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
      (atom-mug a)
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

(defun hashed-ideal (world deep noun)
  (let ((found (gethash noun (if deep
                                 (world-cells world)
                                 (world-atoms world)))))
    (when found (setf (cached-ideal noun) found))
    found))

(defun put-iatom (world a)
  (let ((int (cl-integer a)))
    (etypecase int
      (fixnum (setf (cached-ideal a) int)
              int)
      (bignum (let ((big (make-iatom int (mug a))))
                (setf (cached-ideal a) big)
                (setf (gethash big (world-atoms world) big))
                big)))))

(defun create-ideal (world deep mugged)
  (if (not deep)
      (put-iatom world mugged)
      (let* ((accum nil)
             (frame (cons 0 (head mugged)))
             (stack (list frame (cons 1 mugged) nil)))
        (flet ((more (n)
                 (setq frame (cons 0 n))
                 (push frame stack))
               (retn (i)
                 (setq accum i)
                 (pop stack)
                 (setq frame (car stack))))
          (do () ((null frame) accum)
              (ecase (car frame)
                (0 (let* ((n (cdr frame))
                          (c (cached-ideal n)))
                     (if c
                         (retn c)
                         (let* ((deep (deep n))
                                (has  (hashed-ideal world deep n)))
                           (if hash
                               (retn has)
                               (if (not deep)
                                   (retn (put-iatom world n))
                                   (progn
                                     (setf (car frame) 1)
                                     (more (head n)))))))))
                (1 (let ((n (cdr frame)))
                     (setf (car frame) 2)
                     (setf (cdr frame) (cons n accum))
                     (more (tail n))))
                (2 (destructuring-bind (n . ideal-head) (cdr frame)
                     (let ((i (icons ideal-head accum
                                     (murmugs (imug ideal-head)
                                              (imug accum)))))
                       (setf (cached-ideal n) i)
                       (setf (gethash i (world-cells world)) i)
                       (retn i))))))))))

(defun ideal (world noun)
  (or (cached-ideal noun)
      (let ((deep (deep noun)))
        (or (hashed-ideal world deep noun)
            (create-ideal world deep noun)))))

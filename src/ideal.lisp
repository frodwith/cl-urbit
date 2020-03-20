(defpackage #:urbit/ideal
  (:use #:cl #:urbit/data #:urbit/mug #:urbit/dostack)
  (:export #:make-world #:ideal #:imug
           #:iatom #:iatom-mug #:iatom-int #:iatom=mugatom
           #:icell #:icell-mug #:icell-head #:icell-tail #:icell=mugcell))

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

; TODO: write data interface tests and pass them lisp objects
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
  (and (= (icell-mug i) (mug n))
       (not (dostack
              (main (more (icmf t i n))
                    (more (icmf nil (icell-tail i) (tail n)))
                    (give (cons (icell-head i) (head n))))
              (give (pair)
                    (destructuring-bind (i . n) pair
                      (etypecase i
                        (fixnum (if (= i n)
                                    (take)
                                    (return t)))
                        (iatom (if (and (not (deep n))
                                        (iatom=mugatom i n))
                                   (take)
                                   (return t)))
                        (icell (unless (= (icell-mug i) (mug n)) (return t))
                               (more (icmf t i n))
                               (more (icmf nil (icell-tail i) (tail n)))
                               (give (cons (icell-head i) (head n)))))))
              (take (top)
                    (let ((i (icmf-ideal top))
                          (n (icmf-noun top)))
                      (if (icmf-waiting top)
                          (progn (setf (cached-ideal n) i)
                                 (take))
                          (give (cons i n)))))))))

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
  (dostack-accumulate
    (main (more (cons t mugged))
          (give (head mugged)))
    (give (n)
      (let ((c (cached-ideal n)))
        (when c (take c))
        (let* ((d (deep n))
               (h (hashed-ideal (if d cells atoms) n)))
          (when h (take h))
          (unless d (take (create-iatom atoms n)))
          (more (cons t n))
          (give (head n)))))
    (take (r top)
      (if (car top)
          (let ((c (cdr top)))
            (setf (car top) nil)
            (setf (cdr top) (cons c r))
            (give (tail c)))
          (destructuring-bind (c . h) (cdr top)
            (let ((i (icons h r (murmugs (imug h) (imug r)))))
              (setf (cached-ideal c) i)
              (setf (gethash i cells) i)
              (take i)))))))

(defun ideal (world noun)
  (or (cached-ideal noun)
      (if (deep noun)
          (let ((cells (world-cells world)))
            (or (hashed-ideal cells noun)
                (create-icell (world-atoms world) cells noun)))
          (let ((atoms (world-atoms world)))
            (or (hashed-ideal atoms noun)
                (create-iatom atoms noun))))))

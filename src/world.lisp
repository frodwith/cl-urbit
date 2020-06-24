(defpackage #:urbit/world
  (:use #:cl
        #:urbit/data #:urbit/ideal #:urbit/common #:urbit/math #:urbit/mug)
  (:export #:make-world #:world-stencils #:world-roots #:world-hinter
           #:find-ideal #:find-ideal-atom #:find-ideal-cell #:find-cons
           #:get-ideal #:get-ideal-atom #:get-ideal-cell
           #:in-world #:bottle #:*world*))

(in-package #:urbit/world)

(defvar *world*)

(defmacro in-world (world &body forms)
  `(let ((*world* ,world))
     ,@forms))

(defmacro bottle (&body forms)
  `(in-world (make-world) ,@forms))

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
          (ideal-atom=mugatom a b))
      (ideal-atom=mugatom b a)))

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
  ; stack/log of installed stencils, most recent first (see jets.lisp)
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

(defun find-ideal-atom (a)
  (let ((atoms (world-atoms *world*)))
    (or (hashed-ideal atoms a)
        (create-iatom atoms a))))

(defun get-ideal-atom (a)
  (or (cached-ideal a) (find-ideal-atom a)))

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

(defun find-ideal-cell (cell)
  (let ((cells (world-cells *world*)))
    (or (hashed-ideal cells cell)
        (create-icell (world-atoms *world*) cells cell))))

(defun get-ideal-cell (c)
  (or (cached-ideal c) (find-ideal-cell c)))

(defun find-ideal (noun)
  (if (deep noun)
      (find-ideal-cell noun)
      (find-ideal-atom noun)))

(defun get-ideal (noun)
  (or (cached-ideal noun) (find-ideal noun)))

(defun find-cons (ihead itail)
  (find-ideal-cell (cons ihead itail)))

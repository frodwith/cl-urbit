(defpackage #:urbit/world
  (:use #:cl #:urbit/data))

(defstruct world
  (ideal-table :type hash-table))

; TODO: commit something
;       switch tests to 5am
;       get editor integration working again
;       write a data implementation (the lisp object interface maybe),
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
  (noun :read-only t))

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

(defun hashmug (a)
  (typecase a
    (ideal (imug a))
    (t (mug a))))

(defun hash= (a b)
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

(define-hash-table-test hash= hashmug)

(defun new-world ()
  (make-world
    :ideal-table (make-hash-table :test 'hash= :weakness :key)))

(defun hashed-ideal (table noun)
  (let ((found (gethash noun table)))
    (when found (setf (cached-ideal noun) found))
    found))

(defun iatom-for (table a)
  (let ((i (make-iatom :int (cl-integer a) :mug (mug a))))
    (setf (cached-ideal a) i)
    (setf (gethash i table) i)
    i))

(defun make-ideal (table mugged)
  (if (not (deep mugged))
    (iatom-for table mugged)
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
                           (retn (iatom-for table n))
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

; this is recursive in the cache-miss case, could be explicit-stack
(defun find-ideal (world noun)
  (or (cached-ideal noun)
      (let ((table (world-ideal-table world)))
        (or (hashed-ideal table noun)
            (make-ideal table noun)))))

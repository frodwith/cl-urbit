(defpackage urbit/data/constant-cell
 (:use :cl)
 (:import-from :urbit/error :oops)
 (:import-from :urbit/formula :formula)
 (:import-from :urbit/mug :mug :cached-mug :murmug-two)
 (:import-from :urbit/cell :cellp :head :tail 
               :get-constant-cell :learn-constant-cell)
 (:import-from :urbit/equality :teach)
 (:import-from :urbit/data/constant-atom :constant-atom))

(in-package :urbit/data/constant-cell)

(defclass constant-cell ()
 ((head :initarg :head
        :reader chead
        :type noun)
  (tail :initarg :tail
        :reader ctail
        :type noun)
  (form :initform nil
        :accessor :rawform
        :type list)
  (code :initform nil
        :accessor :rawcode
        :type function)
  (mug  :initarg :mug
        :reader cmug
        :type (unsigned-byte 31))))

(defun make-constant-cell (head tail &optional mug)
 (make-instance 'constant-cell
  :head head
  :tail tail
  :mug (or mug (murmug-two (mug head) (mug tail)))))

(defmethod cellp ((a constant-cell))
 t)

(defmethod code (a)
 (or (rawcode a)
  (setf (rawcode a)
   (compile nil (qcell a)))))

(defmethod formula ((a constant-cell))
 (code a))

(defmethod head ((a constant-cell))
 (chead a))

(defmethod tail ((a constant-cell))
 (ctail a))

(defmethod cached-mug ((a constant-cell))
 (cmug a))

(defmethod teach ((a constant-cell) (b t))
 (learn-constant-cell b a))

(defmethod unify ((a constant-cell) (b constant-cell))
 (error 'oops))

(defmethod get-constant-cell ((a constant-cell))
 a)

(defconstant crash '(error 'exit))

(defun qcell (a)
 (or (rawform a)
  (setf (rawform a)
   (let ((op (head a))
         (ar (tail a)))
    (typecase op
     (constant-cell (qcons op ar))
     (fixnum
      (case op
       (0  (q0  ar))
       (1  (q1  ar))
       (2  (q2  ar))
       (3  (q3  ar))
       (4  (q4  ar))
       (5  (q5  ar))
       (6  (q6  ar))
       (7  (q7  ar))
       (8  (q8  ar))
       (9  (q9  ar))
       (10 (q10 ar))
       (11 (q11 ar))
       (12 (q12 ar))
       (t crash)))
     (t crash))))))

(defun qf (a)
 (typecase a
  (constant-cell (qcell a))
  (t crash)))

(defun qcons (head tail)
 `(dcons ,(qcell head) ,(qf tail)))

(defun qax (a)
 (case a
  (0 crash)
  (1 'a)
  (t (list (ecase (cap a)
            (2 'head)
            (3 'tail))
           (qax (mas a))))))

(defun q0 (a)
 (typecase a
  (fixnum (qax a))
  (constant-atom (qax (cnum a)))
  (t crash)))

(defun q1 (a)
 `(quote ,a))

(defun nc (a)
 (typecase a
  (constant-cell nil)
  (t crash)))

(defun q2 (a)
 (or (nc a)
  `(nock ,(qf (head a)) ,(qf (tail a)))))

(defun q3 (a)
 `(deep ,(qf a)))

(defun q4 (a)
 `(bump ,(qf a)))

(defun q5 (a)
 (or (nc a)
 `(same ,(qf (head a)) ,(qf (tail a)))))

(defun q6 (a)
 (or (nc a) (nc (tail a))
  (let ((bran (ctail a)))
   (or (nc bran)
    `(case ,(qf (head a))
      (0 ,(qf (head bran)))
      (1 ,(qf (tail bran)))
      (t ,crash))))))

(defun q7 (a)
 (or (nc a)
  `(let ((a ,(qf (head a))))
    ,(qf (tail a)))))

(defun q8 (a)
 (or (nc a)
  `(let ((a (dcons ,(qf (head a)) a)))
    ,(qf (tail a)))))

(defun q9 (a)
 crash)

(defun q10 (a)
 crash)

(defun q11 (a)
 crash)

(defun q12 (a)
 crash)

(defpackage cl-urbit/noun
 (:use :cl :cl-urbit/error)
 (:import-from :murmurhash :murmurhash)
 (:export :noun :cellp :atomp :head :tail :mug))

(in-package cl-urbit/noun)

(defgeneric atomp (a))
(defgeneric to-integer (a))
(defgeneric atom= (a b))
(defgeneric cellp (a))
(defgeneric head (a))
(defgeneric tail (a))
(defgeneric cached-mug (a))
(defgeneric compute-mug (a))

(defun nounp (a) (or (atomp a) (cellp a)))
(deftype noun () `(satisfies nounp))
(deftype cell () `(satisfies cellp))
(deftype natom () `(satisfies atomp))

; most things are not cells
(defmethod cellp ((a t)) nil)
(defmethod head ((a t)) (error (if (atomp a) 'exit 'oops)))
(defmethod tail ((a t)) (error (if (atomp a) 'exit 'oops)))

; nor are they atoms
(defmethod atomp ((a t)) nil)
(defmethod to-integer ((a t)) (error 'oops))
(defmethod atom= ((a t) (b t)) (error 'oops))

; things have no cached mug by default
(defmethod cached-mug ((a t)) nil)
(defun mug (a)
 (or (cached-mug a) (compute-mug a)))

; these all work on regular lisp numbers, not atoms.
; (so you have to unpack bigatoms, in particular)
(defun mask (bits)
 (1- (ash 1 bits)))

(defun low-bits (bits of)
 (logand of (mask bits)))

(defun end (bloq n a)
 (low-bits (ash n bloq) a))

(defun met (bloq a)
 (let ((bits (integer-length a)))
  (if (zerop bloq)
   a
   (let* ((shift (ash bits bloq))
          (trunc (ash a (- shift)))
          (left  (low-bits shift a)))
     (if (zerop left) trunc (1+ trunc))))))

(defun mix (a b)
 (logxor a b))

(defun lsh (bloq n a)
 (ash a (ash n bloq)))

(defun rsh (bloq n a)
 (ash a (- (ash n bloq))))

(defun murmug (key)
 (loop for syd upfrom #xcafebabe
       for haz = (murmurhash key :seed syd)
       for ham = (mix (ash haz -31) (low-bits 31 haz))
       unless (zerop ham) return ham))

; fixnums are atoms
(defmethod atomp ((a fixnum)) t)
(defmethod to-integer ((a fixnum)) a)
(defmethod compute-mug ((a fixnum)) (murmug a))
(defmethod atom= ((a fixnum) (b fixnum))
 (= a b))

(defclass bigatom ()
  ((num :initarg :num
        :accessor bnum
        :type bignum)
   (mug :initform nil
        :accessor bmug
        :type (unsigned-byte 31)))
  (:documentation "wrapping around bignum to cache mug"))

(defmethod atomp ((a bigatom)) t)
(defmethod to-integer ((a bigatom)) (bnum a))
(defmethod cached-mug ((a bigatom)) (bmug a))
(defmethod compute-mug ((a bigatom))
 (setf (bmug a) (murmug (bnum a))))

(defmethod atom= ((a bigatom) (b bigatom))
 (and (= (bnum a) (bnum b))
      (setf (bnum b) (bnum a))))

;; table that remembers when we've compared two nouns
(defun compair= (a b)
 (and (eq (car a) (car b))
      (eq (cdr a) (cdr b))))

(defun compair-hash (a)
 (mug-both (mug (car a)) (mug (cdr a))))

; SBCL only
(define-hashtable-test compair= compair-hash)

(defun no-comparison ()
 (make-hash-table :test 'compair=))

(defun compared (s a b)
 (let* ((key (cons a b))
        (had (gethash key s)))
  (unless had
   (setf (gethash key s) t))
  had))

;; helper functions for same
(defun same-in (a b)
 (if (eq a b)
  'yes
  (if (atomp a)
   (if (atomp b)
    (if (atom= a b) 'yes 'no)
    'no)
   (if (known-unequal a b) 'no 'maybe))))

(defun same-push (a b stack)
 (list (cons 'compare (cons (head a) (head b)))
       (cons 'compare (cons (tail a) (tail b)))
       (cons 'unify (cons a b))
       stack))

;; we have another iteration path for when it's a really really big noun,
;; that makes sure we don't do the same work more than once (to keep us
;; from comparing the same dags over and over)
(defun same-dag (stack)
 (loop with c  = (no-comparison)
       for top = (pop stack)
       for a   = (cadr top)
       for b   = (cddr top)
       do (case (car top)
           ((unify) (unify a b))
           ((compare)
            (unless (compared c a b)
             (case (same-in a b)
              ((no) (return nil))
              ((maybe) (setq stack (same-push a b stack)))))))
       while   stack
       finally (return t)))

;; XX todo - need to test this
;; unifying equality with explict stack and dag detection
(defun same (a b)
 (or (eq a b)
  (if (atomp a)
   (and (atomp b) (atom= a b))
   (and (cellp b)
    (loop with stack = (same-push a b '())
          for  top   = (pop stack)
          for  a     = (cadr top)
          for  b     = (cddr top)
          do (case (car top)
              ((unify) (unify a b))
              ((compare)
               (case (same-in a b)
                ((no) (return nil))
                ((maybe) (setq stack (same-push a b stack))))))
          repeat  #xffff  ;; arbitrary, but a good heuristic.
          while   stack
          finally (return (or (null stack) (same-dag stack))))))))

(defclass dynamic-cell ()
  ((head :initarg :head
         :accessor dhead
         :type noun)
   (tail :initarg :tail
         :accessor dtail
         :type noun)
   (mug :initarg :mug
        :initform nil
        :accessor dmug
        :type (unsigned-byte 31)))
  (:documentation "pair of nouns with cached mug"))

(defun mug-both (a b)
 (murmug (mix a (mix #x7fffffff b))))

; explicit stack traversal of a noun
;  quick should give an answer with no further noun traversal, or nil
;  combine should take the answers from two sides and combine them
(defun sum (a quick combine)
 (loop with stack = (list (cons 0 a))
       with r     = nil
       for top    = (pop stack)
       for item   = (cdr top)
       do (ecase (car top)
           ((0)
            (setq r (funcall quick item))
            (unless r
             (push (cons 1 item) stack)
             (push (cons 0 (head item)) stack)))
           ((1)
            (push (cons 2 r) stack)
            (push (cons 0 (tail item)) stack))
           ((2)
            (setq r (funcall combine item r))))
       while   stack
       finally (return r)))

(defun mug-cell (a)
 (sum a
  (lambda (a)
     (or (cached-mug a) (and (atomp a) (compute-mug a))))
  #'mug-both))

(defmethod cellp ((a dynamic-cell)) t)
(defmethod head ((a dynamic-cell)) (dhead a))
(defmethod tail ((a dynamic-cell)) (dtail a))
(defmethod cached-mug ((a dynamic-cell)) (dmug a))
(defmethod compute-mug ((a dynamic-cell))
 (setf (dmug a) (mug-cell a)))

(defgeneric to-noun (a))

(defmethod to-noun ((a cons))
 (let* ((head (car a))
        (tail (cdr a))
        (here (to-noun head)))
  (if (null tail)
   here
   (make-instance 'dynamic-cell :head here :tail (to-noun tail)))))

(defmethod to-noun ((a fixnum)) a)
(defmethod to-noun ((a bignum))
 (make-instance 'bigatom :num a))

(define-condition no-noun-coercion (oops) ())
(defmethod to-noun ((a t))
 (if (nounp a)
  a
  (error 'no-noun-coercion)))

(defun noun (&rest args)
 (to-noun args))

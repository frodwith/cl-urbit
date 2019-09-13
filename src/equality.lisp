(defpackage urbit/equality
 (:use :cl)
 (:import-from :urbit/mug :cached-mug)
 (:import-from :urbit/atom :atomp :to-integer)
 (:import-from :urbit/cell :cellp :head :tail))

(in-package :urbit/equality)

; tell an equal object everything you know
; return value is ignored
(defgeneric teach (a b))
(defmethod teach ((a t) (b t))
 nil)

; copy slots and return a representative
(defgeneric unify (a b))
(defmethod unify ((a t) (b t))
 (teach a b)
 (teach b a)
 a)

(defgeneric atom= (a b))
; implementations are encouraged to add specializations, this is the fallback
(defmethod atom= ((a t) (b t))
 (when (= (to-integer a) (to-integer b))
  (unify a b)))

;; fast-cell= should be specialized if you have a shortcut for known types.
;; mismatching noun type, eq, and mug shortcuts are applied in elsewhere,
;; so do not duplicate those checks in specializations.
;; Return values are yes, no, or maybe (symbols)
(defgeneric fast-cell= (a b))
(defmethod fast-cell= ((a t) (b t))
 'maybe)

(defun more (stack a b)
 (cons (cons 'compare (cons (head a) (head b)))
  (cons (cons 'compare (cons (tail a) (tail b)))
   (cons (cons 'unify (cons a b)) stack))))

(defun compare (a b)
 (if (eq a b)
  'yes
  (if (atomp a)
   (if (atomp b)
    (if (atom= a b) 'yes 'no)
    'no)
   (let ((ca (cached-mug a))
         (cb (cached-mug b)))
    (if (and ca cb (not (= ca cb)))
     'no
     (fast-cell= a b))))))

; unifying equality with explict stack
;   "dag detection" a la vere is only necessary if there are some nouns
;   (e.g. on the home road) that you don't unify
;   we unify everything, so dag detection is useless overhead
(defun same (a b)
 (or (eq a b)
  (if (atomp a)
   (and (atomp b) (atom= a b))
   (and (cellp b)
    (loop with stack = (more nil a b)
          for  top   = (pop stack)
          for  a     = (cadr top)
          for  b     = (cddr top)
          do (case (car top)
              ((unify) (unify a b))
              ((compare)
               (case (compare a b)
                ((no) (return nil))
                ((maybe) (setq stack (more stack a b))))))
          while   stack
          finally (return t))))))

(defpackage #:urbit/equality
  (:use :cl)
  (:import-from :urbit/mug :cached-mug)
  (:import-from :urbit/atom :atomp :to-integer)
  (:import-from :urbit/cell :cellp :head :tail :get-constant-cell))

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
          (let ((ka (get-constant-cell a))
                (kb (get-constant-cell b)))
            (if (and ka kb)
                (if (eq ka kb) 'yes 'no)
                (let ((ca (cached-mug a))
                      (cb (cached-mug b)))
                  (if (and ca cb (not (= ca cb)))
                      'no
                      'maybe)))))))

; unifying equality with explicit stack
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

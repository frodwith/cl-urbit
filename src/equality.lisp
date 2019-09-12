(defpackage cl-urbit/equality
 (:use :cl)
 (:import-from :cl-urbit/mug :cached-mug)
 (:import-from :cl-urbit/atom :atomp :atom=)
 (:import-from :cl-urbit/cell :cellp :fast-cell=))

(in-package cl-urbit/equality)

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

(defun more (stack a b)
 (cons (cons 'compare (cons (head a) (head b)))
  (cons (cons 'compare (cons (tail a) (tail b)))
   (cons (cons 'unify (cons a b)) stack))))

;; same will punt to finish after 64k iterations. finish will start keeping a
;; deduplication set and continue iterating on the stack until finishedj.
(defun finish (stack)
 (loop with done = (make-pairset)
       for top   = (pop stack)
       for a     = (cadr top)
       for b     = (cddr top)
       do (case (car top)
           ((unify) (unify a b))
           ((compare)
            (unless (has-pair done a b)
             (have-pair done a b)
             (case (compare a b)
              ((no) (return nil))
              ((maybe) (setq stack (more stack a b)))))))
       while   stack
       finally (return t)))

;; unifying equality with explict stack and dag detection
;; XX todo - need to test this
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
          repeat  #xffff  ;; arbitrary, but a good heuristic.
          while   stack
          finally (return (or (null stack) (finish stack))))))))

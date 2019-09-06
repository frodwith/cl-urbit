(defpackage cl-urbit/noun
 (:use :cl :cl-urbit/error))

(in-package cl-urbit/noun)

(defgeneric atomp (a))
(defgeneric to-integer (a))
(defgeneric atom= (a b))
(defgeneric cellp (a))
(defgeneric head (a))
(defgeneric tail (a))

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

(defgeneric to-noun (a))

(define-condition no-noun-coercion (oops) ())
(defmethod to-noun ((a t))
 (if (nounp a)
  a
  (error 'no-noun-coercion)))

(defun noun (&rest args)
 (to-noun args))

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

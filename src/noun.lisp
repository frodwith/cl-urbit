(defpackage urbit/noun
  (:use :cl)
  (:import-from :urbit/error :oops)
  (:import-from :urbit/atom :atomp)
  (:import-from :urbit/cell :cellp :head :tail))

(in-package :urbit/noun)

(defun nounp (a) 
  (or (atomp a) (cellp a)))

(deftype noun () 
  `(satisfies nounp))

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

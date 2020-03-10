(defpackage #:urbit/data
  (:use #:cl)
  (:import-from #:urbit/math 
  (:export #:noun-required #:atom-required #:cell-required
           #:deep #:head #:tail #:cl-integer #:cached-mug #:mug))

(in-package #:urbit/data)

; the data protocol: types which represent nouns should implement these methods

(define-condition noun-required (error)
  ((object :initarg #:given))

(define-condition atom-required (required) ())
(define-condition cell-required (required) ())

; say you're a noun by saying whether you're an atom or a cell
; implement EITHER cl-integer OR head/tail
(defgeneric deep (object)
  (:documentation "give a true value for a cell and a false value for an object")
  (:method (non-noun)
    (error 'noun-required :given non-noun))))

(defgeneric cl-integer (object)
  (:documentation "return a common-lisp integer (fixnum or bignum) representing this atom")
  (:method (non-atom)
    (error 'atom-required :given non-atom)))

(defgeneric (setf cl-integer) (num object)
  (:documentation "request that an atom change its representation (can be ignored)")
  (:method (num object)
    nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist name '(head tail)
    (eval `(defgeneric ,name (object)
             (:method (non-cell)
               (error 'cell-required :given non-cell))))
    (eval `(defgeneric (setf ,name) (value object)
             (:method (value object)
               nil)))))

(defgeneric cached-mug (object)
  (:documentation "give the mug stored for this object (or nil for none)")
  (:method (object) nil))

(defgeneric (setf cached-mug) (mug object)
  (:documentation "remember that this object's mug is <mug>, if possible")
  (:method (mug object) nil))

(defun mug-atom (a)
  (let ((m (murmug (cl-integer a))))
    (setf (cached-mug a) m)
    m))

(defun mug (n)
  "compute the mug (lazy hash) for a noun"
  (or (cached-mug n)
      (if (not (deep n))
        (mug-atom n)
        (let* ((accum nil)
               (frame (cons 0 (head n)))
               (stack (list frame (cons 1 n) nil)))
          (flet ((more (n)
                   (setq frame (cons 0 n))
                   (push frame stack))
                 (retn (m)
                   (pop stack)
                   (setq accum m)
                   (setq frame (car stack))))
            (do () ((null frame) accum)
              (ecase (car frame)
                ((0) (let* ((n (cdr frame))
                            (c (cached-mug n)))
                       (if c
                         (retn c)
                         (if (not (deep n))
                           (retn (mug-atom n))
                           (progn
                             (setf (car frame) 1)
                             (more (head n)))))))
                ((1) (let ((n (cdr frame)))
                       (setf (car frame) 2)
                       (setf (cdr frame) (cons n accum))
                       (more (tail n))))
                ((2) (destructuring-bind (n . m) (cdr frame)
                       (let ((m (murmugs m accum)))
                         (setf (cached-mug n) m)
                         (retn m)))))))))))

(defgeneric cached-ideal (object)
  (:documentation "gives the object we already remember is our ideal")
  (:method (object) nil))

(defgeneric (setf cached-ideal) (ideal object)
  (:documentation "remember an ideal")
  (:method (object) nil))

; find-ideal is a function that works on contexts, uses cached-ideal methods

; the cons cell implementation doesnt need to have its weak maps on context.
; since it's pointer equality, you're saying something like "don't reuse these",
; and they don't function as de-facto forgetful proxies. which they shouldn't
; have anyways, since they can unify etc. with contextualized nouns. so no need
; to pass context to mug to get that to work.

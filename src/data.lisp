(defpackage #:urbit/data
  (:use #:cl)
  (:import-from #:urbit/math #:murmug #:murmugs)
  (:export #:noun-required #:atom-required #:cell-required
           #:deep #:head #:tail #:cl-integer #:cached-mug
           #:mug))

(in-package #:urbit/data)

; the data protocol: types which represent nouns should implement these methods

(define-condition noun-required (error)
  ((object :initarg #:given))

(define-condition atom-required (noun-required) ())
(define-condition cell-required (noun-required) ())

; say you're a noun by saying whether you're an atom or a cell
(defgeneric deep (object)
  (:documentation "give a true value for a cell and a false value for an object")
  (:method (non-noun)
    (error 'noun-required :given non-noun))))

(defmacro defdata (name condition-symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defgeneric ,name (object)
       ; signal a condition
       (:method (o) (error ,condition-symbol :given o)))
     (defgeneric (setf ,name)
       ; ignore the provided value
       (:method (value object) value))))

; all nouns must implement
(defdata cached-mug 'noun-required)
(defdata cached-ideal 'noun-required)

; atom must implement
(defdata cl-integer 'atom-required)

; cells must implement
(defdata head 'cell-required)
(defdata tail 'cell-required)
(defdata cached-speed 'cell-required)
(defdata cached-ideal-head 'cell-required)

; we can provide the mug operation, but speed/ideal etc. will require
; further functionality provided in later layers. we only define the
; "slots" here.

(defun mug-atom (a)
  (let ((m (murmug (cl-integer a))))
    (setf (cached-mug a) m)
    m))

(defun mug (n)
  "get the mug (lazy hash) for a noun"
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

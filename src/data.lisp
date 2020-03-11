(defpackage #:urbit/data
  (:use #:cl)
  (:export #:noun-required #:atom-required #:cell-required
           #:deep #:head #:tail #:cl-integer
           #:cached-mug #:cached-ideal #:cached-battery #:cached-speed))

(in-package #:urbit/data)

; the data protocol: types which represent nouns should implement these methods

(define-condition noun-required (error)
  ((object :initarg #:given)))

(define-condition atom-required (noun-required) ())
(define-condition cell-required (noun-required) ())

; say you're a noun by saying whether you're an atom or a cell
(defgeneric deep (object)
  (:documentation "give a true value for a cell and a false value for an object")
  (:method (non-noun)
   (error 'noun-required :given non-noun)))

(defmacro defdata (name condition)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defgeneric ,name (object)
       ; signal a condition
       (:method (o) (error ,condition :given o)))
     (defgeneric (setf ,name) (value object)
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

; battery is a combination of head and ideal,
; so it has a reasonable default implementation,
; but override it if you can do better.

(defgeneric cached-battery (o)
  (:method (cell)
   (let ((found (cached-ideal (head cell))))
     (when found
       (setf (head cell) found)
       found))))

(defgeneric (setf cached-battery) (value object)
  (:method (value object)
   (setf (head object) value)
   value))

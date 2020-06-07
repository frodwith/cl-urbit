(defpackage #:urbit/data
  (:use #:cl #:urbit/math)
  (:export #:data-error #:unimplemented
           #:exit #:exit-stack #:atom-required #:cell-required
           #:deep #:head #:tail #:cl-integer #:dfrag
           #:cached-mug #:cached-ideal #:cached-battery #:cached-speed))

(in-package #:urbit/data)

; the data protocol: types which represent nouns should implement these methods

(define-condition data-error (error)
  ((object :initarg :given)))

(define-condition unimplemented (data-error)
  ((name :initarg :name :type symbol)))

(define-condition exit (data-error) 
  ((stack :initform nil :accessor exit-stack :type list)))

(define-condition atom-required (exit) ())
(define-condition cell-required (exit) ())

; say you're a noun by saying whether you're an atom or a cell
(defgeneric deep (object)
  (:documentation "give a true value for a cell or a false value for an atom")
  (:method (non-noun)
   (error 'unimplemented :name 'deep :given non-noun)))

(defmacro defdata (name kind)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defgeneric ,name (object)
       (:method (o) 
        ,(ecase kind
           (noun `(error 'unimplemented :name ',name :given o))
           (atom `(if (deep o)
                      (error 'atom-required :given o)
                      (error 'unimplemented :name ',name :given o)))
           (cell `(if (deep o)
                      (error 'unimplemented :name ',name :given o)
                      (error 'cell-required :given o))))))
     (defgeneric (setf ,name) (value object)
       ; ignore the provided value
       (:method (value object) value))))

(defdata cached-mug noun)
(defdata cached-ideal noun)

; atom must implement
(defdata cl-integer atom)

(defdata head cell)
(defdata tail cell)
(defdata cached-speed cell)

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

(defun dfrag (iax data)
  (case iax
    (0 (error 'exit))
    (1 data)
    (t (loop for d = data then (if tail (tail d) (head d))
             for tail in (pax iax)
             finally (return d)))))

(defpackage #:urbit/nock/data
  (:use #:cl #:urbit/nock/math #:urbit/nock/axis)
  (:export #:data-error #:unimplemented #:atom-required #:cell-required
           #:exit #:exit-with #:exit-stack #:nullify-exit
           #:deep #:head #:tail #:cl-integer #:fragment #:d0
           #:cached-mug #:cached-ideal #:cached-battery #:cached-speed))

(in-package #:urbit/nock/data)

; the data protocol: types which represent nouns should implement these methods

(define-condition data-error (error)
  ((object :initarg :given)))

(define-condition unimplemented (data-error)
  ((name :initarg :name :type symbol)))

(define-condition exit (data-error) 
  ((stack :initform 0 :accessor exit-stack)))

(defun exit-with (stack-item)
  (let ((e (make-condition 'exit)))
    (push stack-item (exit-stack e))
    (error e)))

(defmacro nullify-exit (&body forms)
  `(handler-case (progn ,@forms)
     (exit () nil)))

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

(defun fragment (axis data)
  (declare (decomposable axis))
  (for-axis (tail axis)
    (setq data (if tail (tail data) (head data))))
  data)

(defun d0 (axis data)
  (declare (uint axis))
  (case axis
    (0 (error 'exit))
    (1 data)
    (t (fragment axis data))))

(defpackage cl-urbit-worker/error
 (:use :cl)
 (:export :bail :exit :fail :oops))

(in-package cl-urbit-worker/error)

(define-condition bail (error) ())
(define-condition exit (bail) ())
(define-condition fail (bail) ())
(define-condition oops (bail) ())

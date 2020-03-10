(in-package #:urbit/error)

(define-condition bail (error) ())
(define-condition exit (bail) ())
(define-condition fail (bail) ())
(define-condition oops (bail) ())

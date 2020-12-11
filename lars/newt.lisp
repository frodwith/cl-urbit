(defpackage #:urbit/lars/newt
  (:use #:cl #:urbit/hoon/serial #:urbit/nock/world)
  (:export #:newt-read #:newt-write *newt-input* *newt-output*))

(in-package #:urbit/lars/newt)

(defvar *newt-input*)
(defvar *newt-output*)

(defun get-byte ()
  (read-byte *newt-input*))

(defun put-byte (b)
  (write-byte b *newt-output*))

(defun read-size ()
  (logior
    (get-byte)
    (ash (get-byte) 8)
    (ash (get-byte) 16)
    (ash (get-byte) 24)
    (ash (get-byte) 32)
    (ash (get-byte) 40)
    (ash (get-byte) 48)
    (ash (get-byte) 56)))

(defun read-bytes (n)
  (let ((seq (make-array n :element-type '(unsigned-byte 8))))
    (read-sequence seq *newt-input* :end n)
    seq))

(defun newt-read ()
  (let* ((size (read-size))
         (bytes (read-bytes size)))
    (cue-slim-from-read (read-from-octets size bytes))))

(define-condition oversized-newt (error) ())

(defun newt-write (noun)
  (let* ((bytes (jam-to-bytes (find-ideal noun)))
         (len (length bytes)))
    (unless (typep len '(unsigned-byte 64))
      (error 'oversized-newt))
    (loop repeat 8 for pos upfrom 0 by 8
          for byt = (ldb (byte 8 pos) len)
          do (put-byte byt))
    (write-sequence bytes *newt-output*)
    (force-output *newt-output*)
    (values)))

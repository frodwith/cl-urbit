(defpackage #:urbit/lars/newt
  (:use #:cl #:trivial-bit-streams #:urbit/hoon/serial #:urbit/nock/world)
  (:export #:newt-read #:newt-write))

(in-package #:urbit/lars/newt)

(defparameter *newt-input* *standard-input*)
(defparameter *newt-output* *standard-output*)

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
  (loop for r = 0 then (dpb byt (byte 8 pos) r)
        repeat n
        for pos upfrom 0 by 8
        for byt = (get-byte)
        finally (return r)))

(defun read-frame ()
  (read-bytes (read-size)))

(defun newt-read ()
  (cue-slim-from-int (read-frame)))

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
    (values)))

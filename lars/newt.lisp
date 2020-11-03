(defpackage #:urbit/lars/newt
  (:use #:cl #:trivial-bit-streams #:urbit/hoon/serial #:urbit/nock/world)
  (:export #:newt-read #:newt-write))

(in-package #:urbit/lars/newt)

(defun read-size (in)
  (logior (read-byte in t)
          (ash (read-byte in t) 8)
          (ash (read-byte in t) 16)
          (ash (read-byte in t) 24)
          (ash (read-byte in t) 32)
          (ash (read-byte in t) 40)
          (ash (read-byte in t) 48)
          (ash (read-byte in t) 56)))

(defun read-bytes (n in)
  (loop for r = 0 then (dpb byt (byte 8 pos) r)
        repeat n
        for pos upfrom 0 by 8
        for byt = (read-byte in)
        finally (return r)))

(defun read-frame (in)
  (read-bytes (read-size in) in))

(defun newt-read (in)
  (cue-slim-from-int (read-frame in)))

(define-condition oversized-newt (error) ())

(defun newt-write (out noun)
  (let* ((bytes (jam-to-bytes (find-ideal noun)))
         (len (length bytes)))
    (unless (typep len '(unsigned-byte 64))
      (error 'oversized-newt))
    (loop repeat 8 for pos upfrom 0 by 8
          for byt = (ldb (byte 8 pos) len)
          do (write-byte byt out))
    (write-sequence bytes out)
    (values)))

(defpackage #:urbit/crypto
  (:use #:cl #:cl-intbytes #:cffi)
  (:import-from #:urbit/math #:met #:uint)
  (:export #:scalarmult #:ed-sign))

(in-package #:urbit/crypto)

(define-foreign-library
  liburcrypt
  (t (:default "liburcrypt")))

(use-foreign-library liburcrypt)

(defcfun "urcrypt_ed_scalarmult" :int
  (a :pointer)
  (b :pointer)
  (c :pointer))

; not necessarily true, see grovel?
(defctype size :unsigned-int)

(defcfun "urcrypt_ed_sign" :void
  (message :pointer)
  (length size)
  (seed :pointer)
  (out :pointer))

(deftype octets (n) `(unsigned-byte ,(ash n 3)))

(defun write-bytes-to-ptr (ptr bytes int)
  (declare (uint int bytes))
  (loop for i from 0 below bytes
        for pos upfrom 0 by 8
        for byt = (ldb (byte 8 pos) int)
        do (setf (mem-ref ptr :uint8 i) byt)))

(defun read-bytes-from-ptr (ptr bytes)
  (declare (uint bytes))
  (loop for r = 0 then (dpb byt (byte 8 pos) r)
        for i from 0 below bytes
        for pos upfrom 0 by 8
        for byt = (mem-ref ptr :uint8 i)
        finally (return r)))

(defun scalarmult (a b)
  (declare ((octets 32) a b))
  (with-foreign-pointer (aptr 32)
    (with-foreign-pointer (bptr 32)
      (with-foreign-pointer (cptr 32)
        (write-bytes-to-ptr aptr 32 a)
        (write-bytes-to-ptr bptr 32 b)
        (when (zerop (urcrypt-ed-scalarmult aptr bptr cptr))
          (the (octets 32) (read-bytes-from-ptr cptr 32)))))))

(defun ed-sign (msg seed)
  (declare (uint msg) ((octets 32) seed))
  (let ((len (met 3 msg)))
    (with-foreign-pointer (msg-ptr len)
      (with-foreign-pointer (seed-ptr 32)
        (with-foreign-pointer (out-ptr 64)
          (write-bytes-to-ptr msg-ptr len msg)
          (write-bytes-to-ptr seed-ptr 32 seed)
          (urcrypt-ed-sign msg-ptr len seed-ptr out-ptr)
          (the (octets 64) (read-bytes-from-ptr out-ptr 64)))))))

(defpackage #:urbit/crypto
  (:use #:cl #:cl-intbytes #:cffi)
  (:import-from #:urbit/math #:met #:uint)
  (:export #:ed-point-add #:ed-scalarmult #:ed-sign))

(in-package #:urbit/crypto)

(deftype octets (n) `(unsigned-byte ,(ash n 3)))

(defun write-ptr (ptr bytes int)
  (declare (uint int bytes))
  (loop for i from 0 below bytes
        for pos upfrom 0 by 8
        for byt = (ldb (byte 8 pos) int)
        do (setf (mem-ref ptr :uint8 i) byt)))

(defun read-ptr (ptr bytes)
  (declare (uint bytes))
  (loop for r = 0 then (dpb byt (byte 8 pos) r)
        for i from 0 below bytes
        for pos upfrom 0 by 8
        for byt = (mem-ref ptr :uint8 i)
        finally (return r)))

(defmacro read-out (ptr bytes)
  (assert (constantp bytes)) ; note the duplication of bytes
  `(the (octets ,bytes) (read-ptr ,ptr ,bytes)))

(defmacro with-foreign-octets (bindings &body forms)
  (multiple-value-bind (out-bindings writes)
    (loop for (name size int) in bindings
          collect `(,name :uint8 ,size) into out-bindings
          collect `(write-ptr ,name ,size ,int) into writes
          finally (return (values out-bindings writes)))
    `(with-foreign-objects ,out-bindings
       ,@writes
       ,@forms)))

(define-foreign-library
  liburcrypt
  (t (:default "liburcrypt")))

(use-foreign-library liburcrypt)

; not necessarily true, see grovel?
(defctype size :unsigned-int)

(defcfun "urcrypt_ed_point_add" :int
  (a :pointer)
  (b :pointer)
  (out :pointer))

(defun ed-point-add (a b)
  (declare ((octets 32) a b))
  (with-foreign-octets ((aptr 32 a) (bptr 32 b))
    (with-foreign-pointer (out 32)
      (when (zerop (urcrypt-ed-point-add aptr bptr out))
        (read-out out 32)))))

(defcfun "urcrypt_ed_scalarmult" :int
  (a :pointer)
  (b :pointer)
  (out :pointer))

(defun ed-scalarmult (a b)
  (declare ((octets 32) a b))
  (with-foreign-octets ((aptr 32 a) (bptr 32 b))
    (with-foreign-pointer (out 32)
      (when (zerop (urcrypt-ed-scalarmult aptr bptr out))
        (read-out out 32)))))

(defcfun "urcrypt_ed_scalarmult_base" :void
  (a :pointer)
  (out :pointer))

(defun ed-scalarmult-base (a)
  (declare ((octets 32) a))
  (with-foreign-octets ((in 32 a))
    (with-foreign-pointer (out 32)
      (urcrypt-ed-scalarmult-base in out)
      (read-out out 32))))

(defcfun "urcrypt_ed_add_scalarmult_scalarmult_base" :int
  (a :pointer)
  (a-point :pointer)
  (b :pointer)
  (out :pointer))

(defun ed-add-scalarmult-scalarmult-base (a a-point b)
  (declare ((octets 32) a a-point b))
  (with-foreign-octets ((aptr 32 a) (a-point-ptr 32 a-point) (bptr 32 b))
    (with-foreign-pointer (out 32)
      (when (zerop (urcrypt-ed-add-scalarmult-scalarmult-base
                     aptr a-point-ptr bptr out))
        (read-out out 32)))))

(defcfun "urcrypt_ed_sign" :void
  (message :pointer)
  (length size)
  (seed :pointer)
  (out :pointer))

(defun ed-sign (msg seed)
  (declare (uint msg) ((octets 32) seed))
  (let ((len (met 3 msg)))
    (with-foreign-octets ((msg-ptr len msg) (seed-ptr 32 seed))
      (with-foreign-pointer (out 64)
        (urcrypt-ed-sign msg-ptr len seed-ptr out)
        (read-out out 64)))))

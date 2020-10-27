(in-package #:urbit/urcrypt)

(define-foreign-library
  liburcrypt
  (t (:default "liburcrypt")))

(use-foreign-library liburcrypt)

(deftype octets (n)
  `(unsigned-byte ,(ash n 3)))

(deftype uint () 'unsigned-byte)

(defun byte-length (i)
  (declare (uint i))
  (the uint (values (ceiling (integer-length i) 8))))

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

(defcfun "urcrypt_ed_add_double_scalarmult" :int
  (a :pointer)
  (a-point :pointer)
  (b :pointer)
  (b-point :pointer)
  (out :pointer))

(defun ed-add-double-scalarmult (a a-point b b-point)
  (declare ((octets 32) a a-point b b-point))
  (with-foreign-octets ((aptr 32 a)
                        (a-point-ptr 32 a-point)
                        (bptr 32 b)
                        (b-point-ptr 32 b-point))
    (with-foreign-pointer (out 32)
      (when (zerop (urcrypt-ed-add-double-scalarmult
                     aptr a-point-ptr bptr b-point-ptr out))
        (read-out out 32)))))

(defcfun "urcrypt_ed_puck" :void
  (seed :pointer)
  (out :pointer))

(defun ed-puck (seed)
  (declare ((octets 32) seed))
  (with-foreign-octets ((seed-ptr 32 seed))
    (with-foreign-pointer (out 32)
      (urcrypt-ed-puck seed-ptr out)
      (read-out out 32))))

(defcfun "urcrypt_ed_shar" :void
  (public :pointer)
  (seed :pointer)
  (out :pointer))

(defun ed-shar (public seed)
  (declare ((octets 32) public seed))
  (with-foreign-octets ((public-ptr 32 public) (seed-ptr 32 seed))
    (with-foreign-pointer (out 32)
      (urcrypt-ed-shar public-ptr seed-ptr out)
        (read-out out 32))))

(defcfun "urcrypt_ed_sign" :void
  (message :pointer)
  (length size-t)
  (seed :pointer)
  (out :pointer))

(defmacro with-measured-octets ((ptr-name length-name expr) &body forms)
  (declare (symbol ptr-name length-name))
  (let ((esym (gensym)))
    `(let ((,esym ,expr))
       (declare (uint ,esym))
       (let ((,length-name (byte-length ,esym)))
         (with-foreign-octets ((,ptr-name ,length-name ,esym))
           ,@forms)))))

(defun ed-sign (msg seed)
  (declare (uint msg) ((octets 32) seed))
  (with-measured-octets (msg-ptr len msg)
    (with-foreign-octets ((seed-ptr 32 seed))
      (with-foreign-pointer (out 64)
        (urcrypt-ed-sign msg-ptr len seed-ptr out)
        (read-out out 64)))))

(defcfun "urcrypt_ed_veri" :boolean
  (message :pointer)
  (length size-t)
  (public :pointer)
  (signature :pointer))

(defun ed-veri (msg public signature)
  (declare (uint msg)
           ((octets 32) public)
           ((octets 64) signature))
  (with-measured-octets (msg-ptr len msg)
    (with-foreign-octets ((pub-ptr 32 public)
                          (sig-ptr 64 signature))
      (urcrypt-ed-veri msg-ptr len pub-ptr sig-ptr))))

(defmacro defecb (wrapper-name key-size c-name lisp-name)
  (assert (constantp key-size))
  `(progn
     (defcfun (,c-name ,lisp-name) :int
       (key :pointer)
       (block :pointer)
       (out :pointer))
     (defun ,wrapper-name (key block)
       (declare ((octets ,key-size) key)
                ((octets 16) block))
       (with-foreign-octets ((key-ptr ,key-size key)
                             (blk-ptr 16 block))
         (with-foreign-pointer (out 16)
           (when (zerop (,lisp-name key-ptr blk-ptr out))
             (read-out out 16)))))))

(defecb aes-ecba-en 16 "urcrypt_aes_ecba_en" urcrypt-aes-ecba-en)
(defecb aes-ecba-de 16 "urcrypt_aes_ecba_de" urcrypt-aes-ecba-de)

(defecb aes-ecbb-en 24 "urcrypt_aes_ecbb_en" urcrypt-aes-ecbb-en)
(defecb aes-ecbb-de 24 "urcrypt_aes_ecbb_de" urcrypt-aes-ecbb-de)

(defecb aes-ecbc-en 32 "urcrypt_aes_ecbc_en" urcrypt-aes-ecbc-en)
(defecb aes-ecbc-de 32 "urcrypt_aes_ecbc_de" urcrypt-aes-ecbc-de)

; we use these with realloc in defcbc
(defcfun "malloc" :pointer (size size-t))
(defcfun "free" :void (ptr :pointer))

(defmacro defcbc (wrapper-name key-size c-name lisp-name)
  (assert (constantp key-size))
  `(progn
     (defcfun (,c-name ,lisp-name) :int
       (message :pointer)
       (length :pointer)
       (key :pointer)
       (ivec :pointer)
       (realloc :pointer))
     (defun ,wrapper-name (message key ivec)
       (declare (uint message)
                ((octets ,key-size) key)
                ((octets 16) ivec))
       (with-foreign-octets ((key-ptr ,key-size key)
                             (ivec-ptr 16 ivec))
         (with-foreign-objects ((size-ptr 'size-t)
                                (buf-ptr :pointer))
           (let* ((len (byte-length message))
                  (buf (malloc len)))
             (write-ptr buf len message)
             (setf (mem-ref size-ptr 'size-t) len)  
             (setf (mem-ref buf-ptr :pointer) buf)
             (if (zerop (,lisp-name buf-ptr size-ptr key-ptr ivec-ptr
                                    (foreign-symbol-pointer "realloc")))
                 (let* ((out-size (mem-ref size-ptr 'size-t))
                        (out-ptr (mem-ref buf-ptr :pointer))
                        (out (read-ptr out-ptr out-size)))
                   (free out-ptr)
                   out)
                 (progn (free buf) nil))))))))

(defcbc aes-cbca-en 16 "urcrypt_aes_cbca_en" urcrypt-aes-cbca-en)
(defcbc aes-cbca-de 16 "urcrypt_aes_cbca_de" urcrypt-aes-cbca-de)

(defcbc aes-cbcb-en 24 "urcrypt_aes_cbcb_en" urcrypt-aes-cbcb-en)
(defcbc aes-cbcb-de 24 "urcrypt_aes_cbcb_de" urcrypt-aes-cbcb-de)

(defcbc aes-cbcc-en 32 "urcrypt_aes_cbcc_en" urcrypt-aes-cbcc-en)
(defcbc aes-cbcc-de 32 "urcrypt_aes_cbcc_de" urcrypt-aes-cbcc-de)

(defun measure-associations (as)
  (loop with alen = (length as)
        with lens = (make-array alen)
        for i below alen
        for a = (aref as i)
        for b = (byte-length a)
        do (setf (aref lens i) b)
        sum b into total
        finally (return (values alen total lens))))

(defun fill-associations (as len lengths data-block dest-ptr)
  (loop for i below len
        for blk-ptr = data-block then (inc-pointer blk-ptr dlen)
        for dlen = (aref lengths i)
        for a = (aref as i)
        for el-ptr = (mem-aptr dest-ptr '(:struct aes-siv-data) i)
        do (write-ptr blk-ptr dlen a)
        do (with-foreign-slots
             ((dlength bytes) el-ptr (:struct aes-siv-data))
             (setq dlength dlen)
             (setq bytes blk-ptr))))

(defmacro with-foreign-associations (associations (ptr len) &body forms)
  (let ((asym (gensym))
        (tot (gensym))
        (lens (gensym))
        (blk (gensym)))
    `(let ((,asym ,associations))
       (multiple-value-bind (,len ,tot ,lens) (measure-associations ,asym)
         (with-foreign-objects ((,ptr '(:struct aes-siv-data) ,len)
                                (,blk :uint8 ,tot))
           (fill-associations ,asym ,len ,lens ,blk ,ptr)
           ,@forms)))))

(defmacro with-siv (message associations key key-size
                    (message-ptr message-length
                                 data-ptr data-length
                                 key-ptr)
                    &body forms)
  (declare (symbol message-ptr message-length data-ptr data-length key-ptr))
  (declare (fixnum key-size))
  (assert (constantp key-size))
  (let ((msym (gensym))
        (asym (gensym))
        (ksym (gensym)))
    `(let ((,msym ,message)
           (,asym ,associations)
           (,ksym ,key))
       (declare (uint ,msym)
                ((vector uint) ,asym)
                ((octets ,key-size) key))
       (with-measured-octets (,message-ptr ,message-length ,msym)
         (with-foreign-associations ,asym (,data-ptr ,data-length)
           (with-foreign-octets ((,key-ptr ,key-size ,ksym))
             ,@forms))))))

(defmacro defcsiv (c-name lisp-name)
  `(defcfun (,c-name ,lisp-name) :int
     (message :pointer)
     (length size-t)
     (data :pointer)
     (data-length size-t)
     (key :pointer)
     (iv :pointer)
     (out :pointer)))

(defmacro defsiv-en (wrapper-name key-size c-name lisp-name)
  `(progn
     (defcsiv ,c-name ,lisp-name)
     (defun ,wrapper-name (message associations key)
       (with-siv message associations key ,key-size (mptr mlen dptr dlen kptr)
         (with-foreign-objects ((iv :uint8 16)
                                (out :uint8 mlen))
           (when (zerop (,lisp-name mptr mlen dptr dlen kptr iv out))
             (values (read-ptr iv 16) mlen (read-ptr out mlen))))))))

(defmacro defsiv-de (wrapper-name key-size c-name lisp-name)
  `(progn
     (defcsiv ,c-name ,lisp-name)
     (defun ,wrapper-name (message associations key iv)
       (declare (uint iv))
       (with-siv message associations key ,key-size (mptr mlen dptr dlen kptr)
         (with-foreign-octets ((iv-ptr 16 iv))
           (with-foreign-pointer (out mlen)
             (when (zerop (,lisp-name mptr mlen dptr dlen kptr iv-ptr out))
               (read-ptr out mlen))))))))

(defsiv-en aes-siva-en 32 "urcrypt_aes_siva_en" urcrypt-aes-siva-en)
(defsiv-de aes-siva-de 32 "urcrypt_aes_siva_de" urcrypt-aes-siva-de)

(defsiv-en aes-sivb-en 48 "urcrypt_aes_sivb_en" urcrypt-aes-sivb-en)
(defsiv-de aes-sivb-de 48 "urcrypt_aes_sivb_de" urcrypt-aes-sivb-de)

(defsiv-en aes-sivc-en 64 "urcrypt_aes_sivc_en" urcrypt-aes-sivc-en)
(defsiv-de aes-sivc-de 64 "urcrypt_aes_sivc_de" urcrypt-aes-sivc-de)

(defcfun "urcrypt_ripemd160" :int
  (message :pointer)
  (length size-t)
  (out :pointer))

(defun ripemd-160 (message)
  (declare (uint message))
  (with-measured-octets (ptr len message)
    (with-foreign-pointer (out 20)
      (when (zerop (urcrypt-ripemd160 ptr len out))
        (read-out out 20)))))

(defmacro defsha (name size c-name lisp-name)
  `(progn
     (defcfun (,c-name ,lisp-name) :void
       (message :pointer)
       (length size-t)
       (out :pointer))
     (defun ,name (message)
       (declare (uint message))
       (with-measured-octets (ptr len message)
         (with-foreign-pointer (out ,size)
           (,lisp-name ptr len out)
           (read-out out ,size))))))

(defsha sha-1 20 "urcrypt_sha1" urcrypt-sha1)
(defsha shay 32 "urcrypt_shay" urcrypt-shay)
(defsha shal 64 "urcrypt_shal" urcrypt-shal)

(defcfun "urcrypt_shas" :void
  (salt :pointer)
  (salt-length size-t)
  (message :pointer)
  (message-length size-t)
  (out :pointer))

(defun shas (salt message)
  (with-measured-octets (salt-ptr salt-len salt)
    (with-measured-octets (msg-ptr msg-len message)
      (with-foreign-pointer (out 32)
        (urcrypt-shas salt-ptr salt-len msg-ptr msg-len out)
        (read-out out 32)))))

(in-package #:urbit/urcrypt)

(define-foreign-library
  liburcrypt
  (t (:default "liburcrypt")))

(use-foreign-library liburcrypt)

(deftype octets (n)
  `(unsigned-byte ,(ash n 3)))

(deftype uint () 'unsigned-byte)

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

; we never take raw unsized atoms, they're always accompanied by a length
; if you just take the uints and measure them, it can ignore zero padding

(defun ed-sign (message message-length seed)
  (declare (uint message message-length)
           ((octets 32) seed))
  (with-foreign-octets ((msg-ptr message-length message)
                        (seed-ptr 32 seed))
    (with-foreign-pointer (out 64)
      (urcrypt-ed-sign msg-ptr message-length seed-ptr out)
      (read-out out 64))))

(defcfun "urcrypt_ed_veri" :boolean
  (message :pointer)
  (length size-t)
  (public :pointer)
  (signature :pointer))

(defun ed-veri (message message-length public signature)
  (declare (uint message message-length)
           ((octets 32) public)
           ((octets 64) signature))
  (with-foreign-octets ((msg-ptr message-length message)
                        (pub-ptr 32 public)
                        (sig-ptr 64 signature))
    (urcrypt-ed-veri msg-ptr message-length pub-ptr sig-ptr)))

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
     (defun ,wrapper-name (message len key ivec)
       (declare (uint message len)
                ((octets ,key-size) key)
                ((octets 16) ivec))
       (with-foreign-octets ((key-ptr ,key-size key)
                             (ivec-ptr 16 ivec))
         (with-foreign-objects ((size-ptr 'size-t)
                                (buf-ptr :pointer))
           (let ((buf (malloc len)))
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
  (loop for (len . nil) across as
        summing len))

(defun fill-associations (as len data-block dest-ptr)
  (loop for i below len
        for blk-ptr = data-block then (inc-pointer blk-ptr dlen)
        for (dlen . data) = (aref as i)
        for el-ptr = (mem-aptr dest-ptr '(:struct aes-siv-data) i)
        do (write-ptr blk-ptr dlen data)
        do (with-foreign-slots
             ((dlength bytes) el-ptr (:struct aes-siv-data))
             (setq dlength dlen)
             (setq bytes blk-ptr))))

(defmacro with-foreign-associations ((ptr len associations) &body forms)
  (declare (symbol ptr len))
  (let ((asym (gensym))
        (tot (gensym))
        (blk (gensym)))
    `(let* ((,asym ,associations)
            (,len (length ,asym))
            (,tot (measure-associations ,asym)))
       (with-foreign-objects ((,ptr '(:struct aes-siv-data) ,len)
                              (,blk :uint8 ,tot))
         (fill-associations ,asym ,len ,blk ,ptr)
         ,@forms))))

(defmacro with-siv (message message-length associations key key-size
                    (message-ptr data-ptr data-length key-ptr)
                    &body forms)
  (declare (symbol message-ptr data-ptr data-length key-ptr))
  (declare (fixnum key-size))
  (assert (constantp key-size))
  (let ((msym (gensym))
        (mlns (gensym))
        (asym (gensym))
        (ksym (gensym)))
    `(let ((,msym ,message)
           (,mlns ,message-length)
           (,asym ,associations)
           (,ksym ,key))
       (declare (uint ,msym)
                ((vector uint) ,asym)
                ((octets ,key-size) key))
       (with-foreign-associations (,data-ptr ,data-length ,asym)
         (with-foreign-octets ((,message-ptr ,mlns ,msym)
                               (,key-ptr ,key-size ,ksym))
           ,@forms)))))

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
     (defun ,wrapper-name (message message-length associations key)
       (with-siv message message-length associations key ,key-size
                 (mptr dptr dlen kptr)
         (with-foreign-objects ((iv :uint8 16)
                                (out :uint8 message-length))
           (when (zerop (,lisp-name mptr message-length dptr dlen kptr iv out))
             (values (read-ptr iv 16)
                     message-length
                     (read-ptr out message-length))))))))

(defmacro defsiv-de (wrapper-name key-size c-name lisp-name)
  `(progn
     (defcsiv ,c-name ,lisp-name)
     (defun ,wrapper-name (message message-length associations key iv)
       (declare (uint iv))
       (with-siv message message-length associations key ,key-size
                 (mptr dptr dlen kptr)
         (with-foreign-octets ((iv-ptr 16 iv))
           (with-foreign-pointer (out message-length)
             (when (zerop (,lisp-name mptr message-length
                                      dptr dlen kptr iv-ptr out))
               (read-ptr out message-length))))))))

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

(defun ripemd-160 (message message-length)
  (declare (uint message message-length))
  (with-foreign-octets ((ptr message-length message))
    (with-foreign-pointer (out 20)
      (when (zerop (urcrypt-ripemd160 ptr message-length out))
        (read-out out 20)))))

(defmacro defsha (name size c-name lisp-name)
  `(progn
     (defcfun (,c-name ,lisp-name) :void
       (message :pointer)
       (length size-t)
       (out :pointer))
     (defun ,name (message message-length)
       (declare (uint message message-length))
       (with-foreign-octets ((ptr message-length message))
         (with-foreign-pointer (out ,size)
           (,lisp-name ptr message-length out)
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

(defun shas (salt salt-length message message-length)
  (declare (uint salt salt-length message message-length))
  (with-foreign-octets ((salt-ptr salt-length salt)
                        (msg-ptr message-length message))
    (with-foreign-pointer (out 32)
        (urcrypt-shas salt-ptr salt-length msg-ptr message-length out)
        (read-out out 32))))

(defcfun "urcrypt_argon2" :string
  (type :uint8)
  (version :uint32)
  (threads :uint32)
  (memory-cost :uint32)
  (time-cost :uint32)
  (secret-length size-t)
  (secret :pointer)
  (associated-length size-t)
  (associated :pointer)
  (password-length size-t)
  (password :pointer)
  (salt-length size-t)
  (salt :pointer)
  (out-length size-t)
  (out :pointer)
  (alloc :pointer)
  (free :pointer))

(deftype argon2-specifier ()
  '(member :d :i :id :u))

(deftype argon2-type ()
  `(member ,urcrypt-argon2-d
           ,urcrypt-argon2-i
           ,urcrypt-argon2-id
           ,urcrypt-argon2-u))

(defun argon2-type (spec)
  (declare (argon2-specifier spec))
  (the (or null argon2-type)
       (case spec
         (:d urcrypt-argon2-d)
         (:i urcrypt-argon2-i)
         (:id urcrypt-argon2-id)
         (:u urcrypt-argon2-u))))

(defcallback argon2-alloc :int ((mem :pointer) (size size-t))
  (let ((ptr (foreign-alloc :uint8 :count size)))
    (if (null-pointer-p ptr)
        0
        (progn
          (setf (mem-ref mem :pointer) ptr)
          1))))

(defcallback argon2-free :void ((mem :pointer) (size size-t))
  (declare (ignore size))
  (foreign-free mem))

(defun argon2 (&key output-length type version
               threads memory-cost time-cost
               secret secret-length
               associated associated-length
               password password-length
               salt salt-length)
  (declare (argon2-specifier type)
           ((unsigned-byte 32) version threads memory-cost time-cost)
           (uint secret secret-length
                 associated associated-length
                 password password-length
                 salt salt-length))
  (let ((ty (argon2-type type)))
    (if (null ty)
        (values nil "invalid argon2 type")
        (with-foreign-octets ((secret-ptr secret-length secret)
                              (assocated-ptr associated-length associated)
                              (password-ptr password-length password)
                              (salt-ptr salt-length salt))
          (with-foreign-pointer (out output-length)
            (let ((err (urcrypt-argon2
                         ty version threads memory-cost time-cost
                         secret-length secret-ptr
                         associated-length assocated-ptr
                         password-length password-ptr
                         salt-length salt-ptr
                         output-length out
                         (callback argon2-alloc)
                         (callback argon2-free))))
              (if (null err)
                  (values (read-ptr out output-length))
                  (values nil err))))))))

(defcfun "urcrypt_blake2" :int
  (message-length size-t)
  (message :pointer)
  (key-length size-t)
  (key :pointer)
  (out-length size-t)
  (out :pointer))

(defun blake2 (message message-length key key-length output-length)
  (declare (uint message message-length output-length)
           ((integer 0 64) key-length)
           ((octets 64) key))
  (with-foreign-octets ((msg-ptr message-length message)
                        (key-ptr key-length key))
    (with-foreign-pointer (out output-length)
      (when (zerop (urcrypt-blake2
                     message-length msg-ptr
                     key-length key-ptr
                     output-length out))
        (read-ptr out output-length)))))

(defcfun "urcrypt_secp_prealloc_size" size-t)

(defcfun "urcrypt_secp_init" :void
  (context :pointer)
  (entropy :pointer))

(defcfun "urcrypt_secp_destroy" :void
  (context :pointer))

; works with dynamic variables, too.
(defmacro with-secp-context ((name) &body forms)
  (let ((ent (gensym)))
    `(with-foreign-pointer (,name (urcrypt-secp-prealloc-size))
       (with-foreign-pointer (,ent 32)
         (write-ptr ,ent 32 (secure-random:number (ash 1 256)))
         (urcrypt-secp-init ,name ,ent)
         (unwind-protect (progn ,@forms)
           (urcrypt-secp-destroy ,name))))))

(defcfun "urcrypt_secp_make" :int
  (hash :pointer)
  (key :pointer)
  (out :pointer))

(defun secp-make (hash key)
  (declare ((octets 32) hash key))
  (with-foreign-octets ((hash-ptr 32 hash)
                        (key-ptr 32 key))
    (with-foreign-pointer (out 32)
      (when (zerop (urcrypt-secp-make hash-ptr key-ptr out))
        (read-out out 32)))))

(defcfun "urcrypt_secp_sign" :int
  (context :pointer)
  (hash :pointer)
  (key :pointer)
  (out-v :pointer)
  (out-r :pointer)
  (out-s :pointer))

(defun secp-sign (context hash key)
  (declare ((octets 32) hash key))
  (with-foreign-octets ((hash-ptr 32 hash)
                        (key-ptr 32 key))
    (with-foreign-objects ((v :uint8)
                           (r :uint8 32)
                           (s :uint8 32))
      (when (zerop (urcrypt-secp-sign context hash-ptr key-ptr v r s))
        (values (read-out v 1)
                (read-out r 32)
                (read-out s 32))))))

(defcfun "urcrypt_secp_reco" :int
  (context :pointer)
  (hash :pointer)
  (key-v :uint8)
  (key-r :pointer)
  (key-s :pointer)
  (out-x :pointer)
  (out-y :pointer))

(defun secp-reco (context hash key-v key-r key-s)
  (declare ((octets 32) key-r key-s)
           ((integer 0 3) key-v))
  (with-foreign-octets ((hash-ptr 32 hash)
                        (r-ptr 32 key-r)
                        (s-ptr 32 key-s))
    (with-foreign-objects ((x-ptr :uint8 32)
                           (y-ptr :uint8 32))
      (when (zerop (urcrypt-secp-reco context hash-ptr
                                      key-v r-ptr s-ptr
                                      x-ptr y-ptr))
        (values (read-out x-ptr 32)
                (read-out y-ptr 32))))))

(defcfun "urcrypt_scrypt_pbkdf_sha256" :void
  (passwd :pointer)
  (passwdlen size-t)
  (salt :pointer)
  (saltlen size-t)
  (count :uint64)
  (outlen size-t)
  (out :pointer))

(defun scrypt-pbkdf-sha256 (password password-length salt salt-length
                                     count output-length)
  (declare (uint password password-length salt salt-length)
           ((unsigned-byte 64) count)
           ((integer 0 137438953440) output-length))
  (with-foreign-octets ((pwd-ptr password-length password)
                        (salt-ptr salt-length salt))
    (with-foreign-pointer (out output-length)
      (urcrypt-scrypt-pbkdf-sha256 pwd-ptr password-length
                                   salt-ptr salt-length
                                   count output-length out)
      (read-ptr out output-length))))

(defcfun "urcrypt_scrypt" :int
  (passwd :pointer)
  (passwdlen size-t)
  (salt :pointer)
  (saltlen size-t)
  (n :uint64)
  (r :uint32)
  (p :uint32)
  (outlen size-t)
  (out :pointer))

(defun scrypt (password password-length salt salt-length n r p output-length)
  (declare (uint password password-length salt salt-length output-length)
           ((unsigned-byte 64) n)
           ((unsigned-byte 32) r p))
  (with-foreign-octets ((pwd-ptr password-length password)
                        (salt-ptr salt-length salt))
    (with-foreign-pointer (out output-length)
      (when (zerop (urcrypt-scrypt pwd-ptr password-length salt-ptr salt-length
                                   n r p output-length out))
        (read-ptr out output-length)))))

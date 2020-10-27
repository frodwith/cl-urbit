
(in-package #:urbit/urcrypt)

(pkg-config-cflags "liburcrypt-0")
(include "urcrypt.h")

(ctype size-t "size_t")
(cstruct aes-siv-data "urcrypt_aes_siv_data"
  (dlength "length" :type size-t)
  (bytes "bytes" :type :pointer))

(constant (urcrypt-argon2-d  "urcrypt_argon2_d"))
(constant (urcrypt-argon2-i  "urcrypt_argon2_i"))
(constant (urcrypt-argon2-id "urcrypt_argon2_id"))
(constant (urcrypt-argon2-u  "urcrypt_argon2_u"))

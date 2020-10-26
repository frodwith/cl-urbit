
(in-package #:urbit/urcrypt)

(pkg-config-cflags "liburcrypt-0")
(include "urcrypt.h")

(ctype size-t "size_t")
(cstruct aes-siv-data "urcrypt_aes_siv_data"
  (dlength "length" :type size-t)
  (bytes "bytes" :type :pointer))

(defpackage #:urbit/urcrypt/test
  (:use #:cl #:fiveam #:urbit/urcrypt)
  (:export #:test-urcrypt #:urcrypt-tests))

(in-package #:urbit/urcrypt/test)

(def-suite urcrypt-tests
           :description "smoke tests for liburcrypt ffi bindings")

(defun test-urcrypt ()
  (run! 'urcrypt-tests))

(in-suite urcrypt-tests)

; NOTE: For now this is just a smoke test, but this would be a reasonable
;       place to put a full test suite for urcrypt. Another, more reasonable
;       place would be in urcrypt itself.

(test ed-point-add
  (is (= #xec02ed6842a03579488316444f1c90fd98bf129c21472f437652238b74d6ffdc
         (ed-point-add 42 42))))

(test ed-scalarmult
  (is (= #xd876e661032de6af16258ca99fea69ca6b57e05cf217bec46b764d1458336901
         (ed-scalarmult 42 42))))

(test ed-scalarmult-base
  (is (= #xc237066783c4352092fdf0de4df92cae7343f40939f32b3e195c834e99321ace
         (ed-scalarmult-base 42))))

(test ed-add-scalarmult-scalarmult-base
  (is (= #x8d31b8acda3058fff4b25839c8aceb3baa3df789e7708bc6a8fde104c44a600a
         (ed-add-scalarmult-scalarmult-base 42 42 42))))

(test ed-add-double-scalarmult
  (is (= #xdb75c7cfd8df573fa00327e9f27773800f0f5236b7bced3b0aea077183632cbe
         (ed-add-double-scalarmult 42 42 42 42))))

(test ed-puck
  (is (= #x9f49539565ef87da425912ee017fec92ee11c8fd556a8a9a39f711f2e62fefe1
         (ed-puck 42))))

(test ed-shar
  (is (= #xe65a0c467722065aca17aed392869f1b8c4edb308035f7001a8c843bf0e2de0
         (ed-shar 42 42))))

(test ed-sign
  (is (= #xb32e0ff43a269755faa984fffe9d9b916dd11c0ead263500d66420191149dfce83d6182017607d70c0303437e9a239e4f75038f4b1c8b3a29b65a078f5c90d8
         (ed-sign 42 1 42))))

(test ed-veri
  (is (ed-veri 42 1
               #x9f49539565ef87da425912ee017fec92ee11c8fd556a8a9a39f711f2e62fefe1
               #xb32e0ff43a269755faa984fffe9d9b916dd11c0ead263500d66420191149dfce83d6182017607d70c0303437e9a239e4f75038f4b1c8b3a29b65a078f5c90d8)))

(defun is-ecb (en de encrypted)
  (is (= encrypted (funcall en 42 42)))
  (is (= 42 (funcall de 42 encrypted))))

(test aes-ecb
  (is-ecb #'aes-ecba-en #'aes-ecba-de #x769adafeb03c569e16582f0dd18ee888)
  (is-ecb #'aes-ecbb-en #'aes-ecbb-de #x8217be177250fe023ce605944b1c3ff)
  (is-ecb #'aes-ecbc-en #'aes-ecbc-de #x3e438be4ce97cfc2720f424e225d79b1))

(defun is-cbc (en de encrypted)
  (is (= encrypted (funcall en 42 1 42 42)))
  (is (= 42 (funcall de encrypted 16 42 42))))

(test aes-cbc
  (is-cbc #'aes-cbca-en #'aes-cbca-de #x5eb4689e8c22cbe20340ac72770fa712)
  (is-cbc #'aes-cbcb-en #'aes-cbcb-de #x9f63fee8ebcded42bffef8e9e03fc9a3)
  (is-cbc #'aes-cbcc-en #'aes-cbcc-de #xa74fd8ec91dcbf3904d679b33c7a4b81))

(defun is-siv (en de encrypted)
  (let ((text #xdeadbeefcadefeeddeafbeefdeadcadedeedfadedeaf)
        (len 22)
        (ass #((0 . 0)
               (1 . 42)
               (22 . #xdeadbeefcadefeeddeafbeefdeadcadedeedfadedeaf)
               (1 . 42)
               (0 . 0)
               (0 . 0)))
        (key 42))
    (multiple-value-bind (iv rlen enc) (funcall en text len ass key)
      (declare (ignore rlen))
      (is (= encrypted enc))
      (is (= text (funcall de enc len ass key iv))))))

(test aes-siv
  (is-siv #'aes-siva-en #'aes-siva-de
          #x6b8a0805104a7ce07c8f93e972ec18247ede1a8c218e)
  (is-siv #'aes-sivb-en #'aes-sivb-de
          #x3327efa318f77da69bfada8793c125c50790c0d352b3)
  (is-siv #'aes-sivc-en #'aes-sivc-de
          #x6c483c2b881ef22b5d42a1d29f05e01c719605b42ad9))

(test ripemd-160
  (is (= 1413132018540087757400171133956762286659735740531 (ripemd-160 42 1))))

(test sha-1
  (is (= 1275070591239822890430087490021002412718171930465 (sha-1 42 1))))

(test shay
  (is (= #xc14f495f09c1e1bb7ecc1b704c0966c0267580e25eb69842377fb1ebc0884868
         (shay 42 1))))

(test shal
  (is (= #x2a593488d26a73ec0ceb89bcc78a74a6ecf3842e7ff71fefd9307d9087f1c7823ee48d4723476a71283a5a633b8e082522e12401690b8682705b9c2d4cd4678
         (shal 42 1))))

(test shas
  (is (= #x3b639b7ca7cdb9e9ea07814c4967589bde044df8e97a74b5485a34c411e9a494
         (shas 42 1 42 1))))

(test argon2
  (is (= #x11f29a514b82c5a550cddd0415c7027240a08670b9db2a19e1efd21fe7f388ab
         (argon2 :output-length 32
                 :type :u
                 :version #x13
                 :threads 1
                 :memory-cost 1024
                 :time-cost 1
                 :secret 0
                 :secret-length 0
                 :associated 0
                 :associated-length 0
                 :password #x70617373776f7264
                 :password-length 8
                 :salt #x736f6d6573616c74
                 :salt-length 8))))

(test blake2
  (is (= #x73500a252b436d8407e2a3b84bdde1c5 (blake2 42 1 #xdeadbeef 4 16))))

(test secp
  (is (= #xf0168a6da839696f4ad792dd9baf6d5cbeb1a47e7746cf9e81a5f64b3ca34e84
         (secp-make 42 42)))
  (with-secp-context (ctx)
    (multiple-value-bind (v r s) (secp-sign ctx 41 42)
      (is (= 0 v))
      (is (= #xaffea04ec20600e0e2e61ab44f303fa3c273b28d155b8b31f6d23d4dc8d0aefc
             r))
      (is (= #x518c1d2e9dabbc45f6ba8756f07586b5970db8ebca9a074be543b74a311f609f
             s))
      (multiple-value-bind (x y) (secp-reco ctx 41 v r s)
        (is (= #xfe8d1eb1bcb3432b1db5833ff5f2226d9cb5e65cee430558c18ed3a3c86ce1af
               x))
        (is (= #x7b158f244cd0de2134ac7c1d371cffbfae4db40801a2572e531c573cda9b5b4
               y))))))

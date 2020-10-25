(defpackage #:urbit/urcrypt/test
  (:use #:cl #:fiveam #:urbit/urcrypt)
  (:export #:test-urcrypt #:urcrypt-tests))

(in-package #:urbit/urcrypt/test)

(def-suite urcrypt-tests
           :description "all of cl-urbit's tests")

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
         (ed-sign 42 42))))

(defun is-ecb (en de encrypted)
  (is (= encrypted (funcall en 42 42)))
  (is (= 42 (funcall de 42 encrypted))))

(test aes-ecb
  (is-ecb #'aes-ecba-en #'aes-ecba-de #x769adafeb03c569e16582f0dd18ee888)
  (is-ecb #'aes-ecbb-en #'aes-ecbb-de #x8217be177250fe023ce605944b1c3ff)
  (is-ecb #'aes-ecbc-en #'aes-ecbc-de #x3e438be4ce97cfc2720f424e225d79b1))

(defun is-cbc (en de encrypted)
  (is (= encrypted (funcall en 42 42 42)))
  (is (= 42 (funcall de encrypted 42 42))))

(test aes-cbc
  (is-cbc #'aes-cbca-en #'aes-cbca-de #x5eb4689e8c22cbe20340ac72770fa712)
  (is-cbc #'aes-cbcb-en #'aes-cbcb-de #x9f63fee8ebcded42bffef8e9e03fc9a3)
  (is-cbc #'aes-cbcc-en #'aes-cbcc-de #xa74fd8ec91dcbf3904d679b33c7a4b81))

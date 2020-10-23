(defpackage #:urbit/tests/data
  (:use #:cl #:fiveam #:urbit/tests #:urbit/hoon/syntax
        #:urbit/nock/world #:urbit/nock/data
        #:urbit/nock/equality #:urbit/nock/ideal
        #:urbit/nock/data/slimcell #:urbit/nock/data/slimatom))

(in-package #:urbit/tests/data)

(def-suite data-tests
           :description "exercise the noun protocol for various implementations"
           :in all-tests)

(enable-brackets)

(defmacro def-noun-suite (name convert)
  (flet ((prefix (p)
           (intern (string-upcase (format nil "~a-~a"
                                          (symbol-name p)
                                          (symbol-name name))))))
    (let ((suite-name (prefix 'data-tests)))
      `(progn
         (def-suite ,suite-name
                    :description ,(format nil "data tests for ~a" name)
                    :in data-tests)
         (in-suite ,suite-name)
         (test ,(prefix 'atom)
           (is (= 1 (cl-integer (,convert 1))))
           (signals atom-required (cl-integer (,convert '(0 . 42)))))
         (test ,(prefix 'pair)
           (let ((n (,convert [1 2])))
             (is (= 1 (cl-integer (head n))))
             (is (= 2 (cl-integer (tail n))))))
         (test ,(prefix 'three)
           (let* ((n (,convert [1 2 3]))
                  (m (tail n)))
             (is (= 1 (cl-integer (head n))))
             (is (= 2 (cl-integer (head m))))
             (is (= 3 (cl-integer (tail m))))))
         (test ,(prefix 'nested)
           (let* ((n (,convert [1 [2 3 4] 5]))
                  (m (tail n))
                  (o (head m))
                  (p (tail o)))
             (is (= 1 (cl-integer (head n))))
             (is (= 2 (cl-integer (head o))))
             (is (= 3 (cl-integer (head p))))
             (is (= 4 (cl-integer (tail p))))
             (is (= 5 (cl-integer (tail m))))))
         (test ,(prefix 'cells)
           (signals cell-required (head (,convert 1)))
           (is (= 42 (cl-integer (head (,convert [42 0])))))
           (signals cell-required (tail (,convert 1)))
           (is (= 42 (cl-integer (tail (,convert [0 42]))))))
         (test ,(prefix 'deep)
           (is-true (deep (,convert [0 0])))
           (is-false (deep (,convert 42))))
         (test ,(prefix 'fixnum-same)
               (is (same 1 (,convert 1)))
               (is (not (same 1 (,convert 2)))))
         (test ,(prefix 'bignum-same)
           (let ((a (,convert #xdeadbeefcafebabedeed))
                 (b (,convert (parse-integer "deadbeefcafebabedeed" :radix 16))))
             (is (same a b))
             (is (not (same a (,convert #xdeedbabecafebeefdead))))))
         (test ,(prefix 'cell-same)
           (let ((a (,convert [[1 2] 3 4]))
                 (b (,convert [[1 2] 3 4]))
                 (c (,convert [[3 4] 1 2])))
             ; we need data-specific tests to properly test unification
             (is (not (same a c)))
             (is (same a b))
             (is (eq (head a) (head b)))
             (is (eq (tail a) (tail b)))))))))


(def-noun-suite lisp copy-tree)

(defparameter *test-world* (make-world))
(defun iconv (n) (in-world *test-world* (find-ideal n)))
(def-noun-suite ideal iconv)

(defun copy-slim (in)
  (if (deep in)
      (slim-cons
        (copy-slim (head in))
        (copy-slim (tail in)))
      (slim-malt (cl-integer in))))

(def-noun-suite slim copy-slim)

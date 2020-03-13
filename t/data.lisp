(defpackage #:urbit/tests/data
  (:use #:cl #:fiveam #:urbit/tests #:urbit/data))

(in-package #:urbit/tests/data)

(def-suite data-tests
           :description "exercise the noun protocol for various implementations"
           :in all-tests)

(defun recons (data atomic kons)
  (if (consp data)
      (funcall kons (recons (car data) atomic kons) 
                    (recons (cdr data) atomic kons))
      (funcall atomic data)))

(defmacro def-noun-suite (name convert)
  (flet ((prefix (prefix name)
           (intern (string-upcase (format nil "~a-~a"
                                   (symbol-name prefix) (symbol-name name)))) ))
   (macrolet ((n (two) `(funcall convert ',two)))
   (let ((suite-name (prefix name 'data-tests)))
    `(progn
       (def-suite ,suite-name
                  :description ,(format nil "data tests for ~a" name)
                  :in data-tests)
       (in-suite ,suite-name)
       (test ,(prefix name 'integer-test)
         (is (= 1 (cl-integer ',(n 1)))))
       (test ,(prefix name 'pair-test)
         (let ((n ',(n (1 . 2))))
           (is (= 1 (cl-integer (head n))))
           (is (= 2 (cl-integer (tail n)))))))))))

(def-noun-suite lisp identity)

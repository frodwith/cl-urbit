(defpackage #:urbit/tests/data
  (:use #:cl #:fiveam #:urbit/tests #:urbit/data #:urbit/ideal))

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
           (let ((n (,convert '(1 . 2))))
             (is (= 1 (cl-integer (head n))))
             (is (= 2 (cl-integer (tail n))))))
         (test ,(prefix 'three)
           (let* ((n (,convert '(1 2 . 3)))
                  (m (tail n)))
             (is (= 1 (cl-integer (head n))))
             (is (= 2 (cl-integer (head m))))
             (is (= 3 (cl-integer (tail m))))))
         (test ,(prefix 'nested)
           (let* ((n (,convert '(1 (2 3 . 4) . 5)))
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
           (is (= 42 (cl-integer (head (,convert '(42 . 0))))))
           (signals cell-required (tail (,convert 1)))
           (is (= 42 (cl-integer (tail (,convert '(0 . 42)))))))
         (test ,(prefix 'deep)
           (is-true (deep (,convert '(0 . 0))))
           (is-false (deep (,convert 42))))))))

; TODO: for equality, we short cicruit with cached-ideal and cached-mug
;       otherwise do normal structural equality checking with teach

(def-noun-suite lisp identity)
; (defparameter *test-world* (make-world))
; (defun iconv (n) (ideal *test-world* n))
;  not implemented yet
; (def-noun-suite ideal iconv)

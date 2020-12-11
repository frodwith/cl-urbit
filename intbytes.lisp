(defpackage #:urbit/intbytes
  (:use #:cl)
  (:export #:int->bytes #:bytes->int #:byte-length))

(in-package #:urbit/intbytes)

(declaim (optimize
           (speed 3)
           (space 1)
           (compilation-speed 0)
           (debug 0)
           (safety 0)))

(defun byte-length (int)
  (declare (integer int))
  (values (ceiling (integer-length int) 8)))

(deftype u8 ()
  '(unsigned-byte 8))

(defun int->bytes (int len)
  (declare (integer int) (fixnum len))
  (let ((vec (make-array len :element-type 'u8)))
    (labels
      ((rec (start len int stack)
         (if (> len 1)
             (let* ((half (ash len -1))
                    (more (- len half))
                    (hbyt (ash half 3))
                    (defer (lambda (stack)
                             (rec (+ start half) more
                                  (ldb (byte (ash more 3) hbyt) int)
                                  stack))))
               (rec start half (ldb (byte hbyt 0) int)
                    (cons defer stack)))
             (progn
               (unless (zerop len) (setf (aref vec start) int))
               (when stack (funcall (car stack) (cdr stack)))))))
      (rec 0 len int nil)
      vec)))

(defun bytes->int (bytes len)
  (declare (vector bytes) (fixnum len))
  (flet ((take (r stack) (if stack (funcall (car stack) r (cdr stack)) r)))
    (labels
      ((give (len start stack)
         (if (< len 2)
             (take (if (zerop len) 0 (aref bytes start)) stack)
             (let* ((half (ash len -1))
                    (kont
                      (lambda (low stack)
                        (let ((kont
                                (lambda (high stack)
                                  (take (+ low (ash high (ash half 3))) stack))))
                          (give (- len half) (+ start half) (cons kont stack))))))
               (give half start (cons kont stack))))))
      (give len 0 nil))))

(defpackage #:urbit/chunker
  (:use :cl)
  (:export :chunker-wrap :chunker-put :chunker-finish :with-chunker))

(in-package :urbit/chunker)

(defstruct (chunker (:constructor chunker-wrap (writer)))
  (writer nil :type function)
  (scratch 0 :type (unsigned-byte 8)))

(defmacro with-chunker (write-fn (name) &body forms)
  `(let ((,name (chunker-wrap ,write-fn)))
     ,@forms
     (chunker-finish ,name)))

(defun chunker-put (out chunk)
  (declare (type chunker out)
           (type integer chunk))
  (let* ((pad  (chunker-scratch out))
         (have (integer-length pad))
         (need (- 8 have))
         (bits (integer-length chunk)))
    (if (< bits need)
        (setf (chunker-scratch out) (dpb chunk (byte bits have) pad))
        (progn
          (chunker-write out (dpb (ldb (byte need 0) chunk)
                                  (byte need have)
                                  pad))
          (loop for pos from need to (- bits 9) by 8
                do (chunker-write out (ldb (byte 8 pos) chunk))
                finally (setf (chunker-scratch out) 
                              (ldb (byte 8 pos) chunk)))))))

(defun chunker-write (out byte)
  (declare (type chunker out)
           (type (unsigned-byte 8) byte))
  (funcall (chunker-writer out) byte))

(defun write-past-finish (byte)
  (declare (ignore byte))
  (error "chunker write after finish"))

(defun chunker-finish (out)
  (declare (type chunker out))
  (let ((pad (chunker-scratch out)))
    (unless (zerop pad)
      (chunker-write out pad)))
  (setf (chunker-writer out) #'write-past-finish)
  nil)

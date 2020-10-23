(defpackage #:urbit/hoon/serial
  (:use #:cl #:cl-intbytes #:trivial-bit-streams
        #:urbit/nock/math #:urbit/nock/data #:urbit/nock/ideal
        #:urbit/nock/data/slimcell #:urbit/nock/data/slimatom)
  (:export #:jam #:cue #:jam-to-write #:jam-to-bytes #:cue-from-read
           #:cue-slim-from-int #:cue-pill))

(in-package #:urbit/hoon/serial)

; ideals are already deduplicated, efficiently and globally, allowing us
; to use an efficient eq hashtable for backreferences.
; see trivial-bit-streams for write-fn
(defun jam-to-write (ideal write-fn)
  (with-bit-output-stream (s :callback write-fn)
    (let ((cursor 0)
          (dupes (make-hash-table :test 'eq)))
      (labels
        ((zero ()
           (write-bit 0 s)
           (incf cursor))
         (one ()
           (write-bit 1 s)
           (incf cursor))
         (back (ref)
           (one) (one) (mat ref))
         (save (a)
           (setf (gethash a dupes) cursor))
         (mat (i)
           (if (= 0 i)
               (one)
               (let* ((a (integer-length i))
                      (b (integer-length a))
                      (above (1+ b))
                      (below (1- b)))
                 (write-bits (ash 1 b) above s)
                 (write-bits (logand a (1- (ash 1 below))) below s)
                 (write-bits i a s)
                 (setq cursor (+ cursor above below a)))))
         (take (stack)
           (unless (null stack)
             (give (car stack) (cdr stack))))
         (give (a stack)
           (let ((deep (ideep a))
                 (dupe (gethash a dupes)))
             (if deep
                 (if dupe
                     (progn (back dupe) (take stack))
                     (progn (save a) (one) (zero) 
                            (give (icell-head a)
                                  (cons (icell-tail a) stack))))
                 (let ((i (iint a)))
                   (if dupe
                       (let ((isize (integer-length i))
                             (dsize (integer-length dupe)))
                         (if (< isize dsize)
                             (progn (zero) (mat i))
                             (back dupe)))
                       (progn (save a) (zero) (mat i)))
                   (take stack))))))
        (give ideal nil)))))

; trivial-bit-streams read-bits will build the integer incrementally.
; we avoid some overhead by reading fixnums-at-a-time, and make a nicer
; api by just bailing if the bits aren't available
(defun chunk-bits (bit-stream bits-to-read)
  (multiple-value-bind (whole left) (truncate bits-to-read +fixnum-bits+)
    (multiple-value-bind (big len)
      (loop for total = 0 then (logior total (ash fix len))
            for len upfrom 0 by +fixnum-bits+
            for i below whole
            for fix = (multiple-value-bind (n bits-read)
                        (read-bits +fixnum-bits+ bit-stream)
                        (if (= bits-read +fixnum-bits+)
                            n
                            (error 'exit)))
            finally (return (values total len)))
      (multiple-value-bind (r got) (read-bits left bit-stream)
        (if (= got left)
            (logior big (ash r len))
            (error 'exit))))))

; see trivial-bit-streams for read-fn
; atom-fn will be passed a common lisp integer to make atoms
; cell-fn is passed head and tail to make cells
(defun cue-from-read (read-fn atom-fn cell-fn)
  (with-bit-input-stream (s :callback read-fn)
    (let ((cursor 0)
          (refs (make-hash-table :test 'eql)))
      (labels ((bits (n)
                 (case n
                   (0 0)
                   (1 (incf cursor)
                      (read-bit s))
                   (t (setq cursor (+ cursor n))
                      (chunk-bits s n))))
               (one ()
                 (incf cursor)
                 (let ((b (read-bit s)))
                   (= b 1)))
               (save (pos ref)
                 (setf (gethash pos refs) ref))
               (rub ()
                 (let ((zeros (loop for i upfrom 0
                                    for b = (one)
                                    until b
                                    finally (return i))))
                   (if (zerop zeros)
                       0
                       (let* ((below (1- zeros))
                              (lbits (bits below))
                              (bex (ash 1 below))
                              (len (logxor bex lbits)))
                         (bits len)))))
               (take (r stack)
                 (if (null stack)
                     r
                     (give (cons r (car stack)) (cdr stack))))
               (give (frame stack)
                 (if (consp frame)
                     (destructuring-bind (r . m) frame
                       (if (consp m)
                           (destructuring-bind (l . pos) m
                             (take (save pos (funcall cell-fn l r)) stack))
                           (give cursor (cons frame stack))))
                     (if (one)
                         (if (one)
                             (take (save frame (or (gethash (rub) refs)
                                                   (error 'exit)))
                                   stack)
                             (give cursor (cons frame stack)))
                         (take (save frame (funcall atom-fn (rub))) stack)))))
        (give 0 nil)))))

(defun read-from-octets (len octs)
  (let ((done 0))
    (lambda (buf)
      (loop for i from done below (min (- len done) (length buf))
            do (setf (aref buf i) (aref octs i))
            finally (progn (setq done (+ i done))
                           (return i))))))

(defun read-from-int (a)
  (let ((len (met 3 a)))
    (read-from-octets len (int->octets a len))))

(defun jam-to-bytes (ideal)
  (let ((oct (make-array 100 :adjustable t :fill-pointer 0)))
    (jam-to-write
      ideal
      (lambda (buf end)
        (loop for i below end
              do (vector-push-extend (aref buf i) oct))))
    (values oct (fill-pointer oct))))

(defun jam (ideal)
  (multiple-value-bind (oct len)
    (jam-to-bytes ideal)
    (octets->uint oct len)))

(defun cue (int atom-fn cell-fn)
  (cue-from-read (read-from-int int) atom-fn cell-fn))

(defun cue-from-stream (s atom-fn cell-fn)
  (cue-from-read (make-stream-input-callback s) atom-fn cell-fn))

(defun cue-slim-from-read (read-fn)
  (cue-from-read read-fn #'slim-malt #'slim-cons))

(defun cue-slim-from-int (int)
  (cue-slim-from-read (read-from-int int)))

(defun cue-slim-from-stream (s)
  (cue-slim-from-read (make-stream-input-callback s)))

(defun cue-pill (path)
  (with-open-file (s path :direction :input
                          :if-does-not-exist :error
                          :element-type '(unsigned-byte 8))
    (cue-slim-from-stream s)))

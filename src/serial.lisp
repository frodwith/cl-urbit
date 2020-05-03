(defpackage #:urbit/serial
  (:use #:cl #:urbit/math #:urbit/data #:urbit/ideal
        #:cl-intbytes #:trivial-bit-streams)
  (:export #:jam #:cue #:jam-to-write #:cue-from-read))

(in-package #:urbit/serial)

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

; see trivial-bit-streams for read-fn
(defun cue-from-read (kons read-fn)
  (with-bit-input-stream (s :callback read-fn)
    (let ((cursor 0)
          (refs (make-hash-table :test 'eql)))
      (labels ((bits (n)
                 (loop with done = 0
                       with accu = 0
                       for left = (- n done)
                       until (= 0 left)
                       do (multiple-value-bind (int got) (read-bits left s)
                            (setq accu (logxor (ash int done) accu))
                            (setq done (+ done got)))
                       finally (progn (setq cursor (+ cursor done))
                                      (return accu))))
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
                             (take (save pos (funcall kons l r)) stack))
                           (give cursor (cons frame stack))))
                     (if (one)
                         (if (one)
                             (take (save frame (or (gethash (rub) refs)
                                                   (error 'exit)))
                                   stack)
                             (give cursor (cons frame stack)))
                         (take (save frame (rub)) stack)))))
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

(defun jam (ideal)
  (let ((oct (make-array 100 :adjustable t :fill-pointer 0)))
    (jam-to-write
      ideal
      (lambda (buf end)
        (loop for i below end
              do (vector-push-extend (aref buf i) oct))))
    (octets->uint oct (fill-pointer oct))))

(defun cue (kons int)
  (cue-from-read kons (read-from-int int)))

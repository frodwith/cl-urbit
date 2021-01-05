(defpackage #:urbit/lars/test
  (:use #:cl #:fiveam #:named-readtables #:flexi-streams
        #:bordeaux-threads #:calispel
        #:urbit/nock/world #:urbit/hoon/syntax #:urbit/nock/equality
        #:urbit/lars/newt #:urbit/lars/earth #:urbit/lars/main)
  (:shadowing-import-from #:calispel #:!)
  (:export #:test-lars #:lars-tests))

(in-package #:urbit/lars/test)
(in-readtable hoon)

(def-suite lars-tests
           :description "test the earthly (io/threads) parts of lars")

(defun test-lars ()
  (run! 'lars-tests))

(def-suite newt-tests
           :description "test the wire protocol (newt)"
           :in lars-tests)

(in-suite newt-tests)

(test read-examples
  (let ((bytes #(5 0 0 0 0 0 0 0 192 51 123 123
                 3 9 0 0 0 0 0 0 0 1 79 44 76 14 60 177 48 61)))
    (with-input-from-sequence (*newt-input* bytes)
      (is (same %foo (newt-read)))
      (is (same [%bar %baz] (newt-read))))))

(defun vector= (v1 v2)
  (and (= (length v1) (length v2))
       (loop for a across v1
             for b across v2
             unless (= a b) return nil
             finally (return t))))

(test write-examples
  (let ((bytes (bottle
                 (with-output-to-sequence (*newt-output*)
                   (newt-write %foo)
                   (newt-write (find-ideal [%bar %baz]))))))
    (is (vector= bytes
                 #(5 0 0 0 0 0 0 0 192 51 123 123
                   3 9 0 0 0 0 0 0 0 1 79 44 76 14 60 177 48 61)))))

(test round-trip-examples
  (let* ((nouns (list %foo %bar [1 2 3 4 5]))
         (bytes (with-output-to-sequence (*newt-output*)
                  (bottle
                    (loop for n in nouns
                          do (newt-write (find-ideal n)))))))
    (with-input-from-sequence (*newt-input* bytes)
      (loop for n in nouns
            do (is (same n (newt-read)))))))

(def-suite thread-tests
           :description "test lars' earthly threads"
           :in lars-tests)

(in-suite thread-tests)

(test reader-test
  (let ((bytes (with-output-to-sequence (*newt-output*)
                 (bottle
                   (newt-write %foo)
                   (newt-write (find-ideal [%bar 2]))
                   (newt-write (find-ideal [%baz 3]))
                   ; write one extra value, stop before it is read
                   (newt-write %no)))))
    (with-input-from-sequence (in bytes)
      (multiple-value-bind (chan stop) (make-newt-reader in)
        (is (same %foo (? chan)))
        (is (same [%bar 2] (? chan)))
        (is (same [%baz 3] (? chan)))
        (handler-case
          (with-timeout (5)
            (funcall stop)
            (is (null (? chan))))
          (timeout () (fail)))))))

(test writer-test
  (let ((bytes
          (with-output-to-sequence (out)
            (multiple-value-bind (chan thread) (make-newt-writer out)
              (bottle
                (! chan %foo)
                (! chan (find-ideal [%bar 40]))
                (! chan (find-ideal [%baz 2]))
                (! chan nil)
                (handler-case
                  (with-timeout (5)
                    (join-thread thread) 
                    (pass))
                  (timeout () (fail))))))))
    (with-input-from-sequence (*newt-input* bytes)
      (is (same %foo (newt-read)))
      (is (same [%bar 40] (newt-read)))
      (is (same [%baz 2] (newt-read)))
      (signals stream-error (newt-read)))))

(defun sigint (thread)
  (make-thread
    (lambda ()
      (interrupt-thread thread (lambda () (error 'sb-sys:interactive-interrupt))))))

(test sigint-soak
  (let* ((to (make-instance 'channel))
         (from (make-instance 'channel))
         (ints (make-instance 'channel))
         (work
           (make-thread
             (lambda ()
               (handler-bind
                 ((stop #'(lambda (e) (declare (ignore e)) (! ints t))))
                 (! from (? to))
                 (handler-case (! from (? to))
                   (stop
                     ()
                     (! ints t)
                     (! from :stop-1)))
                 (loop for i from 1 to 10
                       do (! from i))
                 (handler-case
                   (loop for i from 1 to 10
                         do (! from i))
                   (stop 
                     ()
                     (! ints t)
                     (! from :stop-loop)))))))
         (soak
           (make-thread
             (lambda ()
               (! from :begin)
               (stop-sigint work)))))
    (flet ((int ()
             (sigint soak)
             (is (? ints))))
      (is (eq :begin (? from)))
      (! to 1)
      (int)
      (is (= 1 (? from)))
      (! to 2)
      (int)
      (is (eq :stop-1 (? from)))
      (loop for i from 1 to 5 
            do (is (= i (? from))))
      (dotimes (i 10) (int))
      (loop for i from 6 to 10
            do (is (= i (? from))))
      (is (= 1 (? from)))
      (is (= 2 (? from)))
      (dotimes (i 50) (int))
      (is (eq :stop-loop (? from)))
      (handler-case 
        (with-timeout (5)
          (join-thread soak)
          (pass))
        (timeout () (fail))))))

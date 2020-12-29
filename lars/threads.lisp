(defpackage #:urbit/lars/threads
  (:use #:cl #:calispel #:bordeaux-threads #:urbit/lars/newt #:urbit/nock/world)
  (:export #:stop #:stop-sigint #:make-newt-writer #:make-newt-reader
           #:in-relative-silence))

(in-package #:urbit/lars/threads)

; auxilliary threads that support main thread in serf.lisp

(define-condition stop (condition) ()
  (:documentation "A (non-error) condition indicating that some party (often another thread) wants you to stop what you're doing."))

(defun stop (thread)
  "Signal the STOP condition on THREAD."
  (interrupt-thread thread
    (lambda ()
      (signal 'stop))))

(defun make-newt-writer (output)
  "Start a thread which reads nouns from a channel (the return value) and newt-writes them to the given binary output stream. Sending the channel nil causes the thread to shut down."
  (let ((chan (make-instance 'channel)))
    (prog1 chan
      (make-thread
        (lambda ()
          (let ((*newt-output* output))
            (loop for i = (? chan)
                  until (null i)
                  do (newt-write i))))
        :name "newt-writer"))))

(defun make-newt-reader (input)
  "Start a thread which (newt-read)s nouns from the given binary input stream and sends them to a channel (the primary value). The secondary value is a function that can be called to stop the thread. Any stream error while reading from input also stops the thread. The thread will write a nil to its output channel just before stopping."
  (let* ((chan (make-instance 'channel))
         (thread
           (make-thread
             (lambda ()
               (! chan
                  (let ((*newt-input* input))
                    (handler-case 
                      (loop (! chan (newt-read)))
                      (stop () nil)
                      (stream-error () nil)))))
             :name "newt-reader")))
    (values chan (lambda () (stop thread)))))

(defmacro in-relative-silence (&body forms)
  "redirect stdout to stderr and stdin from the empty input"
  (let ((in (gensym)) (io (gensym)))
    `(let* ((,in (make-string-input-stream ""))
            (,io (make-two-way-stream ,in *error-output*))
            (*debug-io* ,io)
            (*query-io* ,io)
            (*standard-input* ,in)
            (*standard-output* *error-output*)
            (*trace-output* *error-output*))
       ,@forms)))

; fn is the "meat" of the program (serve, normally). The desired behavior there
; is that SIGINT is ignored unless we explicitly want to listen to it, which we
; represent with the STOP condition. There is no way to ignore SIGINT on the
; foreground thread in SBCL, since it enters the debugger after signalling.
(defun stop-sigint (fn)
  "Run fn in a new thread, and if the foreground thread gets SIGINT it will send STOP to the thread running fn. Returns what fn returns."
  (block
    nil
    (let ((thread (make-thread fn)))
      (sb-sys:without-interrupts
        (tagbody
          again
          (handler-case
            (return
              (sb-sys:with-local-interrupts
                (join-thread thread)))
            (sb-sys:interactive-interrupt
              ()
              (stop thread)
              (go again))))))))


; TODO: make real tests for the stuff in this file
;(defun test-reader ()
;  (with-ivory *ivory*
;    (let ((bytes (flexi-streams:with-output-to-sequence (out)
;                   (let ((*newt-output* out))
;                     (newt-write %foo)
;                     (newt-write %bar)
;                     (newt-write [%baz 42])))))
;      (flexi-streams:with-input-from-sequence (in bytes)
;        (loop with chan = (make-newt-reader in)
;              for noun = (? chan)
;              until (null noun)
;              do (format t "~a~%" noun)
;              finally (write-line "done"))))))
;
;(defun test-writer ()
;  (with-ivory *ivory*
;    (macrolet ((i (n) `(find-ideal ,n)))
;      (let ((bytes (flexi-streams:with-output-to-sequence (out)
;                     (let ((chan (make-newt-writer out)))
;                       (! chan (i %foo))
;                       (! chan (i %bar))
;                       (! chan (i [%bar 42]))
;                       (! chan nil)))))
;        (flexi-streams:with-input-from-sequence (in bytes)
;          (let ((*newt-input* in))
;            (format t "~a ~a ~a~%" (newt-read) (newt-read) (newt-read))))))))

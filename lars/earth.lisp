(defpackage #:urbit/lars/earth
  (:use #:cl #:calispel #:bordeaux-threads
        #:urbit/lars/newt #:urbit/nock/world)
  (:export #:stop #:stop-sigint #:make-newt-writer #:make-newt-reader
           #:in-relative-silence))

(in-package #:urbit/lars/earth)

; earthly threads that feed the mars thread

(define-condition stop (condition) ()
  (:documentation "A (non-error) condition indicating that some party (often another thread) wants you to stop what you're doing."))

(defun stop (thread)
  "Signal the STOP condition on THREAD."
  (ignore-errors (interrupt-thread thread (lambda () (signal 'stop)))))

(defun make-newt-writer (output)
  "Start a thread which reads nouns from a channel (the primary value) and newt-writes them to the given binary output stream. Sending the channel nil causes the thread to shut down. The thread is returned as the secondary value."
  (let ((chan (make-instance 'channel)))
    (values
      chan
      (make-thread
        (lambda ()
          (let ((*newt-output* output))
            (loop for i = (? chan)
                  until (null i)
                  do (newt-write i))))
        :name "newt-writer"))))

(defun make-newt-reader (input)
  "Start a thread which (newt-read)s nouns from the given binary input stream and sends them to a channel (the primary value). The secondary value is a function that can be used to stop the thread. The thread will always write a final nil to its channel before stopping."
  (let* ((chan (make-instance 'channel))
         (stop (make-instance 'channel))
         (thread
           (make-thread
             (lambda ()
               (let ((*newt-input* input))
                 (loop for n = (handler-case (newt-read)
                                 (stream-error () (loop-finish))
                                 (stop () (? stop) (loop-finish)))
                       do (pri-alt
                            ((? stop) (loop-finish))
                            ((! chan n)))
                       finally (! chan nil))))
             :name "newt-reader")))
    (values chan (lambda ()
                   (stop thread) ; to interrupt blocking read
                   (! stop nil)))))

(defmacro in-relative-silence (&body forms)
  "redirect standard streams such that there is no input and all output goes to stderr"
  (let ((in (gensym)) (io (gensym)))
    `(let* ((,in (make-string-input-stream ""))
            (,io (make-two-way-stream ,in *error-output*))
            (*debug-io* ,io)
            (*query-io* ,io)
            (*standard-input* ,in)
            (*standard-output* *error-output*)
            (*trace-output* *error-output*))
       ,@forms)))

; SIGINT is ignored unless we explicitly want to listen to it, which we
; represent with the STOP condition. There is no way to ignore SIGINT on the
; foreground thread in SBCL, since it enters the debugger after signalling.
(defun stop-sigint (thread)
  "Join thread but deliver sigints in the calling thread as stops in the target thread"
  (sb-sys:without-interrupts
    (loop
      (handler-case
        (return (sb-sys:with-local-interrupts (join-thread thread)))
        (sb-sys:interactive-interrupt
          ()
          (stop thread))))))

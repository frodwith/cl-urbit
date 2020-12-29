(defpackage #:urbit/lars/main
  (:use #:cl #:named-readtables #:bordeaux-threads #:calispel
        #:urbit/hoon/syntax #:urbit/nock/nock #:urbit/nock/world
        #:urbit/nock/cord #:urbit/hoon/k141 #:urbit/hoon/hints
        #:urbit/nock/data #:urbit/nock/common #:urbit/lars/newt
        #:urbit/nock/data/slimatom #:urbit/nock/mug #:urbit/hoon/ivory)
  (:import-from #:uiop/image #:register-image-dump-hook)
  (:export #:entry))

(in-package #:urbit/lars/main)
(in-readtable hoon)

(define-condition writ-foul (simple-error) ())

(defvar *ivory* (lite-boot *ivory-pill-path* urbit/lars/jets:+tree+))

(defvar *ivory-trace*
  (with-ivory *ivory*
    (sure
      (let ((slam (wish-slam "mook")))
        (lambda (trace)
          (tail (funcall slam [2 (noun-trace trace)])))))))

(defparameter *eve* 0)
(defparameter *kernel* 0)

(defparameter *newt-interrupt-handler*
  (lambda ()
    ; by default, newt ignores interrupts
    (values)))

(defun newt-handle-interrupt ()
  (funcall *newt-interrupt-handler*))

(defun compute-fn (timeout main)
  (flet ((quint (thread fn)
           (handler-case (sb-thread:interrupt-thread thread fn)
             (sb-thread:interrupt-thread-error () nil))
           (values)))
    (let ((thread (sb-thread:make-thread main :name "compute")))
      (flet ((stop (mote)
               (quint thread (lambda ()
                               (sb-thread:return-from-thread
                                 (values mote *bug-stack*))))))
        (let ((*newt-interrupt-handler* (lambda () (stop %intr))))
          (if (or (null timeout) (zerop timeout))
            (sb-thread:join-thread thread)
            ; you don't get a bug stack with the :timeout parameter
            (let ((timer (sb-thread:make-thread
                           (lambda ()
                             (sleep (/ timeout 1000))
                             (stop %alrm))
                           :name "alarm")))
              (multiple-value-prog1 (sb-thread:join-thread thread)
                (quint timer #'sb-thread:abort-thread)))))))))

; execute forms. return multiple values (mote val). if mote is nil,
; val is the result of executing forms. Otherwise, mote is
; ?(%exit %intr %alrm) and val is the *bug-stack* from the thread.
; timeout is a number of milliseconds after which to %alrm bail.
; if timeout is 0 or nil, the computation will not time out.
(defmacro compute (timeout &body forms)
  `(compute-fn
     ,timeout
     (lambda () ; happens in a new thread
       (multiple-value-bind (val tax)
         ; so we have to with-ivory
         (with-ivory *ivory* (with-bug-trap ,@forms))
         (if val
             (values nil val)
             (values %exit tax))))))

; mook. an exit or an interrupt will result in the empty trace.
(defun process-trace (trace)
  (multiple-value-bind (mote val)
    (compute nil (funcall *ivory-trace* trace))
    (if mote 0 val)))

(defun goof (mote tax)
  [mote (process-trace tax)])

(defun peek (now path gang)
  (declare (ignore gang))
  (with-fresh-memos (slam (nock *kernel* [9 46 0 1]) [now path])))

(defun writ-peek (bulb)
  (dedata (@@timeout @@now gang path) bulb
    (when (zerop *eve*)
      (error 'writ-foul :format-control "peek at 0"))
    (multiple-value-bind (mote val)
      (compute timeout (peek now gang path))
      (if mote
          [%peek %bail (goof mote val)]
          [%peek %done val]))))

(defun poke (event)
  (dedata (new-kernel effects)
          (with-fresh-memos (slam (nock *kernel* [9 47 0 1]) event))
    (setq *kernel* new-kernel)
    (incf *eve*)
    effects))

(defun boot (events)
  (setq *kernel* (with-fresh-memos (nock events [7 [2 [0 3] 0 2] 0 7]))
        *eve* (lent events)))

(define-symbol-macro kmug (mug *kernel*))

(defun writ-play (bulb)
  (dedata (@@asserted events) bulb
    (unless (= asserted (1+ *eve*))
      (error 'writ-foul
             :format-control "play(~a) at ~a"
             :format-arguments (list asserted *eve*)))
    (multiple-value-bind (mote val)
      (compute nil
        (if (zerop *eve*)
            (boot events)
            (for-?~ (e events) (poke e))))
      (if mote
          [%play %bail *eve* kmug (goof mote val)]
          [%play %done kmug]))))

(defun writ-work-swap (event event-goof)
  (dedata (@@then wire card) event
    (let* ((now (slim-malt (1+ then)))
           (crud [%crud event-goof card])
           (job [now wire crud]))
      (multiple-value-bind (mote val)
        (compute nil (poke job))
        (if mote
            [%work %bail (goof mote val) event-goof 0]
            [%work %swap *eve* kmug job val])))))

(defun writ-work (bulb)
  (dedata (@@timeout event) bulb
    (when (zerop *eve*)
      (error 'writ-foul
             :format-control "work at ~a"
             :format-arguments (list *eve*)))
    (multiple-value-bind (mote val)
      (compute timeout (poke event))
      (if mote
          (writ-work-swap event (goof mote val))
          [%work %done *eve* kmug val]))))



(defun plea-slog (priority msg)
  (newt-write [%slog priority msg]))

(defun handle-slog (slog)
  (plea-slog (slog-priority slog) (slog-tank slog)))

(defun handle-unregistered (w)
  (plea-slog
    0
    (string->cord 
      (format nil "unregistered: ~a at axis ~a"
              (cord->string (unregistered-name w))
              (unregistered-axis w))))
  (muffle-warning w))

(defun writ-loop ()
  ; this handler-binding is dynamic and won't apply to the execution threads
  ; need to do this binding near with-ivory in compute
  (handler-bind
    ((slog #'handle-slog)
     (unregistered-parent #'handle-unregistered))
    (loop (newt-write (handle-writ (newt-read))))))

(defun newt-main (opts)
  (declare (ignore opts))
  ; the standard streams work fine on sbcl as binary streams
  ; use those for newt io
  ; get all input from an empty stream
  ; send all output to log.txt
  ; XX: we should probably boot the ivory pill here rather than at load time,
  ;     so we can respect --quiet etc.
  (let* ((logfile (open "log.txt"
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :rename-and-delete))
         (empty-input (make-string-input-stream ""))
         (io (make-two-way-stream empty-input logfile))
         (*newt-input* *standard-input*)
         (*newt-output* *standard-output*)
         (*debug-io* io)
         (*error-output* logfile)
         (*query-io* io)
         (*standard-input* empty-input)
         (*standard-output* logfile)
         (*trace-output* logfile))
    (handler-case
      (progn
        (newt-write [%ripe [1 141 4] *eve* kmug])
        (writ-loop))
      (exit
        ()
        (format logfile "unhandled exit~%")
        (force-output logfile)
        (sb-ext:exit :abort t))
      (writ-foul
        (f)
        (format logfile "~a~%" f)
        (force-output logfile)
        (sb-ext:exit :abort t)))))

;(defun entry ()
;  ; The main thread absorbs interrupts so newt can choose to ignore them
;  (loop with opts = (parse-arguments (uiop:command-line-arguments))
;        with newt = (sb-thread:make-thread
;                      #'newt-main :name "newt" :arguments opts)
;        do (with-simple-restart (continue "Ignore interrupt.")
;             (handler-case (return (sb-thread:join-thread newt))
;               (sb-sys:interactive-interrupt
;                 ()
;                 (sb-thread:interrupt-thread newt #'newt-handle-interrupt)
;                 (continue))))))

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

; main thread gets a channel back from starting the control thread
; read from this channel in a loop, setting variable for thread-to-interrupt
; on interactive-interrupt, interrupt that thread (report *bug-stack*)
; this way, the control thread gets to (synchronously!) turn interrupts on/off

; to get timeouts, nock should be on its own thread
; yeah, and main thread should start it, deliver interrupts to it
; if it's currently running something, it stops and sends an interrupt result

; control thread gives a timeout to ? if it wants to timeout
; if it expires, it interrupts the nock thread and reads again

; in fact the main thread could be the executor thread. in a loop, it reads
; tasks from the control thread. if it gets SIGINT during this process, it
; writes an interrupt result to its response channel. if it gets interrupts
; while it's waiting for a task from the control thread, it ignores them
; and continues waiting. pretty good! can utilize "with-timeout" that way,
; in the main/executor thread.

; might not work exactly because you want to be shielded from sigints while
; you're handling sigint (like, constructing the bail result and delivering it
; to the control thread).

; you could probably get the main thread to only deliver one interrupt at a
; time, and only when it is told to, on the other hand.

; possibly use sb-ext:atomic-update in some fashion, because
; asynchrony (inherent in interrupting threads) is hard to deal with.

; actually there is without-interrupts, which you could use to atomically
; handle sigint in the main thread.
; (handler-case ... (interactive-interrupt () (without-interrupts ...)))

;(test-writer)

(defvar *interrupt-channel*)

; when we do peek (here, next)
; for compute we write a thread and fn to *interrupt-channel*
; the fn will raise an interrupt condition
; do our stuff (possibly wrapped in bordeaux-threads:with-timeout)
; and then write nil to the same channel.

(defun save-portable-snapshot ()
  ;(jam *kernel*)...
  nil)

(defun save-snapshot ()
  ;(sb-ext:save-lisp-and-die ...)
  nil)

(defun pack ()
  (setq *kernel* (find-ideal *kernel*)))

(defun writ-live (bulb)
  (dedata (@@stem @@bulb) bulb
    (case stem
      (%cram (unless (= bulb *eve*)
               (error 'writ-foul
                      :format-control "cram(~a) at ~a"
                      :format-arguments (list bulb *eve*)))
             (save-portable-snapshot)
             [%live 0])
      (%save (unless (= bulb *eve*)
               (error 'writ-foul
                      :format-control "bulb(~a) at ~a"
                      :format-arguments (list bulb *eve*)))
             (save-snapshot)
             [%live 0])
      (%pack (unless (zerop bulb)
               (error 'writ-foul :format-control "pack at 0"))
             (pack)
             [%live 0])
      (%exit (! *interrupt-channel* bulb)
             nil)
      (t (error 'writ-foul
                :format-control "bad live stem ~a"
                :format-arguments (list stem))))))

(defun handle-writ (writ)
  (handler-case
    (dedata (@@stem bulb) writ
      (case stem
        (%live (writ-live bulb))
        (%peek (writ-peek bulb))
        (%play (writ-play bulb))
        (%work (writ-work bulb))
        (t (error 'writ-foul
                  :format-control "bad stem: ~a"
                  :format-arguments (list stem)))))
    (exit () (error 'writ-foul
                    :format control "exit: ~a"
                    :format-arguments (list writ)))))

(defun make-control (opts writ plea)
  (let ((chan (make-instance 'channel)))
    (prog1 chan
      (make-thread
        (lambda ()
          (handler-bind
            ((slog
               (lambda (c)
                 (! plea [%slog (slog-priority c) (slog-tank c)])))
             (unregistered-parent
               (lambda (w)
                 (let ((msg (format nil "unregistered: ~a at axis ~a"
                                    (cord->string (unregistered-name w))
                                    (unregistered-axis w))))
                   (! plea [slog 0 (string->cord msg)])))))
            (let ((*interrupt-channel* chan))
              (with-ivory *ivory*
                (loop for w = (? writ)
                      when (null w) do (! chan -1) and do (return)
                      for p = (handler-case (handle-writ w)
                                (writ-foul
                                  (f)
                                  (write-line *error-output* f)
                                  (! chan -1)
                                  (return)))
                      until (null p) ; writ-live returns null on exit
                      do (! plea (find-ideal p)))))))
        :name "control"))))

; on the main thread, ignore interrupts until we get a receiver on chan.
; until the receiver is changed, deliver it at most 1 interrupt and loop.
; receiver is a cons of thread and interrupt function,
; nil (ignore interrupts for now),
; or an integer exit code (returned as final value).
(defun soak-interrupts (chan)
  (let (receiver)
    (sb-sys:without-interrupts
      (loop 
        (handler-case
          (let ((r (sb-sys:with-local-interrupts (? chan))))
            (if (integerp r)
                (return r)
                (setq receiver r)))
          (sb-sys:interactive-interrupt
            ()
            (when receiver
              (interrupt-thread (car receiver) (cdr receiver))
              (setq receiver nil))))))))

(defun make-newt-writer (output)
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
  (let ((chan (make-instance 'channel)))
    (values
      chan
      (make-thread
        (lambda ()
          (! chan
             (let ((*newt-input* input))
               (handler-case 
                 (loop (! chan (newt-read)))
                 (stream-error () nil)))))
        :name "newt-reader"))))

(defun main (opts input output)
  (multiple-value-bind (writ stop-writ) (make-newt-reader input)
    (let* ((plea (make-newt-writer output))
           (code (soak-interrupts (make-control opts writ plea))))
      ; the plea thread reads from a channel, so we can just send it a quit
      (! plea nil)
      ; the writ thread just reads from a stream, so to shut it down 
      ; we could either close that stream (which is usually standard-input),
      ; or just destroy the thread
      (stop-writ)
      (sb-ext:exit :code code))))

(defun parse-key (str)
  (multiple-value-bind (sub matches)
    (cl-ppcre:scan-to-strings 
      "^([0-9a-fA-F]{1,16}):([0-9a-fA-F]{1,16}):([0-9a-fA-F]{1,16}):([0-9a-fA-F]{1,16})$"
      str)
    (when sub
      (loop for digits across matches
          collecting (read-from-string (concatenate 'string "#x" digits))))))

(defun parse-hap (str)
  (when (cl-ppcre:scan "^\\d{1,10}$" str)
    (let ((n (read-from-string str)))
      (when (<= (integer-length n) 32)
        (if (zerop n)
            nil
            n)))))

(defun parse-eve (str)
  (when (cl-ppcre:scan "^\\d+$" str)
    (values (read-from-string str))))

(defun parse-wag (str)
  (when (cl-ppcre:scan "^\\d{1,10}$" str)
    (let ((flags (read-from-string str)))
      (when (<= (integer-length flags) 32)
        (loop for (name val) in '((:debug-ram #x1)
                                  (:debug-cpu #x2)
                                  (:check-corrupt #x4)
                                  (:check-fatal #x8)
                                  (:verbose #x10)
                                  (:dry-run #x20)
                                  (:quiet #x40)
                                  (:hashless #x80)
                                  (:trace #x100))
              unless (zerop (logand flags val)) collect name and collect t)))))

(defun parse-arguments (arglist)
  ; (analogically) [%serf dir=@t key=@t wag=@t hap=@ud eve=@ud]
  (destructuring-bind (command dir key wag hap eve) arglist
    (let ((path (uiop:parse-unix-namestring dir :ensure-directory t)))
      `(:command ,(if (string= command "serf")
                      :serf
                      (error "bad command: ~a" command))
        :directory ,path
        :encryption-key ,(parse-key key)
        :memoization-cap ,(parse-hap hap)
        :event-number ,(parse-eve eve)
        ,@(parse-wag wag)))))

(defun entry ()
  (main (parse-arguments (uiop:comand-line-arguments))
        *standard-input*
        *standard-output*))

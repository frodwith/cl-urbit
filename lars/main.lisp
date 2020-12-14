(defpackage #:urbit/lars/main
  (:use #:cl #:named-readtables #:trivial-timeout
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
             (save-portable-snapshot))
      (%save (unless (= bulb *eve*)
               (error 'writ-foul
                      :format-control "bulb(~a) at ~a"
                      :format-arguments (list bulb *eve*)))
             (save-snapshot))
      (%pack (unless (zerop bulb)
               (error 'writ-foul :format-control "pack at 0"))
             (pack))
      (%exit (sb-ext:exit :code bulb))
      (t (error 'writ-foul
                :format-control "bad live stem ~a"
                :format-arguments (list stem)))))
  [%live 0])

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

(defun handle-writ (writ)
  (dedata (@@stem bulb) writ
    (case stem
      (%live (writ-live bulb))
      (%peek (writ-peek bulb))
      (%play (writ-play bulb))
      (%work (writ-work bulb))
      (t (error 'writ-foul
                :format-control "bad writ ~a"
                :format-arguments (list stem))))))

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
  (handler-bind
    ((slog #'handle-slog)
     (unregistered-parent #'handle-unregistered))
    (loop (handler-case (newt-write (handle-writ (newt-read)))))))

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
  ; The main thread absorbs interrupts so newt can choose to ignore them
  (loop with opts = (parse-arguments (uiop:command-line-arguments))
        with newt = (sb-thread:make-thread
                      #'newt-main :name "newt" :arguments opts)
        do (with-simple-restart (continue "Ignore interrupt.")
             (handler-case (return (sb-thread:join-thread newt))
               (sb-sys:interactive-interrupt
                 ()
                 (sb-thread:interrupt-thread newt #'newt-handle-interrupt)
                 (continue))))))

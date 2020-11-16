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

(define-condition writ-foul (error) ())

(defvar *ivory* (lite-boot *ivory-pill-path* urbit/lars/jets:+tree+))
(defvar *ivory-trace*
  (with-ivory *ivory*
    (sure
      (let ((slam (wish-slam "mook")))
        (lambda (trace)
          (tail (funcall slam [2 (noun-trace trace)])))))))

(defvar *eve*)
(defvar *kernel*)
(defvar *options*)

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
               (error 'writ-foul))
             (save-portable-snapshot))
      (%save (unless (= bulb *eve*)
               (error 'writ-foul))
             (save-snapshot))
      (%pack (unless (zerop bulb)
               (error 'writ-foul))
             (pack))
      (%exit (sb-ext:exit :code bulb))
      (t (error 'writ-foul))))
  [%live 0])

(defun process-trace (trace)
  (handler-case (sb-sys:with-interrupts (funcall *ivory-trace* trace))
    (exit () 0)
    (sb-sys:interactive-interrupt () 0)))

(defun goof (mote)
  [mote (process-trace *bug-stack*)])

(defmacro with-noun-timeout (millinoun &body forms)
  (let ((mils (gensym)) )
    `(let ((,mils ,millinoun))
       (if (zerop ,mils)
           (progn ,@forms)
           (with-timeout ((ceiling ,mils 1000)) ,@forms)))))

(defun peek (now path gang)
  (declare (ignore gang))
  (slam (nock *kernel* [9 46 0 1]) [now path]))

(defun writ-peek (bulb)
  (dedata (@@timeout @@now gang path) bulb
    (if (zerop *eve*)
        (error 'writ-foul)
        (let (*bug-stack*)
          (flet ((bail (mote) [%peek %bail (goof mote)]))
            (handler-case (with-noun-timeout timeout
                            (sb-sys:with-interrupts
                              (peek now gang path)))
              (exit () (bail %exit))
              (timeout-error () (bail %alrm))
              (sb-sys:interactive-interrupt () (bail %intr))))))))

(defun poke (event)
  (dedata (new-kernel effects) (slam (nock *kernel* [9 47 0 1]) event)
    (setq *kernel* new-kernel)
    (incf *eve*)
    effects))

(defun boot (events)
  (setq *kernel* (nock events [7 [2 [0 3] 0 2] 0 7])
        *eve* (lent events)))

(defun writ-play (bulb)
  (dedata (@@asserted events) bulb
    (unless (= asserted *eve*)
      (error 'writ-foul))
    (let (*bug-stack*)
      (flet ((bail (mote) [%play %bail *eve* (mug *kernel*) (goof mote)]))
        (handler-case
          (sb-sys:with-interrupts
            (if (zerop *eve*)
                (boot events)
                (for-?~ (e events) (poke e)))
            [%play %done (mug *kernel*)])
          (exit () (bail %exit))
          (sb-sys:interactive-interrupt () (bail %intr)))))))

(defun writ-work-swap (event event-goof)
  (dedata (@@then wire card) event
    (let* ((now (slim-malt (1+ then)))
           (crud [%crud event-goof card])
           (job [now wire crud])
           *bug-stack*)
      (flet ((bail (mote) [%work %bail (goof mote) event-goof 0]))
        (handler-case
          (let ((effects (sb-sys:with-interrupts (poke job))))
            [%work %swap *eve* (mug *kernel*) job effects])
          (exit () (bail %exit))
          (sb-sys:interactive-interrupt () (bail %intr)))))))

(defun writ-work (bulb)
  (dedata (@@timeout event) bulb
    (if (zerop *eve*)
        (error 'writ-foul)
        (let (*bug-stack*)
          (flet ((bail (mote) (writ-work-swap event (goof mote))))
            (handler-case
              (with-noun-timeout
                timeout
                (let ((effects (sb-sys:with-interrupts (poke event))))
                  [%work %done *eve* (mug *kernel*) effects]))
              (exit () (bail %exit))
              (timeout-error () (bail %alrm))
              (sb-sys:interactive-interrupt () (bail %intr))))))))

(defun handle-writ (writ)
  (dedata (@@stem bulb) writ
    (case stem
      (%live (writ-live bulb))
      (%peek (writ-peek bulb))
      (%play (writ-play bulb))
      (%work (writ-work bulb))
      (t (error 'writ-foul)))))

(defun plea-slog (priority msg)
  (sb-sys:without-interrupts
    (newt-write [%slog priority msg])))

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
    (loop
      (handler-case (sb-sys:without-interrupts
                      (sb-sys:allow-with-interrupts
                        (newt-write (handle-writ (newt-read)))))
        ; ignore deferred interrupts from above
        (sb-sys:interactive-interrupt () (values))))))

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
  ; TODO: use a separate world from the ivory pill so we can respect some
  ;       of these options (particularly quiet for no slog compilation)
  ;       but most of these options are currently ignored.
  ; the standard streams work fine on sbcl as binary streams,
  ; use those for newt io
  ; get all input from an empty stream 
  ; send all output to log.txt
  (let ((*options* (parse-arguments (uiop:command-line-arguments)))
        (logfile (open "log.txt" 
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :rename-and-delete)))
      (format logfile "opened logfile~%")
      (let* ((empty-input (make-string-input-stream ""))
             (io (make-two-way-stream empty-input logfile))
             (*newt-input* *standard-input*) 
             (*newt-output* *standard-output*)
             (*debug-io* io)
             (*error-output* logfile)
             (*query-io* io)
             (*standard-input* empty-input)
             (*standard-output* logfile)
             (*trace-output* logfile)
             (*kernel* 0)
             (*eve* 0))
        (format t "starting up~%")
        (handler-case (with-ivory *ivory*
                        (format t "writing ripe~%")
                        (newt-write [%ripe [1 141 4] *eve* (mug *kernel*)])
                        (format t "entering loop~%")
                        (finish-output)
                        (writ-loop))
          (exit () (sb-ext:exit :abort t))    
          (writ-foul () (sb-ext:exit :abort t))))))

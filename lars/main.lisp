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

(defun entry ()
  ; the standard streams work fine on sbcl as binary streams,
  ; use those for newt io
  ; get all input from an empty stream 
  ; send all output to log.txt
  (with-open-file (logfile (merge-pathnames "log.txt")
                           :direction :output
                           :if-exists :append)
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
      (handler-case (with-ivory *ivory*
                      (newt-write [%ripe [1 141 4] *eve* (mug *kernel*)])
                      (writ-loop))
        (exit () (sb-ext:exit :abort t))    
        (writ-foul () (sb-ext:exit :abort t))))))

(defpackage #:urbit/lars/serf
  (:use #:cl #:named-readtables #:bordeaux-threads #:calispel
        #:urbit/nock/nock #:urbit/nock/world #:urbit/nock/common 
        #:urbit/nock/data #:urbit/nock/mug
        #:urbit/nock/cord #:urbit/nock/data/slimatom
        #:urbit/hoon/syntax #:urbit/hoon/hints
        #:urbit/hoon/k141 #:urbit/hoon/ivory
        #:urbit/lars/jets #:urbit/lars/threads)
  (:export #:serve))

(in-package #:urbit/lars/serf)
(in-readtable hoon)

(define-condition writ-foul (simple-error) ())

(defvar *ivory* (lite-boot *ivory-pill-path* urbit/lars/jets:+tree+))

(defvar *ivory-trace*
  (with-ivory *ivory*
    (sure
      (let ((slam (wish-slam "mook")))
        (lambda (stack)
          (tail (funcall slam [2 (noun-trace stack)])))))))

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

(define-condition shutdown (condition)
  ((code :initarg :code :accessor exit-code)))

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
                      :format-control "save(~a) at ~a"
                      :format-arguments (list bulb *eve*)))
             (save-snapshot))
      (%pack (unless (zerop bulb)
               (error 'writ-foul :format-control "pack at 0"))
             (pack))
      (%exit (signal 'shutdown :code bulb))
      (t (error 'writ-foul
                :format-control "bad live stem ~a"
                :format-arguments (list stem))))
    [%live 0]))

(defun compute (milliseconds function &rest arguments)
  "Run interruptable (timeout or ctrl-c) function. Primary value is nil or a bail mote. Secondary value is product or bug stack."
  (let (*bug-stack*)
    (flet ((bail (mote) (values mote *bug-stack*)))
      (handler-case
        (values nil
          (if (zerop milliseconds)
              (apply function arguments)
              (let ((seconds (/ milliseconds 1000)))
                (with-timeout (seconds) (apply function arguments)))))
        (exit () (bail %exit))
        (stop () (bail %intr))
        (timeout () (bail %alrm))))))

(defun goof (mote stack)
  "mook raw *bug-stack* trace. Exit or stop will result in the empty trace"
  (let ((tax (multiple-value-bind (bail val)
               (compute 0 *ivory-trace* stack)
               (if bail 0 val))))
    [mote tax]))

(defun peek (now path gang)
  (declare (ignore gang))
  (with-fresh-memos (slam (nock *kernel* [9 46 0 1]) [now path])))

(defun writ-peek (bulb)
  (dedata (@@timeout @@now gang path) bulb
    (when (zerop *eve*)
      (error 'writ-foul :format-control "peek at 0"))
    (multiple-value-bind (mote val)
      (compute timeout #'peek now gang path)
      (if mote
          [%peek %bail (goof mote val)]
          [%peek %done val]))))

(defun boot (events)
  (setq *kernel* (with-fresh-memos (nock events [7 [2 [0 3] 0 2] 0 7]))
        *eve* (lent events)))

(defun poke (event)
  (dedata (new-kernel effects)
          (with-fresh-memos (slam (nock *kernel* [9 47 0 1]) event))
    (setq *kernel* new-kernel)
    (incf *eve*)
    effects))

(defun play (events)
  (for-?~ (e events)
    (poke e)))

(define-symbol-macro kmug (mug *kernel*))

(defun writ-play (bulb)
  (dedata (@@asserted events) bulb
    (unless (= asserted (1+ *eve*))
      (error 'writ-foul
             :format-control "play(~a) at ~a"
             :format-arguments (list asserted *eve*)))
    (multiple-value-bind (mote val)
      (compute 0 (if (zerop *eve*) #'boot #'play) events)
      (if mote
          [%play %bail *eve* kmug (goof mote val)]
          [%play %done kmug]))))

(defun writ-work-swap (event event-goof)
  (dedata (@@then wire card) event
    (let* ((now (slim-malt (1+ then)))
           (crud [%crud event-goof card])
           (job [now wire crud]))
      (multiple-value-bind (mote val)
        (compute 0 #'poke job)
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
      (compute timeout #'poke event)
      (if mote
          (writ-work-swap event (goof mote val))
          [%work %done *eve* kmug val]))))

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
                    :format-control "exit: ~a"
                    :format-arguments (list writ)))))

(defvar *plea-channel*)
(defvar *writ-channel*)

(defun plea (noun)
  ; taking ideal finding/deduplication off the main thread would require either
  ; washing (which would destroy internal sharing) or some kind of thread-safety
  ; for the world (mutex, etc) so multiple threads finding ideals don't race,
  ; but would increase parallelism slightly.
  (! *plea-channel* (find-ideal noun)))

(defmacro format-plea (priority format-string &rest format-args)
  `(plea [%slog ,priority
               (string->cord (format nil ,format-string ,@format-args))]))

(defun writ-loop ()
  (with-ivory *ivory*
    (handler-bind
      ((slog (lambda (c) (plea [%slog (slog-priority c) (slog-tank c)])))
       (unregistered-parent
         (lambda (w)
           (format-plea 0 "unregistered: ~a at axis ~a"
                        (cord->string (unregistered-name w))
                        (unregistered-axis w)))))
      (loop for w = (? *writ-channel*)
            if (null w) return -1
            else do (handler-case (plea (handle-writ w))
                      (shutdown (c) (return (exit-code c)))   
                      (writ-foul
                        (c)
                        (format-plea 2 "~a" c)
                        (return -1)))))))

(defun ripe ()
  (plea [%ripe [1 141 4] *eve* kmug]))

(defun serve (opts input output)
  (declare (ignore opts))
  (multiple-value-bind (writ stop) (make-newt-reader input)
    (let ((plea (make-newt-writer output)))
      (in-relative-silence
        (let ((code (stop-sigint
                      (lambda ()
                        (let ((*writ-channel* writ)
                              (*plea-channel* plea))
                          (ripe)
                          (writ-loop))))))
          (! plea nil)
          (funcall stop)
          (or code -1))))))

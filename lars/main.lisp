(defpackage #:urbit/lars/main
  (:use #:cl #:trivial-timeout
        #:urbit/hoon/syntax #:urbit/nock/nock #:urbit/nock/world
        #:urbit/nock/cord #:urbit/hoon/k141 #:urbit/hoon/hints
        #:urbit/nock/data #:urbit/nock/common #:urbit/lars/newt
        #:urbit/nock/data/slimatom)
  (:import-from #:uiop/image #:register-image-dump-hook)
  (:export #:entry))

(in-package #:urbit/lars/main)

(enable-syntax)

(define-condition writ-foul (error) ())

(defmacro with-0mil-timeout (milliseconds form ((condition-symbol) error-form))
  (let ((mils (gensym))
        (secs (gensym)))
    `(let ((,mils ,milliseconds))
       (if (zerop ,mils)
           ,form
           (let ((,secs (ceiling ,mils 1000)))
             (handler-case (with-timeout (,secs) ,form)
               (timeout-error
                 (,condition-symbol)
                 (declare (ignorable ,condition-symbol))
                 ,error-form)))))))

(defun plea (noun)
  (newt-write *standard-output* noun))

(defun save-snapshot (eve kernel)
  (declare (ignore eve kernel))
  ;(sb-ext:save-lisp-and-die ...)
  nil)

(defun save-portable-snapshot (eve kernel)
  (declare (ignore eve kernel))
  ;(jam kernel)...
  nil)

(defun peek (kernel now path gang)
  (declare (ignore gang))
  (slam (nock kernel [9 46 0 1]) [now path]))

(defun writ-peek (eve kernel bulb)
  (dedata (@@timeout @@now gang path) bulb
    (if (zerop eve)
        (error 'writ-foul)
        (handler-case
          (with-0mil-timeout
            timeout
            (plea [%peek %done (peek kernel now gang path)])
            ((e) (plea [%peek %bail %alrm 0]))) ; mook? only have tax on exit
          (exit
            (e) 
            (declare (ignore e)) ; MOOK
            (plea [%peek %bail %exit 0]))))))

(defun do-serf (eve kernel)
  (symbol-macrolet ((kmug '(mug kernel)))
    (labels
      ((boot (events)
         (setq kernel (nock events [7 [2 [0 3] 0 2] 0 7])
               eve (lent events)))
       (poke (event)
         (dedata
           (new-kernel effects)
           (slam (nock kernel [9 47 0 1]) event)
           (setq kernel new-kernel)
           (incf eve)
           effects))
       (writ-live (bulb)
         (dedata (@@stem @@bulb) bulb
           (case stem
             (%cram (unless (= bulb eve)
                      (error 'writ-foul))
                    (save-portable-snapshot eve kernel))
             (%save (unless (= bulb eve)
                      (error 'writ-foul))
                    (save-snapshot eve kernel))
             (%pack (unless (zerop bulb)
                      (error 'writ-foul))
                    (setq kernel (find-ideal kernel)))
             (%exit (sb-ext:exit :code bulb))
             (t (error 'writ-foul)))
           (plea [%live 0])))
       (writ-play (bulb)
         (handler-case
           (dedata (@@asserted events) bulb
             (unless (= asserted eve)
               (error 'writ-foul))
             (if (zerop eve)
                 (boot events)
                 (for-?~ (e events) (poke e)))
             (plea [%play %done kmug]))
           (exit
             (e)
             (declare (ignore e)) ; MOOK
             (plea [%play %bail eve kmug %exit 0]))))
       (writ-work-swap (event e)
         (declare (ignore e)) ; MOOK
         (let ((goof [%exit 0]))
           (dedata (@@then wire card) event
             (let* ((now (slim-malt (1+ then)))
                    (crud [%crud goof card])
                    (job [now wire crud]))
               (handler-case
                 (let ((effects (poke job)))
                   (plea [%work %swap eve kmug job effects]))
                 (exit
                   (e)
                   (declare (ignore e)) ; MOOK
                   (let ((rgoof [%exit 0]))
                     (plea [%work %bail rgoof goof 0]))))))))
       (writ-work (bulb)
         (dedata (@@timeout event) bulb
           (if (zerop eve)
               (error 'writ-foul)
               (with-0mil-timeout 
                 timeout
                 (handler-case
                   (let ((effects (poke event)))
                     (plea [%work %done eve kmug effects]))
                   (exit (e) (writ-work-swap event e)))
                 ((e) (let ((goof [%alrm 0])) ; mook?
                        (plea [%work %bail goof 0])))))))
       (writ (noun)
             (dedata (@@stem bulb) noun
               (case stem
                 (%live (writ-live bulb))
                 (%peek (writ-peek eve kernel bulb))
                 (%play (writ-play bulb))
                 (%work (writ-work bulb))
                 (t (error 'writ-foul))))))
      (plea [%ripe [1 141 4] eve kmug])
      (loop for noun = (newt-read *standard-input*)
            do (handler-case (writ noun)
                 (exit
                   (e)
                   (declare (ignore e))
                   (error 'writ-foul)))))))

(defun slog-string (msg)
  (plea [%slog 0 (string->cord msg)]))

(defun handle-toplevel-crash (tax)
  (declare (ignore tax))
  (slog-string "toplevel crash"))

(defun log-unregistered (w)
  (slog-string
    (format nil "unregistered: ~a at axis ~a"
            (cord->string (unregistered-name w))
            (unregistered-axis w)))
  (continue))

(defun log-slog (slog)
  (let ((tank (slog-tank slog))
        (priority (slog-priority slog)))
    (plea [%slog priority tank])))

(defun make-toplevel ()
  (lambda ()
    (in-world (load-k141 urbit/lars/jets:+tree+)
      (with-fresh-memos
        (handler-case
          (handler-bind
            ((unregistered-parent #'log-unregistered)
             (slog #'log-slog))
            (do-serf 0 0))
        (exit (e)
          (handle-toplevel-crash (exit-stack e))))))))

(defvar *lars-toplevel*)
(defun save-toplevel ()
  (setf *lars-toplevel* (make-toplevel)))

(defun entry ()
  (funcall *lars-toplevel*))

(defparameter *dump-hooked* nil)
(unless *dump-hooked*
  (setq *dump-hooked* t)
  (register-image-dump-hook #'save-toplevel))


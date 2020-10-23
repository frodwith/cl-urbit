(defpackage #:urbit/hepl/main
  (:use #:cl #:urbit/hoon/serial #:urbit/hoon/syntax #:urbit/hoon/hints
        #:urbit/nock/common #:urbit/nock/cord #:urbit/nock/nock
        #:urbit/nock/data #:urbit/nock/mug #:urbit/nock/world
        #:urbit/nock/data/slimatom #:urbit/nock/data/slimcell
        #:urbit/hoon/k141)
  (:import-from #:cl-intbytes #:octets->uint)
  (:import-from #:alexandria #:when-let)
  (:import-from #:uiop/image #:register-image-dump-hook)
  (:export #:prologue #:epilogue #:entry))

(in-package #:urbit/hepl/main)

(enable-syntax)

(deftype octet ()
  '(unsigned-byte 8))

(defun read-cord (s)
  (loop with a = (make-array 100
                             :adjustable t
                             :fill-pointer 0
                             :element-type 'octet)
        for c = (read-char s nil nil)
        while c
        do (vector-push-extend (char-code c) a)
        finally (return (octets->uint a (fill-pointer a)))))

(defun cord-from-file (path)
  (with-open-file (in path)
    (read-cord in)))

(defvar *life-source*)
(defvar *wish-source*)
(defvar *ivory-kernel*)

(defun life (eve)
  (nock eve *life-source*))

(defun lite (arv)
  (dedata (eve nil nil) arv
    (life eve)))

(defun wish (cord)
  (with-fresh-memos
    (nock [*ivory-kernel* cord] *wish-source*)))

(defmacro with-lite-boot (pill-path &body forms)
  `(let ((arv (cue-pill ,pill-path)))
     (format t "lite: arvo formula ~(~x~)~%" (mug arv))
     (let* ((*life-source* (copy-tree [7 [2 [0 3] 0 2] 0 7]))
            (*wish-source* (copy-tree [9 2 10 [6 0 3] 9 22 0 2]))
            (*ivory-kernel* (lite arv)))
       (format t "lite: core ~(~x~)~%" (mug *ivory-kernel*))
       ,@forms)))

(defun print-tape (tape &optional out)
  (loop for n = tape then (tail n)
        while (deep n)
        for c = (code-char (cl-integer (head n)))
        do (write-char c out)))

(defun print-wall (w &optional out)
  (loop for n = w then (tail n)
        while (deep n)
        for tape = (head n)
        do (print-tape tape out)
        do (terpri)))

(defun boot-unregistered (c)
  (declare (ignore c))
  (write "Tried to register a core with an unregistered parent during ")
  (write-line "ivory boot. Refusing to continue...")
  (sb-ext:exit :abort t))

(defun boot-slog (slog)
  (let ((tank (slog-tank slog)))
    (handler-case
      (dedata (@@tag tape) tank 
        (case tag
          (%leaf (print-tape tape *standard-output*)
                 (terpri))
          (t (error 'exit))))
      (exit ()
        (format t "ivory boot slog: ~x" (jam (find-ideal tank)))))
    (continue)))

(defun stack-trace-printer (wash mook out)
  (lambda (tax)
    (handler-case
      (loop for n = (funcall mook (flop tax)) then (tail n)
            while (deep n)
            for i = (head n)
            do (print-wall (funcall wash i) out))
      (exit (e)
        (warn "mook failed: ~x" (jam (find-ideal (exit-stack e))))))))

(defun slog-washer (wash tracer out)
  (lambda (slog)
    (let ((tank (slog-tank slog)))
      (let ((p (slog-priority slog)))
        (loop repeat p do (princ #\>))
        (unless (zerop p) (princ #\space)))
      (handler-case (print-wall (funcall wash tank) out)
        (exit (e) 
          (warn "ivory wash failed: ~x" (jam (find-ideal tank)))
          (funcall tracer (exit-stack e))))
      (continue))))

(defun log-unregistered (w)
  (format t "unregistered: ~a at axis ~a~%"
          (cord->string (unregistered-name w))
          (unregistered-axis w))
  (continue))

(opts:define-opts
  (:name :help
   :description "print this message and exit"
   :short #\h
   :long "help")
  (:name :repl
   :description "run a REPL against the assembled subject"
   :short #\r
   :long "repl"))

(defun help (name)
  (opts:describe
         :prefix "standalone hoon interpreter. =~ arguments and pretty-print."
          :usage-of name
          :args "[one.hoon two.hoon tre.hoon ...]")
  (sb-ext:exit))

(defun make-hepl-toplevel (&key name world init sell slap mook wash)
  (lambda ()
    (multiple-value-bind (options args) (opts:get-opts)
      (when (getf options :help)
        (help name))
      (in-world world
        (let* ((out *standard-output*)
               (tracer (stack-trace-printer wash mook out))
               (slogger (slog-washer wash tracer out)))
          (macrolet ((with-trace (&body forms)
                       `(handler-case ,@forms
                          (exit (e)
                            (warn "exit")
                            (funcall tracer (exit-stack e))
                            nil)))
                     (try-print (vase)
                       `(with-trace
                          (with-fresh-memos
                            (print-wall
                              (funcall wash (funcall sell ,vase)))))))

            (handler-bind
              ((unregistered-parent #'log-unregistered)
                (slog slogger))
              (when-let (subject
                          (with-trace
                            (loop for vase = (with-fresh-memos init)
                                  then (with-fresh-memos
                                         (funcall slap vase path cord))
                                  for filename in args
                                  for file = (parse-namestring filename)
                                  for cord = (cord-from-file file)
                                  for path = (slim-cons (string->cord filename) 0)
                                  finally (return vase))))
                (if (or (null args) (getf options :repl))
                  (loop with path = (slim-cons %repl 0)
                        for line = (progn
                                     (princ "> ")
                                     (force-output)
                                     (read-line *standard-input* nil))
                        while line
                        for cord = (string->cord line)
                        do (try-print (funcall slap subject path cord)))
                  (try-print subject))))))))))

; readable hoon dotted notation
(defun print-ux-bytes (bytes len)
  (if (zerop len)
      (format t "0x0")
      (flet ((chunk (hi lo)
               (logior (ash (aref bytes hi) 8) (aref bytes lo))))
        ; write first byte
        (format t "0x~(~x~)"
                (let ((hi (1- len)))
                  (if (oddp len)
                      (progn
                        (setq len hi)
                        (aref bytes hi))
                      (let ((lo (1- hi)))
                        (setq len lo)
                        (chunk hi lo)))))
        ; length has been evened
        (loop for i from (1- len) above 0 by 2
              do (format t ".~(~4,'0x~)" (chunk i (1- i)))))))

(defun hepl-toplevel-from-pill (name pill-path)
  (in-world (load-k141 urbit/hepl/jets:+tree+)
    (with-fresh-memos
      (handler-case
        (handler-bind
          ((unregistered-parent #'boot-unregistered)
           (slog #'boot-slog))
          (with-lite-boot pill-path
            (make-hepl-toplevel
              :name name
              :world *world*
              :init (wish (string->cord "!>(.)"))
              :sell (let ((slam (make-slam (wish %sell))))
                      (lambda (vase)
                        (funcall slam vase)))
              :slap (let ((slap (make-slam (wish %slap)))
                          (rain (make-slam (wish %rain))))
                      (lambda (vase path cord)
                        (funcall
                          slap
                          (slim-cons
                            vase
                            (funcall rain (slim-cons path cord))))))
              :mook (let ((slam (make-slam (wish %mook))))
                      (lambda (tax)
                        (tail (funcall slam (slim-cons 2 tax)))))
              :wash (let ((slam (make-slam (wish %wash)))
                          (win (slim-cons 0 80)))
                      (lambda (tank)
                        (funcall slam (slim-cons win tank)))))))
        (exit (e)
          (format t "lite boot crash: printing jammed trace as @ux:~%")
          (multiple-value-bind (oct len)
            (jam-to-bytes (find-ideal (exit-stack e)))
            (print-ux-bytes oct len))
          (terpri)
          (sb-ext:exit :abort t))))))

(defvar *hepl-toplevel*)
(defun save-toplevel ()
  ; looks for "ivory.pill" in toplevel cl-urbit directory
  (setf *hepl-toplevel*
        (hepl-toplevel-from-pill
          "hepl"
          (merge-pathnames
            (uiop/pathname:parse-unix-namestring "../ivory.pill")
            #.*compile-file-pathname*))))

(defun entry ()
  (funcall *hepl-toplevel*))

(defparameter *dump-hooked* nil)
(unless *dump-hooked*
  (setq *dump-hooked* t)
  (register-image-dump-hook #'save-toplevel))

(defun test-toplevel (ivory-path)
  (let ((sb-ext:*posix-argv* '("test" "--repl"))
        (top (hepl-toplevel-from-pill "test" ivory-path)))
    (funcall top)))
    ;(sb-sprof:with-profiling (:report :flat
    ;                          :loop t
    ;                          :max-samples 10000
    ;                          :show-progress t)
    ;  (with-input-from-string (*standard-input* "(add 40 2)")
    ;    ))))

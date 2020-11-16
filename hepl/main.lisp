(defpackage #:urbit/hepl/main
  (:use #:cl #:named-readtables #:urbit/intbytes
        #:urbit/hoon/serial #:urbit/hoon/syntax #:urbit/hoon/hints
        #:urbit/nock/common #:urbit/nock/cord #:urbit/nock/nock
        #:urbit/nock/data #:urbit/nock/mug #:urbit/nock/world
        #:urbit/nock/data/slimatom #:urbit/nock/data/slimcell
        #:urbit/hoon/k141 #:urbit/hoon/ivory)
  (:import-from #:alexandria #:when-let)
  (:export #:entry))

(in-package #:urbit/hepl/main)
(in-readtable hoon)

(defvar *ivory* (lite-boot *ivory-pill-path* urbit/hepl/jets:+tree+))
(defmacro defivar (name &body forms)
  `(defvar ,name (with-ivory *ivory* (sure ,@forms))))

(defivar *initial-vase* (wish "!>(.)"))
(defivar *pretty-print* (wish-slam "sell"))

(defivar *eval*
  (let ((slap (wish-slam "slap"))
        (rain (wish-slam "rain")))
    (lambda (vase path cord)
      (let ((hoon (funcall rain [path cord])))
        (funcall slap [vase hoon])))))

(defivar *process-trace*
  (let ((slam (wish-slam "mook")))
    (lambda (trace)
      (tail (funcall slam [2 (noun-trace trace)])))))

(defparameter *wash-window* [0 80])

(defivar *tank->wall*
  (let ((slam (wish-slam "wash")))
    (lambda (tank)
      (funcall slam [*wash-window* tank]))))

(defun read-cord ()
  (loop with a = (make-array 100
                             :adjustable t
                             :fill-pointer 0
                             :element-type '(unsigned-byte 8))
        for c = (read-char nil nil nil)
        while c
        do (vector-push-extend (char-code c) a)
        finally (return (bytes->int a (fill-pointer a)))))

(defun cord-from-file (path)
  (with-open-file (*standard-input* path)
    (read-cord)))

(defun print-wall (w)
  (loop for n = w then (tail n)
        while (deep n)
        for tape = (head n)
        do (print-tape tape)
        do (terpri)))

(defun print-stack-trace (trace)
  (multiple-value-bind (pro mtax)
    (with-bug-trap
      (loop for tanks = (funcall *process-trace* trace) then (tail tanks)
            while (deep tanks)
            for tank = (head tanks)
            do (print-wall (funcall *tank->wall* tank))))
    (declare (ignore pro))
    (when mtax
      (warn "crash while printing stack trace")
      (print-jammed-ux (noun-trace trace)))))

(defmacro with-trace (&body forms)
  (let ((pro (gensym)) (tax (gensym)))
    `(multiple-value-bind (,pro ,tax)
       (with-bug-trap ,@forms)
       (or ,pro (print-stack-trace ,tax)))))

(defun print-vase (vase)
  (with-trace
    (print-wall
      (funcall *tank->wall* (funcall *pretty-print* vase)))))

(defun print-slog (slog)
  (let ((tank (slog-tank slog))
        (p (slog-priority slog)))
    (loop repeat p do (princ #\>))
    (unless (zerop p) (princ #\space))
    (multiple-value-bind (pro tax) (funcall *tank->wall* tank)
      (if pro
          (print-wall pro)
          (progn
            (warn "ivory wash failed")
            (write "tank: ")
            (print-jammed-ux tank)
            (write "trace: ")
            (print-stack-trace tax))))))

(defun log-unregistered (w)
  (format t "unregistered: ~a at axis ~a~%"
          (cord->string (unregistered-name w))
          (unregistered-axis w))
  (muffle-warning w))

(opts:define-opts
  (:name :help
   :description "print this message and exit"
   :short #\h
   :long "help")
  (:name :repl
   :description "run a REPL against the assembled subject"
   :short #\r
   :long "repl"))

(defun help ()
  (opts:describe
         :prefix "standalone hoon interpreter. =~ arguments and pretty-print."
          :usage-of "hepl"
          :args "[one.hoon two.hoon tre.hoon ...]")
  (sb-ext:exit))

(defun vase-from-args (args)
  (with-trace
    (loop for vase = *initial-vase*
          then (with-fresh-memos
                 (funcall *eval* vase path cord))
          for filename in args
          for file = (parse-namestring filename)
          for cord = (cord-from-file file)
          for path = [(string->cord filename) 0]
          finally (return vase))))

(defun eval-and-print (subject path cord)
  (with-fresh-memos
    (let (*bug-stack*)
      (restart-case
        (handler-case (print-vase (funcall *eval* subject path cord))
          (exit
            (e) (declare (ignore e))
            (invoke-restart 'trace)))
        (abort ()
          :report "Stop processing and skip this input."
          nil)
        (trace ()
          :report "Print the hoon stack trace."
          (print-stack-trace *bug-stack*))))))

(defun repl (subject)
  (loop with path = [%repl 0]
        for line = (progn
                     (princ "> ")
                     (force-output)
                     (read-line *standard-input* nil))
        while (and line (not (string= line ":quit")))
        for cord = (string->cord line)
        do (eval-and-print subject path cord)))

(defun entry ()
  (multiple-value-bind (options args) (opts:get-opts)
    (when (getf options :help)
      (help))
    (with-ivory *ivory*
      (handler-bind
        ((unregistered-parent #'log-unregistered)
         (slog #'print-slog))
        (when-let (subject (vase-from-args args))
          (if (or (null args) (getf options :repl))
              (repl subject)
              (with-fresh-memos (print-vase subject))))))))

;(sb-sprof:with-profiling (:report :flat
;                          :loop t
;                          :max-samples 10000
;                          :show-progress t)
;  (with-input-from-string (*standard-input* "(add 40 2)")
;    ))))

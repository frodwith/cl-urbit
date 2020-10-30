(defpackage #:urbit/lars/main
  (:use #:cl #:urbit/hoon/syntax #:urbit/nock/nock #:urbit/nock/world
        #:urbit/nock/cord #:urbit/hoon/k141 #:urbit/hoon/hints
        #:urbit/nock/data)
  (:import-from #:uiop/image #:register-image-dump-hook)
  (:export #:entry))

(in-package #:urbit/lars/main)

(enable-syntax)

(defun main ()
  (format t "*[42 0 1]: ~a~%" (nock 42 [0 1])))

(defun handle-toplevel-crash (tax)
  (declare (ignore tax))
  (format t "toplevel crash~%"))

(defun log-unregistered (w)
  (format t "unregistered: ~a at axis ~a~%"
          (cord->string (unregistered-name w))
          (unregistered-axis w))
  (continue))

(defun log-slog (slog)
  (let ((tank (slog-tank slog))
        (priority (slog-priority slog)))
    (declare (ignore tank priority))
    (format t "a slog happened~%")))

(defun make-toplevel ()
  (lambda ()
    (in-world (load-k141 urbit/lars/jets:+tree+)
      (with-fresh-memos
        (handler-case
          (handler-bind
            ((unregistered-parent #'log-unregistered)
             (slog #'log-slog))
            (main))
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


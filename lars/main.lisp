(defpackage #:urbit/lars/main
  (:use #:cl #:urbit/lars/serf)
  (:export #:entry))

(in-package #:urbit/lars/main)

(defun parse-key (str)
  (multiple-value-bind (sub matches)
    (cl-ppcre:scan-to-strings 
      "^([0-9a-fA-F]{1,16}):([0-9a-fA-F]{1,16}):([0-9a-fA-F]{1,16}):([0-9a-fA-F]{1,16})$"
      str)
    (when sub
      (loop for digits across matches
            collecting (read-from-string
                         (concatenate 'string "#x" digits))))))

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
              unless (zerop (logand flags val))
              collect name and collect t)))))

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
  (serve (parse-arguments (uiop:command-line-arguments))
         *standard-input*
         *standard-output*))

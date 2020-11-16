(defpackage #:urbit/hoon/ivory
  (:use #:cl #:named-readtables
        #:urbit/hoon/syntax #:urbit/hoon/hints #:urbit/hoon/serial
        #:urbit/hoon/k141 #:urbit/nock/mug #:urbit/nock/data
        #:urbit/nock/cord #:urbit/nock/world
        #:urbit/nock/nock #:urbit/nock/common)
  (:export #:ivory-kernel #:ivory-world *ivory-pill-path*
           #:lite-boot #:with-ivory #:wish #:wish-slam
           #:sure #:noun-trace #:print-tape
           #:print-ux-bytes #:print-jammed-ux))

(in-package #:urbit/hoon/ivory)
(in-readtable hoon)

(defstruct (ivory (:constructor make-ivory (kernel world)))
  (kernel nil :read-only t)
  (world nil :read-only t))

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

(defun noun-trace (trace)
  (loop for out = 0 then [item out]
        for item in trace
        finally (return out)))

(defun print-jammed-ux (noun)
  (multiple-value-bind (oct len)
    (jam-to-bytes (find-ideal noun))
    (print-ux-bytes oct len)))

(defun print-tape (tape)
  (loop for n = tape then (tail n)
        while (deep n)
        for c = (code-char (cl-integer (head n)))
        do (write-char c)))

(defun ivory-slog (slog)
  (let ((tank (slog-tank slog)))
    (handler-case
      (dedata (@@tag tape) tank 
        (case tag
          (%leaf (print-tape tape)
                 (terpri))
          (t (error 'exit))))
      (exit 
        (e) (declare (ignore e))
        (format t "lite: slog ")
        (print-jammed-ux tank)
        (terpri)))))

(defun lite-boot (pill-path jet-tree)
  (let ((arv (cue-pill pill-path)))
    (format t "lite: arvo formula ~(~x~)~%" (mug arv))
    (dedata (eve nil nil) arv
      (in-world (load-k141 jet-tree)
        (with-fresh-memos
          (multiple-value-bind (kernel tax)
            (with-bug-trap 
              (handler-bind ((slog #'ivory-slog))
                (nock eve [7 [2 [0 3] 0 2] 0 7])))
            (unless kernel
              (print-jammed-ux (noun-trace tax))
              (terpri)
              (error "lite: crash"))
            (format t "lite: core ~(~x~)~%" (mug kernel))
            (make-ivory kernel *world*)))))))

(defvar *ivory-pill-path*
  (merge-pathnames
    (uiop/pathname:parse-unix-namestring "../ivory.pill")
    #.*compile-file-pathname*))

(defvar *ivory*)

(defmacro sure (&body forms)
  `(or (with-bug-trap ,@forms)
       (error "unsure")))

(defmacro with-ivory (ivory &body forms)
  `(let ((*ivory* ,ivory))
     (in-world (ivory-world *ivory*)
       ,@forms)))

(defun wish (hoon)
  (with-fresh-memos
    (nock [(ivory-kernel *ivory*) (string->cord hoon) ]
          [9 2 10 [6 0 3] 9 22 0 2])))

(defun wish-slam (hoon)
  (make-slam (wish hoon)))

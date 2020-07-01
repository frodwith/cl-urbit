(defpackage #:urbit/hoon/ivory
  (:use #:cl #:urbit/serial #:urbit/syntax #:urbit/nock #:urbit/common
        #:urbit/convert #:urbit/hints #:urbit/data #:urbit/mug 
        #:urbit/hoon/k141 #:urbit/data/slimatom #:urbit/data/slimcell)
  (:import-from #:cl-intbytes #:octets->uint))

(in-package #:urbit/hoon/ivory)

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

(defparameter +rep-hoon+
  (string->cord
    (with-output-to-string (s)
      (loop for (line . more) on '("=/  context=vase  !>(.)"
                                   "|=  source=@t"
                                   "%+  wash  [0 80]"
                                   "%-  sell"
                                   "%+  slap  context"
                                   "%-  ream  source")
            if more do (write-line line s)
            else do (write-string line s)))))

(defmacro with-lite-boot (pill-path &body forms)
  `(let ((arv (cue-pill ,pill-path)))
     (format t "lite: arvo formula ~x~%" (mug arv))
     (let* ((*life-source* (copy-tree [7 [2 [0 3] 0 2] 0 7]))
            (*wish-source* (copy-tree [9 2 10 [6 0 3] 9 22 0 2]))
            (*ivory-kernel* (lite arv)))
       (format t "lite: core ~x~%" (mug *ivory-kernel*))
       ,@forms)))

;(defun print-noun (n out tail)
;  (if (deep n)
;      (print-cell n out tail)
;      (print-atom n out)))
;
;(defun print-atom (a out)
;  (prin1 (cl-integer a) out))
;
;(defun print-cell (c out tail)
;  (unless tail (princ #\[ out))
;  (print-noun (head c) out nil)
;  (princ #\space out)
;  (print-noun (tail c) out nil) 
;  (unless tail (princ #\] out)))

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

(defmacro with-prints (&body forms)
  `(handler-bind
     ((unregistered-parent #'log-unregistered)
      (slog #'log-slog))
     ,@forms))

(defun make-toplevel (ivory-path in out)
  (let* ((world (with-prints (load-k141 t)))
         (rep (in-world world
                (with-fresh-memos
                  (with-lite-boot ivory-path
                    (wish +rep-hoon+))))))
    (lambda ()
      (print-wall
          (let ((input (read-cord in)))
            (in-world world
              (with-fresh-memos
                (slam rep input))))
          out))))

(defun log-unregistered (w)
  (format t "unregistered: ~a at axis ~a~%"
          (cord->string (unregistered-name w))
          (unregistered-axis w)))

(defun log-slog (w)
  (let ((tank (slog-tank w)))
    (handler-case
      (dedata (@@tag tape) tank 
        (case tag
          (%leaf (write-line (tape->string tape)))
          (t (error 'exit))))
      (exit () (format t "weird slog ~a~%" tank)))))

(defvar *test-output*)

(defun test-toplevel ()
  (with-input-from-string (in "(add 40 2)")
    (with-output-to-string (out)
      (funcall (make-toplevel #P"/tmp/ivory.pill" in out)))))

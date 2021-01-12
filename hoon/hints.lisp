(defpackage #:urbit/hoon/hints
  (:use #:cl #:named-readtables
        #:urbit/nock/math #:urbit/nock/axis #:urbit/nock/cord
        #:urbit/nock/data #:urbit/nock/ideal #:urbit/nock/world
        #:urbit/nock/jets #:urbit/nock/common #:urbit/nock/mug
        #:urbit/nock/nock #:urbit/nock/equality #:urbit/nock/data/slimcell
        #:urbit/hoon/cache #:urbit/hoon/syntax)
  (:export #:compose-hinters #:+handle-slog+ #:handle-memo #:handle-stack
           #:handle-fast #:fast-hinter #:bad-fast #:unregistered-parent
           #:unregistered-name #:unregistered-axis #:unregistered-core
           #:slog #:handle-slog #:slog-hinter #:slog-priority #:slog-tank
           #:with-fresh-memos #:*memo-size* #:*memo-cache* #:memo-hinter
           #:stack-hinter #:handle-stack))

(in-package #:urbit/hoon/hints)
(in-readtable cord-readtable)

(defun compose-hinters (a b)
  (lambda (tag clue next)
    (or (funcall a tag clue next)
        (funcall b tag clue next))))

; % fast

(define-condition unregistered-parent (warning)
  ((name :type uint :initarg :name :reader unregistered-name)
   (axis :type uint :initarg :axis :reader unregistered-axis)
   (core :initarg :core :reader unregistered-core)))

(define-condition bad-fast (warning)
  ((clue :initarg :clud)
   (core :initarg :core)))

(defun parse-fast-name (name)
  (if (not (deep name))
      (cl-integer name)
      (let ((str (head name))
            (num (tail name)))
        (unless (or (deep str) (deep num))
          (string->cord (format nil "~a~d"
                                (cord->string (cl-integer str))
                                (cl-integer num)))))))

(defun parse-fast-parent (parent)
  ; skip hints and give (t nil) for root, (t ax) for child, or nil
  (loop for n = parent then tail
        while (deep n)
        for head = (head n)
        until (deep head)
        for tail = (tail n)
        do (case (cl-integer head)
             (0 (return
                  (unless (deep tail)
                    (let ((ax (cl-integer tail)))
                      (when (and (> ax 2) (tax ax))
                        (values t ax))))))
             (1 (return
                  (unless (deep tail)
                    (when (zerop tail)
                      (values t nil)))))
             (11 (if (deep tail)
                     (setq tail (tail tail))
                     (return nil)))
             (t (return nil)))))

(defun frag-to-parent (axis core)
  (if (= 1 axis)
      (cell->core core)
      (loop for o = core then (if (deep o)
                                  (if (tax a)
                                      (tail o)
                                      (head o))
                                  (return nil))
            for a = axis then (mas a)
            while (> a 3)
            finally (if (not (deep o))
                        (return nil)
                        (let* ((head (= 2 a))
                               (cnoun (if head (head o) (tail o))))
                          (if (deep cnoun)
                              (let ((parent (cell->core cnoun)))
                                (if head
                                    (setf (head o) parent)
                                    (setf (tail o) parent))
                                (return parent))
                              (return nil)))))))

(defun handle-fast (subject clue core)
  (declare (ignore subject))
  (let ((spd (get-speed core)))
    (unless (typep spd 'fast)
      (symbol-macrolet ((bad '(warn 'bad-fast :clue clue :core core)))
        (macrolet ((sure (test w &body forms)
                     `(if ,test (progn ,@forms) ,w)))
          (sure (deep clue) bad
            (let ((name (parse-fast-name (head clue))))
              (sure name bad
                (let* ((more (tail clue)))
                  (sure (deep more) bad
                    (let ((pform (head more))
                          (hooks (tail more)))
                      (multiple-value-bind (valid axis)
                        (parse-fast-parent pform)
                        (sure valid bad
                          (if (null axis)
                            (let ((payload (tail core)))
                              (sure (not (deep payload)) bad
                                (setf (cached-speed core)
                                      (install-root-stencil
                                        name
                                        (get-ideal-cell core)
                                        (get-ideal hooks)))))
                            (let ((parent (frag-to-parent axis core)))
                              (symbol-macrolet
                                ((unregistered (warn 'unregistered-parent
                                                     :name name :core core
                                                     :axis axis)))
                                (sure parent unregistered
                                  (let ((pspd (core-speed parent)))
                                    (sure (typep pspd 'fast) unregistered
                                      (setf (cached-speed core)
                                            (install-child-stencil
                                              name (head core) (mas axis)
                                              pspd
                                              (get-ideal hooks))))))))))))))))))))))

(defun fast-hinter (tag clue next)
  (declare (ignore next))
  (when (and clue (= %fast tag))
    (cons :after #'handle-fast)))

; %slog

(define-condition slog ()
  ((priority :initarg :priority :reader slog-priority :type integer)
   (tank :initarg :tank :reader slog-tank)))

(defun slog-handler (subject clue)
  (declare (ignore subject))
  (handler-case
    (dedata (@pri ^tank) clue
      (with-simple-restart (continue "Disregard further slog handlers.")
        (signal 'slog :priority pri :tank tank)))
    (exit () nil)))

(defparameter +handle-slog+ (cons :before #'slog-handler))

(defun slog-hinter (tag clue next)
  (declare (ignore next))
  (when (and clue (= %slog tag))
    +handle-slog+))

; %memo
; use WITH-FRESH-MEMOS to dynamically bind a fresh cache (per road, etc)

(defvar *memo-cache*)

(defun memo= (a b)
  (and (eq (car a) (car b))
       (same (cdr a) (cdr b))))

(defun memo-hash (k)
  (sxhash (cons (car k) (mug (cdr k)))))

; keys are a cons of unique object (via eq, ie. ideal, symbol) and noun

(sb-ext:define-hash-table-test memo= memo-hash)

(defparameter *memo-size* 8192)

(defmacro with-fresh-memos (&body forms)
  `(let ((*memo-cache* (make-cache *memo-size* 'memo=)))
     ,@forms))

(defun memo-before (formula)
  (lambda (subject clue)
    (declare (ignore clue))
    (let ((key (cons formula subject)))
      (values (cache-get *memo-cache* key) key))))

(defun memo-after (key product)
  (cache-put *memo-cache* key product))

(defun handle-memo (next)
  (cons :around (cons (memo-before next) #'memo-after)))

(defun memo-hinter (tag clue next)
  (when (and clue (= tag %memo))
    (handle-memo next)))

(defun stack-before (tag)
  (lambda (subject clue)
    (declare (ignore subject))
    (let ((old *bug-stack*))
      (push (cons tag clue) *bug-stack*)
      (values nil old))))

(defun stack-after (old pro)
  (declare (ignore pro))
  (setq *bug-stack* old))

(defun make-stack-handler (tag)
  (cons :around (cons (stack-before tag) #'stack-after)))

(defvar +handle-hunk+ (make-stack-handler %hunk))
(defvar +handle-hand+ (make-stack-handler %hand))
(defvar +handle-mean+ (make-stack-handler %mean))
(defvar +handle-lose+ (make-stack-handler %lose))
(defvar +handle-spot+ (make-stack-handler %spot))

(defun handle-stack (tag)
  (case tag
    (%hunk +handle-hunk+)
    (%hand +handle-hand+)
    (%mean +handle-mean+)
    (%lose +handle-lose+)
    (%spot +handle-spot+)))

(defun stack-hinter (tag clue next)
  (declare (ignore next))
  (when clue (handle-stack tag)))

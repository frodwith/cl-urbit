(defpackage #:urbit/hints
  (:use #:cl #:urbit/data #:urbit/math #:urbit/ideal #:urbit/world
        #:urbit/jets #:urbit/syntax #:urbit/common)
  (:export #:handle-fast #:fast-hinter #:bad-fast #:unregistered-parent
           #:slog #:handle-slog #:slog-hinter #:slog-priority #:slog-tank
           #:stack-handler #:stack-hinter))

(in-package #:urbit/hints)

(enable-cords)

; % fast

(define-condition unregistered-parent (warning)
  ((name :type uint :initarg :name)
   (axis :type uint :initarg :axis)
   (core :initarg :core)))

(define-condition bad-fast (warning)
  ((clue :initarg :clud)
   (core :initarg :core)))

(defun handle-fast (subject clue core)
  (declare (ignore subject))
  (block
    register
    (let ((spd (get-speed core)))
      (unless (typep spd 'fast)
        (handler-case
          (dedata (@name (@num @ax) hooks) clue
            (case num
              (0 (when (and (> ax 2) (tax ax))
                   (let* ((parent (dfrag ax core))
                          (pspeed (get-speed parent)))
                     (when (typep pspeed 'fast)
                       (setf (cached-speed core)
                             (install-child-stencil
                               name (head core) (mas ax)
                               pspeed (get-ideal hooks)))
                       (return-from register))
                     (warn 'unregistered-parent
                           :name name :core core :axis ax))))
              (1 (when (zerop ax)
                   (let ((payload (tail core)))
                     (unless (deep payload)
                       (setf (cached-speed core)
                             (install-root-stencil
                               name
                               (get-ideal-cell core)
                               (get-ideal hooks)))
                       (return-from register)))))))
          (exit () nil))
        (warn 'bad-fast :clue clue :core core))
      (setf (cached-speed core) spd))))

(defun fast-hinter (tag clue next)
  (declare (ignore next))
  (when (and clue (= %fast tag))
    (cons :after #'handle-fast)))

; %slog

(define-condition slog ()
  ((priority :initarg :priority :reader slog-priority :type integer)
   (tank :initarg :tank :reader slog-tank)))

(defun handle-slog (subject clue)
  (declare (ignore subject))
  (handler-case
    (dedata (@pri ^tank) clue
      (signal 'slog :priority pri :tank tank))
    (exit () nil)))

(defun slog-hinter (tag clue next)
  (declare (ignore next))
  (when (and clue (= %slog tag))
    (cons :before #'handle-slog)))

; ?(%hunk %hand %mean %lose %spot)

(defun stack-handler (tag)
  (lambda (subject clue exit)
    (declare (ignore subject))
    (push (cons tag clue) (exit-stack exit))))

(defun stack-hinter (tag clue next)
  (declare (ignore next))
  (when clue
    (case tag
      ((%hunk %hand %mean %lose %spot)
       (cons :catch (stack-handler tag))))))

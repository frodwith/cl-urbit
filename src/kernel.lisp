(in-package #:urbit/kernel)

;; abstract nested cores with hooks 

(deftype static-kernel () '(or root-kernel static-child-kernel))

(defstruct kernel 
  (name nil :type keyword)
  (hooks nil :type (or null hash-table)))

(defstruct (root-kernel
             (:include kernel)
             (:constructor root (name constant &optional hooks)))
  (constant nil :type integer))

(defstruct (static-child-kernel
             (:include kernel)
             (:constructor static (name parent &optional hooks)))
  (parent nil :type static-kernel))

(defstruct (child-kernel
             (:include kernel)
             (:constructor child (name axis parent &optional hooks)))
  (parent nil :type kernel)
  (axis nil :type integer))

(defgeneric kernel-parent (k)
  (:method ((s root-kernel))
   nil)
  (:method ((s static-child-kernel))
   (static-child-kernel-parent s))
  (:method ((s child-kernel))
   (child-kernel-parent s)))

(defgeneric kernel-parent-axis (k)
  (:method ((s root-kernel))
   nil)
  (:method ((s static-child-kernel))
   3)
  (:method ((s child-kernel))
   (child-kernel-axis s)))

; list of (keyword . function)
(defun hooks (pairs)
  (loop with m = (make-hash-table :test 'eq)
        for pair in pairs
        do (setf (gethash (car pair) m) (cdr pair))
        finally (return m)))

(defun parent-core (kernel core)
  (frag core (kernel-parent-axis kernel)))

(defun hook (name kernel core)
  (let* ((hooks (kernel-hooks kernel))
         (hook (and hooks (gethash name hooks))))
    (if hook
        (funcall hook core)
        (typecase kernel
          (root-kernel (error 'oops))
          (otherwise (hook name (kernel-parent kernel)
                           (parent-core kernel core)))))))

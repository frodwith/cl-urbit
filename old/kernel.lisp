(in-package #:urbit/kernel)

;; abstract nested cores with hooks 

(defstruct kernel 
  (name nil :type keyword)
  (hooks nil :type (or null hash-table)))

(defstruct (root-kernel (:include kernel))
  (constant nil :type integer))

(defstruct (static-child-kernel (:include kernel))
  (parent nil :type static-kernel))

(defstruct (dynamic-child-kernel (:include kernel))
  (parent nil :type kernel)
  (axis nil :type integer))

(deftype static-kernel ()
  '(or root-kernel static-child-kernel))

(deftype child-kernel ()
  '(or dynamic-child-kernel static-child-kernel))

(defun kernel-parent (kernel)
  (etypecase kernel
    (root-kernel nil)
    (static-child-kernel (static-child-kernel-parent kernel))
    (dynamic-child-kernel (dynamic-child-kernel-parent kernel))))

(defun kernel-parent-axis (kernel)
  (etypecase kernel
    (static-kernel 1)
    (dynamic-child-kernel (dynamic-child-kernel-axis kernel))))

(defun parent-core (kernel core)
  (frag (tail core) (kernel-parent-axis kernel)))

; list of (keyword . function)
(defun make-hooks (pairs)
  (loop with m = (make-hash-table :test 'eq)
        for pair in pairs
        do (setf (gethash (car pair) m) (cdr pair))
        finally (return m)))

(defun hook (kernel core name)
  (let* ((hooks (kernel-hooks kernel))
         (hook (and hooks (gethash name hooks))))
    (if hook
        (funcall hook core)
        (etypecase kernel
          (root-kernel (error 'oops))
          (child-kernel (hook name (kernel-parent kernel)
                              (parent-core kernel core)))))))

(in-package #:urbit/matcher)

(deftype no-regisrations () 'null)

(deftype single-root () 'stencil)

(deftype multiple-roots () 'hash-table)

(deftype single-child () '(cons integer (cons stencil stencil)))

(deftype multiple-children () '(cons integer hash-table))

(deftype matcher ()
  (or no-registrations single-stencil multiple-roots multiple-children))

(defun match-root (matcher constant)
  (etypecase matcher
    ((or no-registrations multiple-children single-child) nil)
    (single-root (= constant (root-kernel-constant
                               (warm-node-kernel (stencil-node matcher)))))
    (multiple-roots (gethash constant matcher))))

(defun match-child (matcher payload)
  (typecase matcher
    ((or no-registrations single-root multiple-roots) nil)  
    (t (destructuring-bind (axis . more) matcher
         (handler-case
           (let ((inner (frag payload axis)))
             (etypecase matcher
               (single-child
                 (destructuring-bind (parent . child) more
                   (and (check-stencil parent inner) child)))
               (multiple-children
                 (gethash (essence inner) more))))
           (exit () nil))))))

(defun root-stencil-constant (stencil)
  (root-kernel-constant (warm-node-kernel (stencil-node stencil))))

(define-condition inconsistent-registration (warning) ())

(defun inconsistent (matcher)
  (warn 'inconsistent-registration)
  matcher)

(defun add-root (matcher stencil)
  (etypecase matcher
    ((or single-child multiple-children)
     (inconsistent matcher))
    (no-registrations stencil)
    (single-root
      (let ((multi (make-hash-table :test 'eq)))
        (setf (gethash (root-stencil-constant matcher) multi) matcher)
        (setf (gethash (root-stencil-constant stencil) multi) stencil)
        multi))
    (multiple-roots
      (setf (gethash (root-stencil-constant stencil) matcher) stencil)
      multi)))

; XX must make some reconciliation - kernel axis should always be in
; axis-in-payload, not axis-in-core, because of course they're not in the
; battery. save a bit.

(defun add-child (matcher stencil)
  (if (typep matcher '(or single-root multiple-roots))
      (inconsistent matcher) 
      (let* ((node (stencil-node stencil))
             (kernel (warm-node-kernel node))
             (axis (kernel-parent-axis kernel))
             (parent (stencil-parent stencil)))
        (if (typep matcher 'no-registrations)
            (cons axis (cons parent stencil))
            (destructuring-bind (eaxis . more) matcher
              (if (not (= axis eaxis))
                  (inconsistent matcher)
                  (let ((table (etypecase matcher
                                 (multiple-children more)
                                 (single-child
                                   (destructuring-bind (single-parent . child) more
                                     (let ((newt (make-hash-table :test 'eq)))
                                       (setf (gethash single-parent newt) child)
                                       newt))))))
                    (setf (gethash parent table) stencil)
                    (cons axis table))))))))

;; XX: proof/assumptions
;; each battery has an assumption object that says "i haven't been registered
;; yet". it's just a pointer to a boolean value.

;; one possible essence is (simple-array assumption) (t is now verboten)

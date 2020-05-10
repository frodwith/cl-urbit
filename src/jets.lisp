(defpackage #:urbit/jets
  (:use #:cl #:urbit/ideal #:urbit/serial)
  (:import-from #:urbit/data #:exit))

(in-package #:urbit/jets)

(defun zero-terminate (list)
  (loop for c on list
        unless (cdr c) do (rplacd c 0))
  list)

(defun save-jet-pack (world)
  (let ((r (loop with parents = (make-hash-table :test 'eq)
                 for i upfrom 0
                 for stencil in (reverse (world-stencils world))
                 for kernel = (stencil-kernel stencil)
                 for name = (kernel-name kernel)
                 for ideal = (stencil-ideal stencil)
                 for hooks = (stencil-hooks stencil)
                 do (setf (gethash i parents) stencil)
                 collect
                 (etypecase kernel
                   (root
                     `(0 ,name ,ideal . ,hooks))
                   (child
                     (let* ((axis (parent-axis kernel))
                            (pstencil (child-stencil-parent stencil))
                            (pindex (gethash pstencil parents))
                            (battery (if (kernel-static kernel)
                                         (icell-head ideal)
                                         ideal)))
                       `(1 ,name ,battery ,axis ,pindex . ,hooks))))))))
  (jam (find-ideal world (zero-terminate r))))

(defun install-jet-pack (world pack)
  (loop for more = (cue pack #'identity #'cons) then (cdr more)
        while (consp more)
        with parents = (make-array 100 :adjustable t :fill-pointer 0
                                       :element-type 'stencil)
        for s = (macrolet ((! (e) `(get-ideal world ,e))
                           (@ (e) `(get-ideal-atom world ,e))
                           (^ (e) `(get-ideal-cell world ,e)))
                  (decons (@@stem bulb) (head more)
                    (ecase stem
                      (0 (decons (@name core hooks) bulb
                           (decons (^ @) core
                             (install-root-stencil
                               world (@ name) (^ core) (! hooks)))))
                      (1 (decons (@name ^battery @axis @@parent hooks) bulb
                           (install-child-stencil
                             world (@ name) (^ battery) (@ axis) 
                             (aref parents parent) (! hooks)))))))
        do (vector-push-extend s parents)
        finally (unless (= 0 (cl-integer more))
                  (error 'exit))))

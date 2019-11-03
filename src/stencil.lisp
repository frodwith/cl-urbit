(in-package #:urbit/stencil)

(defun make-warm-table ()
  (make-hash-table :test 'equal :weakness :value))

(defstruct (warm-node
             (:constructor cons-warm (kernel parent children stencils)))
  (kernel nil :type kernel)
  (parent nil :type (or null warm-node))
  (children nil :type hash-table)
  (stencils nil :type hash-table))

(defun make-warm-node (kernel &optional parent)
  (cons-warm kernel parent
             (make-warm-table)
             (make-warm-table)))

(defnoun-meta stencil)

(defstruct (stencil (:constructor cons-stencil (node noun parent)))
  (node nil :type warm-node)
  (noun nil :type constant-cell)
  (parent nil :type (or null stencil)))

(defun check-inner (stencil core)
  (let ((cached (cached-stencil core)))
    (if cached
        (eq cached stencil)
        (let ((kernel (warm-node-kernel (stencil-node stencil)))
              (noun   (stencil-noun stencil)))
          (when (etypecase kernel
                  (static-kernel (same noun core))
                  (child-kernel
                    (and (same noun (head core))
                         (check-inner (stencil-parent stencil)
                                      (parent-core kernel core)))))
              (learn-stencil core stencil)
              t)))))

(defun check-stencil (stencil core)
  (handler-case (check-inner stencil core)
    (exit () nil)))

(defun find-stencil (node core)
  (let ((kernel (warm-node-kernel node)))
    (multiple-value-bind (noun parent)
      (etypecase kernel
        (static-kernel (unique core))
        (child-kernel  (values (unique-head core)
                               (stencil (warm-node-parent node)
                                        (parent-core kernel core)))))
      (cache-hash (if parent (cons noun parent) noun)
                  (warm-node-stencils node)
                  (cons-stencil node noun parent)))))

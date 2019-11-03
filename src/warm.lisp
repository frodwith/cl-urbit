(in-package #:urbit/warm)

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

; hooks is a list of (keyword nock-val) pairs
;   nock-val is something you can pass to (noun)
;               that compares correctly under (equal)
;   i.e. a list like '(9 2 0 1)
(defun process-hooks (pairs)
  (hooks 
    (mapcar
      (lambda (p)
        (cons (car p)
              (formula (noun (cdr p)))))
      pairs)))

(defun warm-root (table name constant &optional hooks)
  (cache-hash (cons (cons name constant) hooks) table 
    (make-warm-node (root name constant (process-hooks hooks)))))

(defun warm-child (name axis parent-node &optional hooks)
  (cache-hash (cons (cons name axis) hooks) (warm-node-children parent-node)
    (let ((parent (warm-node-kernel parent-node))
          (phooks (process-hooks hooks)))
      (make-warm-node
        (if (and (= axis 3) (typep parent 'static-kernel))
            (static name parent phooks)
            (child name axis parent phooks))))))

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


; could be find-static-stencil, give me unique core, node
;          find-child-stencil, give me unique battery, node, parent stencil

(defun find-stencil (node core)
  (let ((kernel (warm-node-kernel node)))
    (multiple-value-bind (noun parent)
      (etypecase kernel
        (static-kernel (unique core))
        (child-kernel
          (values (unique-head core)
                  (let ((par (parent-core kernel core)))
                    (or (cached-stencil par)
                        (let ((found (find-stencil (warm-node-parent node) par)))
                          (learn-stencil par found)
                          found))))))
      (cache-hash (if parent (cons noun parent) noun)
                  (warm-node-stencils node)
                  (cons-stencil node noun parent)))))

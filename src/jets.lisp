(defpackage #:urbit/jets
  (:use #:cl #:urbit/common #:urbit/math
        #:urbit/data #:urbit/ideal #:urbit/serial)
  (:import-from #:urbit/data #:exit))

(in-package #:urbit/jets)

; kernel installation is done at most once per world per kernel that
; the runtime has a driver for.

(defmacro hash-place (place-symbol hash-form key-form &body forms)
  (let ((h (gensym)) (k (gensym)))
    `(let ((,h ,hash-form)
           (,k ,key-form))
       (symbol-macrolet ((,place-symbol (gethash ,k ,h))) 
         ,@forms))))

(defmacro root-place (root-symbol world name constant &body forms)
  `(hash-place ,root-symbol (world-roots ,world) (cons ,constant ,name)
     ,@forms))

(defmacro child-place (child-symbol parent name axis &body forms)
  `(hash-place ,child-symbol (kernel-children ,parent) (cons ,axis ,name)
     ,@forms))

(defun kernel-static (kernel)
  (not (typep kernel 'dynamic-kernel)))

(defun make-child (name axis parent driver)
  (if (and (= 1 axis) (kernel-static parent))
      (child-kernel parent name driver)
      (dynamic-kernel parent name axis driver)))

(define-condition reinstall-kernel (error) (kernel))

(defun install-root (world name constant driver)
  (root-place r world name constant
    (if r
        (error 'reinstall-kernel r)
        (setq r (root-kernel constant name driver)))))

(defun install-child (parent name axis driver)
  (child-place c parent name axis
    (if c
        (error 'reinstall-kernel :kernel c)
        (setq c (make-child parent name axis driver)))))

; kernel finding is used by stencil installation, and will create a 
; kernel if one has not already been installed.

(defun find-root (world name constant)
  (root-place r world name constant
    (or r (setq r (root-kernel constant name nil)))))

(defun find-child (parent name axis)
  (child-place c parent name axis
    (or c (setq c (make-child parent name axis nil)))))

; installing a stencil causes matching cores to clock fast.
; usually it is done either by running something like a fast hint
; or by installing a battery pack.

(defun call-kernel-driver (kernel parent-stencil ideal hooks)
  (let ((kdriver (kernel-driver kernel)))
    (when kdriver
      (funcall kdriver kernel parent-stencil ideal hooks))))

(define-condition reinstall-stencil (error) (stencil))

(defun hash-pairs (h)
  (loop for k being the hash-keys in h using (hash-value v)
        collect `(k v)))


(defmacro get-battery (world core)
  (let ((s (gensym)))
    `(let ((,s ,core))
       (let ((bat (or (cached-battery ,s)
                      (let ((i (get-ideal ,world (head ,s))))
                        (setf (cached-battery ,s) i)
                        i))))
         (if (ideep bat)
             bat
             (error 'cell-required :given bat))))))

(defun compile-meter (world battery)
  (let* ((axis (iint (battery-parent-axis battery)))
         (frag (if (= axis 1)
                   'payload
                   (axis-parts axis 'payload 'head 'tail))))
    (setf (battery-meter battery)
          (compile
            nil
            `(lambda (payload)
               (or (if (deep payload)
                       (handler-case
                         (case (get-speed ',world ,frag)
                           ,@(hash-pairs (battery-parents battery)))
                         (exit () nil))
                       (case payload ,@(hash-pairs (battery-roots battery))))
                   :slow))))))

(defun clock (world core)
  (let ((battery (icell-battery (get-battery world core))))
    (funcall (or (battery-meter battery)
                 (compile-meter world battery))
             (tail core))))

(defmacro get-speed (world core)
  (let ((s (gensym)))
    `(let ((,s ,core))
       (or (cached-speed ,s)
           (clock ,world ,s)))))

(defun install-root-stencil (world name icore hooks)
  (let* ((constant (icell-tail icore))
         (kernel (find-root world name constant))
         (stencil (stencil icore hooks kernel
                           (call-kernel-driver kernel nil icore hooks)))
         (battery (icell-head icore))
         (roots (battery-roots (icell-battery battery)))
         (old (gethash constant roots)))
    (if old
        (error 'reinstall-stencil :stencil old)
        (progn (push stencil (world-stencils world))
               (setf (gethash constant roots) stencil)
               (setf (battery-meter battery) nil)))))

(defun install-child-stencil (world name battery axis parent hooks)
  (let* ((parentk (stencil-kernel parent))
         (kernel (find-child parentk name axis))
         (ideal (if (kernel-static kernel)
                    (find-cons world battery (stencil-ideal parent))
                    battery))
         (stencil (child-stencil
                    parent ideal hooks kernel
                    (call-kernel-driver kernel parent ideal hooks)))
         (parents (battery-parents (icell-battery battery)))
         (old (gethash parent parents)))
    (if old
        (error 'reinstall-stencil :stencil old)
        (progn (push stencil (world-stencils world))
               (setf (gethash parent parents) stencil)
               (setf (battery-meter battery) nil)))))

; jet trees are used to specify kernel drivers
; in general you should construct such a list of ROOTs
; (probably with nested CHILD cores with GATEs as leaves)
; and pass it to LOAD-WORLD.

(defstruct jet-core
  (name nil :type uint)
  (driver nil :type (or null function))
  (children nil :type list))

(defstruct (jet-root
             (:constructor root (name constant driver &rest children))
             (:include jet-core))
  (constant nil :type uint))

(defstruct (jet-child
             (:constructor core (name axis driver &rest children))
             (:include jet-core))
  (axis nil :type uint))

; kernel drivers produce stencil drivers at stencil installation time
; stencil drivers produce a function (or nil) when given the axis of
; a formula within their battery. Presuming they returned a function,
; the function will receive a whole core as its argument and should produce
; a noun product or nil to fall back to nock.

; here is a sample driver that falls back in several ways:
; (lambda (kernel parent-stencil ideal hooks)
;   ; can also return nil if you only have drivers for certain stencils
;   ; but generally resolve hooks, do partial evaluation, etc.
;   (lambda (axis-in-battery)
;     ; for gates, axis-in-battery would be 1
;     (when (= 1 axis-in-battery) 
;       (lambda (core) nil))))

(defun gate-driver (sample-function)
  (lambda (kernel parent-stencil hooks ideal)
    (declare (ignore kernel parent-stencil ideal hooks))
    (lambda (axis)
      (when (= axis 1)
        (lambda (core)
          (funcall sample-function (head (tail core))))))))

; probably most of the leaf notes in your jet tree can be expressed
; by saying something like (GATE %add (lambda (sample) ...))
(defun gate (name sample-function)
  (core name 3 (gate-driver sample-function)))

; INSTALL-TREE and its helpers should probably only be called by LOAD-WORLD
(defun install-jet-children (world jet-parent parent-kernel)
  (dolist (child (jet-core-children jet-parent))
    (install-jet-child world parent-kernel child)))

(defun install-jet-child (world parent-kernel child)
  (install-jet-children
    world child (install-child
                  parent-kernel
                  (jet-core-name child)
                  (get-ideal-atom world (jet-child-axis child))
                  (jet-core-driver child))))

(defun install-jet-root (world root)
  (install-jet-children
    world root (install-root
                 world
                 (jet-core-name root)
                 (get-ideal-atom world (jet-root-constant root))
                 (jet-core-driver root))))

(defun install-tree (world roots)
  (dolist (r roots) (install-jet-root world r)))

; jet packs are used to persist jet registrations across worlds
; in general you should call INSTALL-JET-PACK indirectly by passing a saved
; pack to LOAD-WORLD. SAVE-JET-PACK is used for collecting registrations
; from a world (presumably with something like %fast hints enabled)
; into just such a pack.

(defun install-jet-pack (world pack)
  (loop for more = (cue pack #'identity #'cons) then (cdr more)
        while (consp more)
        with parents = (make-array 100 :adjustable t :fill-pointer 0)
        for s = (macrolet ((! (e) `(get-ideal world ,e))
                           (@ (e) `(get-ideal-atom world ,e))
                           (^ (e) `(get-ideal-cell world ,e)))
                  (decons (@@stem bulb) (car more)
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
        finally (unless (= 0 more)
                  (error 'exit))))

; destructively set the final nil of a list to a 0 (lisp list -> noun list)
(defun zero-terminate (list)
  (loop for c on list
        unless (cdr c) do (rplacd c 0))
  list)

(defun parent-axis (kernel)
  (etypecase kernel
    (dynamic-kernel (dynamic-kernel-axis kernel))
    (child-kernel 1)))

(defun save-jet-pack (world)
  (jam
    (find-ideal
      world
      (zero-terminate
        (loop with parents = (make-hash-table :test 'eq)
              for i upfrom 0
              for stencil in (reverse (world-stencils world))
              for kernel = (stencil-kernel stencil)
              for name = (kernel-name kernel)
              for ideal = (stencil-ideal stencil)
              for hooks = (stencil-hooks stencil)
              do (setf (gethash i parents) stencil)
              collect
              (etypecase kernel
                (root-kernel
                  `(0 ,name ,ideal . ,hooks))
                (child-kernel
                  (let* ((axis (parent-axis kernel))
                         (pstencil (child-stencil-parent stencil))
                         (pindex (gethash pstencil parents))
                         (battery (if (kernel-static kernel)
                                      (icell-head ideal)
                                      ideal)))
                    `(1 ,name ,battery ,axis ,pindex . ,hooks)))))))))

; for running real code (that deals with jets), this is the recommended
; toplevel call for creating a world. It's theoretically possible to
; install trees and jet packs into a running world, but unsupported.
(defun load-world (jet-tree &optional jet-pack)
  (let ((w (make-world)))
    (install-tree w jet-tree)
    (when jet-pack (install-jet-pack w jet-pack))
    w))

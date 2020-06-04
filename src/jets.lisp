(defpackage #:urbit/jets
  (:use #:cl #:urbit/common #:urbit/math #:urbit/zig
        #:urbit/data #:urbit/ideal #:urbit/serial)
  (:import-from #:alexandria #:if-let)
  (:import-from #:urbit/data #:exit)
  (:export #:root #:core #:gate #:get-speed #:get-battery
           #:measure #:zig-changes-speed
           #:install-child-stencil #:install-root-stencil
           #:load-world #:save-jet-pack #:install-jet-pack))

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
      (dynamic-kernel parent name (axis->zig axis) driver)))

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
        (setq c (make-child name axis parent driver)))))

; kernel finding is used by stencil installation, and will create a 
; kernel if one has not already been installed.

(defun find-root (world name constant)
  (root-place r world name constant
    (or r (setq r (root-kernel constant name nil)))))

(defun find-child (parent name axis)
  (child-place c parent name axis
    (or c (setq c (make-child name axis parent nil)))))

; installing a stencil causes matching cores to measure fast.
; usually it is done either by running something like a fast hint
; or by installing a battery pack.

(defun call-kernel-driver (kernel parent-stencil ideal hooks)
  (let ((kdriver (kernel-driver kernel)))
    (when kdriver
      (funcall kdriver kernel parent-stencil ideal hooks))))

(define-condition reinstall-stencil (error) (stencil))
(define-condition payload-conflict (error) (battery))

(defun invalidate-battery (b)
  (setf (assumption-valid (battery-stable b)) nil)
  (setf (battery-stable b) (make-assumption)))

(defun install-root-stencil (world name icore hooks)
  (let* ((constant (icell-tail icore))
         (battery (icell-battery (icell-head icore))))
    (flet ((make-stencil ()
             (let* ((kernel (find-root world name constant))
                    (driver (call-kernel-driver kernel nil icore hooks)))
               (stencil icore hooks kernel driver)))
           (meet (pairs)
             (invalidate-battery battery)
             (compile-root-meter (battery-stable battery) pairs))
           (save (stencil)
             (push stencil (world-stencils world))
             stencil))
      (let ((match (battery-match battery)))
        (typecase match
          (child-match (error 'payload-conflict :battery battery))
          (null
            (let* ((constants (make-hash-table :test 'eql))
                   (stencil (make-stencil))
                   (meter (meet `((,constant ',stencil))))
                   (match (make-root-match constants meter)))
              (setf (gethash constant constants) stencil)
              (setf (battery-match battery) match)
              (save stencil)))
          (root-match
            (let ((constants (root-match-constants match)))
              (if-let (old (gethash constant constants))
                (error 'reinstall-stencil :stencil old)
                (let ((stencil (make-stencil)))
                  (setf (gethash constant constants) stencil)
                  (setf (match-meter match) (meet (case-pairs constants)))
                  (save stencil))))))))))

(defun install-child-stencil (world name battery-ideal axis parent hooks)
  (let* ((battery (icell-battery battery-ideal))
         (match (battery-match battery)))
    (flet ((save (stencil)
             (push stencil (world-stencils world))
             stencil)
           (meet (z pairs)
             (invalidate-battery battery)
             (compile-child-meter world (battery-stable battery) z pairs))
           (make-stencil ()
             (let* ((parentk (stencil-kernel parent))
                    (kernel (find-child parentk name axis))
                    (ideal (if (kernel-static kernel)
                               (find-cons world battery-ideal
                                          (stencil-ideal parent))
                               battery-ideal))
                    (driver (call-kernel-driver kernel parent ideal hooks)))
               (child-stencil parent ideal hooks kernel driver)))) 
      (typecase match
        (root-match (error 'payload-conflict :battery battery)) 
        (null
          (let* ((parents (make-hash-table :test 'eq))
                 (stencil (make-stencil))
                 (z (axis->zig axis))
                 (meter (meet z `((,parent ',stencil))))
                 (match (make-child-match z parents meter)))
            (setf (gethash parent parents) stencil)
            (setf (battery-match battery) match)
            (save stencil)))
        (child-match
          (if (not (= (iint axis) (zig->axis (child-match-axis match))))
              (error 'payload-conflict :battery battery)
              (let ((parents (child-match-parents match)))
                (if-let (old (gethash parent parents))
                  (error 'reinstall-stencil :stencil old)
                  (let ((stencil (make-stencil)))
                    (setf (gethash parent parents) stencil)
                    (setf (match-meter match)
                          (meet (axis->zig axis) (case-pairs parents)))
                    (save stencil))))))))))

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
        finally (unless (zerop more) (error 'exit))))

; destructively set the final nil of a list to a 0 (lisp list -> noun list)
(defun zero-terminate (list)
  (if (null list)
      0
      (loop for c on list
            unless (cdr c) do (rplacd c 0)
            finally (return list))))

(defun parent-zig (kernel)
  (etypecase kernel
    (dynamic-kernel (dynamic-kernel-axis kernel))
    (child-kernel #*)))

(defun parent-axis (kernel)
  (zig->axis (parent-zig kernel)))

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
              do (setf (gethash stencil parents) i)
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
(defun load-world (&key jet-tree jet-pack hinter allow-registration)
  (let ((w (if hinter
               (make-world :hinter hinter :stable (not allow-registration))
               (make-world))))
    (install-tree w jet-tree)
    (when jet-pack (install-jet-pack w jet-pack))
    w))

; core measurement, for identifying cores that match prior registrations

(defun zig-changes-speed (z spd)
  (declare (zig z) (core-speed spd))
  (or (zerop (bit z 0)) ; editing the battery always changes speed
      (etypecase spd
        ((or void mean) nil)
        (slug t)
        (stop (zig-sub-p (subseq z 1) spd))
        ; spry is a struct-subtype of slow, and not treated differently here
        (slow
          (multiple-value-bind (shared more)
            (zig-common (subseq z 1) (slow-to spd))
            (and shared
                 (or (zerop (length more))
                     (zig-changes-speed more (slow-parent spd))))))
        (fast
          (or (= 1 (length z))
              (labels
                ((recur (z k)
                   (or (kernel-static k)
                       (multiple-value-bind (shared more)
                         (zig-common z (parent-zig k))
                         (when shared
                           (or (zerop (length more))
                               (zerop (bit more 0))
                               (recur (subseq more 1)
                                      (child-kernel-parent k))))))))
                (recur (subseq z 1) (stencil-kernel spd))))))))

(defun case-pairs (h)
  (loop for k being the hash-keys in h using (hash-value v)
        collect `(,k ',v)))

(defun compile-root-meter (assumption case-pairs)
  (compile
    nil
    `(lambda (payload)
       (if (deep payload)
           #*
           (case payload
             ,@case-pairs
             (t (cons :slug ,assumption)))))))

(defun compile-child-meter (world assumption z case-pairs)
  (compile
    nil
    `(lambda (payload)
       (if (not (deep payload))
           #* ; stop whole payload
           (multiple-value-bind (parent fail)
             ,(zig-compile-fail z 'payload 'head 'tail 'deep)
             (if (null parent)
                 fail ; stop at fail axis
                 (if (not (deep parent))
                     ,z ; stop at parent axis
                     (let ((pspd (get-speed ',world parent)))
                       (case pspd
                         ,@case-pairs
                         (t (if (typep pspd 'fast)
                                (make-spry ,z pspd ',assumption)
                                (make-slow ,z pspd))))))))))))

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

(defun measure (world core)
  (let ((head (get-battery world core)))
    (if (not (ideep head))
        :void   
        (let ((battery (icell-battery head)))
          (if-let (m (battery-match battery))
            (funcall (match-meter m) (tail core))
            (battery-stable battery)))))) ; mean

(defmacro get-speed (world core)
  (let ((c (gensym))
        (s (gensym)))
    `(let ((,c ,core))
       (or (valid-cached-speed ,c)
           (let ((,s (measure ,world ,c)))
             (setf (cached-speed ,c) ,s)
             ,s)))))

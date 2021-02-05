(defpackage #:urbit/nock/jets
  (:use #:cl #:urbit/nock/common #:urbit/nock/math #:urbit/nock/axis
        #:urbit/nock/data/core #:urbit/nock/data
        #:urbit/nock/ideal #:urbit/nock/world)
  (:import-from #:alexandria #:if-let)
  (:import-from #:urbit/nock/cord #:cord->string)
  (:export #:kernel-label #:kernel-static #:kernel-battery
           #:jet-root #:jet-core #:jet-deaf-gate
           #:deaf #:trap #:gate #:deaf-gate-driver
           #:get-speed #:get-battery #:measure #:measure-battery
           #:axle-changes-speed #:cell->core #:parent-axis
           #:install-child-stencil #:install-root-stencil
           #:load-world #:save-jet-pack #:install-jet-pack))

(in-package #:urbit/nock/jets)

; kernel installation is done at most once per world per kernel that
; the runtime has a driver for.

(defmacro hash-place (place-symbol hash-form key-form &body forms)
  (let ((h (gensym)) (k (gensym)))
    `(let ((,h ,hash-form)
           (,k ,key-form))
       (symbol-macrolet ((,place-symbol (gethash ,k ,h))) 
         ,@forms))))

(defmacro root-place (root-symbol name constant &body forms)
  `(hash-place ,root-symbol (world-roots *world*) (cons ,constant ,name)
     ,@forms))

(defmacro child-place (child-symbol parent name axis &body forms)
  `(hash-place ,child-symbol (kernel-children ,parent) (cons ,axis ,name)
     ,@forms))

(defun kernel-static (kernel)
  (not (typep kernel 'dynamic-kernel)))

(defun kernel-battery (kernel ideal)
  (if (kernel-static kernel)
      (icell-head ideal)
      ideal))

(defun stencil-battery (stencil)
  (kernel-battery (stencil-kernel stencil) (stencil-ideal stencil)))

(defun make-child (name axis parent driver)
  (if (and (= 1 axis) (kernel-static parent))
      (child-kernel parent name driver)
      (dynamic-kernel parent name axis driver)))

(define-condition reinstall-kernel (error) (kernel))

(defun install-root (name constant driver)
  (root-place r name constant
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

(defun find-root (name constant)
  (root-place r name constant
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

(defun kernel-label (kernel)
  (format nil "~{~A~^/~}"
          (loop for k = kernel then (child-kernel-parent k)
                collecting (cord->string (kernel-name k))
                until (typep k 'root-kernel))))

(defun install-root-stencil (name icore hooks)
  (let* ((constant (icell-tail icore))
         (battery-ideal (icell-head icore))
         (battery (icell-battery battery-ideal)))
    (flet ((make-stencil ()
             (let* ((kernel (find-root name constant))
                    (jet (call-kernel-driver kernel nil icore hooks)))
               (stencil icore hooks kernel jet)))
           (meet (pairs)
             (invalidate-battery battery)
             (compile-root-meter (battery-stable battery) pairs))
           (save (stencil)
             (push stencil (world-stencils *world*))
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

(defun install-child-stencil (name battery-ideal a parent hooks)
  (declare (axis a))
  (let* ((battery (icell-battery battery-ideal))
         (match (battery-match battery)))
    (flet ((save (stencil)
             (push stencil (world-stencils *world*))
             stencil)
           (meet (a pairs)
             (invalidate-battery battery)
             (compile-child-meter (battery-stable battery) a pairs))
           (make-stencil ()
             (let* ((parentk (stencil-kernel parent))
                    (kernel (find-child parentk name a))
                    (ideal (if (kernel-static kernel)
                               (find-cons battery-ideal
                                          (stencil-ideal parent))
                               battery-ideal))
                    (jet (call-kernel-driver kernel parent ideal hooks)))
               (child-stencil parent ideal hooks kernel jet))))
      (typecase match
        (root-match (error 'payload-conflict :battery battery)) 
        (null
          (let* ((parents (make-hash-table :test 'eq))
                 (stencil (make-stencil))
                 (meter (meet a `((,parent ',stencil))))
                 (match (make-child-match a parents meter)))
            (setf (gethash parent parents) stencil)
            (setf (battery-match battery) match)
            (save stencil)))
        (child-match
          (if (not (= (iint a) (child-match-axis match)))
              (error 'payload-conflict :battery battery)
              (let ((parents (child-match-parents match)))
                (if-let (old (gethash parent parents))
                  (error 'reinstall-stencil :stencil old)
                  (let ((stencil (make-stencil)))
                    (setf (gethash parent parents) stencil)
                    (setf (match-meter match)
                          (meet a (case-pairs parents)))
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
             (:constructor jet-root (name constant driver &rest children))
             (:include jet-core))
  (constant nil :type uint))

(defstruct (jet-child
             (:constructor jet-core (name axis driver &rest children))
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

; INSTALL-TREE and its helpers should probably only be called by LOAD-WORLD
(defun install-jet-children (jet-parent parent-kernel)
  (dolist (child (jet-core-children jet-parent))
    (install-jet-child parent-kernel child)))

(defun install-jet-child (parent-kernel child)
  (install-jet-children
    child (install-child
            parent-kernel
            (jet-core-name child)
            (get-ideal-atom (jet-child-axis child))
            (jet-core-driver child))))

(defun install-jet-root (root)
  (install-jet-children
    root (install-root
           (jet-core-name root)
           (get-ideal-atom (jet-root-constant root))
           (jet-core-driver root))))

(defun install-tree (roots)
  (dolist (r roots) (install-jet-root r)))

; jet packs are used to persist jet registrations across worlds
; in general you should call INSTALL-JET-PACK indirectly by passing a saved
; pack to LOAD-WORLD. SAVE-JET-PACK is used for collecting registrations
; from a world (presumably with something like %fast hints enabled)
; into just such a pack.

(defun install-jet-pack (pack)
  (loop for more = pack then (tail more)
        while (deep more)
        with parents = (make-array 100 :adjustable t :fill-pointer 0)
        for s = (macrolet ((! (e) `(get-ideal ,e))
                           (@ (e) `(get-ideal-atom ,e))
                           (^ (e) `(get-ideal-cell ,e)))
                  (dedata (@@stem bulb) (head more)
                    (ecase stem
                      (0 (dedata (@name core hooks) bulb
                           (dedata (^ @) core
                             (install-root-stencil
                               (@ name) (^ core) (! hooks)))))
                      (1 (dedata (@name ^battery @axis @@parent hooks) bulb
                           (install-child-stencil
                             (@ name) (^ battery) (@ axis)
                             (aref parents parent) (! hooks)))))))
        do (vector-push-extend s parents)
        finally (unless (zerop (cl-integer more)) (error 'exit))))

; destructively set the final nil of a list to a 0 (lisp list -> noun list)
(defun zero-terminate (list)
  (if (null list)
      0
      (loop for c on list
            unless (cdr c) do (rplacd c 0)
            finally (return list))))

(defun parent-axis (kernel)
  (etypecase kernel
    (dynamic-kernel (dynamic-kernel-axis kernel))
    (child-kernel 1)))

(defun save-jet-pack ()
  (find-ideal
    (zero-terminate
      (loop with parents = (make-hash-table :test 'eq)
            for i upfrom 0
            for stencil in (reverse (world-stencils *world*))
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
                  `(1 ,name ,battery ,axis ,pindex . ,hooks))))))))

; for running real code (that deals with jets), this is the recommended
; toplevel call for creating a world. It's theoretically possible to
; install trees and jet packs into a running world, but unsupported.
(defun load-world (&key jet-tree jet-pack hinter)
  (let ((*world* (if hinter
                     (make-world :hinter hinter)
                     (make-world))))
    (install-tree jet-tree)
    (when jet-pack (install-jet-pack jet-pack))
    *world*))

; core measurement, for identifying cores that match prior registrations

(defun axle-changes-speed (a len spd)
  (declare (axle a) (axle-length len) (core-speed spd))
  (dax (a len) (in-tail more)
    (or (not in-tail) ; editing the battery always changes speed
        (etypecase spd
          ((or void mean) nil)
          (slug t)
          (stop ; payload wrong shape: more must include stopped axis
            (let ((long (stop-axis spd))
                  (end (zerop more)))
              (if (= 1 long)
                  end
                  (or end
                      (let ((lang (axis-length long)))
                        (and (<= more lang)
                             (subaxle-p a long more (- lang more))))))))
          ; spry is a struct-subtype of slow, and treated identically here
          (slow
            (let* ((to (slow-to spd))
                   (tal (axis-length to)))
              (if (<= more tal)
                  ; includes to
                  (subaxle-p a to more (- tal more))
                  ; somewhere inside to, recurse
                  (let ((diff (- more tal)))
                    (and (subaxle-p to a tal diff)
                         (axle-changes-speed a diff (slow-parent spd)))))))
          (fast
            (labels
              ((rec (len k)
                 (declare (axis-length len) (kernel k))
                 (or
                   (zerop len) ; whole core
                   (kernel-static k) ; any part of static core
                   (let* ((to (parent-axis k))
                          (tal (axis-length to)))
                     (if (<= len tal)
                         (subaxle-p a to len (- tal len)) ; includes to
                         (let ((diff (- len tal)))
                           (when (subaxle-p to a tal diff) ; under to
                             (dax (a diff) (in-tail more)
                               (or (not in-tail) ; battery
                                   (rec more (child-kernel-parent k)))))))))))
              (rec more (stencil-kernel spd))))))))

(defun case-pairs (h)
  (loop for k being the hash-keys in h using (hash-value v)
        collect `(,k ',v)))

(defun compile-root-meter (assumption case-pairs)
  (compile
    nil
    `(lambda (payload)
       (if (deep payload)
           (make-stop 1)
           (case payload
             ,@case-pairs
             (t (make-slug ,assumption)))))))

(defmacro get-speed (core)
  (let ((c (gensym))
        (s (gensym)))
    `(let ((,c ,core))
       (or (valid-cached-speed ,c)
           (let ((,s (measure ,c)))
             (setf (cached-speed ,c) ,s)
             ,s)))))

(defun cell->core (cell)
  (typecase cell
    (core cell)
    (t (let ((spd (get-speed cell)))
         ; assumes get-speed will populate cached-battery
         (core-cons (cached-battery cell) (tail cell) spd
                    (or (cached-ideal cell) (cached-mug cell)))))))

(defun compile-frag-fail (a)
  (declare (axis a))
  (if (= 1 a)
      'payload
      `(let ((o payload))
         (block
           nil
           ,@(loop
               with len = (axis-length a)
               for i below len
               collect `(unless (deep o)
                          (return
                            (values
                              nil
                              ',(make-stop (if (zerop i) 1 (axle-mint a i))))))
                   collect `(setq o (,(if (axle-nth i a len) 'tail 'head) o)))
           o))))

(defun compile-parent-frag (a)
  (declare (axis a))
  (if (= 1 a)
      `(if (deep payload)
           (get-speed payload)
           (values nil ',(make-stop 1)))
      (let* ((last-accessor (if (logbitp 0 a) 'tail 'head))
             (sub (ash a -1)))
        `(multiple-value-bind (last-cell fail)
           ,(compile-frag-fail sub)
           (if (null last-cell)
               (values nil fail)
               (if (not (deep last-cell))
                   (values nil ',(make-stop sub))
                   (let ((core-cell (,last-accessor last-cell)))
                     (if (not (deep core-cell))
                         (values nil ',(make-stop a))
                         (let ((core (cell->core core-cell)))
                           (setf (,last-accessor last-cell) core)
                           (core-speed core))))))))))

(defun compile-child-meter (assumption a case-pairs)
  (declare (axis a))
  (compile
    nil
    `(lambda (payload)
       (multiple-value-bind (pspd fail) ,(compile-parent-frag a)
         (case pspd
           ((nil) fail)
           ,@case-pairs
           (t (if (typep pspd 'fast)
                  (make-spry ,a pspd ',assumption)
                  (make-slow ,a pspd))))))))

(defun get-battery (core)
  (or (cached-battery core)
      (let ((i (find-ideal (head core))))
        (setf (cached-battery core) i)
        i)))

(defun measure-battery (battery-icell payload)
  (declare (icell battery-icell))
  (let ((bat (icell-battery battery-icell)))
    (if-let (m (battery-match bat))
      (funcall (match-meter m) payload)
      (battery-stable bat)))) ; mean

(defun measure (core)
  (let ((battery (get-battery core)))
    (if (ideep battery)
        (measure-battery battery (tail core))
        :void)))

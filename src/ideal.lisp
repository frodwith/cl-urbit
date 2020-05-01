(defpackage #:urbit/ideal
  (:use #:cl #:urbit/data #:urbit/mug #:urbit/control)
  (:import-from #:urbit/math #:uint)
  (:export #:make-world #:find-ideal #:imug #:iint
           #:iatom #:iatom-mug #:iatom-int #:iatom=mugatom
           #:icell #:icell-mug #:icell-head #:icell-tail #:icell-meta
           #:formula #:make-formula #:formula-func #:formula-form
           #:fat #:make-fat #:fat-formula
           #:core #:battery
           #:icell=mugcell #:ideep #:icell-copy))

(in-package #:urbit/ideal)

; TL;DR: Use MAKE-WORLD to create a uniqueness context, and FIND-IDEAL to find
; the ideal for any noun within that context.

; ideals - values which represent "ideal" noun values. while there may be many
; cell objects in memory with a head of 0 and a tail of 0, there is only one
; ideal [0 0]. It is often valuable to use an ideal as the key in an eq
; hashtable, and certain kinds of metadata (particularly compiled nock formulae
; / battery information) are stored directly on ideals because looking them up
; is in the interpreter hot code path. The data protocol has a CACHED-IDEAL
; accessor to give this system a place to cache its lookups.

; in particular, core speed is generally not accessed through the ideal
; (though this is possible) because ideal-finding is expensive (a hashtable
; lookup+) when not cached, and so not suitable for values with varying parts
; (i.e. the samples of gates)

; placeholders
(deftype assumption () 'null)
(deftype axis-map (element-type)
  (declare (ignore element-type))
  'null)
(deftype core ()
  '(or (eql :fast) (eql :slow)))

(defstruct battery
  (arms nil :type (axis-map function))
  (unregistered nil :type (or null assumption))
  (parent-axis nil :type (or null ideal-atom))
  (roots (make-hash-table :test 'eql) :type hash-table)
  (parents (make-hash-table :test 'eq) :type hash-table))

; TODO: roots and parents could probably be faster in most cases than
; hash-tables, since quite often they contain a small number of entries.

(defstruct (formula (:constructor make-formula (form)))
  (form nil :read-only t :type (or list symbol))
  (func nil :type (or null function)))

(defstruct fat
  (battery nil :type (or null battery))
  (formula nil :type (or null formula))
  (core nil :type (or null core)))

(defstruct (icell (:constructor icons (head tail mug))
                  (:print-object print-icell))
  (head nil :read-only t :type ideal)
  (tail nil :read-only t :type ideal)
  (mug nil :read-only t :type mug)
  (meta nil :type (or null formula battery core fat)))

(defun print-icell (c &optional out)
  (print-unreadable-object (c out :type t)
    (labels ((recur (o tail)
               (if (ideep o)
                   (progn
                     (unless tail (write-char #\[ out))
                     (recur (icell-head o) nil)
                     (write-char #\space out)
                     (recur (icell-tail o) t)
                     (unless tail (write-char #\] out)))
                   (prin1 o out))))
      (recur c nil))))

(defun icell-battery (c)
  (macrolet ((with-b (&body forms)
               `(let ((b (make-battery)))
                  ,@forms
                  b)))
    (let ((m (icell-meta c)))
      (typecase m
        (battery m)
        (fat (or (fat-battery m)
                 (with-b (setf (fat-battery m) b))))
        (t (with-b (setf (icell-meta c)
                         (etypecase m
                           (null b)
                           (core (make-fat :core m :battery b))
                           (formula (make-fat :formula m :battery b))))))))))

(defstruct (iatom (:constructor make-iatom (int mug)))
  (int nil :read-only t :type bignum)
  (mug nil :read-only t :type mug))

(deftype ideal-atom () '(or fixnum iatom))
(deftype ideal () '(or ideal-atom icell))

(defun iint (i)
  (etypecase i
    (fixnum i)
    (iatom (iatom-int i))))

(defun imug (i)
  (declare (ideal i))
  (the mug
       (etypecase i
         (fixnum (murmug i))
         (iatom (iatom-mug i))
         (icell (icell-mug i)))))

(defun ideep (i)
  (declare (ideal i))
  (etypecase i
    (icell t)
    (ideal-atom nil)))

(defun iatom=mugatom (i a)
  (or (eql i a) ; takes care of actual fixnums (no cached mug)
      (and (not (cached-ideal a))
           (= (iatom-mug i) (cached-mug a))
           (= (iatom-int i) (cl-integer a))
           (progn
             (setf (cached-ideal a) i)
             t))))

(defun icell-copy (i c)
  (declare (icell i))
  ; TODO: copy speed from c if we don't have and available
  (setf (cached-ideal c) i))

; compare a non-eq icell and mugged cell with equal mugs
(defun icell=mugcell (i c)
  (declare (icell i))
  (if-let (ci (cached-ideal c))
    (eq i ci)
    (flet ((fast (i c)
             (if-let (ci (cached-ideal c))
               (shallow (eq i ci))
               (if (= (icell-mug i) (cached-mug c))
                   :deep
                   :diff))))
      (when (cell= i c
              #'ideep #'deep
              #'icell-head #'head
              #'icell-tail #'tail
              #'iatom=mugatom #'icell-copy #'fast)
        (icell-copy i c)))))

(defun cells-hash (c)
  (if (typep c 'icell)
      (icell-mug c)
      (mug c)))

(defun cells= (a b)
  (if (typep a 'icell)
      (if (typep b 'icell)
          (eq a b)
          (icell=mugcell a b))
      (icell=mugcell b a)))

(sb-ext:define-hash-table-test cells= cells-hash)

(defun atoms-hash (a)
  (if (typep a 'iatom)
      (iatom-mug a)
      (mug a)))

(defun atoms= (a b)
  (if (typep a 'iatom)
      (if (typep b 'iatom)
          (eq a b)
          (iatom=mugatom a b))
      (iatom=mugatom b a)))

(sb-ext:define-hash-table-test atoms= atoms-hash)

(defstruct (world (:constructor init-world ()))
  "the context in which uniqueness is ensured"
  ; split into two tables because
  ; 1) smaller tables = faster lookups
  ; 2) distinguished groups = faster hash/comparison
  (atoms (make-hash-table :test 'atoms= :weakness :key) :read-only t)
  (cells (make-hash-table :test 'cells= :weakness :key) :read-only t)
  (roots (make-hash-table :test 'equal) :read-only t)
  ; we push onto this list whenever we install a new stencil
  ; 1) keeps registered batteries from weakref-ing out, losing registrations
  ; 2) serves as a registration log for producing battery packs
  (stencils nil :type list)) 

(defun make-world (&optional jet-tree jet-pack)
  (let ((w (init-world)))
    (install-tree w jet-tree)
    (when jet-pack (install-jet-pack w jet-pack))
    w))

(defun hashed-ideal (table noun)
  (let ((found (gethash noun table)))
    (when found (setf (cached-ideal noun) found))
    found))

(defun create-iatom (atoms a)
  (let ((int (cl-integer a)))
    (etypecase int
      (fixnum (setf (cached-ideal a) int)
              int)
      (bignum (let ((big (make-iatom int (mug a))))
                (setf (cached-ideal a) big)
                (setf (gethash big atoms) big)
                big)))))

(defun create-icell (atoms cells mugged)
  (flet ((atomic (atom)
           (let ((iatom (create-iatom atoms atom)))
             (setf (cached-ideal atom) iatom)
             iatom))
         (fast (noun)
           (or (cached-ideal noun)
               (let ((d (deep noun)))
                 (values (hashed-ideal (if d cells atoms) noun) d))))
         (slow (cell head tail)
           (let ((icell (icons head tail (murmugs (imug head) (imug tail)))))
             (setf (cached-ideal cell) icell)
             (setf (gethash icell cells) icell)
             icell)))
    (sum-cell mugged #'atomic #'fast #'slow)))

(defun find-cons (world ihead itail)
  (find-ideal-cell world (cons ihead itail)))

(defun find-ideal-cell (world cell)
  (let ((cells (world-cells world)))
    (or (hashed-ideal cells cell)
        (create-icell (world-atoms world) cells cell))))

(defun find-ideal-atom (world a)
  (let ((atoms (world-atoms world)))
    (or (hashed-ideal atoms a)
        (create-iatom atoms a))))

(defun find-ideal (world noun)
  (if (deep noun)
      (find-ideal-cell world noun)
      (find-ideal-atom world noun)))

; world will not be evaluated unless no cached-ideal
(defmacro idealm (finder world noun)
  (let ((s (gensym)))
    `(let ((,s ,noun))
       (or (cached-ideal ,s)
           (,finder ,world ,s)))))

(defmacro get-ideal (world noun)
  `(idealm find-ideal ,world ,noun))

(defmacro get-ideal-atom (world a)
  `(idealm find-ideal-atom ,world ,a))

(defmacro get-ideal-cell (world cell)
  `(idealm find-ideal-cell ,world ,cell))

(defstruct kernel
  (name nil :type uint :read-only t)
  (driver nil :type (or null function) :read-only t)
  (children (make-hash-table :test 'equal) :read-only t)
  (stencils (make-hash-table :test 'eq) :read-only t))

(defstruct (root (:constructor make-root (constant name driver))
                 (:include kernel))
  (constant nil :type uint :read-only t))

(defmacro hash-place (place-symbol hash-form key-form &body forms)
  (let ((h (gensym)) (k (gensym)))
    `(let ((,h ,hash-form)
           (,k ,key-form))
       (symbol-macrolet ((,place-symbol (gethash ,k ,h))) 
         ,@forms))))

(defmacro root-place (root-symbol world name constant &body forms)
  `(hash-place ,root-symbol (world-roots ,world) (cons ,constant ,name)
     ,@forms))

; install functions are called during world initialization.
; TODO: devise a data structure we can pass to the world
; constructor that will call this, and don't export it
; (can still call from test)

(define-condition reinstall-kernel (error) (kernel))

(defun install-root (world name constant driver)
  (root-place r world name constant
    (if r
        (error 'reinstall-kernel r)
        (setq r (make-root constant name driver)))))

(defun find-root (world name constant)
  (root-place r world name constant
    (or r (setq r (make-root constant name nil)))))

(defstruct (child (:constructor make-static (parent name driver))
                  (:include kernel))
  (parent nil :type kernel :read-only t))

(defstruct (dynamic (:constructor make-dynamic (parent name axis driver))
                    (:include child))
  (axis nil :type uint :read-only t))

(defun make-child (name axis parent driver)
  (if (and (= 1 axis) (kernel-static parent))
      (make-static name parent driver)
      (make-dynamic name axis parent driver)))

(defmacro child-place (child-symbol parent name axis &body forms)
  `(hash-place ,child-symbol (kernel-children ,parent) (cons ,axis ,name)
     ,@forms))

(defun install-child (parent name axis driver)
  (child-place c parent name axis
    (if c
        (error 'reinstall-kernel :kernel c)
        (setq c (make-child parent name axis driver)))))

(defun find-child (parent name axis)
  (child-place c parent name axis
    (or c (setq c (make-child parent name axis nil)))))

(defun kernel-static (kernel)
  (not (typep kernel 'dynamic)))

(defun parent-axis (kernel)
  (etypecase kernel
    (dynamic (dynamic-axis kernel))
    (child 1)))

; stencils
; installing a stencil causes cores that match it to clock fast
; they are typically installed by
; 1) processing a jet pack
; 2) core registration (i.e. %fast hints)

(defstruct stencil
  (ideal nil :type ideal :read-only t) ; battery or static core (see kernel)
  (hooks nil :type ideal :read-only t) ; unprocessed hook list
  (kernel nil :type kernel :read-only t)
  (driver nil :type (or null function) :read-only t))

(defstruct (child-stencil (:include stencil))
  (parent nil :type stencil :read-only t))

(defun call-kernel-driver (kernel parent-stencil ideal hooks)
  (let ((kdriver (kernel-driver kernel)))
    (when kdriver
      (funcall kdriver kernel parent-stencil ideal hooks))))

(define-condition reinstall-stencil (error) (stencil))

(defun install-root-stencil (world name icore hooks)
  (let* ((constant (icell-tail icore))
         (kernel (find-root world name constant))
         (stencil (make-stencil
                    :ideal icore :kernel kernel :hooks hooks
                    :driver (call-kernel-driver kernel nil icore hooks)))
         (battery (icell-head icore))
         (roots (battery-roots (icell-battery battery)))
         (old (gethash constant roots)))
    (if old
        (error 'reinstall-stencil :stencil old)
        (progn (push stencil (world-stencils world))
               (setf (gethash constant roots) stencil)))))

(defun install-child-stencil (world name battery axis parent hooks)
  (let* ((parentk (stencil-kernel parent))
         (kernel (find-child parentk name axis))
         (ideal (if (kernel-static kernel)
                    (find-cons world battery (stencil-ideal parent))
                    battery))
         (stencil (make-child-stencil
                    :ideal ideal :kernel kernel :parent parent :hooks hooks
                    :driver (call-kernel-driver kernel parent ideal hooks)))
         (parents (battery-parents (icell-battery battery)))
         (old (gethash parent parents)))
    (if old
        (error 'reinstall-stencil :stencil old)
        (progn (push stencil (world-stencils world))
               (setf (gethash parent parents) stencil)))))

; jet trees 
; MAKE-WORLD takes an optional jet-tree argument
; a jet tree is a list of jet-root
; make them with ROOT and the optional children with CORE
; each kernel is associated with a driver. see GATE-DRIVER for an example.
; driver should be (lambda (kernel parent-stencil ideal hooks)
;                    ; resolve hooks, do partial evaluation, etc.
;                    (lambda (axis-in-battery)
;                      ; for gates, axis-in-battery is 1
;                      (when (= 1 axis-in-battery) #'driver)                      )
; driver can be nil or return nil at either level, in which case the nock 
; inside the battery will be used in the usual way.
;
; GATE is provided as a convenience wrapper for the leaves, as it's by far the
; most common case.

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

(defun gate-driver (sample-function)
  (lambda (kernel parent-stencil hooks ideal)
    (declare (ignore kernel parent-stencil ideal hooks))
    (lambda (axis)
      (when (= axis 1)
        (lambda (core)
          (funcall sample-function (head (tail core))))))))

(defun gate (name sample-function)
  (core name 3 (gate-driver sample-function)))

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

; jet packs

(defun zero-terminate (list)
  (loop for c on list
        unless (cdr c) do (rplacd c 0))
  list)

(defmacro need (cell expr)
  (let ((tmp (gensym)))
    `(let ((,tmp ,expr))
       (if ,(if cell
                `(not (deep ,tmp))
                `(deep ,tmp)) 
           (error ',(if cell 'cell-required 'atom-required)
                  :given ,tmp)
           ,tmp))))

(defmacro need-atom (expr)
  `(need nil ,expr))

(defmacro need-cell (expr)
  `(need t ,expr))

(defmacro denoun (bindings expr &body forms)
  (etypecase bindings
    (null `(progn ,@forms))
    (symbol
      (case bindings
        (@ `(progn (need-atom ,expr) ,@forms))
        (^ `(progn (need-cell ,expr) ,@forms))
        (t (let ((name (symbol-name bindings)))
             (multiple-value-bind (sym val)
               (let ((start (char name 0))
                     (pack (symbol-package bindings)))
                 (if (char= #\@ start)
                     (if (char= #\@ (char name 1))
                         (values (intern (subseq name 2) pack)
                                 `(cl-integer ,expr))
                         (values (intern (subseq name 1) pack)
                                 `(need-atom ,expr)))
                     (if (char= #\^ start)
                         (values (intern (subseq name 1) pack)
                                 `(need-cell ,expr))
                         (values bindings expr))))
               `(let ((,sym ,val)) ,@forms))))))
    (cons
      (destructuring-bind (one . more) bindings
        (if (null more)
            `(denoun ,one ,expr ,@forms)
            (let ((cell (gensym "CELL"))
                  (head (gensym "HEAD"))
                  (tail (gensym "TAIL")))
              `(let* ((,cell ,expr)
                      (,head (head ,cell))
                      (,tail (tail ,cell)))
                 (denoun ,one ,head (denoun ,more ,tail ,@forms)))))))))

(defun save-jet-pack (world)
  (jam
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
              (root `(0 ,name ,ideal . ,hooks))
              (child (let* ((axis (parent-axis kernel))
                            (pstencil (child-stencil-parent stencil))
                            (parenti (gethash pstencil parents))
                            (battery (if (kernel-static kernel)
                                         (icell-head ideal)
                                         ideal)))
                       `(1 ,name ,battery ,axis ,parenti . ,hooks))))))))

(defun cue (a)
  (declare (ignore a))
  ; TODO
  0)

(defun jam (n)
  (declare (ignore n))
  ; TODO
  0)

(defun install-jet-pack (world pack)
  (loop for more = (cue pack) then (tail more)
        while (deep more)
        with parents = (make-array 100 :adjustable t :fill-pointer 0
                                       :element-type 'stencil)
        for s = (macrolet ((! (e) `(get-ideal world ,e))
                           (@ (e) `(get-ideal-atom world ,e))
                           (^ (e) `(get-ideal-cell world ,e)))
                  (denoun (@@stem bulb) (head more)
                    (ecase stem
                      (0 (denoun (@name core hooks) bulb
                           (denoun (^ @) core
                             (install-root-stencil
                               world (@ name) (^ core) (! hooks)))))
                      (1 (denoun (@name ^battery @axis @@parent hooks) bulb
                           (install-child-stencil
                             world (@ name) (^ battery) (@ axis) 
                             (aref parents parent) (! hooks)))))))
        do (vector-push-extend s parents)
        finally (unless (= 0 (cl-integer more))
                  (error 'exit))))

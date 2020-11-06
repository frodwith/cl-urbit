(defpackage #:urbit/nock/nock
  (:use #:cl #:urbit/nock/math #:urbit/nock/axis #:urbit/nock/zig
        #:urbit/nock/common #:urbit/nock/cord
        #:urbit/nock/jets #:urbit/nock/equality
        #:urbit/nock/ideal #:urbit/nock/world
        #:urbit/nock/data #:urbit/nock/data/core
        #:urbit/nock/data/slimcell #:urbit/nock/data/slimatom)
  (:import-from #:alexandria #:when-let #:when-let* #:if-let)
  (:export #:bottle #:in-world #:need #:need-sample #:*world*
           #:icell-function #:cell-function #:fuzzy
           #:nock #:slam #:make-slam #:soft #:resolve-hook #:call-hook
           #:compile-dynamic-hint #:compile-static-hint
           #:hint-tag #:hint-next #:hint-clue
           #:with-bug-trap #:*bug-stack* #:exit-with
           #:before #:after #:around))

(in-package #:urbit/nock/nock)

; helpers

(defun icell-formula (c)
  (macrolet ((with-f (&body forms)
               `(let ((f (make-formula (compile-cell-raw c))))
                  ,@forms
                  f)))
    (let ((m (icell-meta c)))
      (typecase m
        (formula m)
        (fat (or (fat-formula m)
                 (with-f (setf (fat-formula m) f))))
        (t (with-f (setf (icell-meta c)
                         (etypecase m
                           (null f)
                           (core-speed (make-fat :speed m :formula f))
                           (battery (make-fat :battery m :formula f))))))))))

(define-symbol-macro
  formula-declarations
  '((ignorable s)
    (sb-ext:muffle-conditions sb-ext:compiler-note)))

(defun heavy-form (formula)
  `(lambda (s)
     (declare ,@formula-declarations
              (optimize (speed 3)
                        (space 1)
                        (compilation-speed 0)
                        (debug 0)
                        (safety 0)))
     ,(formula-form formula)))


(defconstant +light-threshold+ 100)
(defconstant +heavy-threshold+ 1000)
(declaim (fixnum +light-threshold+ +heavy-threshold+))

(defstruct light-memory
  (heavy nil :type (or null function))
  (runs 0 :type fixnum)
  (formula nil :read-only t))

(defun light-form (formula)
  `(lambda (s)
     (declare ,@formula-declarations
              (optimize (compilation-speed 3)
                        (space 3)
                        (debug 0)
                        (safety 0)
                        (speed 0)))
     (let* ((mem (load-time-value (make-light-memory :formula ',formula)))
            (heavy (light-memory-heavy mem)))
       (cond (heavy (funcall heavy s))
             ((= (light-memory-runs mem) +heavy-threshold+)
              (let ((formula (light-memory-formula mem)))
                (setq heavy (compile nil (heavy-form formula)))
                (setf (light-memory-heavy mem) heavy)
                (setf (formula-func formula) heavy)
                (funcall heavy s)))
             (t
              (incf (light-memory-runs mem))
              ,(formula-form formula))))))

(defun formula-function (formula)
  (or (formula-func formula)
      (setf (formula-func formula)
            (let ((runs 0)
                  (light nil)
                  (form (formula-form formula)))
              (lambda (subject)
                (cond (light (funcall light subject))
                      ((= runs +light-threshold+)
                       (setq light (compile nil (light-form formula)))
                       (setf (formula-func formula) light)
                       (funcall light subject))
                      (t (let ((sb-ext:*evaluator-mode* :interpret)
                               (s subject))
                           (declare (special s))
                           (incf runs)
                           (eval form)))))))))

(defun icell-function (icell)
  (formula-function (icell-formula icell)))

(defun cell-function (cell)
  (icell-function (get-ideal-cell cell)))

(defun nock (subject formula)
  (if (deep formula)
      (funcall (cell-function formula) subject)
      (error 'exit)))

(defparameter +crash+ '(error 'exit))

(defun compile-noun (i)
  (if (ideep i)
      (compile-cell i)
      +crash+))

(defun compile-cell (c)
  (formula-form (icell-formula c)))

(defmacro split (expr (head tail) &body forms)
  (let ((s (gensym)))
    `(let* ((,s ,expr)
            (,head (icell-head ,s))
            (,tail (icell-tail ,s)))
       ,@forms)))

(defmacro splash (expr (head tail) &body forms)
  (let ((s (gensym)))
    `(let ((,s ,expr))
       (if (ideep ,s)
           (split ,s (,head ,tail) ,@forms)
           +crash+))))

(defmacro atomic (expr name &body forms)
  (let ((s (gensym)))
    `(let ((,s ,expr))
       (if (ideep ,s)
           +crash+
           (let ((,name (iint ,s)))
             ,@forms)))))

; compiler proper -- converts ideals to familiar looking lisp,
; i.e. #<ICELL [0 0]> becomes (@0 0).
; improper formula shapes become +crash+.

(defun compile-cell-raw (c)
  (split c (op ar)
    (if (ideep op)
        (compile-autocons op ar)
        (case op
          (0  (compile-0  ar))
          (1  (compile-1  ar))
          (2  (compile-2  ar))
          (3  (compile-3  ar))
          (4  (compile-4  ar))
          (5  (compile-5  ar))
          (6  (compile-6  ar))
          (7  (compile-7  ar))
          (8  (compile-8  ar))
          (9  (compile-9  ar))
          (10 (compile-10 ar))
          (11 (compile-11 ar))
          (12 (compile-12 ar))
          (t +crash+)))))

(defun compile-autocons (head tail)
  `(^ ,(compile-cell head) ,(compile-noun tail)))

(defun compile-0 (a)
  (atomic a i
    `(@0 ,i)))

(defun compile-1 (a)
  `(@1 ,a))

(defun compile-2 (a)
  (splash a (subject formula)
    `(@2 ,(compile-noun subject) ,(compile-noun formula))))

(defun compile-3 (a)
  `(@3 ,(compile-noun a)))

(defun compile-4 (a)
  `(@4 ,(compile-noun a)))

(defun compile-5 (a)
  (splash a (one two)
    `(@5 ,(compile-noun one) ,(compile-noun two))))

(defun compile-6 (a)
  (splash a (test branches)
    (splash branches (yes no)
      `(@6 ,(compile-noun test)
           ,(compile-noun yes)
           ,(compile-noun no)))))

(defun compile-7 (a)
  (splash a (one two)
    `(@7 ,(compile-noun one) ,(compile-noun two))))

(defun compile-8 (a)
  (splash a (one two)
    `(@8 ,(compile-noun one) ,(compile-noun two))))

(defun compile-9 (a)
  (splash a (ax core)
    (atomic ax i
      `(@9 ,i ,(compile-noun core)))))

(defun compile-10 (a)
  (splash a (spec big)
    (splash spec (ax small)
      (atomic ax i
        `(@10 (,i ,(compile-noun small)) ,(compile-noun big))))))

(defun compile-11 (a)
  (splash a (hint next-formula)
    (let ((next-form (compile-noun next-formula)))
      (if (ideep hint)
          (split hint (tag clue-formula)
            (let* ((clue-form (compile-noun clue-formula))
                   (itag (iint tag))
                   (hinter (funcall (world-hinter *world*) itag
                                    clue-formula next-formula)))
              (or (when (consp hinter)
                    (destructuring-bind (tag . data) hinter
                      (case tag
                        (:before
                          `(@11d-before (,itag ,clue-form) ,next-form ',data))
                        (:after
                          `(@11d-after (,itag ,clue-form) ,next-form ',data))
                        (:around
                          (when (consp data)
                            (destructuring-bind (before . after) data
                              `(@11d-around
                                 (,itag ,clue-form)
                                 ,next-form ',before ',after)))))))
                  `(@11d (,itag ,clue-form) ,next-form))))
          (let* ((itag (iint hint))
                 (hinter (funcall (world-hinter *world*)
                                  itag nil next-formula)))
            (or (when (consp hinter)
                  (destructuring-bind (tag . data) hinter
                    (case tag
                      (:before
                        `(@11s-before ,itag ,next-form ',data))
                      (:after
                        `(@11s-after ,itag ,next-form ',data))
                      (:around
                        (when (consp data)
                          (destructuring-bind (before . after) data
                            `(@11s-around ,itag ,next-form
                                          ',before ',after)))))))
                `(@11s ,itag ,next-form)))))))

(defun compile-12 (a)
  `(@12 ,(compile-noun a)))

; nock operators implemented as macros

(defmacro ^ (head tail) `(slim-cons ,head ,tail))

(defmacro @0 (ax)
  (case ax
    (0 +crash+)
    (t (compile-axis ax 's 'head 'tail))))

(defmacro @1 (a)
  `(quote ,a))

(defmacro @2 (subject formula)
  `(nock ,subject ,formula))

(defmacro @3 (a)
  `(loob (deep ,a)))

(defmacro @4 (a)
  `(slim-malt (1+ (cl-integer ,a))))

(defmacro @5 (a b)
  `(loob (same ,a ,b)))

(defmacro @6 (test yes no)
  `(case (cl-integer ,test)
     (0 ,yes)
     (1 ,no)
     (t ,+crash+)))

(defmacro @7 (a b)
  `(let ((s ,a))
     (declare (ignorable s))
     ,b))

(defmacro @8 (a b)
  `(@7 (^ ,a s) ,b))

(defun call-jet (core axis-in-battery)
  (let ((spd (core-speed core)))
    (when (typep spd 'fast)
      (funcall
        (funcall (or (stencil-dispatch spd) (first-stencil-dispatch spd))
                 axis-in-battery)
        core))))

(defmacro @9 (axis core)
  (let ((jet-forms (and (> axis 1)
                        (not (tax axis))
                        `((call-jet s ,(mas axis))))))
    `(@7 (cell->core ,core)
         (let ((f (@0 ,axis)))
           (or ,@jet-forms (@2 s f))))))

(defun econs (z old head tail)
  (declare (zig z))
  (let ((spd (valid-cached-speed old)))
    (if (and spd (not (zig-changes-speed z spd)))
        (core-cons head tail spd nil)
        (slim-cons head tail))))

(defun edit-on (zig)
  (let ((end (1- (length zig))))
    (labels ((rec (i)
               (let ((tail (not (zerop (bit zig i)))))
                 (if (= i end)
                     (if tail
                         '(econs #*1 o (head o) n)
                         '(econs #*0 o n (tail o)))
                     (let* ((side (if tail 'tail 'head))
                            (more `(let ((o (,side o)))
                                     ,(rec (1+ i))))
                            (parts (if tail
                                       `((head o) ,more)
                                       `(,more (tail o)))))
                       `(econs ,(subseq zig i) o ,@parts))))))
      (rec 0))))

(defmacro @10 ((ax small) big)
  (case ax
    (0 +crash+)
    (1 `(progn ,big ,small))
    (t `(let ((o ,big) (n ,small))
          ,(edit-on (axis->zig ax))))))

; hint macros

(defmacro @11s (tag next)
  (declare (ignore tag))
  next)

(defmacro @11d ((tag clue) next)
  (declare (ignore tag))
  `(progn ,clue ,next))

(defmacro @11s-before (tag next handler)
  (declare (ignore tag))
  `(progn (funcall ,handler s) ,next))

(defmacro @11d-before ((tag clue) next handler)
  (declare (ignore tag))
  `(progn (funcall ,handler s ,clue) ,next))

(defmacro @11s-after (tag next handler)
  (declare (ignore tag))
  `(let ((pro ,next))
     (funcall ,handler s pro)
     pro))

(defmacro @11d-after ((tag clue) next handler)
  (declare (ignore tag))
  `(let ((clu ,clue)
         (pro ,next))
     (funcall ,handler s clu pro)
     pro))

(defmacro @11s-around (tag next before after)
  (declare (ignore tag))
  `(multiple-value-bind (pro token) (funcall ,before s)
     (unless pro 
       (setq pro ,next)
       (funcall ,after token pro))
     pro))

(defmacro @11d-around ((tag clue) next before after)
  (declare (ignore tag))
  `(multiple-value-bind (pro token) (funcall ,before s ,clue)
     (unless pro 
       (setq pro ,next)
       (funcall ,after token pro))
     pro))

; get the effect of establishing a new "toplevel" by dynamically binding to nil
(defparameter *djinn-stack* nil)

(define-condition need (error)
  ((sample :initarg :sample :reader need-sample)))

(define-condition skip ()
  ((levels :initform 1 :accessor skip-levels :type (integer 0))
   (wrapped :initarg :wrap :reader skip-wrapped :type exit)))

; make a function that caches the speed of gate for mutants.
; call the resulting function with a sample to slam the gate.
; use if repeatedly calling a gate with different samples.
(defun make-slam (gate)
  (let ((gate-speed (get-speed gate))
        (battery (get-battery gate)))
    (if (typep gate-speed 'void)
        (error 'cell-required :given battery)
        (let ((context (tail (tail gate)))
              (nock (icell-function battery))
              (sample-changes-speed (zig-changes-speed #*10 gate-speed)))
          (labels ((pay (sample) (slim-cons sample context))
                   (slam (payload spd)
                     (let ((subject (core-cons battery payload spd nil)))
                       (or (call-jet subject 1) (funcall nock subject))))
                   (quick (sample)
                     (slam (pay sample) gate-speed))
                   (measure (sample)
                     (let ((payload (pay sample)))
                       (slam payload (measure-battery battery payload))))
                   (recheck (sample)
                      (unless (speed-valid gate-speed)
                        (setq gate-speed (get-speed gate))
                        (setq sample-changes-speed
                              (zig-changes-speed #*10 gate-speed)))
                      (if sample-changes-speed
                          (measure sample)
                          (quick sample))))
            (if (typep gate-speed '(or fast stop))
                ; fast and stop stay valid forever
                (if sample-changes-speed #'measure #'quick)
                #'recheck))))))

(defun slam (gate sample)
  (funcall (make-slam gate) sample))

(defparameter +hunk+ (string->cord "hunk"))

(defmacro @12 (a)
  `(if (null *djinn-stack*)
      ,+crash+
      (let* ((sample ,a)
             (djinn (car *djinn-stack*))
             (boon (let ((*djinn-stack* (cdr *djinn-stack*)))
                     (handler-case (slam djinn sample)
                       (exit (e) (error 'skip :wrap e))
                       (skip (s) (incf (skip-levels s))
                                 (error s))))))
        (if (deep boon)
            (let ((u (tail boon)))
              (if (deep u)
                  (tail u)
                  (exit-with (slim-cons +hunk+ sample))))
            (error 'need :sample sample)))))

(defun compile-stencil-arm (battery name axis)
  (when-let (formula (icell-fragment-safe axis battery))
    (and (ideep formula)
         (symbol-function
           (compile
             (gensym
               (string-upcase
                 (format nil "~a/~a" (peg 2 axis) name)))
             (heavy-form (icell-formula formula)))))))

(defun compile-stencil-dispatch (stencil battery name sfx jet arms)
  (let ((dis `(case axis
                ,@arms
                (t (let ((arms ',arms)
                         (battery ',battery)
                         (sfx ',sfx)
                         (stencil ',stencil))
                     (if-let (arm (compile-stencil-arm battery sfx axis))
                       (progn
                         (setf (stencil-dispatch stencil)
                               (compile-stencil-dispatch
                                 stencil battery ',name sfx jet
                                 (cons `(,axis ',arm) arms)))
                         arm)
                       (error 'exit)))))))
    (compile
      name
      `(lambda (axis)
         (let ((jet ',jet))
           ,(if jet
                `(or (funcall jet axis) ,dis)
                dis))))))

; wrap an arm driver with code that runs the outer layer of nock and compares
; for sameness with the driver
; on success, prints a "."
; breaks into debugger on failure
; (for driver testing only!)
(defun fuzzy (kernel ideal axis driver)
  (let* ((battery (kernel-battery kernel ideal))
         (name (kernel-label kernel))
         (arm (compile-stencil-arm battery name axis))
         (testing t))
    (lambda (core)
      (when-let (pro (funcall driver core))
        (when testing
          (setq testing nil)
          (unwind-protect
            (if (same pro (funcall arm core))
                (princ #\.)
                (break))
            (setq testing t)))
        pro))))

(defun first-stencil-dispatch (stencil)
  (let* ((kernel (stencil-kernel stencil))
         (label (kernel-label kernel))
         (jet (stencil-jet stencil))
         (battery (kernel-battery kernel (stencil-ideal stencil)))
         (name (gensym (format nil "dispatch/~a" label))))
    (setf (stencil-dispatch stencil)
          (compile-stencil-dispatch stencil battery name label jet nil))))

; after code crashes, the stack items from spot/mean etc. are left on
; the bug stack. We don't use dynamic bindings for the hint execution
; because we specifically do not want stack unwind protection, allowing
; for bug stack inspection after an interrupt.
(defvar *bug-stack*)

; common usage when all you want is to trap for exits and get a stack trace
; primary value is nil on error, and secondary value will be the bug stack.
(defmacro with-bug-trap (&body forms)
  `(let (*bug-stack*)
     (handler-case (progn ,@forms)
       (exit (e) (declare (ignore e)) (values nil *bug-stack*)))))

(defmacro exit-with (item)
  `(progn
     (push ,item *bug-stack*)
     (error 'exit)))

(defmacro soft (gate &body forms)
  `(let (*bug-stack*)
     (handler-case
       (values :success (let ((*djinn-stack* (cons ,gate *djinn-stack*)))
                          ,@forms))
       (exit (e) (declare (ignore e)) (values :error *bug-stack*))
       (need (n) (values :block (need-sample n)))
       (skip (s) (let ((less (1- (skip-levels s))))
                   (if (zerop less)
                       (error (skip-wrapped s))
                       (progn
                         (setf (skip-levels s) less)
                         (error s))))))))

(defun call-hook (hook-fn subject)
  (nullify-exit (funcall hook-fn subject)))

(defun resolve-hook (name kernel parent-stencil hooks &key (skip 0))
  (declare (uint name skip))
  (labels ((formula (name kernel parent-stencil hooks skip)
             (or (when (zerop skip)
                   (loop for n = hooks then (icell-tail n)
                         while (ideep n)
                         for pair = (icell-head n)
                         do (when (ideep pair)
                              (let ((hook-name (icell-head pair))
                                    (hook-formula (icell-tail pair)))
                                (when (and (ideep hook-formula)
                                           (= name (iint hook-name)))
                                  (return hook-formula))))))
                 (when parent-stencil
                   (when-let
                     (inner
                       (formula
                         name
                         (stencil-kernel parent-stencil)
                         (and (typep parent-stencil 'child-stencil)
                              (child-stencil-parent parent-stencil))
                         (stencil-hooks parent-stencil)
                         (1- skip)))
                     (let ((pax (peg 3 (parent-axis kernel))))
                       (if (zerop skip)
                           (find-ideal `(7 (0 . ,pax) . ,inner))
                           inner)))))))
    (when-let (found (formula name kernel parent-stencil hooks skip))
      (icell-function found))))

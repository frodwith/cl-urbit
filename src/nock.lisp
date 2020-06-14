(defpackage #:urbit/nock
  (:use #:cl #:urbit/math #:urbit/zig #:urbit/jets #:urbit/equality
        #:urbit/ideal #:urbit/world #:urbit/data #:urbit/common #:urbit/syntax
        #:urbit/data/core #:urbit/data/slimcell #:urbit/data/slimatom)
  (:import-from #:alexandria #:when-let #:when-let*)
  (:export #:nock #:bottle #:in-world #:soft #:need #:need-sample
           #:compile-dynamic-hint #:compile-static-hint
           #:hint-tag #:hint-next #:hint-clue
           #:before #:after #:around))

(in-package #:urbit/nock)

; helpers

(defun compile-form (form)
  (compile nil `(lambda (s)
                  (declare (ignorable s) ; ignore unused subject (i.e. [1 1])
                           ; delete unreachable note (code after crash) (SBCL ONLY)
                           (sb-ext:muffle-conditions sb-ext:compiler-note))
                  ,form)))

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

(defun formula-function (formula)
  (or (formula-func formula)
      (setf (formula-func formula)
            (compile-form (formula-form formula)))))

(defparameter +crash+ '(error 'exit))

(defun nock (subject formula)
  (if (deep formula)
      (funcall (formula-function (icell-formula (get-ideal-cell formula)))
               subject)
      (error 'exit)))

(defun compile-noun (i)
  (if (ideep i)
      (compile-cell i)
      +crash+))

(defun compile-cell (c)
  (formula-form (icell-formula c)))

(defun loob (bool)
  (if bool 0 1))

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
                          `(@11d-before (,itag ,clue-form) ,next-form ,data))
                        (:after
                          `(@11d-after (,itag ,clue-form) ,next-form ,data))
                        (:catch
                          `(@11d-catch (,itag ,clue-form) ,next-form ,data))
                        (:around
                          (when (consp data)
                            (destructuring-bind (before . after) data
                              `(@11d-around
                                 (,itag ,clue-form)
                                 ,next-form ,before ,after)))))))
                  `(@11d (,itag ,clue-form) ,next-form))))
          (let* ((itag (iint hint))
                 (hinter (funcall (world-hinter *world*)
                                  itag nil next-formula)))
            (or (when (consp hinter)
                  (destructuring-bind (tag . data) hinter
                    (case tag
                      (:before
                        `(@11s-before ,itag ,next-form ,data))
                      (:after
                        `(@11s-after ,itag ,next-form ,data))
                      (:catch
                        `(@11s-catch ,itag ,next-form ,data))
                      (:around
                        (when (consp data)
                          (destructuring-bind (before . after) data
                            `(@11s-around ,itag ,next-form
                                          ,before ,after)))))))
                `(@11s ,itag ,next-form)))))))

(defun compile-12 (a)
  `(@12 ,(compile-noun a)))

; nock operators implemented as macros

(defmacro ^ (head tail) `(slim-cons ,head ,tail))

(defmacro @0 (ax)
  (case ax
    (0 +crash+)
    (1 's)
    (t (axis-parts ax 's 'head 'tail))))

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
  `(let ((s ,a)) ,b))

(defmacro @8 (a b)
  `(@7 (^ ,a s) ,b))

(defun call-jet (core axis-in-battery)
  (let ((spd (core-speed core)))
    (when (typep spd 'fast)
      (when-let* ((driver (stencil-driver spd))
                  (jet (funcall driver axis-in-battery)))
        (funcall jet core)))))

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

(defun pvax (pax)
  (map 'bit-vector (lambda (bool) (if bool 1 0)) pax))

; TODO: this loop might be clearer with a zig, making pvax unneccessary
(defun edit-on (pax)
  (destructuring-bind (tail . remain) pax
    (if (null remain)
        (if tail
            '(econs #*1 o (head o) n)
            '(econs #*0 o n (tail o)))
        (let* ((side (if tail 'tail 'head))
               (more `(let ((o (,side o)))
                        ,(edit-on remain)))
               (parts (if tail
                          `((head o) ,more)
                          `(,more (tail o)))))
          `(econs ,(pvax pax) o ,@parts)))))

(defmacro @10 ((ax small) big)
  (case ax
    (0 +crash+)
    (1 `(progn ,big ,small))
    (t `(let ((o ,big) (n ,small))
          ,(edit-on (pax ax))))))

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

(defmacro @11s-catch (tag next handler)
  (declare (ignore tag))
  `(handler-case ,next
     (exit (e)
       (funcall ,handler s e)
       (error e))))

(defmacro @11d-catch ((tag clue) next handler)
  (declare (ignore tag))
  `(let ((clu ,clue))
     (handler-case ,next
       (exit (e)
         (funcall ,handler s clu e)
         (error e)))))

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

(define-condition meta ()
  ((exit :initarg :exit :reader meta-exit)))

(enable-syntax)

(defun slam (gate sample)
  (let ((gate-speed (get-speed gate))
        (battery (get-battery gate)))
    (if (typep gate-speed 'void)
        (error 'cell-required :given battery)
        (let* ((context (tail (tail gate)))
               (payload (slim-cons sample context))
               (mutant-speed (if (zig-changes-speed #*10 gate-speed)
                                 (measure-battery battery payload)
                                 gate-speed))
              (subject (core-cons battery payload mutant-speed nil)))
          (or (call-jet subject 1)
              (funcall (formula-function (icell-formula battery))
                       subject))))))

(defmacro @12 (a)
  `(if (null *djinn-stack*)
      ,+crash+
      (let* ((sample ,a)
             (djinn (car *djinn-stack*))
             (boon (let ((*djinn-stack* (cdr *djinn-stack*)))
                     (handler-case (slam djinn sample)
                       (exit (e) (error 'meta :exit e))))))
        (if (deep boon)
            (let ((u (tail boon)))
              (if (deep u)
                  (tail u)
                  (exit-with [%hunk sample])))
            (error 'need :sample sample)))))

(defmacro soft (gate &body forms)
  `(let ((*djinn-stack* (cons ,gate *djinn-stack*)))
     (handler-case (values :success (progn ,@forms))
       (exit (e) (values :error (exit-stack e)))
       (need (n) (values :block (need-sample n)))
       (meta (m) (error (meta-exit m))))))

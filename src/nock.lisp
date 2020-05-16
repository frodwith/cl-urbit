(defpackage #:urbit/nock
  (:use #:cl
        #:urbit/ideal #:urbit/data #:urbit/math #:urbit/syntax #:urbit/jets)
  (:import-from #:urbit/common #:dedata)
  (:import-from #:urbit/equality #:same)
  (:import-from #:alexandria #:when-let*)
  (:export #:nock #:bottle #:in-world #:with-fast-hints
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
                           (core (make-fat :core m :formula f))
                           (battery (make-fat :battery m :formula f))))))))))

(defun formula-function (formula)
  (or (formula-func formula)
      (setf (formula-func formula)
            (compile-form (formula-form formula)))))

(defparameter +crash+ '(error 'exit))

(defparameter *world* nil)

(defmacro in-world (world &body forms)
  `(let ((*world* ,world))
     ,@forms))

(defmacro bottle (&body forms)
  `(in-world (make-world) ,@forms))

(defun ifind (noun)
  (or (cached-ideal noun)
      (if *world*
          (find-ideal *world* noun)
          (error "nock world unbound"))))

(defun nock (subject formula)
  (if (deep formula)
      (funcall (formula-function (icell-formula (ifind formula)))
               subject)
      (error 'exit)))

(defun copy (axis old new)
  ; TODO: copy speed if possible (see axis) from old to new
  (declare (ignore axis old))
  new)

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

; hints are handled specially - conditions are signalled allowing the caller
; to provide new forms. helper restarts allow the caller to supply only a
; handler function, generating wrapper code to call it properly.

(define-condition compile-hint ()
  ((tag :type integer :initarg :tag :accessor hint-tag)
   (next :type (or symbol list) :initarg :next :accessor hint-next)))

(define-condition compile-dynamic-hint (compile-hint)
  ((clue :type (or symbol list) :initarg :clue :accessor hint-clue)))

(define-condition compile-static-hint (compile-hint) ())

(defun compile-11 (a)
  (splash a (hint next-formula)
    (let ((next-form (compile-noun next-formula)))
      (if (ideep hint)
          (split hint (tag clue-formula)
            (let ((clue-form (compile-noun clue-formula))
                  (itag (iint tag)))
              (or (restart-case (signal 'compile-dynamic-hint
                                        :tag itag
                                        :next next-form
                                        :clue clue-form)
                    (before (handler)
                      `(@11d-before (,itag ,clue-form) ,next-form ,handler))
                    (after (handler)
                      `(@11d-after (,itag ,clue-form) ,next-form ,handler))
                    (around (before after)
                      `(@11d-around (,itag ,clue-form) ,next-form
                                    ,next-formula ,before ,after)))
                  `(@11d (,itag ,clue-form) ,next-form))))
          (let ((itag (iint hint)))
            (or (restart-case (signal 'compile-static-hint
                                      :tag itag
                                      :next next-form)
                  (before (handler)
                    `(@11s-before ,itag ,next-form ,handler))
                  (after (handler)
                    `(@11s-after ,itag ,next-form ,handler))
                  (around (before after)
                    `(@11s-around ,itag ,next-form
                                  ,next-formula ,before ,after)))
                `(@11s ,itag ,next-form)))))))

(defun compile-12 (a)
  `(@12 ,a))

; nock operators implemented as macros

(defmacro ^ (head tail) ; FIXME: special cells
  `(cons ,head ,tail))

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

(defmacro @4 (a) ;FIXME: special bignums
  `(1+ (cl-integer ,a)))

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
  (let ((spd (get-speed *world* core)))
    (when (typep spd 'fast)
      (when-let* ((driver (stencil-driver spd))
                  (jet (funcall driver axis-in-battery)))
        (funcall jet core)))))

(defmacro @9 (axis core)
  (let ((jet-forms (and (> axis 1)
                        (not (tax axis))
                        `((call-jet s ,(mas axis))))))
    `(@7 ,core
         (let ((f (@0 ,axis)))
           (or ,@jet-forms (@2 s f))))))

(defmacro @12 (a)
  (declare (ignore a))
  +crash+)

(defmacro @10 ((ax small) big)
  (case ax
    (0 +crash+)
    (1 `(progn ,big ,small))
    (t `(let ((o ,big) (n ,small))
          (edit-on ,ax)))))

(defmacro edit-on (ax)
  (case ax
    (2 '(copy 2 o (^ n (tail o))))
    (3 '(copy 3 o (^ (head o) n)))
    (t (let* ((tail (tax ax))
              (side (if tail 'tail 'head))
              (more `(let ((o (,side o)))
                       (edit-on ,(mas ax))))
              (parts (if tail
                         `((head o) ,more)
                         `(,more (tail o)))))
         `(copy ,ax o (^ ,@parts))))))

; hint macros

(defmacro @11s (tag next)
  (declare (ignore tag))
  next)

(defmacro @11d ((tag clue) next)
  (declare (ignore tag))
  `(progn ,clue ,next))

(defmacro @11s-before (tag next handler)
  `(progn (funcall ,handler s ,next ,tag)
          ,next))

(defmacro @11d-before ((tag clue) next handler)
  `(progn (funcall ,handler s ,next ,tag ,clue)
          ,next))

(defmacro @11s-after (tag next handler)
  `(let ((pro ,next))
     (funcall ,handler s ,next ,tag pro)
     pro))

(defmacro @11d-after ((tag clue) next handler)
  `(let ((clu ,clue)
         (pro ,next))
     (funcall ,handler s ,next ,tag clu pro)
     pro))

(defmacro @11s-around (tag form formula before after)
  `(or (funcall ,before s ,formula ,tag)
       (let ((pro ,form))
         (funcall ,after s ,formula ,tag pro)
         pro)))

(defmacro @11d-around ((tag clue) form formula before after)
  `(let ((clu ,clue))
     (or (funcall ,before s ,formula ,tag clu)
       (let ((pro ,form))
         (funcall ,after s ,formula ,tag clu pro)
         pro))))

(enable-cords)

(define-condition unregistered-parent (warning)
  ((name :type uint :initarg :name)
   (axis :type uint :initarg :axis)
   (core :initarg :core)))

(defun handle-fast (hint)
  (case (hint-tag hint)
    (%fast (invoke-restart
             'after
             (lambda (subject formula tag clue core)
               (declare (ignore subject formula tag))
               (when (typep (get-speed *world* core) 'slow)
                 (handler-case
                   (dedata (@name (@num @ax) hooks) clue
                     (case num
                       (0 (when (and (> ax 2) (tax ax))
                            (let* ((parent (dfrag ax core))
                                   (pspeed (get-speed *world* parent)))
                              (if (typep pspeed 'fast)
                                  (setf (cached-speed core)
                                        (install-child-stencil
                                          *world* name (head core) (mas ax)
                                          pspeed (get-ideal *world* hooks)))
                                  (warn 'unregistered-parent
                                        :name name :core core :axis ax)))))
                       (1 (when (zerop ax)
                            (let ((payload (tail core)))
                              (unless (deep payload)
                                (setf (cached-speed core)
                                      (install-root-stencil
                                        *world* name
                                        (get-ideal-cell *world* core)
                                        (get-ideal *world* hooks)))))))))
                   (exit () nil))))))))

(defmacro with-fast-hints (&body forms)
  `(handler-bind ((compile-dynamic-hint #'handle-fast))
     ,@forms))

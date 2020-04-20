(defpackage #:urbit/nock
  (:use #:cl #:urbit/ideal #:urbit/data #:urbit/math))

(in-package #:urbit/nock)

(defun compile-form (form)
  (compile nil `(lambda (a)
                  (declare (ignorable a) ; ignore unused subject (i.e. [1 1])
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
                           (core-speed (make-fat :core m :formula f))
                           (battery (make-fat :battery m :formula f))))))))))

(defun formula-function (formula)
  (or (formula-func formula)
      (setf (formula-func formula)
            (compile-form (formula-form formula)))))

(defun compile-cell (c)
  (formula-form (icell-formula c)))

(defun compile-noun (i)
  (if (ideep i)
      (compile-cell i)
      +crash+))

(defparameter +crash+ '(error 'exit))

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

(defun autocons (head tail) ; FIXME: special cells
  (cons head tail))

(defun compile-autocons (head tail)
  `(autocons ,(compile-cell head) ,(compile-noun tail)))

(defconstant +small+ 4096)
(defparameter +small-fragments+
  (loop with v = (make-array +small+)
        initially (setf (aref v 0) +crash+
                        (aref v 1) 'a)
        for a from 2 below +small+
        for tail = nil then (not tail)
        for parent = 1 then (if tail parent (1+ parent))
        for part = (if tail 'tail 'head)
        for in = (if tail in (aref v parent))
        do (setf (aref v a) (list part in))
        finally (return v)))

(defun compile-fragment (ax)
  (declare (integer ax))
  (loop for tail in (pax ax)
        for part = (if tail 'tail 'head)
        for s = (list part 'a) then (list part s)
        finally (return s)))

(defun compile-0 (a)
  (if (ideep a)
      +crash+
      (let ((i (cl-integer a)))
        (if (< i +small+)
          (aref +small-fragments+ i)
          (compile-fragment i)))))

(defun compile-1 (a)
  `(quote ,a))

; FIXME: not how we're doing this
(defparameter *world* nil)

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

(defun compile-2 (a)
  (splash a (subject formula)
    `(nock ,(compile-noun subject) ,(compile-noun formula))))

(defun compile-3 (a)
  `(deep ,(compile-noun a)))

(defun bump (a) ;FIXME: special bignums
  (1+ (cl-integer a)))

(defun compile-4 (a)
  `(bump ,(compile-noun a)))

(defun same (a b)
  (if (urbit/equality:same a b) 0 1))

(defun compile-5 (a)
  (splash a (one two)
    `(same ,(compile-noun one) ,(compile-noun two))))

(defun compile-6 (a)
  (splash a (test branches)
    (splash branches (yes no)
      `(case ,(compile-noun test)
         (0 ,(compile-noun yes))
         (1 ,(compile-noun no))
         (t ,+crash+)))))

(defun compile-7 (a)
  (splash a (one two)
    `(let ((a ,(compile-noun one)))
       ,(compile-noun two))))

(defun compile-8 (a)
  (splash a (one two)
    `(let ((a (autocons ,(compile-noun one) a)))
       ,(compile-noun two))))

(defun compile-9 (a)
  (splash a (frag core)
    `(let ((a ,(compile-noun core)))
       (nock a ,(compile-0 frag)))))

(defun copy (axis old new)
  (declare (ignore axis old))
  new)

(defun editcons (ax big head tail)
  (let ((c (autocons head tail)))
    (copy ax big c)))

(defun editcons-form (ax tail more)
  `(editcons ,ax old ,@(if tail
                           `((head old) ,more)
                           `(,more (tail old)))))

(defun more-form (tail more)
  `(let ((old (,(if tail 'tail 'head) old)))
     ,more))

(defparameter +small-edits+
  (loop with v = (make-array +small+)
        initially (setf (aref v 0) +crash+
                        (aref v 1) 'new
                        ; easier to exclude the final lets if we cheat 2 and 3
                        (aref v 2) '(editcons 2 old new (tail old))
                        (aref v 3) '(editcons 3 old (head old) new))
        for i from 4 below +small+
        ; tricky to count tail and mas, compute for clarity/simplicity
        for tail = (tax i)
        do (setf (aref v i)
                 (editcons-form i tail (more-form tail (aref v (mas i)))))
        finally (return v)))

(defun compile-edit (ax)
  (if (< ax +small+)
      (aref +small-edits+ ax)
      (let ((tail (tax ax)))
        (editcons-form ax tail (more-form tail (compile-edit (mas ax)))))))

(defun compile-10 (a)
  (splash a (spec old)
    (splash spec (ax new)
      (if (ideep ax)
          +crash+
          (let ((iax (cl-integer ax)))
            (if (= 0 iax)
                +crash+
                `(let ((old ,(compile-noun old))
                       (new ,(compile-noun new)))
                   ,(compile-edit iax))))))))

(defun compile-11 (a)
  (splash a (hint next-formula)
    (let ((next (compile-noun next-formula)))
      (if (ideep hint)
          (split hint (tag clue-formula)
            (declare (ignore tag))
            (let ((clue (compile-noun clue-formula)))
              ; no dynamic hints yet
              `(progn ,clue ,next)))
          ; no static hints yet
          next))))

(defun compile-12 (a)
  (declare (ignore a))
  +crash+)

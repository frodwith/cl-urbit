(defpackage #:urbit/nock
  (:use #:cl #:urbit/ideal #:urbit/data #:urbit/math)
  (:import-from #:urbit/equality #:same)
  (:export #:.* #:in-world #:bottle))

(in-package #:urbit/nock)

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

(defun compile-cell (c)
  (formula-form (icell-formula c)))

(defparameter +crash+ '(error 'exit))

(defun compile-noun (i)
  (if (ideep i)
      (compile-cell i)
      +crash+))

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

(defun ^ (head tail) ; FIXME: special cells
  (cons head tail))

(defun compile-autocons (head tail)
  `(^ ,(compile-cell head) ,(compile-noun tail)))

(defmacro frag (ax)
  (declare (uint ax))
  (case ax
    (0 +crash+)
    (1 's)
    (t (loop for tail in (pax ax)
             for part = (if tail 'tail 'head)
             for s = (list part 's) then (list part s)
             finally (return s)))))

(defun compile-0 (a)
  (if (ideep a)
      +crash+
      `(frag ,(iint a))))

(defun compile-1 (a)
  `(quote ,a))

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

(defun .* (subject formula)
  (if (deep formula)
      (funcall (formula-function (icell-formula (ifind formula)))
               subject)
      (error 'exit)))

(defun compile-2 (a)
  (splash a (subject formula)
    `(.* ,(compile-noun subject) ,(compile-noun formula))))

(defun loob (bool)
  (if bool 0 1))

(defun .? (a)
  (loob (deep a)))

(defun compile-3 (a)
  `(.? ,(compile-noun a)))

(defun .+ (a) ;FIXME: special bignums
  (1+ (cl-integer a)))

(defun compile-4 (a)
  `(.+ ,(compile-noun a)))

(defun .= (a b)
  (loob (same a b)))

(defun compile-5 (a)
  (splash a (one two)
    `(.= ,(compile-noun one) ,(compile-noun two))))

(defmacro lif (test yes no)
  `(case (cl-integer ,test)
     (0 ,yes)
     (1 ,no)
     (t ,+crash+)))

(defun compile-6 (a)
  (splash a (test branches)
    (splash branches (yes no)
      `(lif ,(compile-noun test)
            ,(compile-noun yes)
            ,(compile-noun no)))))

(defmacro => (a b)
  `(let ((s ,a)) ,b))

(defun compile-7 (a)
  (splash a (one two)
    `(=> ,(compile-noun one) ,(compile-noun two))))

(defmacro =+ (a b)
  `(=> (^ ,a s) ,b))

(defun compile-8 (a)
  (splash a (one two)
    `(=+ ,(compile-noun one) ,(compile-noun two))))

(defmacro kick (axis core)
  `(=> ,core (.* s ,(compile-0 axis))))

(defun compile-9 (a)
  (splash a (frag core)
    `(kick ,frag ,(compile-noun core))))

(defun copy (axis old new)
  (declare (ignore axis old))
  new)

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

(defmacro edit (ax small big)
  (case ax
    (0 +crash+)
    (1 `(progn ,big ,small))
    (t `(let ((o ,big)
              (n ,small))
          (edit-on ,ax)))))

(defun compile-10 (a)
  (splash a (spec big)
    (splash spec (ax small)
      (if (ideep ax)
          +crash+
          `(edit ,(iint ax)
                 ,(compile-noun small)
                 ,(compile-noun big))))))

(let ((s '(1 2 . 3)))
  (edit 6 (.+ (frag 6)) (frag 1)))
  ;(EDIT 6 (.+ (FRAG 6)) (FRAG 1)))
  ;(EDIT 6 (.+ (FRAG 6)) (FRAG 1)))

(define-condition compile-condition () ())

(define-condition compile-dynamic-hint (compile-condition)
  ((tag :type integer)))

(define-condition compile-static-hint (compile-condition)
  ((tag :type integer)))

(defun compile-11 (a)
  (splash a (hint next-formula)
    (let ((next-form (compile-noun next-formula)))
      (if (ideep hint)
          (split hint (tag clue-formula)
            (let ((clue-form (compile-noun clue-formula)))
              (or (restart-case (signal 'compile-dynamic-hint
                                        :tag (iint tag))
                    (before (handler)
                      `(progn (funcall ',handler subject ,next-formula
                                       ,clue-form)
                              ,next-form))
                    (after (handler)
                      `(let ((clue ,clue-form)
                             (product ,next-form))
                         (funcall ',handler subject ,next-formula product
                                  clue)
                         product))
                    (around (before after)
                      `(let ((clue ,clue-form))
                         (or (funcall ',before a ,next-formula clue)
                             (let ((product ,next-form))
                               (funcall ',after subject ,next-formula product
                                        clue))))))
                  `(progn ,clue-form ,next-form))))
          (or (restart-case (signal 'compile-static-hint
                                    :tag (iint hint))
                (before (handler)
                  `(progn (funcall ',handler subject ,next-formula)
                          next-form))
                (after (handler)
                  `(let ((product ,next-formula))
                     (funcall ',handler subject ,next-formula product)
                     product))
                (around (before after)
                  `(or (funcall ',before a ,next-formula)
                       (let ((product ,next-form))
                         (funcall ',after a ,next-formula product)
                         product))))
              next-form)))))

(defun compile-12 (a)
  (declare (ignore a))
  +crash+)

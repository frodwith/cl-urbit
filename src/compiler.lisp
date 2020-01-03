(in-package #:urbit/compiler)

(defparameter +crash+ '(error 'exit))

(defun crash-fn (a)
  (declare (ignore a))
  (error 'exit))

(defun formula (noun)
  (if (atomp noun)
      #'crash-fn
      (let ((u (unique noun)))
        (cache-field (nock-meta u) nock-meta-func
          (compile nil `(lambda (a)
                          (declare (ignorable a) ; ignore unused subject (i.e. [1 1])
                              ; delete unreachable note (code after crash) (SBCL ONLY)
                              (sb-ext:muffle-conditions sb-ext:compiler-note))
                          ,(compile-cell u)))))))

(defun nock (subject formula)
  (funcall (formula formula) subject))

(defun compile-arm (battery battery-axis)
  (let ((body (compile-cell (constant-frag battery battery-axis))))
    (compile nil `(lambda (subject battery-axis)
                    (declare (ignore battery-axis)
                             (sb-ext:muffle-conditions sb-ext:compiler-note))
                    (let ((a (make-core (quote ,battery)
                                        (tail subject)
                                        (cached-mug subject))))
                      (declare (ignorable a))
                      ,body)))))

(defun need-atom (a)
  (etypecase a
    (constant-cell +crash+)
    (constant-atom nil)))

(defun need-cell (a)
  (etypecase a
    (constant-cell nil)
    (constant-atom +crash+)))

(defmacro split (cell-expr (head tail) &body forms)
  (let ((s (gensym)))
    `(let* ((,s ,cell-expr)
            (,head (constant-cell-head ,s))
            (,tail (constant-cell-tail ,s)))
       ,@forms)))

(defmacro splash (cell-expr (head tail) &body forms)
  (let ((s (gensym)))
    `(let ((,s ,cell-expr))
       (or (need-cell ,s)
           (split ,s (,head ,tail) ,@forms)))))

(defun compile-raw (formula)
  (declare (type formula constant-cell))
  (split formula (op ar)
    (etypecase op
      (constant-atom +crash+)
      (constant-cell (compile-autocons op ar))
      (fixnum
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
          (t +crash+))))))

(defun compile-cell (a)
  (nock-meta-form (constant-cell-nock-meta a)))

(defun compile-noun (a)
  (etypecase a
    (constant-cell (compile-cell a))
    (constant-atom +crash+)))

(defun compile-autocons (head tail)
  `(scons ,(compile-cell head) ,(compile-noun tail)))

(defun compile-fragment (axis-integer subject-symbol)
  (case axis-integer
    (0 +crash+)
    (1 subject-symbol)
    (t (let ((access-symbol (ecase (cap axis-integer)
                              (2 'head)
                              (3 'tail))))
         (compile-fragment (mas axis-integer)
                           (list access-symbol subject-symbol))))))

(defun compile-0 (a)
  (etypecase a
    (constant-cell +crash+)
    (constant-atom (compile-fragment (constant-atom-num a) 'a))))

(defun compile-1 (a)
  `(quote ,a))

(defun compile-2 (a)
  (splash a (subject formula)
    `(nock ,(compile-noun subject) ,(compile-noun formula))))

(defun deep (a)
  (if (cellp a) 0 1))

(defun compile-3 (a)
  `(deep ,(compile-noun a)))

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
    `(let ((a (scons ,(compile-noun one) a)))
       ,(compile-noun two))))

(defun pull (core battery-axis)
  (let ((noun (unique-head core)))
    (etypecase noun
      (constant-atom (error 'exit))
      (constant-cell
        (let* ((meta (constant-cell-battery-meta noun))
               (arms (battery-meta-arms meta))
               (arm  (or (lookup arms battery-axis)
                         (let ((func (compile-arm noun battery-axis)))
                           (setf (battery-meta-arms meta)
                                 (insert arms battery-axis func))
                           func))))
          (funcall arm core battery-axis))))))

(defun compile-9 (a)
  (splash a (frag core)
    (or (need-atom frag)
        (case frag
          (0 +crash+)
          (1 `(let ((a ,(compile-noun core)) (nock a a))))
          (t (ecase (cap frag)
               (2 `(pull ,(mas frag) ,(compile-noun core)))
               (3 `(let ((a ,(compile-noun core)))
                     (nock a ,(compile-0 frag))))))))))

(defun compile-edit (a little big kons)
  (case a
    (2 (funcall kons little `(tail ,big)))
    (3 (funcall kons `(head ,big) little))
    (t (let* ((bigsym (gensym))
              (mutant (compile-edit (mas a) little bigsym
                       (lambda (head tail) `(scons ,head ,tail)))))
         (ecase (cap a)
           (2 `(let ((,bigsym (head ,big)))
                 ,(funcall kons mutant `(tail ,big))))
           (3 `(let ((,bigsym (tail ,big)))
                 ,(funcall kons `(head ,big) mutant))))))))

(defun edit-cons (ax big head tail)
  (declare (ignore ax big))
  (scons head tail))

(defun compile-edit-root (ax little big)
  (let* ((bigsym  (gensym))
         (bigform (compile-noun big))
         (body    (compile-edit ax (compile-noun little) bigsym
                       (lambda (head tail)
                         `(edit-cons ,ax ,bigsym ,head ,tail)))))
    `(let ((,bigsym ,bigform))
       ,body)))

(defun compile-10 (a)
  (splash a (spec big)
    (splash spec (ax little)
      (etypecase ax
        (constant-cell +crash+)
        (constant-atom (compile-edit-root (constant-atom-num ax) little big))
        (fixnum (case ax
                  (0 +crash+)
                  (1 `(progn
                        ,(compile-noun big)
                        ,(compile-noun little)))
                  (t (compile-edit-root ax little big))))))))

(defun compile-11 (a)
  (splash a (hint next-formula)
    (let ((next (compile-noun next-formula)))
      (etypecase hint
        ; no currently supported static hints
        ((or fixnum constant-atom) next)
        ; no dynamic hints either
        (constant-cell
          (split hint (tag clue-formula)
            (declare (ignore tag))
            (let ((clue (compile-noun clue-formula)))
              `(progn ,clue ,next))))))))

(defun compile-12 (a)
  (declare (ignore a))
  +crash+)

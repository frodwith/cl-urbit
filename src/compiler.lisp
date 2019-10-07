(defpackage #:urbit/compiler
  (:use :cl)
  (:import-from :urbit/math :cap :mas)
  (:import-from :urbit/atom :bump)
  (:import-from :urbit/cell :cellp :head :tail)
  (:import-from :urbit/error :exit)
  (:import-from :urbit/formula :formula :nock)
  (:import-from :urbit/data/slimcell :scons)
  (:import-from :urbit/data/constant-atom :constant-atom :constant-atom-num)
  (:import-from :urbit/data/constant-cell :constant-cell
                :constant-cell-head :constant-cell-tail :constant-cell-nock
                :make-nock-meta :nock-meta-func :nock-meta-form))

(in-package :urbit/compiler)

(defparameter +crash+ '(error 'exit))

(defmethod formula ((a constant-cell))
  (let ((meta (nock-meta a)))
    (or (nock-meta-func meta)
        (setf (nock-meta-func meta)
              ; ignore unused (i.e. quote only function)
              ; delete unreachable is note (code after crash) (SBCL ONLY)
              (let ((form `(lambda (a)
                             (declare (ignorable a) (sb-ext:muffle-conditions sb-ext:compiler-note))
                             ,(qcell a))))
                (compile nil form))))))

(defmacro split (cell-expr (head tail) &body forms)
  (let ((s (gensym)))
    `(let* ((,s ,cell-expr)
            (,head (constant-cell-head ,s))
            (,tail (constant-cell-tail ,s)))
       ,@forms)))

(defmacro splash (cell-expr (head tail) &body forms)
  (let ((s (gensym)))
    `(let ((,s ,cell-expr))
       (or (nc ,s)
           (split ,s (,head ,tail) ,@forms)))))

(defun nock-meta (a)
  (or (constant-cell-nock a)
      (setf (constant-cell-nock a)
            (make-nock-meta
              (split a (op ar)
                (etypecase op
                  (constant-atom +crash+)
                  (constant-cell (qcons op ar))
                  (fixnum
                    (case op
                      (0  (q0  ar))
                      (1  (q1  ar))
                      (2  (q2  ar))
                      (3  (q3  ar))
                      (4  (q4  ar))
                      (5  (q5  ar))
                      (6  (q6  ar))
                      (7  (q7  ar))
                      (8  (q8  ar))
                      (9  (q9  ar))
                      (10 (q10 ar))
                      (11 (q11 ar))
                      (12 (q12 ar))
                      (t +crash+)))))))))

(defun qf (a)
  (etypecase a
    (constant-cell (qcell a))
    ((or fixnum constant-atom) +crash+)))

(defun qcell (a)
  (nock-meta-form (nock-meta a)))

(defun qcons (head tail)
  `(scons ,(qcell head) ,(qf tail)))

(defun qax (a s)
  (case a
    (0 +crash+)
    (1 s)
    (t (let ((f (ecase (cap a)
                  (2 'head)
                  (3 'tail))))
         (qax (mas a) (list f s))))))

(defun q0 (a)
  (etypecase a
    (fixnum (qax a 'a))
    (constant-atom (qax (constant-atom-num a) 'a))
    (constant-cell +crash+)))

(defun q1 (a)
  `(quote ,a))

(defun na (a)
  (etypecase a
    (constant-cell +crash+)
    ((or fixnum constant-atom) nil)))

(defun nc (a)
  (etypecase a
    (constant-cell nil)
    ((or fixnum constant-atom) +crash+)))

(defun q2 (a)
  (splash a (subject formula)
    `(nock ,(qf subject) ,(qf formula))))

(defun deep (a)
  (if (cellp a) 0 1))

(defun q3 (a)
  `(deep ,(qf a)))

(defun q4 (a)
  `(bump ,(qf a)))

(defun same (a b)
  (if (urbit/equality:same a b) 0 1))

(defun q5 (a)
  (splash a (one two)
    `(same ,(qf one) ,(qf two))))

(defun q6 (a)
  (splash a (test bran)
    (splash bran (yes no)
      `(case ,(qf test)
         (0 ,(qf yes))
         (1 ,(qf no))
         (t ,+crash+)))))

(defun q7 (a)
  (splash a (one two)
    `(let ((a ,(qf one)))
       ,(qf two))))

(defun q8 (a)
  (splash a (one two)
    `(let ((a (scons ,(qf one) a)))
       ,(qf two))))

(defun q9 (a)
  (splash a (frag core)
    (or (na frag)
        (case frag
          (0 +crash+)
          (1 `(let ((a ,(qf core)) (nock a a))))
          (t (ecase (cap frag)
               (2 `(kick ,frag ,(qf core)))
               (3 `(let ((a ,(qf core)))
                     (nock a ,(q0 frag))))))))))

(defun qed (a little big kons)
  (case a
    (2 (funcall kons little `(tail ,big)))
    (3 (funcall kons `(head ,big) little))
    (t (let* ((bigsym (gensym))
              (mutant (qed (mas a) little bigsym
                       (lambda (head tail) `(scons ,head ,tail)))))
         (ecase (cap a)
           (2 `(let ((,bigsym (head ,big)))
                 ,(funcall kons mutant `(tail ,big))))
           (3 `(let ((,bigsym (tail ,big)))
                 ,(funcall kons `(head ,big) mutant))))))))

(defun econs (ax big head tail)
  (declare (ignore ax big))
  (scons head tail))

(defun qed-root (ax little big)
  (let* ((bigsym  (gensym))
         (bigform (qf big))
         (body    (qed ax (qf little) bigsym
                       (lambda (head tail)
                         `(econs ,ax ,bigsym ,head ,tail)))))
    `(let ((,bigsym ,bigform))
       ,body)))

(defun q10 (a)
  (splash a (spec big)
    (splash spec (ax little)
      (etypecase ax
        (constant-cell +crash+)
        (constant-atom (qed-root (constant-atom-num ax) little big))
        (fixnum (case ax
                  (0 +crash+)
                  (1 `(progn
                        ,(qf big)
                        ,(qf little)))
                  (t (qed-root ax little big))))))))

(defun q11 (a)
  (splash a (hint next-formula)
    (let ((next (qf next-formula)))
      (etypecase hint
        ; no currently supported static hints
        ((or fixnum constant-atom) next)
        ; no dynamic hints either
        (constant-cell
          (split hint (tag clue-formula)
            (declare (ignore tag))
            (let ((clue (qf clue-formula)))
              `(progn ,clue ,next))))))))

(defun q12 (a)
  (declare (ignore a))
  +crash+)

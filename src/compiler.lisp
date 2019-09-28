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
              (let ((form `(lambda (a)
                             (declare (ignorable a))
                             ,(qcell a))))
                (compile nil form))))))

(defun nock-meta (a)
  (or (constant-cell-nock a)
      (setf (constant-cell-nock a)
            (make-nock-meta
              (let ((op (constant-cell-head a))
                    (ar (constant-cell-tail a)))
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

(defun nc (a)
  (etypecase a
    (constant-cell nil)
    ((or fixnum constant-atom) +crash+)))

(defun q2 (a)
  (or (nc a)
      (let ((subject (qf (constant-cell-head a)))
            (formula (qf (constant-cell-tail a))))
        `(nock ,subject ,formula))))

(defun deep (a)
  (if (cellp a) 0 1))

(defun q3 (a)
  `(deep ,(qf a)))

(defun q4 (a)
  `(bump ,(qf a)))

(defun same (a b)
  (if (urbit/equality:same a b) 0 1))

(defun q5 (a)
  (or (nc a)
      `(same ,(qf (constant-cell-head a)) ,(qf (constant-cell-tail a)))))

(defun q6 (a)
  (or (nc a)
      (let ((bran (constant-cell-tail a)))
        (or (nc bran)
            (let ((test (qf (constant-cell-head a)))
                  (yes  (qf (constant-cell-head bran)))
                  (no   (qf (constant-cell-tail bran))))
              `(case ,test
                 (0 ,yes)
                 (1 ,no)
                 (t ,+crash+)))))))

(defun q7 (a)
  (or (nc a)
      (let ((one (qf (constant-cell-head a)))
            (two (qf (constant-cell-tail a))))
        `(let ((a ,one))
           ,two))))

(defun q8 (a)
  (or (nc a)
      (let ((one (qf (constant-cell-head a)))
            (two (qf (constant-cell-tail a))))
        `(let ((a (scons ,one a)))
           ,two))))

(defun q9 (a)
  (declare (ignore a))
  +crash+)

(defun q10 (a)
  (declare (ignore a))
  +crash+)

(defun q11 (a)
  (declare (ignore a))
  +crash+)

(defun q12 (a)
  (declare (ignore a))
  +crash+)

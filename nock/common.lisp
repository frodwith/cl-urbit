(defpackage #:urbit/nock/common
  (:use #:cl #:urbit/nock/data)
  (:import-from #:urbit/nock/math #:uint)
  (:export #:sum-cell #:sum-noun #:cell= #:shallow #:loob
           #:denoun #:dedata #:decons))

(in-package #:urbit/nock/common)

(defun sum-cell (cell atomic fast slow)
  (labels ((more (n stack)
             (give (head n) (cons (cons t n) stack)))
           (give (n stack)
             (multiple-value-bind (answer deep) (funcall fast n)
               (if answer
                   (take answer stack)
                   (if deep
                       (more n stack)
                       (take (funcall atomic n) stack)))))
           (take (r stack)
             (if (null stack)
                r
                (let ((top (car stack)))
                  (if (car top)
                      (let ((c (cdr top)))
                        (setf (car top) nil
                              (cdr top) (cons c r))
                        (give (tail c) stack))
                      (destructuring-bind (c . h) (cdr top)
                        (take (funcall slow c h r) (cdr stack))))))))
    (more cell nil)))

; for comparison, a simple recursive version
;(defun sum-cell (cell atomic fast slow)
;  (labels 
;    ((rec (n)
;       (multiple-value-bind (answer deep) (funcall fast n)
;         (or answer
;             (if deep
;                 (funcall slow n (rec (head n)) (rec (tail n)))
;                 (funcall atomic n))))))
;    (rec cell)))

(defun sum-noun (noun atomic fast slow)
  (multiple-value-bind (answer deep) (funcall fast noun)
    (or answer 
        (if deep
            (sum-cell noun atomic fast slow)
            (funcall atomic noun)))))

(defun loob (bool)
  (if bool 0 1))

; helper for cell=, to convert a boolan value to :same or :diff when there is
; no possibility of :deep
(defun shallow (bool)
  (if bool :same :diff))

; structurally compare two non-eq cells (a b)
; adeep/ahead/atail will be used to traverse a, and likewise for the b arguments
; atomic will be called to compare inner atoms, and should unify
; unify will be called (with a b order preserved) on equal inner cells
;   (the caller must unify the outermost a b)
; fast will be called to short-circuit compare cells (a b order preserved).
;   * it will not be called on the outermost (a b) pair because the caller often
;     knows some of the conditions for the outermost comparison. If the
;     conditions are identical, the caller can pre-call fast.
;   * it should return one of:
;     :same (cells are equal, unify will be called)
;     :diff (cells are unequal)
;     :deep (cells must be structurally compared)
(defun cell= (a b adeep bdeep ahead bhead atail btail atomic unify fast)
  (labels ((main (a b stack)
             (give (funcall ahead a)
                   (funcall bhead b)
                   (cons (cons t (cons (funcall atail a)
                                       (funcall btail b)))
                         stack)))
           (more (a b stack)
             ; add a unify frame (skip for outermost layer)
             (main a b (cons (cons nil (cons a b)) stack)))
           (give (a b stack)
             (let ((ad (funcall adeep a))
                   (bd (funcall bdeep b)))
               (if ad
                   (when bd
                     (if (eq a b)
                         (take stack)
                         (ecase (funcall fast a b)
                           (:diff nil)
                           (:deep (more a b stack))
                           (:same 
                             (funcall unify a b)
                             (take stack)))))
                   (when (and (not bd)
                              (or (eql a b)
                                  (funcall atomic a b)))
                     (take stack)))))
           (take (stack)
             (if (null stack)
                 t
                 (destructuring-bind ((fresh a . b) . stack) stack
                   (if fresh
                       (give a b stack)
                       (progn
                         (funcall unify a b)
                         (take stack)))))))
    (main a b nil)))

(defmacro denoun ((&key (head 'head)
                        (tail 'tail)
                        (int 'cl-integer)
                        (deep 'deep))
                  bindings expr &body forms)
  (labels ((binds (val items form)
             (destructuring-bind (one . more) items
               (if (null more)
                   (bind val one form)
                   (let* ((hsym (gensym "HEAD"))
                          (tsym (gensym "TAIL")))
                     `(let ((,hsym (,head ,val))
                            (,tsym (,tail ,val)))
                        ,(bind hsym one (binds tsym more form)))))))
           (bind (val item form)
             (etypecase item
               (null (let ((i (gensym)))
                       `(let ((,i ,val))
                          (declare (ignore ,i))
                          ,form)))
               (list (binds val item form))
               (symbol
                 (let ((name (symbol-name item)))
                   (cond ((string= "@" name)
                          `(if (,deep ,val)
                           (error 'atom-required :given ,val)
                           ,form))
                         ((string= "^" name)
                          `(if (,deep ,val)
                           ,form
                           (error 'cell-required :given ,val))   )
                         (t (let* ((name (symbol-name item))
                                   (start (char name 0))
                                   (pack (symbol-package item)))
                              (flet ((chop (n) (intern (subseq name n) pack)))
                                (if (char= #\@ start)
                                    (if (char= #\@ (char name 1))
                                      `(let ((,(chop 2) (,int ,val))) ,form)
                                      `(if (,deep ,val)
                                           (error 'atom-required :given ,val)
                                           (let ((,(chop 1) ,val)) ,form)))
                                    (if (char= #\^ start)
                                      `(if (,deep ,val)
                                           (let ((,(chop 1) ,val)) ,form)
                                           (error 'cell-required :given ,val))
                                      `(let ((,item ,val)) ,form))))))))))))
    (if (null bindings)
        `(progn ,expr ,@forms)
        (let* ((whole (gensym "WHOLE"))
               (body (binds whole bindings `(progn ,@forms))))
          `(let ((,whole ,expr)) ,body)))))

(defmacro dedata (bindings expr &body forms)
  `(denoun nil ,bindings ,expr ,@forms))

(defun need-int (a)
  (typecase a
    (uint a)
    (t (error 'atom-required))))

(define-condition cons-required (cell-required) ())

(defun decar (c)
  (if (consp c)
      (car c)
      (error 'cons-required)))

(defun decdr (c)
  (if (consp c)
      (cdr c)
      (error 'cons-required)))

(defmacro decons (bindings expr &body forms)
  `(denoun (:head decar :tail decdr :deep consp :int need-int)
           ,bindings ,expr ,@forms))


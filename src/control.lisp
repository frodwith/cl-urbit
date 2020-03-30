(defpackage #:urbit/control
  (:use #:cl #:urbit/data)
  (:export #:sum-cell #:sum-noun #:cell= #:shallow #:if-let))

(defmacro if-let ((name value-form) true-form false-form)
  `(let ((,name ,value-form))
     (if ,name
         ,true-form
         ,false-form)))

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

(defun sum-noun (noun atomic fast slow)
  (multiple-value-bind (answer deep) (funcall fast noun)
    (or answer 
        (if deep
            (sum-cell noun atomic fast slow)
            (funcall atomic noun)))))

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
                                       (funcall atail b)))
                         stack)))
           (more (a b stack)
             ; add a unify frame (skip for outermost layer)
             (main a b (cons (cons nil (cons a b) stack))))
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
                              (funcall atomic a b))
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

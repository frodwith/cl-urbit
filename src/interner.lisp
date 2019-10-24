(defpackage #:urbit/interner
  (:use :cl)
  (:import-from :urbit/util :cache-hash)
  (:import-from :urbit/atom :atomp :learn-integer :to-integer)
  (:import-from :urbit/cell :learn-constant-cell :get-constant-cell :head :tail)
  (:import-from :urbit/data/constant-atom :make-constant-atom :constant-atom)
  (:import-from :urbit/data/constant-cell :constant-cell 
                :make-constant-cell :constant-cell-num)
  (:export :noun-interner :make-noun-interner :intern-with))

(defparameter *interner* nil)

(defgeneric unique (obj))

(defgeneric learn-unique (obj))
(defmethod learn-unique (obj u)
  nil)

(defun intern-with (interner noun &optional mug)
  (let ((*interner* interner))
    (intern-noun noun &optional mug)))

(defstruct (noun-interner (:constructor cons-interner (atoms cells)))
  (atoms nil :type hash-table)
  (cells nil :type hash-table))

; weakness is SBCL-only
(defun make-atom-table ()
  (make-hash-table :test 'eql :weakness :value))

(defun make-cell-table ()
  (make-hash-table :test 'equal :weakness :value))

(defun make-noun-interner ()
  (cons-interner (make-atom-table) (make-cell-table)))

(defun intern-atom (i &optional mug)
  (if (typep i 'fixnum)
      i
      (cache-hash i (noun-interner-atoms *interner*)
        (make-constant-atom i mug))))

; call with INTERNED head and tail
(defun intern-cell (head tail &optional mug)
  (cache-hash (cons head tail) (noun-interner-cells *interner*)
    (make-constant-cell head tail mug)))

; XX: use noun/sum?
; also, do we have to intern left and right every time?
; can't we do something with mug/equality?
; maybe not - equality is still traversal?
; seems like there are some possible hashtable lookups avoided by doing
; mug/equality.
(defun intern-noun (n mug)
  (if (atomp n)
      (if (typep n 'constant-atom)
          n
          (let* ((i (to-integer n))
                 (mug (or mug (cached-mug n)))
                 (k (intern-atom i mug)))
            (learn-integer n (to-integer k))
            k))
      (let ((cc (get-constant-cell n)))
        (or cc
            (let* ((head (intern-noun (head n)))
                   (tail (intern-noun (tail n)))
                   (mug  (or mug (cached-mug n)))
                   (k (intern-cell head tail mug)))
              (learn-constant-cell n k)
              k)))))

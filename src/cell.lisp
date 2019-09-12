(defpackage cl-urbit/noun/cell
 (:use :cl)
 (:import-from cl-urbit/unify :unify))

(in-package :cl-urbit/noun/cell)

(defgeneric cellp (a))
(defgeneric head (a))
(defgeneric tail (a))

;; fast-cell= should be specialized if you have a shortcut for known types.
;; mismatching noun type, eq, and mug shortcuts are applied in elsewhere,
;; so do not duplicate those checks in specializations.
;; Return values are yes, no, or maybe (symbols)
(defgeneric fast-cell= (a b))
(defmethod fast-cell= ((a t) (b t))
 'maybe)

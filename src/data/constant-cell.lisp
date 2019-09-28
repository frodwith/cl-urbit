(defpackage #:urbit/data/constant-cell
  (:use :cl)
  (:import-from :urbit/noun :noun)
  (:import-from :urbit/error :oops)
  (:import-from :urbit/mug :mug :cached-mug :murmug-two :learn-mug)
  (:import-from :urbit/cell :cellp :head :tail
                :get-constant-cell :learn-constant-cell :print-cell)
  (:import-from :urbit/data/constant-atom :constant-atom)
  (:import-from :urbit/equality :teach))

(in-package :urbit/data/constant-cell)

(defstruct (nock-meta (:constructor make-nock-meta (form)))
  (form nil :type (or list symbol))
  (func nil :type (or null (function (noun) noun))))

(defstruct (constant-cell (:constructor make-constant-cell (head tail mug))
                          (:print-object print-constant-cell))
  (head nil :type (or fixnum constant-atom constant-cell) :read-only t)
  (tail nil :type (or fixnum constant-atom constant-cell) :read-only t)
  (mug  nil :type (or null (unsigned-byte 31)))
  (nock nil :type (or null nock-meta)))

(defmethod cellp ((a constant-cell))
  t)

(defmethod head ((a constant-cell))
  (constant-cell-head a))

(defmethod tail ((a constant-cell))
  (constant-cell-tail a))

(defmethod compute-mug ((a constant-cell))
  (setf (constant-cell-mug a)
        (murmug-two (mug (constant-cell-head a))
                    (mug (constant-cell-tail a)))))

(defmethod cached-mug ((a constant-cell))
  (constant-cell-mug a))

(defmethod learn-mug ((a constant-cell) mug)
  (setf (constant-cell-mug a) mug))

(defmethod teach ((a constant-cell) b)
  (let ((m (constant-cell-mug a)))
    (unless (null m) (learn-mug b m)))
  (learn-constant-cell b a))

(defmethod unify ((a constant-cell) (b constant-cell))
  (error 'oops))

(defmethod get-constant-cell ((a constant-cell))
  a)

(defun print-constant-cell (a out)
  (print-cell (constant-cell-head a) (constant-cell-tail a) out))

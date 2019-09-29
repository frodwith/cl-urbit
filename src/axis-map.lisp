(defpackage #:urbit/axis-map
  (:use :cl)
  (:import-from :urbit/error :oops)
  (:import-from :urbit/math :cap :mas)
  (:export :value :left :right :insert :lookup))

(in-package :urbit/axis-map)

; could save space by representing empty bits in the nodes

(defstruct (node (:constructor make-node (value left right)))
  (value nil :read-only t)
  (left nil :type (or null node) :read-only t)
  (right nil :type (or null node) :read-only t))

(defun value (node)
  (and node (node-value node)))

(defun left (node)
  (and node (node-left node)))

(defun right (node)
  (and node (node-right node)))

(defun insert (node axis value)
  (declare (type integer axis))
  (case axis
    (0 (error 'oops))
    (1 (make-node value (left node) (right node)))
    (t (let ((more (mas axis)))
         (ecase (cap axis)
           (2 (make-node (value node)
                         (insert (left node) more value)
                         (right node)))
           (3 (make-node (value node)
                         (left node)
                         (insert (right node) more value))))))))

(defun lookup (node axis)
  (declare (type integer axis))
  (if (<= axis 0)
    (error 'oops))
    (loop when (null node) do (return nil)
          when (= 1 axis)  do (return (node-value node))
          do (setq node (ecase (cap axis)
                          (2 (node-left node))
                          (3 (node-right node))))
          do (setq axis (mas axis))))

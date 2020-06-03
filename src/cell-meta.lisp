(defpackage #:urbit/cell-meta
  (:use #:cl #:urbit/data #:urbit/mug #:urbit/ideal)
  (:import-from #:alexandria #:when-let)
  (:export #:cell-meta #:define-cell-methods))

(in-package #:urbit/cell-meta)

; for defining "dumb" cells - you need a place to store a head, tail,
; and a meta object. DEFINE-CELL-METHODS will do the rest.

(deftype cell-meta ()
  '(or null mug core (cons mug core) icell))

(defmacro define-cell-methods (klass head tail meta)
  `(progn
     (defmethod deep ((c ,klass))
       t)

     (defmethod cached-mug ((c ,klass))
       (let ((m (,meta c)))
         (etypecase m
           (null nil)
           (mug m)
           (core nil)
           (cons (car m))
           (icell (icell-mug m)))))

     (defmethod (setf cached-mug) (val (c ,klass))
       (let ((m (,meta c)))
         (typecase m
           (null (setf (,meta c) val))
           (core (setf (,meta c) (cons val m))))))

     (defmethod cached-ideal ((c ,klass))
       (let ((m (,meta c)))
         (typecase m
           (icell m))))

     (defmethod (setf cached-ideal) ((val icell) (c ,klass))
       (let ((m (,meta c)))
         (when-let (spd (typecase m
                          (core m)
                          (cons (cdr m))))
           (setf (icell-speed val) spd))
         (setf (,head c) (icell-head val))
         (setf (,tail c) (icell-tail val))
         (setf (,meta c) val)
         val))

     (defmethod head ((c ,klass))
       (,head c))

     (defmethod (setf head) (val (c ,klass))
       (let ((m (,meta c)))
         (when (typep m '(or null mug))
           (setf (,head c) val)))
       val)

     (defmethod tail ((c ,klass))
       (,tail c))

     (defmethod (setf tail) (val (c ,klass))
       (let ((m (,meta c)))
         (unless (typep m 'icell)
           (setf (,tail c) val)))
       val)

     ; assumption: if we have a cached speed, our head is already ideal
     (defmethod cached-battery ((c ,klass))
       (let ((m (,meta c)))
         (typecase m
           (icell (icell-head m))
           ((or cons core) (,head c)))))

     (defmethod (setf cached-battery) (val (c ,klass))
       (setf (,head c) val))

     (defmethod cached-speed ((c ,klass))
       (let ((m (,meta c)))
         (typecase m
           (core m)
           (cons (cdr m))
           (icell (icell-speed m)))))

     (defmethod (setf cached-speed) (val (c ,klass))
       (let ((m (,meta c)))
         (typecase m
           ((or null core) (setf (,meta c) val))
           (mug (setf (,meta c) (cons m val)))
           (cons (setf (cdr m) val))
           (icell (setf (icell-speed m) val)))))))

(defpackage #:urbit/nock/bignum-meta
  (:use #:cl #:urbit/nock/data #:urbit/nock/mug #:urbit/nock/ideal)
  (:export #:bignum-meta #:define-atom-methods))

(in-package #:urbit/nock/bignum-meta)

(deftype bignum-meta ()
  '(or null mug iatom))

(defmacro define-atom-methods (klass num meta)
  `(progn
     (defmethod deep ((a ,klass))
       nil)

     (defmethod cached-mug ((a ,klass))
       (let ((m (,meta a)))
         (etypecase m
           (null nil)
           (mug m)
           (iatom (iatom-mug m)))))

     (defmethod (setf cached-mug) (val (a ,klass))
       (declare (mug val))
       (let ((m (,meta a)))
         (when (null m)
           (setf (,meta a) val))))

     (defmethod cached-ideal ((a ,klass))
       (let ((m (,meta a)))
         (and (typep m 'iatom) m)))    

     (defmethod (setf cached-ideal) ((val iatom) (a ,klass))
       (setf (,meta a) val))

     (defmethod cl-integer ((a ,klass))
       (the bignum (,num a)))

     (defmethod (setf cl-integer) (val (a ,klass))
       (setf (,num a) val))))

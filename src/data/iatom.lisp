(defpackage #:urbit/data/iatom
  (:use #:cl #:urbit/data #:urbit/ideal))

(in-package #:urbit/data/iatom)

(defmethod deep ((a iatom))
  nil)

(defmethod cl-integer ((a iatom))
  (iatom-int a))

(defmethod cached-mug ((a iatom))
  (iatom-mug a))

(defmethod cached-ideal ((a iatom))
  a)

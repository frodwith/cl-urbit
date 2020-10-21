(defpackage #:urbit/nock/data/iatom
  (:use #:cl #:urbit/nock/data #:urbit/nock/ideal))

(in-package #:urbit/nock/data/iatom)

(defmethod deep ((a iatom))
  nil)

(defmethod cl-integer ((a iatom))
  (iatom-int a))

(defmethod cached-mug ((a iatom))
  (iatom-mug a))

(defmethod cached-ideal ((a iatom))
  a)

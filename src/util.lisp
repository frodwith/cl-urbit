(in-package #:urbit/util)

(defmacro cachef (accessor-form &body builder-forms)
  `(or ,accessor-form
       (setf ,accessor-form ,@builder-forms)))

(defmacro cache-field (obj accessor &body forms)
  (let ((objname (gensym)))
    `(let ((,objname ,obj))
       (cachef (,accessor ,objname) ,@forms))))

(defmacro cache-hash (key table &body builder-forms)
  (let ((keyname (gensym))
        (tablename (gensym)))
    `(let ((,keyname ,key)
           (,tablename ,table))
       (cachef (gethash ,keyname ,tablename) ,@builder-forms))))

(defmacro slot-etypecase (obj accessor (name) &body forms)
  `(let ((,name (,accessor ,obj)))
     (etypecase ,name
       ,@forms)))

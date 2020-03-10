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

(defmacro if-let ((name test-form) true-form false-form)
  `(let ((,name ,test-form))
     (if ,name
         ,true-form
         ,false-form)))

(defmacro when-let ((name test-form) &body forms)
  `(if-let (,name ,test-form)
     (progn ,@forms)
     nil))

(defmacro unless-let ((name test-form) &body forms)
  `(if-let ((,name ,test-form))
     nil
     (progn ,@forms)))


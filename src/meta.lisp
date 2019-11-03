(in-package #:urbit/meta)

; All our data is nouns. Nouns are very simple. The way we use nouns is less
; simple. We might need to cache several kinds of information about a noun, but
; we don't want a bunch of empty slots cluttering up every object, so we have
; different representations and generic accessors. For each kind of metadata we
; want to cache, we have three generic methods (cached, compute, and learn) and
; a wrapper function that arranges to call them correctly when you just want the
; data (whether it's cached or not).

(defun spfx (prefix suffix)
  (intern (format nil "~a-~a" prefix suffix)))

; TODO move to util
(defmacro if-let ((name test-form) true-form false-form)
  `(let ((,name ,test-form))
     (if ,name
         ,true-form
         ,false-form)))

(defmacro when-let ((name test-form) &body forms)
  `(if-let ((,name ,test-form))
     (progn ,@forms)
     nil))

(defmacro unless-let ((name test-form) &body forms)
  `(if-let ((,name ,test-form))
     nil
     (progn ,@forms)))

(defmacro defnoun-meta (name)
  (let* ((arg (gensym))
         (arg2 (gensym))
         (temp (gensym))
         (suffix (symbol-name name))
         (learn-name (spfx "LEARN" suffix)) 
         (cached-name (spfx "CACHED" suffix))
         (compute-name (spfx "COMPUTE" suffix))
         (unify-name (spfx "UNIFY" suffix)))
    `(progn
       (defgeneric ,cached-name (noun)
         (:method (obj) nil))
       (defgeneric ,learn-name (noun data)
         (:method (obj data) nil))
       (defgeneric ,compute-name (noun)
         (:method (obj) (error 'oops)))
       (defun ,unify-name (,arg ,arg2)
         `(if-let (,temp (,cached-name ,arg))
            (,learn-name ,arg2 ,temp)
            (when-let (,temp (,cached-name ,arg2))
              (,learn-name ,arg ,temp))))
       (defun ,name (,arg)
         (or (,cached-name ,arg)
             (let ((,temp (,compute-name ,arg)))
               (,learn-name ,arg ,temp)
               ,temp))))))

; representations with "flexible" slots will find this macro useful
(defmacro meta-case (meta (meta-name) &body clauses)
  `(let ((,meta-name ,meta))
     (typecase ,meta-name
       ,@clauses
       (t (error 'oops)))))

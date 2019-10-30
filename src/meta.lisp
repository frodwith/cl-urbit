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

(defmacro defnoun-meta (name)
  (let* ((arg (gensym))
         (temp (gensym))
         (suffix (symbol-name name))
         (learn-name (spfx "LEARN" suffix)) 
         (cached-name (spfx "CACHED" suffix))
         (compute-name (spfx "COMPUTE" suffix)))
    `(progn
       (defgeneric ,cached-name (noun)
         (:method (obj) nil))
       (defgeneric ,learn-name (noun data)
         (:method (obj data) nil))
       (defgeneric ,compute-name (noun)
         (:method (obj) (error 'oops)))
       (defun ,name (,arg)
         (or (,cached-name ,arg)
             (let (,temp (,compute-name ,arg))
               (,learn-name ,arg ,temp)
               ,temp))))))

; representations with "flexible" slots will find this macro useful
(defmacro meta-case (meta (meta-name) &body clauses)
  `(let ((,meta-name ,meta))
     (typecase ,meta-name
       ,@clauses
       (t (error 'oops)))))

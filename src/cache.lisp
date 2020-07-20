(defpackage #:urbit/cache
  (:use #:cl #:urbit/equality)
  (:import-from #:alexandria #:when-let)
  (:export #:make-cache #:cache-get #:cache-put #:cache-lookup))

(in-package #:urbit/cache)

; so far caches just have noun keys
(defstruct (cache (:constructor mkcache (slots table)))
  ; goes down to zero as the cache fills up
  (slots 0 :type (integer 0)) 
  ; we use this like a cursor so we can pause iterations around the table
  (clock nil :type list)
  ; the values are (warm-bool . value)
  (table nil :type hash-table :read-only t))

(defun make-cache (size test)
  (mkcache size (make-hash-table :test test)))

; we cycle around all the entries in the table, marking warm things cold
; until we find a cold thing and remove it from the table
(defun evict (cache)
  (let ((table (cache-table cache)))
    (loop named outer 
          do (loop for ((key . node) . more) on (cache-clock cache)
                   do (if (car node)
                          (setf (car node) nil)
                          (progn
                            (setf (cache-clock cache) more)
                            (remhash key table)
                            (return-from outer))))
          do (setf (cache-clock cache) 
                   (loop for k being the hash-keys of table
                         using (hash-value v)
                         collect (cons k v))))))

(defun cache-put (cache key value)
  (let ((slots (cache-slots cache)))
    (if (zerop slots)
        (evict cache)
        (setf (cache-slots cache) (1- slots)))
    (setf (gethash key (cache-table cache)) (cons t value))
    value))

(defun cache-get (cache key)
  (when-let (node (gethash key (cache-table cache)))
    ; the act of looking up a key causes it to be marked warm
    (setf (car node) t)
    (cdr node)))

(defmacro cache-lookup (cache key compute-form)
  (let ((c (gensym))
        (k (gensym)))
    `(let ((,c ,cache)
           (,k ,key))
       (or (cache-get ,c ,k)
           (cache-put ,c ,k ,compute-form)))))

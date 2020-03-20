(defpackage #:urbit/dostack
  (:use #:cl)
  (:export #:dostack #:dostack-accumulate
           #:main #:more #:less #:give #:take))

(in-package #:urbit/dostack)

(defmacro dostack-prim (&key main-forms give-forms take-forms
                        (block-name nil)
                        (give-symbol 'give)
                        (take-symbol 'take)
                        (more-symbol 'more)
                        (less-symbol 'less)
                        (take-arg-symbol 'top)
                        (give-arg-symbol 'gift))
  (declare (symbol give-symbol take-symbol more-symbol less-symbol
                   take-arg-symbol give-arg-symbol))
  (declare (type (or null symbol) block-name))
  (when (some #'null (list main-forms give-forms take-forms))
    (error "missing required form"))
  (destructuring-bind (stack gift give take)
    (loop for str in '("STACK-" "GIFT-" "GIVE-" "TAKE-")
          collect (gensym str))
    `(macrolet ((,more-symbol (form)
                  `(push ,form ,',stack))
                (,less-symbol ()
                  `(pop ,',stack))
                (,give-symbol (form) 
                  `(progn
                     (setq ,',gift ,form)  
                     (go ,',give)))
                (,take-symbol ()
                  `(go ,',take)))
       (let ((,stack nil)
             (,gift nil))
         (block ,block-name
           (tagbody ,@main-forms
                    (error "dostack-main fell through")
                    ,give (let ((,give-arg-symbol ,gift))
                            ,@give-forms)
                    (error "dostack-give fell through")
                    ,take (unless (null ,stack) ; form evals to nil on empty
                            (let ((,take-arg-symbol (car ,stack)))
                              ,@take-forms
                              (error "dostack-take fell through")))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-dostack-forms (forms)
    (let* ((plist (loop for f in forms
;                        collect (intern (symbol-name (car f)) "KEYWORD")
; make them actually use the give/take symbols from this package              
                        collect (car f)
                        collect (cdr f)))
           (main (getf plist 'main))
           (give (getf plist 'give))
           (take (getf plist 'take)))
      (values main give take))))

(defmacro dostack (&body forms)
  (multiple-value-bind (main give take) (parse-dostack-forms forms)
    `(dostack-prim :main-forms ,main
                   :give-forms ,(cdr give)
                   :take-forms ,(cdr take)   
                   :give-arg-symbol ,(caar give)
                   :take-arg-symbol  ,(caar take))))

(defmacro dostack-accumulate (&body forms)
  (let ((accum (gensym "ACCUMULATOR-"))
        (htake (gensym "HIDDEN-TAKE-")))
    (multiple-value-bind (main give take) (parse-dostack-forms forms)
      `(macrolet ((take (form)
                    `(progn
                       (setq ,',accum ,form)
                       (,',htake))))
         (let ((,accum nil))
           (progn (dostack-prim :main-forms ,main
                                :give-forms ,(cdr give)
                                :take-forms ((let ((,(caar take) ,accum))
                                               ,@(cdr take)))
                                :take-symbol ,htake
                                :give-arg-symbol ,(caar give)
                                :take-arg-symbol ,(cadar take))
                  ,accum))))))

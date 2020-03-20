(defpackage #:urbit/equality)

(defun fixnum=noun (i n)
  (declare (fixnum i))
  (when (= i (cl-integer n))
    (setf (cached-ideal n) i)))

(defun iatom=noun (i n)
  (declare (iatom i))
  (and (not (deep n))
       (when (= (iatom-int i) (cl-integer n))
         (setf (cached-ideal n) i))))

(defun icell=noun (i n)
  (declare (icell i))
  ; loop similar to icell=mugged
  )

(defun ideal=noun (i n)
  (funcall (etypecase i
             (fixnum #'fixnum=noun)
             (iatom #'iatom=noun)
             (icell #'icell=noun))
           i n))

(defun atom=atom (a b)
  (when (= (cl-integer a) (cl-integer b))
    (teach a b)
    (teach b a)))

(defun noun=noun (a b)
  (if (deep a)
      (and (deep b) (cell=cell a b))
      (and (not deep b) (atom=atom a b))))

(defun same (a b)
  (or (eql a b)
      (let ((ai (cached-ideal a))
            (bi (cached-ideal b)))
        (if ai
            (and (not bi)
                 (ideal=noun ai bi))
            (if bi
                (ideal=noun bi ai)
                (noun=noun ai bi))))))

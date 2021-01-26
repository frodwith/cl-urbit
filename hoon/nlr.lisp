(defpackage #:urbit/hoon/nlr
  (:use #:cl #:named-readtables
        #:urbit/nock/data #:urbit/hoon/syntax #:urbit/nock/common
        #:urbit/hoon/ord #:urbit/hoon/list #:urbit/nock/equality)
  (:export #:get-by #:put-by #:del-by
           #:has-in #:put-in #:del-in))

(in-package #:urbit/hoon/nlr)
(in-readtable hoon)

(defun del-by (map key)
  (labels
    ((del (n l r)
       (cond ((empty l) r)
             ((empty r) l)
             ((mor (head (head l)) (head (head r)))
              (dedata (nl ll rl) l
                [nl ll (del n rl r)]))
             (t (dedata (nr lr rr) r
                  [nr (del n l lr) rr]))))
     (rec (a)
       (if (empty a)
           ~
           (dedata (n l r) a
             (let ((p (head n)))
               (cond ((same p key) (del n l r))
                     ((gor key p) [n (rec l) r])
                     (t [n l (rec r)])))))))
    (rec map)))

(defun get-by (map key)
  (labels
    ((rec (a)
       (if (empty a)
           ~
           (let* ((n (head a))
                  (p (head n)))
             (if (same key p)
                 [~ (tail n)]
                 (rec
                   (let ((down (tail a)))
                     (if (gor key p)
                         (head down)
                         (tail down)))))))))
    (rec map)))

(defun put-by (map key val)
  (labels
    ((rec (a)
       (if (empty a)
           [[key val] ~ ~]
           (let* ((n (head a))
                  (p (head n)))
             (if (same p key)
                 (if (same (tail n) val)
                     (return-from put-by map)
                     [[p val] (tail a)])
                 (let ((down (tail a)))
                   (if (gor key p)
                       (let ((d (rec (head down)))
                             (r (tail down)))
                         (if (mor p (head (head d)))
                             [n d r]
                             (dedata (nd ld rd) d
                               [nd ld n rd r])))
                       (let ((d (rec (tail down)))
                             (l (head down)))
                         (if (mor p (head (head d)))
                             [n l d]
                             (dedata (nd ld rd) d
                               [nd [n l ld] rd]))))))))))
    (rec map)))

(defun has-in (set key)
  (labels
    ((rec (a)
       (unless (empty a)
         (let ((n (head a)))
           (or (same n key)
               (let ((down (tail a)))
                 (rec (if (gor key n)
                          (head down)
                          (tail down)))))))))
    (rec set)))

(defun put-in (set key)
  (labels
    ((rec (a)
       (if (empty a)
           [key ~ ~]
           (let ((n (head a)))
             (if (same n key)
                 (return-from put-in set)
                 (let ((down (tail a)))
                   (if (gor key n)
                       (let ((d (rec (head down)))
                             (r (tail down)))
                         (if (mor n (head d))
                             [n d r]
                             (dedata (nd ld rd) d
                               [nd ld n rd r])))
                       (let ((d (rec (tail down)))
                             (l (head down)))
                         (if (mor n (head d))
                             [n l d]
                             (dedata (nd ld rd) d
                               [nd [n l ld] rd]))))))))))
    (rec set)))

(defun del-in (set key)
  (labels
    ((del (n l r)
       (cond ((empty l) r)
             ((empty r) l)
             ((mor (head l) (head r))
              (dedata (nl ll rl) l
                [nl ll (del n rl r)]))
             (t (dedata (nr lr rr) r
                  [nr (del n l lr) rr]))))
     (rec (a)
       (if (empty a)
           ~
           (dedata (n l r) a
             (cond ((same n key) (del n l r))
                   ((gor key n) [n (rec l) r])
                   (t [n l (rec r)]))))))
    (rec set)))

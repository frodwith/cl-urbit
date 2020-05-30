(defpackage #:urbit/tests/speed
  (:use #:cl #:fiveam
        #:urbit/syntax #:urbit/tests #:urbit/ideal #:urbit/jets))

(in-package #:urbit/tests/speed)

(def-suite speed-tests
           :description "test the functions on (core) speed objects"
           :in all-tests)

(in-suite speed-tests)

(enable-syntax)

(test valid
  (is (speed-valid :void))
  (is (speed-valid (make-assumption)))
  (let ((expired (make-assumption)))
    (setf (assumption-valid expired) nil)
    (is (not (speed-valid expired))))
  (let* ((w (make-world))
         (root (find-ideal w [[1 %foo] %foo]))
         (root-stencil (install-root-stencil w %foo root 0)))
    (is (speed-valid root-stencil))
    (let* ((kid [[1 42] root])
           (kspd (get-speed w kid)))
      (is (typep kspd 'mean))
      (is (speed-valid kspd))
      (let ((kid-stencil (install-child-stencil
                           w %kid (get-battery w kid) 1 root-stencil 0)))
        (is (not (speed-valid kspd)))
        (setq kspd (get-speed w kid))
        (is (typep kspd 'fast))
        (is (eq kspd kid-stencil)))
      (let* ((root2 (find-ideal w [[1 %foo] %bar]))
             (r2spd (get-speed w root2))
             (kid2 [[1 42] root2])
             (k2spd (get-speed w kid2))) 
        (is (typep r2spd 'slug))
        (is (speed-valid r2spd))
        (is (typep k2spd 'slow))
        (is (speed-valid k2spd))
        (let ((root2-stencil (install-root-stencil w %bar root2 0)))
          (is (not (speed-valid r2spd)))
          (setq r2spd (get-speed w root2))
          (is (eq r2spd root2-stencil))
          (is (not (speed-valid k2spd)))
          (setq k2spd (get-speed w kid2))
          (is (typep k2spd 'spry))
          (is (speed-valid k2spd))
          (let ((k2-stencil (install-child-stencil
                              w %kid2 (get-battery w kid2) 1 root2-stencil 0)))
            (is (not (speed-valid k2spd)))
            (setq k2spd (get-speed w kid2))
            (is (typep k2spd 'fast))
            (is (eq k2spd k2-stencil))))))))

(run! 'speed-tests)

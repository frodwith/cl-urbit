(defpackage #:urbit/tests/zig
  (:use #:cl #:fiveam #:urbit/tests #:urbit/zig))

(in-package #:urbit/tests/zig)

(def-suite zig-tests
           :description "test functions on zigs (axis-as-bitvector)"
           :in all-tests)

(in-suite zig-tests)

(test axis->zig
  (is (equal #* (axis->zig 1))))

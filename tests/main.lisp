(defpackage envy-ningle/tests/main
  (:use :cl
        :envy-ningle
        :rove))
(in-package :envy-ningle/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :envy-ningle)` in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
  (format t "Testing~%")
    (ok (= 1 1))))

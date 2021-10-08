#;
(exn-pred 2)
#lang typed/racket


(assert-typecheck-fail
 (+ 1 2))
(ann (assert-typecheck-fail
      (+ 3 4)
      #:result 42)
     Number)

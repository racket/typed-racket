#;(exn-pred #rx"Expected a type check error!"
            #rx"Expected a type check error!")
#lang typed/racket


(assert-typecheck-fail
 (+ 1 2))
(ann (assert-typecheck-fail
      (+ 3 4)
      #:result 42)
     Number)

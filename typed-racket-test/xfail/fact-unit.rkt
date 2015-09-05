#lang typed/racket

;; It would be nice if this program could typecheck, but because the types
;; of unannotated exports are not added to the type environment the type
;; the fact function is not inferred correctly. Adding a type annotation
;; to fact allows this program to run.

(define-signature fact^
  ([fact : (-> Integer Integer)]))

(define-unit fact@
  (import (prefix i: fact^))
  (export fact^)
  (define (fact n) 
    (if (< n 1) 
        1
        (* n (i:fact (sub1 n)))))
  fact)
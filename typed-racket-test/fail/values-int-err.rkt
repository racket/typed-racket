#;
(exn-pred #rx"(?!match:)")


#lang typed/racket/base

(: bob (-> (Values Real Real)))
(define (bob) 0)

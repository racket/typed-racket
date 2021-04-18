#;
(exn-pred #rx".*duplicate type variable.*")
#lang typed/racket
(define-syntax-rule (def id vars ...)
  (begin
    (: id (All (vars ... a) (-> vars ... a a)))
    (define (id vars ... a) a)))


(def f a)

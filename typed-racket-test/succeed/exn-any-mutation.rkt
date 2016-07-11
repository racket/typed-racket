#lang racket/base
(module untyped racket/base
  (provide f)
  (define (f x) x))
(module typed typed/racket
  (require typed/rackunit)
  (struct (X) s ([i : X]) #:mutable #:transparent)
  (require/typed (submod ".." untyped)
                 [f (-> Any (s (U Integer String)))])
  (: s1 : (s Integer))
  (define s1 (s 42))
  (define s2 (ann s1 Any))
  (define s3 (f s2))
  (check-equal? (s-i s1) 42)
  (check-equal? (s-i s3) 42)
  (check-exn #rx"Attempted to use a higher-order value passed as `Any`"
             (λ () (set-s-i! s3 "hi")))
  (check-equal? (s-i s1) 42
                "if the previous test hadn't errored, this would be \"hi\" with type Integer")
  )
(require 'typed)

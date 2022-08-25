#lang racket/base
(module untyped racket/base
  (provide f)
  (define (f x) x))
(module typed typed/racket/shallow
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
  (check-not-exn (λ () (set-s-i! s3 "hi")))
  (check-exn exn:fail:contract? (lambda () (s-i s1)))
  )
(require 'typed)

#lang racket/base

(module m typed/racket/base

  (struct foo ([x : Integer]) #:prefab)
  (define f (foo 42))
  (struct bar ([y : Integer]) #:prefab #:mutable)
  (define b (bar 43))
  (provide f foo-x b bar-y set-bar-y!))

(module n racket/base
  (require (submod ".." m))
  (display (foo-x f))
  (display (bar-y b))
  (set-bar-y! b 44)
  (unless (with-handlers ([(λ (_) #t) (λ (_) #t)])
            (set-bar-y! b "44")
            #f)
    (error "what happened!")))

(require (submod 'n))
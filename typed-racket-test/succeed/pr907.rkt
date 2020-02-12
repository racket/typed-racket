#lang racket/base

;; Test require/untyped-contract with submod paths

(module a typed/racket
  (define (f (x : (U Symbol String))) : Void (void))
  (provide f))

(module b racket/base
  (require typed/untyped-utils typed/racket/base)
  (require/untyped-contract (submod ".." a) (f (-> Symbol Void)))
  (void))
(require 'b)

(module c racket/base
  (require typed/untyped-utils typed/racket/base)
  (require/untyped-contract (submod "." ".." a) (f (-> Symbol Void)))
  (void))
(require 'c)

(require typed/untyped-utils typed/racket/base)
(require/untyped-contract (submod "." a) (f (-> Symbol Void)))

;; a non-relative submod should not be adjusted
(require/untyped-contract (submod "pr907-non-relative.rkt" m) (g Symbol))

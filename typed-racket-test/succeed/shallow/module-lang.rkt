#lang scheme/load

(module m typed/racket/shallow
  (: f (All (a) (a -> a)))
  (define (f x) x)
  (provide f))

(module n typed/racket/shallow
  (require 'm))

(require typed/racket/shallow)

(require 'n)

(current-namespace (module->namespace ''n))

(f 1)


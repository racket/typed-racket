#lang racket/base

;; Pure functions are fine predicates

(module untyped racket/base
 (define (color? x)
   (memq x '(red green blue)))
 (provide color?))

(module typed typed/racket/base
 (require/typed (submod ".." untyped)
   [#:opaque Color color?])
 (color? 'blue)
 (color? 4)
 (struct s ())
 (color? s))

(require 'typed)

#lang racket/base

;; #:opaque structs should be allowed

(module untyped racket/base
 (struct s ())
 (define val (s))
 (provide val (struct-out s)))

(module typed typed/racket/base
 (require/typed (submod ".." untyped)
   [#:opaque S s?]
   [val S])
 (s? 4)
 (s? val))

(require 'typed)

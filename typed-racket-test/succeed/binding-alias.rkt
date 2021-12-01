#lang racket/base

(module a racket/base
  (require (except-in typed/racket/base U ∩)
           (only-in typed/racket/base [U Un]
                    [∩ Intersection])))

(module b racket/base
  (require (except-in typed/racket/base U)
           (only-in typed/racket/base [U Union])))

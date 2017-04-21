#lang typed/racket/base

(provide (all-from-out racket/flonum)
         (rename-out [for/flvector: for/flvector]
                     [for*/flvector: for*/flvector]))

(require (except-in racket/flonum for/flvector for*/flvector))


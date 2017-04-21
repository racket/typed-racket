#lang typed/racket/base

(provide (all-from-out racket/extflonum)
         (rename-out [for/extflvector: for/extflvector]
                     [for*/extflvector: for*/extflvector]))

(require (except-in racket/extflonum for/extflvector for*/extflvector))


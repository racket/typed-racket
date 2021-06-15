#lang racket/base

(require
 "../utils/utils.rkt"
 "../types/numeric-tower.rkt"
 "../env/init-envs.rkt"
 "base-env-indexing-abs.rkt")

(define e (indexing -Integer))
(define (initialize-indexing) (initialize-type-env e))
(provide initialize-indexing)



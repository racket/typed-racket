#lang racket/base

(require "../env/init-envs.rkt"
         "../types/numeric-tower.rkt"
         "../utils/utils.rkt"
         "base-env-indexing-abs.rkt")

(define e (indexing -Integer))
(define (initialize-indexing) (initialize-type-env e))
(provide initialize-indexing)



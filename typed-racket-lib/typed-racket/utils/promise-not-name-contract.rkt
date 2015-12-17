#lang racket/base

(require racket/contract
         racket/promise)
(provide promise-not-name/c)
(define (promise-not-name/c x) (and/c (promise/c x) (not/c promise/name?)))

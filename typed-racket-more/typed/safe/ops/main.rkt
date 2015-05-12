#lang racket/base
(require racket/unsafe/ops)

(provide (all-defined-out))

(define safe-vector-ref unsafe-vector-ref)

;; (define safe-car unsafe-car)
;; (define safe-cdr unsafe-cdr)
;; (define safe-list-ref unsafe-list-ref)
;; (define safe-mcar unsafe-mcar)
;; (define safe-mcdr unsafe-mcdr)
;; (define safe-set-mcar! unsafe-set-mcar!)
;; (define safe-set-mcdr! unsafe-set-mcdr!)
;; (define safe-cons-list unsafe-cons-list)



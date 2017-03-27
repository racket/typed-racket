#lang racket/base

;; Provides an error handling helper for type aliases / type names

(require syntax/stx)

(provide type-name-error
         prop-name-error)

(define (type-name-error stx)
  (raise-syntax-error
   'type-check
   (format "type name used out of context\n  type: ~a\n in: ~a"
           (syntax->datum (if (stx-pair? stx) (stx-car stx) stx))
           (syntax->datum stx))
   stx
   (and (stx-pair? stx) (stx-car stx))))

(define (prop-name-error stx)
  (raise-syntax-error
   'type-check
   (format "proposition name used out of context\n  proposition: ~a\n in: ~a"
           (syntax->datum (if (stx-pair? stx) (stx-car stx) stx))
           (syntax->datum stx))
   stx
   (and (stx-pair? stx) (stx-car stx))))

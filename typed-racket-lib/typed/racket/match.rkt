#lang racket/base

(provide
  (all-from-out racket/match)
  define/match)

(require
  (except-in racket/match
    define/match)
  (rename-in typed/racket/base
    [define -define])
  (for-syntax racket/base
              syntax/parse
              syntax/parse/experimental/template
              syntax/parse/lib/function-header))

;; Copied from racket/match, replacing `define` with Typed Racket's version
(define-syntax (define/match stx)
  (syntax-parse stx
    [(_ ?header:function-header ?clause ...)
     (quasitemplate
      (-define ?header
        (match*/derived (?? ?header.params) #,stx
          ?clause ...)))]))

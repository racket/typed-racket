#lang typed/racket/base

(require (for-syntax racket/base)
         racket/splicing
         racket/stxparam)

(define-syntax-parameter foo #f)

(splicing-syntax-parameterize ([foo #t])
  (: x Number)
  (define x "string"))

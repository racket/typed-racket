#lang racket/base

(require 
  (for-syntax syntax/parse racket/base
              "renamer.rkt"
              "../utils/tc-utils.rkt"))
(provide def-export)

(define-syntax (def-export stx)
  (syntax-parse stx
    [(def-export export-id:identifier id:identifier cnt-id:identifier)
     #'(define-syntax export-id (typed-renaming (syntax-property #'id 'not-free-identifier=? #t)
                                                (syntax-property #'cnt-id 'not-free-identifier=? #t)))]))

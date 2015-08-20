#lang racket/base

(require 
  (for-syntax syntax/parse racket/base
              "renamer.rkt"
              "../utils/tc-utils.rkt"))
(provide def-export)

(define-syntax (def-export stx)
  (syntax-parse stx
    [(def-export export-id:identifier id:identifier cnt-id:identifier)
     (with-syntax ([(indirection) (generate-temporaries (list 1))])
       #'(begin
           (define-syntax indirection (typed-indirection #'id))
           (define-syntax export-id (typed-renaming #'indirection #'cnt-id))))]))

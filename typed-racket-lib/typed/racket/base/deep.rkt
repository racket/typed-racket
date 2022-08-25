#lang racket/base

(require
  (for-syntax racket/base)
  (except-in typed/racket/base #%module-begin #%top-interaction with-type)
  (only-in typed/racket/base
           [#%module-begin --#%module-begin]
           [#%top-interaction --#%top-interaction]
           [with-type -with-type]))

(provide
  (all-from-out typed/racket/base)
  with-type
  (rename-out [-#%module-begin #%module-begin] [-#%top-interaction #%top-interaction]))

(define-syntax (-#%module-begin stx)
  (quasisyntax/loc stx (--#%module-begin #:deep . #,(cdr (syntax-e stx)))))

(define-syntax (-#%top-interaction stx)
  (quasisyntax/loc stx (--#%top-interaction #:deep . #,(cdr (syntax-e stx)))))

(define-syntax (with-type stx)
  (quasisyntax/loc stx (-with-type . #,(cdr (syntax-e stx)))))


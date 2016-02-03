#lang typed/racket

(provide (all-defined-out))

(define-syntax (require/typed/batch stx)
  (syntax-case stx [id:]
    [(_ modpath [id: id ...] type-definition)
     #'(require/typed/batch modpath [id ...] type-definition)]
    [(_ modpath [id ...] type-definition)
     #'(require/typed modpath [id type-definition] ...)]))

(define-syntax (require/typed/provide/batch stx)
  (syntax-case stx [id:]
    [(_ modpath [id: id ...] type-definition)
     #'(require/typed/provide/batch modpath [id ...] type-definition)]
    [(_ modpath [id ...] type-definition)
     #'(require/typed/provide modpath [id type-definition] ...)]))

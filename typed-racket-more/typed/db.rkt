#lang typed/racket

(define-syntax (require/provide stx)
  (syntax-case stx []
    [(_ db.rkt ...)
     #'(begin (provide (all-from-out db.rkt)) ...
              (require db.rkt) ...)]))

(require/provide "db/base.rkt"
                 "db/sqlite3.rkt")

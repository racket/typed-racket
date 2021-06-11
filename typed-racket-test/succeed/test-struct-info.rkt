#lang racket/base
(module test racket/base
  (require (for-syntax racket/base syntax/parse racket/struct-info racket/match))

  (define-syntax (get-constr stx)
    (syntax-parse stx
      [(_ sname:id)
       (define si (extract-struct-info (syntax-local-value #'sname (lambda () #f))))
       (match-define (list* struct-desc constr rst) si)
       constr]))
    (provide get-constr))

(module typed typed/racket/base
  (require (submod ".." test))
  (require typed/rackunit)
  (struct kiwi ())
  (struct apple () #:constructor-name make-apple)
  (struct pear () #:type-name PEAR)
  (struct dragon-fruit () #:type-name DragonFruit #:constructor-name make-dragon-fruit)
  (provide (struct-out apple)
           (struct-out kiwi)
           (struct-out PEAR)
           (struct-out DragonFruit))

  (check-equal? (object-name (get-constr apple)) 'make-apple)
  (check-equal? (object-name (get-constr kiwi)) 'kiwi)
  (check-equal? (object-name (get-constr apple)) 'make-apple)
  (check-equal? (object-name (get-constr pear)) 'pear)
  (check-equal? (object-name (get-constr dragon-fruit)) 'make-dragon-fruit)
  (check-equal? (object-name (get-constr DragonFruit)) 'make-dragon-fruit)
  (check-equal? (object-name (get-constr PEAR)) 'pear))

(require rackunit)
(require 'typed)
(require 'test)
(check-equal? (object-name (get-constr apple)) 'make-apple)
(check-equal? (object-name (get-constr kiwi)) 'kiwi)
(check-equal? (object-name (get-constr apple)) 'make-apple)
(check-equal? (object-name (get-constr DragonFruit)) 'make-dragon-fruit)
(check-equal? (object-name (get-constr PEAR)) 'pear)

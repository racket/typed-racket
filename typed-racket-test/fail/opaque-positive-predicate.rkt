#lang typed/racket

(module foo racket/base
  (provide foo?)
  (define x #false)
  (define (foo? v) (set! x (not x)) x))

(require/typed 'foo [#:opaque Foo foo?])
;; foo? is a positive predicate: (-> Any Boolean : #:+ Foo)

(: no+ (∀ (T) (-> Any Boolean : #:+ T)))
(define (no+ v) #false)

(: transmute (∀ (T) (-> Any T)))
(define (transmute v)
  (if (and (or (foo? v) ((inst no+ T) v)) (not (foo? v)))
      v
      (error 'absurd)))


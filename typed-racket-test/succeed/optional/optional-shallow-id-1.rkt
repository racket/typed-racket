#lang racket/base

;; shallow should protect itself from optional

(module uuu racket/base
  (provide bad-list)
  (define bad-list '(X X)))

(module sss typed/racket/optional

  (require/typed (submod ".." uuu)
    (bad-list (Listof (List Symbol))))

  (provide f)

  (: f (-> (-> (List Symbol) Symbol) (Listof Symbol)))
  (define (f x)
    (map x bad-list)))

(module ttt typed/racket/shallow
  (require (submod ".." sss))

  (: g (-> (List Symbol) Symbol))
  (define (g n)
    (car n))

  (define (go1)
    (f g))

  (define (go2)
    ;; second function, to be sure all occurrences of `f` get a contract
    (f g))

  (provide go1 go2))

(require 'ttt rackunit)

(check-exn #rx"shape-check" go1)
(check-exn #rx"shape-check" go2)


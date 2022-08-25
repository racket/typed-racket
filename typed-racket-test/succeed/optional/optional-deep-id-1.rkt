#lang racket/base

;; deep should protect itself from optional

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

(module ttt typed/racket
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

(check-exn exn:fail:contract? go1)
(check-exn exn:fail:contract? go2)


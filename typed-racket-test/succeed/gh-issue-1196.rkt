#lang typed/racket/base

(module AAAA typed/racket/base
  (provide (all-defined-out))
  (define-type Kaka (∩ Haha* (-> Any)))
  (define-type Haha* (Intersection Kaka (-> Any))))

(require 'AAAA)

(module CONT typed/racket/base
  (provide (all-defined-out))

  (struct cont
    ([type : Symbol]
     [saved-cont : [-> Cont*]]
     [func : [-> Any Any]])
    #:property prop:procedure
    (ann (λ (self val) ((cont-func self) val))
         [-> Cont Any Any])
    #:transparent
    #:type-name Cont)
  (define-type Cont* (∩ Cont [-> Any Any])))

(require 'CONT)

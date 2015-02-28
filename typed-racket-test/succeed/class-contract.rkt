#lang racket

;; Test class contract generation

(module t1 typed/racket
  (define c%
    (class object%
      (super-new)
      (: m (All (X) (-> X (Listof X))))
      (define/public (m x) (list x))))
  (provide c%))

(module u1 racket
  (require (submod ".." t1))
  (car (send (new c%) m 3)))

(module u2 racket
  (define c%
    (class object%
      (super-new)
      (define/public (m x) (list x))))
  (provide c%))

(module t2 typed/racket
  (require/typed (submod ".." u2)
                 [c% (Class [m (All (X) (-> X (Listof X)))])])
  (car (send (new c%) m 3)))

;; ensure that inner/augment work right
(module t3 typed/racket
  (define c%
    (class object%
      (super-new)
      (: m (-> Void) #:augment (-> Integer Void))
      (define/pubment (m) (void))))
  (provide c%))

(module u3 racket
  (require (submod ".." t3))
  (new c%))

(require 'u1)
(require 't2)
(require 'u3)

#lang typed/racket
(module spec typed/racket
  ;; contract values can be exported from typed modules
  (provide pizza-order/c)
  (define pizza-order/c (->/c (integer-in 12 20)
                              (or/c 'small 'medium 'large)
                              any/c)))
(module store typed/racket
  (require (submod ".." spec))
  (provide
   (contract-out
    ;; and then required and applied by typed clients
    [order-pizza pizza-order/c]))
  (: order-pizza (-> Integer (U 'small 'medium 'large) Boolean))
  (define (order-pizza price size)
    #t))
(require 'store)
(order-pizza 12 'small)


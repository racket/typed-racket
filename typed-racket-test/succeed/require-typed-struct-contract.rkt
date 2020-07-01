#lang racket/base

(module server racket
  (provide
    (contract-out
      (struct posn ((x natural?) (y natural?)))
      (struct (posn2 posn) ([x natural?] [y natural?] [z symbol?]))))
  (struct posn [x y])
  (struct posn2 posn [z]))

(module client typed/racket
  (require/typed (submod ".." server)
    (#:struct posn ((x : Integer) (y : Integer)))
    (#:struct (posn2 posn) ((z : Symbol)))))

(require 'client)


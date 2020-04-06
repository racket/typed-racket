#lang racket/base

(module server racket
  (provide
    (contract-out
      (struct posn ((x natural?) (y natural?)))))
  (struct posn [x y]))

(module client typed/racket
  (require/typed (submod ".." server)
    (#:struct posn ((x : Integer) (y : Integer)))))

(require 'client)


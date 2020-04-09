#lang racket/base

;; Make sure struct names are regexp escaped

(module server racket
  (provide
    (contract-out
      (struct posn ((x natural?) (y natural?)))
      (struct (posn.* posn) ([x natural?] [y natural?] [z symbol?]))))
  (struct posn [x y])
  (struct posn.* posn [z]))

(module client typed/racket
  (require/typed (submod ".." server)
    (#:struct posn ((x : Integer) (y : Integer)))
    (#:struct (posn.* posn) ((z : Symbol)))))

(require 'client)


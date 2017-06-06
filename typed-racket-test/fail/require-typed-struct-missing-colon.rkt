#;
(exn-pred ".*while parsing typed-field.*")
#lang racket/base

(module server racket
  (provide (struct-out posn))
  (struct posn [x y]))

(module client typed/racket
  (require/typed (submod ".." server)
    (#:struct posn ((x Integer) (y Integer)))))

(require 'client)

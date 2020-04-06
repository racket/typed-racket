#;
(exn-pred ".*expected field name 0 to be x, but found y.*")
#lang racket/base

(module server racket
  (provide (struct-out posn))
  (struct posn [x y]))

(module client typed/racket
  (require/typed (submod ".." server)
    (#:struct posn ((y : Integer) (x : Integer)))))

(require 'client)

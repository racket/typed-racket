#;
(exn-pred ".*found 3 fields in type, but 2 fields in struct posn.*")
#lang racket/base

(module server racket
  (provide (struct-out posn))
  (struct posn [x y]))

(module client typed/racket
  (require/typed (submod ".." server)
    (#:struct posn ((x : Integer) (y : Integer) (z : Integer)))))

(require 'client)

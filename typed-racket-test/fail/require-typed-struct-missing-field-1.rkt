#;
(exn-pred ".*found 1 field in type, but 2 fields in struct posn.*")
#lang racket/base

(module server racket
  (provide (struct-out posn))
  (struct posn [x y]))

(module client typed/racket
  (require/typed (submod ".." server)
    (#:struct posn ((x : Integer)))))

(require 'client)

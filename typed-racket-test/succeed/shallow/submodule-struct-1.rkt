#lang typed/racket/shallow

(module u racket/base
  (struct posn [x y])
  (define origin (posn 0 0))
  (provide origin (struct-out posn)))

(require/typed 'u
  (#:struct posn ((x : Symbol) (y : Symbol)))
  (origin posn))
(require typed/rackunit)

(check-exn exn:fail:contract?
  (lambda () (posn-x origin)))


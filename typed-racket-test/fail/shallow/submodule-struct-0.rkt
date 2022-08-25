#;
(exn-pred exn:fail:contract? #rx"shape-check")

#lang typed/racket/shallow

(module u racket/base
  (struct posn [x y])
  (define origin (posn 0 #false))
  (provide origin (struct-out posn)))

(require/typed 'u
  (#:struct posn ((x : Real) (y : Real)))
  (origin posn))

(+ (posn-x origin) (posn-y origin))

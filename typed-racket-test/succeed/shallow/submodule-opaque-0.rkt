#lang typed/racket/shallow

;; Test opaque import

(module u racket/base
  (struct posn [x y])
  (define origin (posn 0 0))
  (provide origin (struct-out posn)))

(require/typed 'u
  (#:opaque Posn posn?)
  (origin Posn)
  (posn (-> Integer Integer Posn))
  (posn-x (-> Posn Integer)))

(+ (posn-x origin) (posn-x (posn 4 0)))


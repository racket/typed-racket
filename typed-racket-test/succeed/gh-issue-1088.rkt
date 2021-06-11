#lang typed/racket

(module foo typed/racket
  (provide (struct-out bar))
  (define-struct/exec
    bar () [(λ (self) 42) : (bar -> Integer)])
  ((bar))
  ((make-bar)))

(require 'foo)
((bar))
((make-bar))

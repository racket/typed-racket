#lang s-exp typed-racket/base-env/extra-env-lang

(module untyped racket/base
  (struct posn [x y])
  (struct color-posn posn [c])
  (provide (struct-out posn) (struct-out color-posn)))
(require 'untyped)

(type-environment
 [#:struct posn ([x : -Real] [y : -Real])]
 [#:struct (color-posn posn) ([c : -Symbol]) (-Real -Real)])


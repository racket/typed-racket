#lang s-exp typed-racket/base-env/extra-env-lang

(module untyped racket/base
  (struct posn [x y])
  (struct color-posn posn [c])
  (provide (struct-out posn) (struct-out color-posn)))
(require 'untyped)

(type-environment
 [#:struct posn ([x : -Real] [y : -Real]) #:kernel-maker posn]
 [#:struct (color-posn posn) ([c : -Symbol]) (-Real -Real) #:kernel-maker color-posn])

(module* test typed/racket
  (require (submod ".."))

  ;; make sure these have types
  (void posn posn? posn-x posn-y
        color-posn color-posn? color-posn-c))

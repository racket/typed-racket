#lang typed/racket/base
(module mod-a typed/racket/base
  (provide (all-defined-out))
  (struct (A) Foo ([a : A])))

(require 'mod-a)
(ann (Foo 10) Foo)

#;(exn-pred "type name `fruit' is unbound")
#lang typed/racket/base

(module mod1 typed/racket/base
  (provide (all-defined-out))
  (struct fruit ()  #:type-name Fruit))

(require 'mod1)
(ann (fruit) fruit)

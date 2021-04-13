#;(exn-pred "type name `fruit' is unbound")
#lang typed/racket/base

(struct fruit ()  #:type-name Fruit)
(ann (fruit) fruit)

#lang typed/racket/base

;; Type-out structs need types for each field, including parent fields

(provide (type-out
  (struct bar ([x : Natural]))
  (struct (baz bar) ([y : Boolean]))))

(struct bar ([x : Natural]))
(struct baz bar ([y : Boolean]))

#lang racket/base

;; Must define the struct before providing it

(module defstruct/type-name typed/racket/base
  (provide
    (type-out [struct bar ()])))

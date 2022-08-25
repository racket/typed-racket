#lang racket

(module typed typed/racket/shallow
  (: d Any)
  (define d (delay (lambda: ([x : Integer]) (+ x 1))))
  (provide d))

(require 'typed)

((force d) 6)


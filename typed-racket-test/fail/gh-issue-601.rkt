#;
(exn-pred #rx"make-my-struct: contract violation\n  expected: natural?")
#lang racket

(module class-helpers typed/racket
  (provide make-my-struct)

  (struct my-struct ([a : Natural])
    #:extra-constructor-name make-my-struct))

(require 'class-helpers)
(make-my-struct 'hi)

#;
(exn-pred #rx"broke its own contract.*promised: 42")
#lang typed/racket
(module server typed/racket
  (provide
   (contract-out
    [favorite-number 42]))
  (define favorite-number 23))
(require 'server)
favorite-number

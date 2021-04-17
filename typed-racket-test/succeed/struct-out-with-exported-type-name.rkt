#lang typed/racket
(module mod1 typed/racket
  (provide (all-defined-out))
  (struct apple () #:type-name ApplePlus)
  (struct banana ApplePlus ())
  (provide (struct-out ApplePlus)))


(require 'mod1)
(provide (struct-out ApplePlus))

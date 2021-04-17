#lang typed/racket
(module mod1 typed/racket
  (provide (all-defined-out))
  (struct ssh-message ([number : Byte] [name : Symbol]) #:type-name SSH-Message)

  (define-syntax (hi stx)
    (syntax/loc stx
      (struct hello ssh-message ()))))
(require 'mod1)
(hi)

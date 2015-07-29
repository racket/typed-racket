#lang typed/racket

;; Test for PR 14521, submodule with struct definition

(module untyped racket
  (provide (struct-out thing))
  (struct thing ()))

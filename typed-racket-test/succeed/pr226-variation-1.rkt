#lang typed/racket

;; Struct predicates should not be wrapped in a contract
;;  when they cross a typed/untyped boundary.
;; We know they're safe! Don't suffer the indirection cost.

(module untyped racket
  (struct s ())
  (provide (struct-out s)))

(require/typed 'untyped
  [#:struct s ()])

(require/typed racket/contract/base
  [has-contract? (-> Any Boolean)])

(when (has-contract? s?)
  (error 'pr226 "safe struct predicate was wrapped in a contract"))

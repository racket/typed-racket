#lang racket/base
(require typed/untyped-utils)
(require/untyped-contract
 (submod typed/racket)
 [identity (Integer -> Integer)])
(void (identity 5))

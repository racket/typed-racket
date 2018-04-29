#lang racket/base

(require
  typed/racket/base
  typed/untyped-utils)

(require/untyped-contract "lib.rkt"
                          (f (-> Symbol Void)))

(provide f)

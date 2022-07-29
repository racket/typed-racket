#lang typed/racket
(require racket/hash)
(hash-union (make-hash) (make-hash) #:combine (lambda (a b) a))

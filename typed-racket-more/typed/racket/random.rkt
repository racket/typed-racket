#lang typed/racket/base

;; Provides base types for racket/random

(require typed/racket/unsafe)

(unsafe-require/typed
 racket/random
 [crypto-random-bytes (-> Integer Bytes)]
 [random-ref
  (All (X)
    (->* [(Sequenceof X)] [Pseudo-Random-Generator] X))]
 [random-sample
  (All (X)
    (->* [(Sequenceof X) Integer]
         [Pseudo-Random-Generator #:replacement? Any]
         (Listof X)))])

(unsafe-provide crypto-random-bytes
                random-ref
                random-sample)

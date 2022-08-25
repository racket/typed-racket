#lang racket/base

;; Hash contracts with non-flat keys should give a good error messages
;;  when applied to hashes that are not `hash-equal?`

(module t typed/racket/base/shallow
  (provide give-me-a-hash)
  (: give-me-a-hash (-> (HashTable (Vectorof Symbol) Symbol) Symbol))
  (define (give-me-a-hash x)
    'thanks))

(require 't rackunit)

(check-not-exn
  (λ () (give-me-a-hash (hasheqv))))

(check-not-exn
  (λ () (give-me-a-hash (hasheq))))

(check-not-exn
  (λ () (give-me-a-hash (hash))))

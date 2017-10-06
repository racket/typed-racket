#lang racket/base

;; Hash contracts with non-flat keys should give a good error messages
;;  when applied to hashes that are not `hash-equal?`

(module t typed/racket/base
  (provide give-me-a-hash)
  (: give-me-a-hash (-> (HashTable (Vectorof Symbol) Symbol) Symbol))
  (define (give-me-a-hash x)
    'thanks))

(require 't rackunit)

(define err-regexp #rx"hash-equal\\?.*key contract is not a flat contract")

(check-exn err-regexp
  (λ () (give-me-a-hash (hasheqv))))

(check-exn err-regexp
  (λ () (give-me-a-hash (hasheq))))

(check-not-exn
  (λ () (give-me-a-hash (hash))))

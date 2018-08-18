#lang racket/base

;; Test custom hash contracts:
;; 1. accept & reject the right kinds of values
;; 2. don't overlap in an or/c

(module+ test
  (require rackunit
           racket/contract
           typed-racket/utils/hash-contract)

  (test-case "hash-contract:correct"
    (define H-ctc (typed-racket-hash/c symbol? symbol?))
    (define I-ctc (immutable-hash/c symbol? symbol?))
    (define M-ctc (mutable-hash/c symbol? symbol?))
    (define W-ctc (weak-hash/c symbol? symbol?))

    (define I-val (make-immutable-hash))
    (define M-val (make-hash))
    (define W-val (make-weak-hash))

    (check-not-exn
      (λ () (contract H-ctc I-val '+ '-)
            (contract H-ctc M-val '+ '-)
            (contract H-ctc W-val '+ '-)))
    (check-not-exn
      (λ () (contract I-ctc I-val '+ '-)))
    (check-not-exn
      (λ () (contract M-ctc M-val '+ '-)))
    (check-not-exn
      (λ () (contract W-ctc W-val '+ '-)))

    (check-exn exn:fail:contract?
      (λ () (contract H-ctc 4 '+ '-)))
    (check-exn exn:fail:contract?
      (λ () (contract I-ctc M-val '+ '-)
            (contract I-ctc W-val '+ '-)))
    (check-exn exn:fail:contract?
      (λ () (contract M-ctc I-val '+ '-)
            (contract M-ctc W-val '+ '-)))
    (check-exn exn:fail:contract?
      (λ () (contract W-ctc M-val '+ '-)
            (contract W-ctc I-val '+ '-))))

  (test-case "hash-contract:overlap"
    (define ctc
      (or/c (immutable-hash/c real? string?)
            (mutable-hash/c integer? string?)
            (weak-hash/c integer? string)))

    (check-not-exn
      (λ () (contract ctc (make-hash) '+ '-)))))
